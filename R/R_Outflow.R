library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(gtable)
library(sharpshootR)


#Path to local drive
root <- "~/GitHub/Summer_Fall_Action"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

#################### Read dayflow data
data_dayflow<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1997-2019.csv"))
data_dayflow$Date <- as.Date(data_dayflow$Date,"%m/%d/%Y")

#Add julian day
data_dayflow$julianday<-yday(data_dayflow$Date)

#Add water year to dayflow
data_dayflow$WY<-as.numeric(ifelse(month(data_dayflow$Date)>9,data_dayflow$Year+1,data_dayflow$Year))

#################### Add 2020 data
#According to Rosemary Hartman (DWR), Dayflow data can't be made available until 2021
#She said to use the following instead:

#I think Net Delta Outflow is CDEC station code “DTO”
#https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=DTO

#and X2 is CX2
#https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=CX2

DTO_2020 <- CDECquery(id='DTO', sensor=23, interval='D', start='2019-10-01', end='2020-10-31')
DTO_2020$Date <- as.Date(DTO_2020$datetime,"%Y-%m-%d")

#Edit 2020 data from CDEC to match Dayflow
data_outflow_2020<-DTO_2020 %>% mutate(OUT=value,WY=water_year,Year=year) %>% select(Date,OUT,Year,WY)
data_outflow_2020$Month<-month(data_outflow_2020$Date)

#Add 2020 to the dayflow data
data_dayflow_added <- dplyr::bind_rows(data_dayflow,data_outflow_2020)
#Order full data set by date
data_dayflow_added <- data_dayflow_added[order(data_dayflow_added$Date),]
#Locate start row for 2020 WY data
which(data_dayflow_added$Date=="2019-10-01")
#8401

#Calculate X2 based on DAYFLOW documentation:
###
#The 1994 Bay-Delta agreement established standards for salinity in the estuary. 
#Specifically, the standards determine the degree to which salinity is allowed 
#to penetrate up-estuary, with salinity to be controlled through delta outflow. 
#The basis for the standards is a series ofrelationships between the salinity 
#pattern and the abundance or survival of various species of fish and 
#invertebrates. These relationships have been expressed in terms of X2, 
#the distance from the Golden Gate to the point where daily average salinity is 
#2 parts per thousand at 1 meter off the bottom (Jassby et. al. 1995).
#In Dayflow, X2 is estimated using the Autoregressive Lag Model:

#X2(t) = 10.16 + 0.945*X2(t-1) – 1.487log(QOUT(t)) #(17)<-Not sure what this is, but it doesn't seem to be relevant based on Anke's notes
#NOTE: It seems like the log in the DAYFLOW notation is referring to Log10 (i.e., not ln)

#Fill in X2 data for most recent WY data
for (i in 8401:nrow(data_dayflow_added)) {
  data_dayflow_added$X2[i] = 10.16 + (0.945*(data_dayflow_added$X2[i-1]))+(-1.487*log10(data_dayflow_added$OUT[i]))
}

data_dayflow_added[c(8401),]


#################### Add Water Year information

#Data from https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
#Copy and pasted into excel
data_wateryear<-read.csv(file.path(data_root, "Dayflow", "DWR_WSIHIST.csv"))

#Add WY type information
data_dayflow_added<-left_join(data_dayflow_added,data_wateryear[,c("WY","Sac_WY")])

#################### Graph for the last 10 years of outflow and X2 data

#Subset just June to October
data_dayflow_summerfall<-data_dayflow_added %>% filter(Month %in% c(6:10))

#Subset just last 5 years
data_dayflow_summerfall_5yrs<-data_dayflow_summerfall %>% filter(Year %in% c(2015:2020))

data_dayflow_summerfall_5yrs$WY<-as.factor(data_dayflow_summerfall_5yrs$WY)

data_dayflow_summerfall_5yrs$Year<-as.factor(data_dayflow_summerfall_5yrs$Year)

#Plot for Outflow
plot_outflow_5yrs <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_line(data=data_dayflow_summerfall_5yrs, ggplot2::aes(x=as.Date(paste(2020,strftime(Date,format="%m-%d"),sep="-")), y=OUT, color=Year, linetype=Year),size=1)+
  ggplot2::scale_linetype_manual(values = c("solid","solid","solid","solid","solid","dotdash"),guide=FALSE) +
  ggplot2::scale_colour_manual(values = c("#000000","#000075","#4363d8","#469990","#9A6324","#e6194B"),name="",labels=c(2015:2020)) +
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::ylab(bquote("Delta"~outflow~"("*ft^3*"/s)"))+
  ggplot2::xlab("Date")

plot_outflow_5yrs

#Plot for X2
plot_X2_5yrs <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_line(data=data_dayflow_summerfall_5yrs, ggplot2::aes(x=as.Date(paste(2020,strftime(Date,format="%m-%d"),sep="-")), y=X2, color=Year, linetype=Year),size=1)+
  ggplot2::scale_linetype_manual(values = c("solid","solid","solid","solid","solid","dotdash"),name="",labels=c(2015:2020)) +
  ggplot2::scale_colour_manual(values = c("#000000","#000075","#4363d8","#469990","#9A6324","#e6194B"),name="",labels=c(2015:2020))+
  #ggplot2::scale_linetype(guide = FALSE)+
  ggplot2::theme(plot.title=element_text(size=9), 
          axis.text.x=element_text(size=9, color="black"), 
          axis.text.y = element_text(size=8, color="black"), 
          axis.title.x = element_text(size = 9, angle = 00), 
          axis.title.y = element_text(size = 9, angle = 90),
          strip.text = element_text(size = 7),
          legend.text=element_text(size = 9),
          strip.background = element_rect(size=0.3),
          legend.position="bottom") + 
  guides(linetype=guide_legend(keywidth = 6, keyheight = 1),
          colour=guide_legend(keywidth = 6, keyheight = 1))+
  ggplot2::ylab("X2 (km)")+
  ggplot2::xlab("Date")

plot_X2_5yrs

gA <- ggplotGrob(plot_outflow_5yrs)
gB <- ggplotGrob(plot_X2_5yrs)
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3])

# Set the widths
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth

#Print figure
tiff(filename=file.path(output_root,"Figure_Dayflow_5yrs.tiff"),
     type="cairo",
     units="in", 
     width=6, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
#pushViewport(viewport(layout = grid.layout(2, 1)))
#print(gA, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#print(gB, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.arrange(gA, gB, ncol=1,nrow=2)

dev.off()

#################### Graph for the last 5 dry years of outflow and X2 data

#Subset just dry years
dry_years<-unique(data_dayflow_summerfall[data_dayflow_summerfall$Sac_WY=="D",c("WY")])
dry_years

data_dayflow_summerfall_dry<-data_dayflow_summerfall %>% filter(Year %in% dry_years)
unique(data_dayflow_summerfall_dry$Year)

data_dayflow_summerfall_dry$Year<-as.factor(data_dayflow_summerfall_dry$Year)

#Plot for Outflow
plot_outflow_dry <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_line(data=data_dayflow_summerfall_dry, ggplot2::aes(x=as.Date(paste(2020,strftime(Date,format="%m-%d"),sep="-")), y=OUT, color=Year, linetype=Year),size=1)+
  #Need to change below when 2020 data is in
  ggplot2::scale_linetype_manual(values = c("solid","solid","solid","solid","solid","dotdash"),guide=FALSE) +
  ggplot2::scale_colour_manual(values = c("#000000","#000075","#4363d8","#469990","#9A6324","#e6194B"),guide=FALSE) +
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::ylab(bquote("Delta"~outflow~"("*ft^3*"/s)"))+
  ggplot2::xlab("Date")

plot_outflow_dry

#Plot for X2
plot_X2_dry <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_line(data=data_dayflow_summerfall_dry, ggplot2::aes(x=as.Date(paste(2020,strftime(Date,format="%m-%d"),sep="-")), y=X2, color=Year, linetype=Year),size=1)+
  #Need to change below when 2020 data is in
  ggplot2::scale_linetype_manual(values = c("solid","solid","solid","solid","solid","dotdash"),name="",labels=dry_years) +
  ggplot2::scale_colour_manual(values = c("#000000","#000075","#4363d8","#469990","#9A6324","#e6194B"),name="",labels=dry_years) +
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.text=element_text(size = 9),
                 legend.position="bottom",
                 strip.background = element_rect(size=0.3)) + 
  guides(linetype=guide_legend(keywidth = 6, keyheight = 1),
         colour=guide_legend(keywidth = 6, keyheight = 1))+
  ggplot2::ylab("X2 (km)")+
  ggplot2::xlab("Date")

plot_X2_dry

gC <- ggplotGrob(plot_outflow_dry)
gD <- ggplotGrob(plot_X2_dry)
maxWidth = unit.pmax(gC$widths[2:3], gD$widths[2:3])

# Set the widths
gC$widths[2:3] <- maxWidth
gD$widths[2:3] <- maxWidth


#Print figure
tiff(filename=file.path(output_root,"Figure_Dayflow_dry_years.tiff"),
     type="cairo",
     units="in", 
     width=6, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
#pushViewport(viewport(layout = grid.layout(1, 2)))
#print(plot_outflow_dry, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#print(plot_X2_dry, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
grid.arrange(gC, gD, ncol=1,nrow=2)

dev.off()

### To check min and max X2
DT2020<-data_dayflow_added %>% filter(Year==2020)
View(DT2020)
DT2020<-DT2020 %>% filter(Month %in% c(6:10))