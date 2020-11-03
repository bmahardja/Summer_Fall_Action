library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(sharpshootR)


#Path to local drive
root <- "~/GitHub/Summer_Fall_Action"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

End_year=2020

#Read EMP data
wq_emp<-read_csv(file.path(data_root, "EMP","edi.458.3", "SACSJ_delta_water_quality_1975_2019.csv"), na=c("NA", "ND"),
                 col_types = cols_only(Station="c", Date="c", Time="c", Chla="d",
                                       Depth="d", Secchi="d", Microcystis="d", SpCndSurface="d",
                                       WTSurface="d", WTBottom='d'))
  
#Read 2020 data
wq_emp_2020<-read_csv(file.path(data_root, "EMP", "EMP 2020 MC Scores Jan20-Oct20.csv"), na=c("NA", "ND"),col_types = cols_only(Station="c", Date="c", Time="c",MC_Score="d")) %>% rename(Microcystis=MC_Score) 

#Bind data
wq_emp_added <- dplyr::bind_rows(wq_emp,wq_emp_2020)

wq_emp_added$Date<-as.Date(wq_emp_added$Date,"%m/%d/%Y")

#Create station list, and take out the floating EZ stations
StationList<-unique(wq_emp_2020$Station)  
StationList<-StationList[!grepl("EZ",StationList)]  
StationList
#24 stations in total

#Subset data to just station in 2020, year 2015 (when EMP started doing microcystis ranking), and summer-fall period

wq_emp_microcystis <- wq_emp_added %>% mutate(Month=month(Date),Year=year(Date)) %>% filter(Year>=2015,Month %in% c(6:10), Station %in% StationList)

#Summarize annual data
#Checked NAs, most are from 2015, with one station missing data in 2020
#Round data
wq_emp_microcystis$Microcystis=round(wq_emp_microcystis$Microcystis) #EMP has some 2.5 and 3.5 values
wq_emp_microcystis_annual <- wq_emp_microcystis %>% filter(complete.cases(Microcystis)) %>% group_by(Year) %>%  dplyr::summarise(N_Microcystis=length(which(!is.na(.data$Microcystis))),
                                                                                            Microcystis1=length(which(.data$Microcystis==1))/.data$N_Microcystis,
                                                                                            Microcystis2=length(which(.data$Microcystis==2))/.data$N_Microcystis,
                                                                                            Microcystis3=length(which(.data$Microcystis==3))/.data$N_Microcystis,
                                                                                            Microcystis4=length(which(.data$Microcystis==4))/.data$N_Microcystis,
                                                                                            Microcystis5=length(which(.data$Microcystis==5))/.data$N_Microcystis, .groups="drop")%>%
    tidyr::pivot_longer(c(.data$Microcystis1, .data$Microcystis2, .data$Microcystis3, .data$Microcystis4, .data$Microcystis5), names_to = "Severity", values_to = "Frequency")%>%
    dplyr::mutate(Severity=dplyr::recode(.data$Severity, "Microcystis1"="Absent", "Microcystis2"="Low", "Microcystis3"="Medium", "Microcystis4"="High", "Microcystis5"="Very high"))
 
#Put the ranking in order 
wq_emp_microcystis_annual$Severity<-ordered(wq_emp_microcystis_annual$Severity, levels = c("Absent", "Low", "Medium","High", "Very high"))


#Plot annual data
plot_microcystis_annual<-ggplot2::ggplot()+theme_classic()+
  ggplot2::geom_bar(data=wq_emp_microcystis_annual, ggplot2::aes(x=.data$Year, y=.data$Frequency, fill=.data$Severity), stat="identity")+
  ggplot2::scale_fill_brewer(type="div", palette = "RdYlBu", direction=-1)+
  ggplot2::scale_x_continuous(breaks = c(2015:End_year))+
  ggplot2::geom_bar(data=tibble::tibble(End_year), ggplot2::aes(x=End_year, y=1), stat="identity", color="firebrick3", fill=NA, size=1)+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3),
                 legend.position = "none")

plot_microcystis_annual


#Summarize monthly data for end_year
wq_emp_microcystis_monthly <- wq_emp_microcystis %>% filter(complete.cases(Microcystis),Year==End_year) %>% group_by(Month) %>%  dplyr::summarise(N_Microcystis=length(which(!is.na(.data$Microcystis))),
                                                                                                                                 Microcystis1=length(which(.data$Microcystis==1))/.data$N_Microcystis,
                                                                                                                                 Microcystis2=length(which(.data$Microcystis==2))/.data$N_Microcystis,
                                                                                                                                 Microcystis3=length(which(.data$Microcystis==3))/.data$N_Microcystis,
                                                                                                                                 Microcystis4=length(which(.data$Microcystis==4))/.data$N_Microcystis,
                                                                                                                                 Microcystis5=length(which(.data$Microcystis==5))/.data$N_Microcystis, .groups="drop")%>%
  tidyr::pivot_longer(c(.data$Microcystis1, .data$Microcystis2, .data$Microcystis3, .data$Microcystis4, .data$Microcystis5), names_to = "Severity", values_to = "Frequency")%>%
  dplyr::mutate(Severity=dplyr::recode(.data$Severity, "Microcystis1"="Absent", "Microcystis2"="Low", "Microcystis3"="Medium", "Microcystis4"="High", "Microcystis5"="Very high"))

#Put the ranking in order 
wq_emp_microcystis_monthly$Severity<-ordered(wq_emp_microcystis_monthly$Severity, levels = c("Absent", "Low", "Medium","High", "Very high"))
wq_emp_microcystis_monthly$Month = factor(month.abb[wq_emp_microcystis_monthly$Month], levels = c("Jun","Jul","Aug","Sep","Oct"))
levels(wq_emp_microcystis_monthly$Month)

#Plot monthly data
plot_microcystis_monthly<-ggplot2::ggplot()+theme_classic()+
  ggplot2::geom_bar(data=wq_emp_microcystis_monthly, ggplot2::aes(x=.data$Month, y=.data$Frequency, fill=.data$Severity), stat="identity")+
  ggplot2::scale_fill_brewer(type="div", palette = "RdYlBu", direction=-1)+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))

plot_microcystis_monthly

#Print figure
tiff(filename=file.path(output_root,"Figure_Microcystis_ranking.tiff"),
     type="cairo",
     units="in", 
     width=9, #10*1, 
     height=6, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
grid.arrange(plot_microcystis_annual, plot_microcystis_monthly, ncol=2,nrow=1)

dev.off()