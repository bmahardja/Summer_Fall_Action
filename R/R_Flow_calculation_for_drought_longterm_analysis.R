library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(gtable)
library(readxl)
library(sharpshootR)

#Path to local drive
root <- "~/GitHub/Summer_Fall_Action"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")


#################### Read dayflow data
data_dayflow_1970_1983<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1970-1983.csv"))
data_dayflow_1984_1996<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1984-1996.csv"))
data_dayflow_1997_2020<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1997-2020.csv"))

#Check column names
#str(data_dayflow_1970_1983)
#str(data_dayflow_1984_1996)
#str(data_dayflow_1997_2020)

#EXPORTS for 1996 and before, EXPORT for the 1997-2020 data
#Change column name to match
data_dayflow_1997_2020$EXPORT<-data_dayflow_1997_2020$EXPORTS

#More columns that don't match, but it doesn't matter for now since we're only interested in OUT, EXPORT, and X2
data_dayflow<-bind_rows(data_dayflow_1970_1983,data_dayflow_1984_1996,data_dayflow_1997_2020)

#convert date column to date
data_dayflow$Date <- as.Date(data_dayflow$Date,"%m/%d/%Y")

######################Add X2 for earlier years based on Hutton et al. paper

data_dayflow<- data_dayflow %>% left_join(readxl::read_xlsx(file.path(data_root, "Dayflow","supplemental_data_wr.1943-5452.0000617_hutton3.xlsx"), sheet = "Daily") %>% 
            transmute(Date = as.Date(Date),X2Hutton = `SacX2`),
            by = "Date")

#data_dayflow<- data_dayflow %>% left_join(readxl::read_xlsx(file.path(data_root, "Dayflow","FullDayflowAndX2WithNotes1930-2011_3-6-2012.xlsx"), sheet = "DAYFLOW1956-2011") %>% 
#            transmute(Date = as.Date(Date),
#                      X2Anke = `1955-2011 X2`),
#          by = "Date") 


data_dayflow$X2<-ifelse(is.na(data_dayflow$X2),data_dayflow$X2Hutton,data_dayflow$X2)

#Add future outflow and X2 until DAYFLOW is available
#Seasons: Winter (Dec-FEb), Spring (Mar-May), Summer (Jun-Aug) or fall (Sep-Nov)
DTO_2021 <- CDECquery(id='DTO', sensor=23, interval='D', start='2020-10-01', end='2021-05-31')
str(DTO_2021)
DTO_2021$Date <- as.Date(DTO_2021$datetime,"%Y-%m-%d")
DTO_2021<-DTO_2021 %>% mutate(OUT=value,Year=year) %>% select(Date,OUT,Year)

#Add to the dayflow data
data_dayflow <- dplyr::bind_rows(data_dayflow,DTO_2021)


#####################Calculate future X2 based on DAYFLOW documentation:
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

#X2(t) = 10.16 + 0.945*X2(t-1) - 1.487log(QOUT(t)) 
#NOTE: It seems like the log in the DAYFLOW notation is referring to Log10 (i.e., not ln)

#Locate end row for 196 WY data (with no X2 data)
which(data_dayflow$Date=="2020-10-01")
#18629

#Fill in X2 data for most current WY data
for (i in which(data_dayflow$Date=="2020-10-01"):nrow(data_dayflow)) {
  data_dayflow$X2[i] = 10.16 + (0.945*(data_dayflow$X2[i-1]))+(-1.487*log10(data_dayflow$OUT[i]))
}

##################### Summarize per Rosemary's instructions
#Adjusted calendar year, December-November, with December of the previous calendar year included with the following year 
#(so December of 2019 is the first month of “2020” in our data set)

data_dayflow$Year_adjusted<-ifelse(month(data_dayflow$Date)==12,year(data_dayflow$Date)+1, year(data_dayflow$Date))

#Seasons: Winter (Dec-FEb), Spring (Mar-May), Summer (Jun-Aug) or fall (Sep-Nov)
data_dayflow <- data_dayflow %>% mutate(Season= case_when(
  month(Date) %in% c(12,1,2) ~ "Winter",
  month(Date) %in% c(3:5) ~ "Spring",
  month(Date) %in% c(6:8) ~ "Summer",
  month(Date) %in% c(9:11) ~ "Fall"
))

#Only years after 1970
data_dayflow <- data_dayflow %>% filter(Year_adjusted>=1970)

#Summarize
data_dayflow_sum <- data_dayflow %>% group_by(Year_adjusted, Season) %>% summarise(Outflow=mean(OUT),X2=mean(X2,na.rm=T),Export=mean(EXPORT))

#Write out csv
write.csv(data_dayflow_sum,file.path(output_root,"Drought_MAST_Flow_Metrics.csv"))

##WARNING!: Hutton et al. had missing X2 data and summarized seasonal X2 may be skewed as a result.
