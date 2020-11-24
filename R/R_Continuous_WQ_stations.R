library(tidyverse)
library(lubridate)
library(grid)
library(sharpshootR)
library(geofacet)
require(wql)
library(magrittr)
library(viridis)
library(scales)
library(sf)
library(ggspatial)
library(plotrix)
library(ggrepel)

#Path to local drive
root <- "~/GitHub/Summer_Fall_Action"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

############################### Lisbon Flow data
#Sensor 20 = Flow, Discharge (cfs)
#Sensor 25 = TEMPERATURE (F)
#Sensor 27 = Turbidity (NTU)
#Sensor 28 = CHLOROPHYLL (ug/L)
#Sensor 61 = DISSOLVED OXYGEN (MG/L)
#Sensor 62 = PH (PH)
#Sensor 100 = Conductivity (us/cm)

#Get Lisbon data

LIS_flow <- CDECquery(id='LIS', sensor=20, interval='E', start='2020-06-01', end='2020-10-31')
str(LIS_flow)

plot_LIS_flow <- ggplot2::ggplot(data=LIS_flow, ggplot2::aes(x=datetime, y=value))+
  ggplot2::theme_bw()+
  ggplot2::geom_point(alpha=0.2)+
  ggplot2::geom_smooth(method = 'loess',se=FALSE,span = 0.3)+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::ylab(bquote(~Flow~"("*ft^3*"/s)"))+
  ggplot2::labs(title="Lisbon Weir (LIS)")
plot_LIS_flow


#Print figure
tiff(filename=file.path(output_root,"Figure_Lisbon_Flow.tiff"),
     type="cairo",
     units="in", 
     width=8, #10*1, 
     height=6, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
print(plot_LIS_flow)
dev.off()

remove(plot_LIS_flow,LIS_flow)

############################### Abiotic factor graphs
#Using data from multiple monitoring stations to depict conditions within the LSZ 
#(e.g., a composite metric using data from National Steel, Hunters Cut, and Belden's Landing stations). 
#Data from MAL station will be used for Suisun Bay and from SDI and RIV station will be used for the Lower Sac. 

###########
#Salinity
MAL_cond <- CDECquery(id='MAL', sensor=100, interval='E', start='2020-06-01', end='2020-10-31')
SDI_cond <- CDECquery(id='SDI', sensor=100, interval='E', start='2020-06-01', end='2020-10-31')
RVB_cond <- CDECquery(id='RVB', sensor=100, interval='E', start='2020-06-01', end='2020-10-31')
#National Steel, Hunters Cut, and Belden's Landing
NSL_cond <- CDECquery(id='NSL', sensor=100, interval='E', start='2020-06-01', end='2020-10-31')
BDL_cond <- CDECquery(id='BDL', sensor=100, interval='E', start='2020-06-01', end='2020-10-31')
HUN_cond <- CDECquery(id='HUN', sensor=100, interval='E', start='2020-06-01', end='2020-10-31')

#Combine Data
SalinityData<-rbind(MAL_cond,SDI_cond,RVB_cond,NSL_cond,BDL_cond,HUN_cond)
remove(MAL_cond,SDI_cond,RVB_cond,NSL_cond,BDL_cond,HUN_cond)

#Convert conductivity to ppt (assuming conductivity adjusted to temp of 25 C)
SalinityData$ppt<-ec2pss(SalinityData$value/1000, t=25)
str(SalinityData)
SalinityData$station_id<-as.factor(SalinityData$station_id)
#Order the factor
SalinityData$station_id <- ordered(SalinityData$station_id, levels = c("HUN", "BDL", "NSL","MAL","SDI","RVB"))

#Create figure
plot_salinity <- ggplot2::ggplot(data=SalinityData, ggplot2::aes(x=datetime, y=ppt))+ facet_wrap(~ station_id)+
  ggplot2::theme_bw()+
  ggplot2::geom_point(alpha=0.2)+
  ggplot2::geom_smooth(method = 'loess',se=FALSE,span = 0.3)+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::geom_hline(yintercept=6, linetype="dashed", color = "red")+
  ggplot2::ylab("Salinity (ppt)")
plot_salinity

#Print figure
png(filename=file.path(output_root,"Figure_Salinity_stations.png"),
     type="cairo",
     units="in", 
     width=8, #10*1, 
     height=6, #22*1, 
     pointsize=5, #12, 
     res=300)
print(plot_salinity)
dev.off()



###########
#Temperature
MAL_temp <- CDECquery(id='MAL', sensor=25, interval='E', start='2020-06-01', end='2020-10-31')
SDI_temp <- CDECquery(id='SDI', sensor=25, interval='E', start='2020-06-01', end='2020-10-31')
RVB_temp <- CDECquery(id='RVB', sensor=25, interval='H', start='2020-06-01', end='2020-10-31')
#National Steel, Hunters Cut, and Belden's Landing
NSL_temp <- CDECquery(id='NSL', sensor=25, interval='E', start='2020-06-01', end='2020-10-31')
BDL_temp <- CDECquery(id='BDL', sensor=25, interval='E', start='2020-06-01', end='2020-10-31')
HUN_temp <- CDECquery(id='HUN', sensor=25, interval='E', start='2020-06-01', end='2020-10-31')

#Combine Data
TemperatureData<-rbind(MAL_temp,SDI_temp,RVB_temp,NSL_temp,BDL_temp,HUN_temp)
remove(MAL_temp,SDI_temp,RVB_temp,NSL_temp,BDL_temp,HUN_temp)

#Convert Fanrenheit to Celsius
TemperatureData$temperature<-(TemperatureData$value-32)*(5/9)

TemperatureData$station_id<-as.factor(TemperatureData$station_id)
#Order the factor
TemperatureData$station_id <- ordered(TemperatureData$station_id, levels = c("HUN", "BDL", "NSL","MAL","SDI","RVB"))

#Temperature limit in BiOp is 75 F
(75-32)*(5/9)

#Create figure
plot_temperature <- ggplot2::ggplot(data=TemperatureData, ggplot2::aes(x=datetime, y=temperature))+ facet_wrap(~ station_id)+
  ggplot2::theme_bw()+
  ggplot2::geom_point(alpha=0.2)+
  ggplot2::geom_smooth(method = 'loess',se=FALSE,span = 0.3)+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::geom_hline(yintercept=23.88889, linetype="dashed", color = "red")+
  ggplot2::ylim(10,30)+
  ggplot2::ylab("Temperature (C)")
plot_temperature

#Print figure
png(filename=file.path(output_root,"Figure_Temperature_stations.png"),
    type="cairo",
    units="in", 
    width=8, #10*1, 
    height=6, #22*1, 
    pointsize=5, #12, 
    res=300)
print(plot_temperature)
dev.off()

###########
#Turbidity

#SDI is a USGS station, so labeled as FNU
MAL_turb <- CDECquery(id='MAL', sensor=27, interval='E', start='2020-06-01', end='2020-10-31')
SDI_turb <- CDECquery(id='SDI', sensor=221, interval='E', start='2020-06-01', end='2020-10-31')
RVB_turb <- CDECquery(id='RVB', sensor=27, interval='E', start='2020-06-01', end='2020-10-31')
#National Steel, Hunters Cut, and Belden's Landing
NSL_turb <- CDECquery(id='NSL', sensor=27, interval='E', start='2020-06-01', end='2020-10-31')
BDL_turb <- CDECquery(id='BDL', sensor=27, interval='E', start='2020-06-01', end='2020-10-31')
HUN_turb <- CDECquery(id='HUN', sensor=27, interval='E', start='2020-06-01', end='2020-10-31')

#Combine Data
TurbidityData<-rbind(MAL_turb,SDI_turb,NSL_turb,BDL_turb,HUN_turb,RVB_turb)
remove(MAL_turb,SDI_turb,NSL_turb,BDL_turb,HUN_turb,RVB_turb)

TurbidityData$station_id<-as.factor(TurbidityData$station_id)
#Order the factor
TurbidityData$station_id <- ordered(TurbidityData$station_id, levels = c("HUN", "BDL", "NSL","MAL","SDI","RVB"))


#Create figure
plot_turbidity <- ggplot2::ggplot(data=TurbidityData, ggplot2::aes(x=datetime, y=value))+ facet_wrap(~ station_id)+
  ggplot2::theme_bw()+
  ggplot2::geom_point(alpha=0.2)+
  ggplot2::geom_smooth(method = 'loess',se=FALSE,span=0.3)+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::geom_hline(yintercept=12, linetype="dashed", color = "red")+
  ggplot2::ylim(0,100)+
  ggplot2::ylab("Turbidity (NTU)")
plot_turbidity

#Print figure
png(filename=file.path(output_root,"Figure_Turbidity_stations.png"),
    type="cairo",
    units="in", 
    width=8, #10*1, 
    height=6, #22*1, 
    pointsize=5, #12, 
    res=300)
print(plot_turbidity)
dev.off()

#########################Percent of time suitable for Delta Smelt

#Calculate percent of time water quality was suitable for Delta Smelt
Salinity_habitat_data<- SalinityData %>% mutate(Date=as.Date(datetime),Parameter="Salinity", PercentSuitable=ifelse(ppt<=6,1,0)) %>% group_by(station_id,Date,Parameter) %>% summarise(PercentSuitable=mean(PercentSuitable))
Temperature_habitat_data<- TemperatureData %>% mutate(Date=as.Date(datetime),Parameter="Temperature", PercentSuitable=ifelse(temperature<=23.88889,1,0)) %>% group_by(station_id,Date,Parameter) %>% summarise(PercentSuitable=mean(PercentSuitable))
Turbidity_habitat_data<- TurbidityData %>% mutate(Date=as.Date(datetime),Parameter="Turbidity", PercentSuitable=ifelse(value>=12,1,0)) %>% group_by(station_id,Date,Parameter) %>% summarise(PercentSuitable=mean(PercentSuitable))

Salinity_habitat_data$station_id<-as.character(Salinity_habitat_data$station_id)
Temperature_habitat_data$station_id<-as.character(Temperature_habitat_data$station_id)
Turbidity_habitat_data$station_id<-as.character(Turbidity_habitat_data$station_id)

#Combine the datasets
Suitable_habitat <- dplyr::bind_rows(Salinity_habitat_data,Temperature_habitat_data,Turbidity_habitat_data)
remove(Salinity_habitat_data,Temperature_habitat_data,Turbidity_habitat_data)


Suitable_habitat$station_id<-as.factor(Suitable_habitat$station_id)
#Order the factor
Suitable_habitat$station_id <- ordered(Suitable_habitat$station_id, levels = c("HUN", "BDL", "NSL","MAL","SDI","RIV"))


#Create figure
plot_habitat <-ggplot2::ggplot(Suitable_habitat,aes(y=Parameter,x=Date,fill=PercentSuitable))+
  ggplot2:: geom_tile() +scale_fill_viridis(name="Percent Suitable",option ="magma")+
  ggplot2::facet_grid(station_id~.)+
  ggplot2::guides(fill = guide_legend(title = "Proportion of time\n parameter was\n 'suitable' for \nDelta Smelt"))
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(),
                 strip.text = element_text(size = 7),
                 legend.text=element_text(size = 9),
                 strip.background = element_rect(size=0.3)) 
plot_habitat


#Print figure
png(filename=file.path(output_root,"Figure_Habitat.png"),
    type="cairo",
    units="in", 
    width=10, #10*1, 
    height=6, #22*1, 
    pointsize=5, #12, 
    res=500)
print(plot_habitat)
dev.off()

######################### Solo Belden's Landing salinity figure per Armin Halston's request
SalinityData

## Get the start and end points for highlighted regions
mindates<-c("2020-09-08 01:00:00", "2020-10-01 01:00:00")
maxdates<-c("2020-09-24 01:00:00", "2020-10-31 01:00:00")

SMSG_highlight<-data.frame(MinDate=mindates,MaxDate=maxdates,mindata=c(0,0),maxdata=c(11,11))
SMSG_highlight$MaxDate<-as.POSIXct(SMSG_highlight$MaxDate)
SMSG_highlight$MinDate<-as.POSIXct(SMSG_highlight$MinDate)


plot_BDL <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_point(data= (SalinityData %>% filter(station_id == "BDL")), ggplot2::aes(x=datetime, y=ppt),alpha=0.2)+
  ggplot2::geom_smooth(data= (SalinityData %>% filter(station_id == "BDL")), ggplot2::aes(x=datetime, y=ppt),method = 'loess',se=FALSE,span = 0.3)+
  ggplot2::geom_rect(data=SMSG_highlight, ggplot2::aes(xmin=MinDate, xmax=MaxDate, ymin=mindata, ymax=maxdata), alpha=0.3, fill="darkorange1")+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::geom_hline(yintercept=6, linetype="dashed", color = "red")+
  ggplot2::ylab("Salinity (ppt)")+ ylim(0,11)
  
plot_BDL

#Print figure
png(filename=file.path(output_root,"Figure_BDL_Salinity.png"),
    type="cairo",
    units="in", 
    width=6, #10*1, 
    height=4, #22*1, 
    pointsize=5, #12, 
    res=400)
print(plot_BDL)
dev.off()

######################### Map figure denoting locations of stations

#Read in streams shape files
centralvalley <- st_read(file.path(data_root,"Map/cdfg_100k_2003_6/Data","cdfg_100k_2003_6.shp"))

# Subset just the Sacramento and San Joaquin tributaries
centralvalley
sort(unique(centralvalley$DOWN_NAME))
sort(unique(centralvalley$NAME))
vec=data.frame(NAME=sort(unique(centralvalley$NAME)))

sac <- subset(centralvalley, grepl("Sacramento River", centralvalley$NAME))
sac_down <- subset(centralvalley, grepl("Sacramento River", centralvalley$DOWN_NAME))
sort(unique(sac_down$NAME))

sj <- subset(centralvalley, grepl("San Joaquin River", centralvalley$NAME))
sj_down <- subset(centralvalley, grepl("San Joaquin River", centralvalley$DOWN_NAME))

yuba <- subset(centralvalley, grepl("Yuba River", centralvalley$NAME))

#Read in bay shape files
CountyWater <- st_read(file.path(data_root,"Map/DJFMP Water bodies","CountyWaterforDJFMPMap.shp"))

CountyWater
sort(unique(CountyWater$FULLNAME))

#Read in bay shape files without county lines
bay <- st_read(file.path(data_root,"Map/BayStudy_Shapefile","sfbs_pg_5bays.shp"))

#Read in Delta files
DeltaSubregionsWater <- st_read("D:/Projects/Pascale's Striped Bass - Salmon Study/Map Files/DJFMP Water bodies","DeltaSubregionsWater")
#------------------------------CDEC stations added here
HUN_station<-data.frame(station_id="HUN",
                        Latitude=paste(CDEC_StationInfo('HUN') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('HUN') %>% extract2(1) %>% select(Longitude)))
BDL_station<-data.frame(station_id="BDL",
                        Latitude=paste(CDEC_StationInfo('BDL') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('BDL') %>% extract2(1) %>% select(Longitude)))
NSL_station<-data.frame(station_id="NSL",
                        Latitude=paste(CDEC_StationInfo('NSL') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('NSL') %>% extract2(1) %>% select(Longitude)))
MAL_station<-data.frame(station_id="MAL",
                        Latitude=paste(CDEC_StationInfo('MAL') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('MAL') %>% extract2(1) %>% select(Longitude)))
SDI_station<-data.frame(station_id="SDI",
                        Latitude=paste(CDEC_StationInfo('SDI') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('SDI') %>% extract2(1) %>% select(Longitude)))
RVB_station<-data.frame(station_id="RVB",
                        Latitude=paste(CDEC_StationInfo('RVB') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('RVB') %>% extract2(1) %>% select(Longitude)))

CDEC_stations<-bind_rows(HUN_station,BDL_station,NSL_station,MAL_station,SDI_station,RVB_station)
#Fix the longitude information
CDEC_stations$Longitude<-as.numeric(CDEC_stations$Longitude)
CDEC_stations$Latitude<-as.numeric(CDEC_stations$Latitude)

CDEC_stations$Longitude<-CDEC_stations$Longitude*(-1)
str(CDEC_stations)

# make the coordinate cols spatial (X/Easting/lon, Y/Northing/lat)
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
df.SP <- st_as_sf(CDEC_stations, coords = c("Longitude", "Latitude"), crs = crsLONGLAT)
df.SP

df.SP$Latitude<-CDEC_stations$Latitude
df.SP$Longitude<-CDEC_stations$Longitude


#Create the map figure
cv_map<-ggplot() + theme_bw()+
  geom_sf(data = sac, fill = 'grey', lwd = 0.5, color='black') + geom_sf(data = sac_down, fill = 'grey', lwd = 0.5, color='black') +
  geom_sf(data = sj, fill = 'grey', lwd = 0.5, color='black') + geom_sf(data = sj_down, fill = 'grey', lwd = 0.5, color='black') +
  geom_sf(data = yuba, fill = 'grey', lwd = 0.5, color='black') +
  geom_sf(data = DeltaSubregionsWater, fill = 'grey', lwd = 0.5, color='black') + 
  geom_sf(data = CountyWater, fill = 'grey', lwd = 0.5, color='black') + 
  geom_sf(data = bay, fill = 'grey', lwd = 0.5, color='black') +
  geom_sf(data=df.SP,color="red",size=2)+ geom_label_repel(data=df.SP, aes(x=Longitude,y=Latitude,label=station_id),nudge_x = c(-0.025, -0.025, 0.025, -0.025, -0.025, 0.025, 0.1),nudge_y = c(0.025,-0.025, 0.025, 0.025, 0.025, -0.025, -0.025),segment.alpha=0.7,color="blue")+
  coord_sf(xlim = c(-122.3, -121.5), ylim = c(37.8, 38.5),crs=crsLONGLAT)  +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_y = unit(1.0, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "tr", width_hint = 0.5)+
  theme(plot.tag= element_text(size=20, color="black"), axis.text.x = element_text(size=16, color="black"),axis.text.y = element_text(size=16, color="black"),axis.title.x=element_blank(),axis.title.y=element_blank())
cv_map


# Print map
tiff(filename=file.path(output_root,"Figure_Stations_Map.png"), 
     type="cairo",
     units="in", 
     width=11*1, 
     height=11*1, 
     pointsize=18, 
     res=450, compression="lzw")
cv_map
dev.off()
