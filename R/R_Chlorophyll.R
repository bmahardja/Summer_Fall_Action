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


############################### Lisbon and RVB chlorophyll data for NDFA summary
#Sensor 20 = Flow, Discharge (cfs)
#Sensor 25 = TEMPERATURE (F)
#Sensor 27 = Turbidity (NTU)
#Sensor 28 = CHLOROPHYLL (ug/L)
#Sensor 61 = DISSOLVED OXYGEN (MG/L)
#Sensor 62 = PH (PH)
#Sensor 100 = Conductivity (us/cm)

#Get Lisbon data

LIS_chloro <- CDECquery(id='LIS', sensor=28, interval='E', start='2020-06-01', end='2020-10-31')
RVB_chloro <- CDECquery(id='RVB', sensor=28, interval='E', start='2020-06-01', end='2020-10-31')
STTD_data<-read.csv(file.path(data_root, "NDFA", "STTD_NCRO.csv"))

STTD_chloro<-read.csv(file.path(data_root, "NDFA", "STTD_NCRO.csv"),skip=2)
STTD_chloro<- STTD_chloro %>% dplyr::select(Date,Point.5,Qual.5) %>% rename(value=Point.5, flag=Qual.5,datetime=Date) %>% mutate(station_id="STTD")
STTD_chloro$flag<-as.character(STTD_chloro$flag)
STTD_chloro$datetime<-strptime(STTD_chloro$datetime, "%m/%d/%Y %H:%M",tz=Sys.timezone())
str(STTD_chloro)

remove(STTD_data)

str(LIS_chloro)
#Combine Data
Chlorophyll_Data<-dplyr::bind_rows(LIS_chloro,RVB_chloro,STTD_chloro)
remove(LIS_chloro,RVB_chloro,STTD_chloro)

Chlorophyll_Data$station_id<-as.factor(Chlorophyll_Data$station_id)
#Order the factor
Chlorophyll_Data$station_id <- ordered(Chlorophyll_Data$station_id, levels = c("LIS","STTD","RVB"))


#Create figure
plot_chloro <- ggplot2::ggplot(data=Chlorophyll_Data, ggplot2::aes(x=datetime, y=value))+ facet_wrap(~ station_id)+
  ggplot2::theme_bw()+
  ggplot2::geom_point(alpha=0.2)+
  ggplot2::geom_smooth(method = 'loess',se=FALSE,span = 0.3,color="green")+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3))+
  ggplot2::ylab("Chlorophyll (ug/L)")
plot_chloro

#Print figure
png(filename=file.path(output_root,"Figure_Chlorophyll_stations.png"),
    type="cairo",
    units="in", 
    width=10, #10*1, 
    height=6, #22*1, 
    pointsize=5, #12, 
    res=300)
print(plot_chloro)
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
DeltaSubregionsWater <- st_read(file.path(data_root,"Map/DJFMP Water bodies","DeltaSubregionsWater.shp"))

#Read in Yolo files
yolo <- st_read(file.path(data_root,"Map/YoloBypass_ShapeFiles/Yolo Bypass Extent","Yolo_Bypass.shp"))

#------------------------------CDEC stations added here
LIS_station<-data.frame(station_id="LIS",
                        Latitude=paste(CDEC_StationInfo('LIS') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('LIS') %>% extract2(1) %>% select(Longitude)))

RVB_station<-data.frame(station_id="RVB",
                        Latitude=paste(CDEC_StationInfo('RVB') %>% extract2(1) %>% select(Latitude)),
                        Longitude=paste(CDEC_StationInfo('RVB') %>% extract2(1) %>% select(Longitude)))

CDEC_stations<-bind_rows(LIS_station,RVB_station)
#Fix the longitude information
CDEC_stations$Longitude<-as.numeric(CDEC_stations$Longitude)
CDEC_stations$Latitude<-as.numeric(CDEC_stations$Latitude)

CDEC_stations$Longitude<-CDEC_stations$Longitude*(-1)
str(CDEC_stations)

#Add STTD data since it's not available on CDEC
CDEC_stations[nrow(CDEC_stations) + 1,] = c("STTD",38.353461,-121.642975)

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
  geom_sf(data = yolo, fill = 'purple', alpha=0.3,lwd = 0.5, color='black') +
  geom_sf(data=df.SP,color="red",size=2) + geom_sf_label(data=df.SP,aes(label = station_id),nudge_x = 0.01, nudge_y = 0.01)+
  coord_sf(xlim = c(-121.75, -121.5), ylim = c(38.1, 38.5),crs=crsLONGLAT)  +
  #annotation_north_arrow(location = "tr", which_north = "true", 
  #                       pad_y = unit(1.0, "in"),
  #                       style = north_arrow_fancy_orienteering) +
  #annotation_scale(location = "tr", width_hint = 0.5)+
  ggthemes::theme_map()
  #theme(plot.tag= element_text(size=20, color="black"), axis.text.x = element_text(size=16, color="black"),axis.text.y = element_text(size=16, color="black"),axis.title.x=element_blank(),axis.title.y=element_blank())
cv_map


# Print map
tiff(filename=file.path(output_root,"Figure_Stations_Map_NDFA.png"), 
     type="cairo",
     units="in", 
     width=6*1, 
     height=11*1, 
     pointsize=18, 
     res=450, compression="lzw")
cv_map
dev.off()

