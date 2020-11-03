library(tidyverse)
library(lubridate)
library(grid)
library(sf)
library(maptools)
library(ggrepel)

#Path to local drive
root <- "~/GitHub/Summer_Fall_Action"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

End_year=2020

#Code from Sam Bashevkin's Delta Smelt Conditions Report
smelt_edsm <- read_csv(file.path(data_root, "EDSM", "edsm_abund_estimates_2020-11-03.csv"))%>%
  mutate(Stratum=recode(Stratum, "Cache Slough LI"="Cache Slough/Liberty Island", "Sac DW Ship Channel"="Sac Deep Water Shipping Channel",
                        "Lower Sacramento"="Lower Sacramento River", "Lower San Joaquin"="Lower San Joaquin River"),
         Date=WeekStartDate+ceiling((WeekEndDate-WeekStartDate)/2))%>%
  select(Region=Stratum, Date, Abundance=nHat, Variance=nVar)%>%
  mutate(MonthYear=floor_date(Date, unit = "month"))

EDSM_regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower San Joaquin River")

EDSM<-smelt_edsm%>%
  dplyr::filter(!is.na(.data$Abundance))%>%
  dplyr::mutate(Variance=tidyr::replace_na(.data$Variance, 0))%>%
  dplyr::group_by(.data$Region, .data$MonthYear)%>%
  dplyr::summarise(Abundance=mean(.data$Abundance, na.rm=T), Abundance_CV=sqrt((1/dplyr::n()^2)*sum(.data$Variance))/.data$Abundance, .groups="drop")%>%
  dplyr::mutate(l95=stats::qlnorm(0.025, meanlog=log(.data$Abundance/sqrt(1+.data$Abundance_CV^2)), sdlog=log(1+.data$Abundance_CV^2)),
                u95=stats::qlnorm(0.975, meanlog=log(.data$Abundance/sqrt(1+.data$Abundance_CV^2)), sdlog=log(1+.data$Abundance_CV^2)))%>%
  dplyr::mutate(Abundance_l=log10(.data$Abundance+1),
                l95_l=log10(.data$l95),
                u95_l=log10(.data$u95))%>%
  {if (is.null(EDSM_regions)){
    .
  } else{
    dplyr::filter(., .data$Region%in%EDSM_regions)
  }}%>%
  dplyr::mutate(missing="na")%>%
  tidyr::complete(.data$MonthYear, .data$Region, fill=list(missing="n.d."))%>%
  dplyr::mutate(missing=dplyr::na_if(.data$missing, "na"))%>%
  dplyr::mutate(Region=factor(.data$Region, levels=EDSM_regions))

EDSMmissing<-EDSM%>%
  dplyr::filter(missing=="n.d.")%>%
  dplyr::select(.data$MonthYear, .data$Region)

EDSM<-EDSM%>%
  dplyr::filter(is.na(.data$missing))%>%
  dplyr::select(-.data$missing)

min(EDSM$l95_l,na.rm = T)
#Data frame to create the highlight for summer-fall of 2020 (or latest year)
EDSM_highlight<-tidyr::crossing(MinDate=lubridate::parse_date_time(paste0("06/", End_year), "%m/%Y"),MaxDate=lubridate::parse_date_time(paste0("10/", End_year), "%m/%Y"),Region=unique(EDSM$Region),mindata=0,maxdata=max(EDSM$u95_l,na.rm=T))
EDSM_highlight$MaxDate<-as.Date(EDSM_highlight$MaxDate)
EDSM_highlight$MinDate<-as.Date(EDSM_highlight$MinDate)

#Order the figure
EDSM$Region<-ordered(EDSM$Region, levels = c("Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower San Joaquin River","Suisun Bay", "Suisun Marsh", "Lower Sacramento River" ))

EDSM_plot<-ggplot2::ggplot()+
  ggplot2::geom_line(data=EDSM, ggplot2::aes(x=.data$MonthYear, y=.data$Abundance_l), color="darkorchid4")+
  ggplot2::geom_errorbar(data=EDSM, ggplot2::aes(x=.data$MonthYear, ymax=.data$u95_l, ymin=.data$l95_l))+
  ggplot2::geom_point(data=dplyr::filter(EDSM, lubridate::year(.data$MonthYear)!=End_year), ggplot2::aes(x=.data$MonthYear, y=.data$Abundance_l), color="darkorchid4")+
  ggplot2::geom_point(data=dplyr::filter(EDSM, lubridate::year(.data$MonthYear)==End_year), ggplot2::aes(x=.data$MonthYear, y=.data$Abundance_l), color="firebrick3", size=2.3)+
  ggplot2::geom_vline(data=EDSMmissing, ggplot2::aes(xintercept=.data$MonthYear), linetype=2)+
  ggplot2::geom_rect(data=EDSM_highlight, ggplot2::aes(xmin=.data$MinDate, xmax=.data$MaxDate, ymin=.data$mindata, ymax=.data$maxdata), alpha=0.3, fill="darkorange1")+
  ggplot2::coord_cartesian(ylim=c(2,5.7))+
  ggplot2::facet_wrap(~.data$Region)+
  ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks="3 months")+
  ggplot2::scale_y_continuous(labels = function(x) format(10^x, scientific=F, big.mark=","))+
  ggplot2::ylab("Delta Smelt abundance")+
  ggplot2::xlab("Date")+
  ggplot2::theme_bw()+
  ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5, size=20), axis.text.x = ggplot2::element_text(angle=45, hjust=1))
EDSM_plot


###############Create map

#Read EDSM subregion shape files and Delta shape files
EDSM_shape <- st_read(file.path(data_root,"Map/EDSM/2018-2019 Phase 1","Strata_1Dec2018_31Mar2019.shp"))
EDSM_shape$Stratum<-as.factor(EDSM_shape$Stratum)

delta_shape <- st_read(file.path(data_root,"Map/Bay Delta Shapefile","DeltaSubregionsWaterUTM.shp"))

#Change Joaquin River stratum to San Joaquin River
levels(EDSM_shape$Stratum)[levels(EDSM_shape$Stratum)=="Lower Joaquin River"] <- "Lower San Joaquin River"

#Subset just the regions used in the report figure
EDSM_shape_subset <- EDSM_shape %>% filter(Stratum %in% EDSM_regions)

#Show names for each subregion

delta_sf <-
  st_as_sf(EDSM_shape_subset) %>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

EDSM_map<-ggplot2::ggplot() +
  ggplot2::geom_sf(data = delta_shape, fill = 'grey', lwd = 0.05) +
  ggplot2::geom_sf(data = EDSM_shape_subset, fill=as.numeric(EDSM_shape_subset$Stratum), alpha=0.3, lwd = 1.5)+
  ggrepel::geom_label_repel(data = delta_sf,mapping = aes(COORDS_X, COORDS_Y, label = Stratum), size = 3, min.segment.length = 0) +
  ggplot2::coord_sf(crs = st_crs(delta_sf), datum = NA) + # styling - not necessary
  ggplot2::theme_void() # styling - not necessary
EDSM_map

#############Print figures side by side

tiff(filename=file.path(output_root,"Figure_EDSM_abundance_Region_Month.png"), 
      type="cairo",
      units="in", 
      width=8, #10*1, 
      height=10, #22*1, 
      pointsize=5, #12, 
      res=500,
      compression="lzw")
pushViewport(viewport(layout = grid.layout(2, 1)))
print(EDSM_map, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(EDSM_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()