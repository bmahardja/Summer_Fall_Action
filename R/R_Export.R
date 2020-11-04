library(tidyverse)
library(lubridate)
library(viridis)

#Path to local drive
root <- "~/GitHub/Summer_Fall_Action"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")


data_export <- read_csv(file.path(data_root, "USBR", "June_Oct_Exports.csv"))
data_export_edit<-data_export %>% mutate (Federal = Tracy, State = CCF) %>% select("Date","State","Federal") %>%
  tidyr::gather("Facility","Export",2:3)

export_plot<-ggplot(data_export_edit, aes(fill=Facility, y=Export, x=Date)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Summer-Fall 2020") +
  theme_bw() +
  ggplot2::ylab(bquote("Daily"~export~"("*ft^3*"/s)"))+
  xlab("")+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.position = c(0.8, 0.8),
                 legend.background = element_rect(fill="white",
                                                  size=0.5, linetype="solid", 
                                                  colour ="black"),
                 strip.background = element_rect(size=0.3)) 

#############Print figure

tiff(filename=file.path(output_root,"Figure_Export.png"), 
     type="cairo",
     units="in", 
     width=12, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
print(export_plot)
dev.off()