#Load packages
library(readr)
library(rgdal)
library(sf)
library(tmap) 
library(tmaptools)
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(sf)
library(highcharter)
library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(raster)
library(downloader)
library(rgdal)
library(caret)
library(viridis)
library(hrbrthemes)
library(magrittr)

#-------------------------EMPLOYMENT SIZE MAPS--------------------------------#
#Create small business employment size map
#Import London Boundary Borough data (lb_borough)
lb_borough <- st_read("ESRI/London_Borough_Excluding_MHW.shp")
#Plot lb_borough using the qtm function
qtm(lb_borough)
summary(lb_borough)
#Read in small business employment data
bus_emp_sizeraw <- read_csv("bus_employment_size.csv",
                            locale = locale(encoding = "latin1"),
                            na = "n/a")
class(bus_emp_sizeraw)
summary(bus_emp_sizeraw)
#Select relevant columns
bus_emp_sizeraw <- bus_emp_sizeraw[,c(1:6,11:14,19:22,27:30,35:38,43:46)]
#Clean names
bus_emp_sizeraw <- clean_names(bus_emp_sizeraw)
#Remove unneccessary rows
bus_emp_sizeraw <- bus_emp_sizeraw[c(2:37),]

#Select empsize boroughs london (rows)
bus_empsize_bor <- bus_emp_sizeraw[c(1:33),]

#Select 2016 
bus_empsize_16 <- bus_empsize_bor[,c(1:2,7:10)]
colnames(bus_empsize_16)
names(bus_empsize_16)[names(bus_empsize_16) == "e16_0_4"] <- "0-4 Employees"
names(bus_empsize_16)[names(bus_empsize_16) == "e16_5_9"] <- "5-9 Employees"
names(bus_empsize_16)[names(bus_empsize_16) == "e16_10_19"] <- "10-19 Employees"
names(bus_empsize_16)[names(bus_empsize_16) == "e16_20_49"] <- "20-49 Employees"
bus_empsize_16
#Select 2017 
bus_empsize_17 <- bus_empsize_bor[,c(1:2,11:14)]
colnames(bus_empsize_17)
names(bus_empsize_17)[names(bus_empsize_17) == "e17_0_4"] <- "0-4 Employees"
names(bus_empsize_17)[names(bus_empsize_17) == "e17_5_9"] <- "5-9 Employees"
names(bus_empsize_17)[names(bus_empsize_17) == "e17_10_19"] <- "10-19 Employees"
names(bus_empsize_17)[names(bus_empsize_17) == "e17_20_49"] <- "20-49 Employees"
bus_empsize_17
#Select 2018 
bus_empsize_18 <- bus_empsize_bor[,c(1:2,15:18)]
colnames(bus_empsize_18)
names(bus_empsize_18)[names(bus_empsize_18) == "e18_0_4"] <- "0-4 Employees"
names(bus_empsize_18)[names(bus_empsize_18) == "e18_5_9"] <- "5-9 Employees"
names(bus_empsize_18)[names(bus_empsize_18) == "e18_10_19"] <- "10-19 Employees"
names(bus_empsize_18)[names(bus_empsize_18) == "e18_20_49"] <- "20-49 Employees"
bus_empsize_18
#Select 2019 
bus_empsize_19 <- bus_empsize_bor[,c(1:2,19:22)]
colnames(bus_empsize_19)
names(bus_empsize_19)[names(bus_empsize_19) == "e19_0_4"] <- "0-4 Employees"
names(bus_empsize_19)[names(bus_empsize_19) == "e19_5_9"] <- "5-9 Employees"
names(bus_empsize_19)[names(bus_empsize_19) == "e19_10_19"] <- "10-19 Employees"
names(bus_empsize_19)[names(bus_empsize_19) == "e19_20_49"] <- "20-49 Employees"
bus_empsize_19
#Select 2020 
bus_empsize_20 <- bus_empsize_bor[,c(1:2,23:26)]
colnames(bus_empsize_20)
names(bus_empsize_20)[names(bus_empsize_20) == "e20_0_4"] <- "0-4 Employees"
names(bus_empsize_20)[names(bus_empsize_20) == "e20_5_9"] <- "5-9 Employees"
names(bus_empsize_20)[names(bus_empsize_20) == "e20_10_19"] <- "10-19 Employees"
names(bus_empsize_20)[names(bus_empsize_20) == "e20_20_49"] <- "20-49 Employees"
bus_empsize_20

#Create a bigger bounding box for the map (to fit the legend and title)
bbox_new <- st_bbox(mergelbempsize20)
xrange <- bbox_new$xmax - bbox_new$xmin 
yrange <- bbox_new$ymax - bbox_new$ymin
bbox_new[4] <- bbox_new[4] + (0.5 * yrange)
bbox_new <- bbox_new %>%
  st_as_sfc()

#Map all years of data
#Set mode to plotting
tmap_mode("plot")
#Merge lb_borough data with bus_empsize_16
mergelbempsize16 <- lb_borough%>%
  filter(str_detect(GSS_CODE, "^E09"))%>%
  merge(.,
        bus_empsize_16,
        by.x=c("GSS_CODE","NAME"),
        by.y=c("code","area"),
        no.dups = TRUE)%>%
  distinct(.,GSS_CODE,
           .keep_all = TRUE)
#Map mergelbempsize16
empsize16_map <- tm_shape(mergelbempsize16, bbox = bbox_new)+
  tm_polygons(c("0-4 Employees","5-9 Employees","10-19 Employees","20-49 Employees"),
              style="pretty",
              palette="Pastel1",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) +
  tm_layout(title = "2016",
            title.size = 1,
            frame = FALSE,
            legend.show = TRUE,
            legend.width = 2) +
  tm_facets(sync = TRUE, ncol = 4) +
  #Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4) +
  tm_credits("(c) ONS (2020)", position=c("right", "bottom"))+
  tm_compass(position = c("left","bottom"),type = "arrow")+
  tm_scale_bar(position = c("left","bottom"))
#Print map
empsize16_map


#Merge lb_borough data with bus_empsize_17
mergelbempsize17 <- lb_borough%>%
  filter(str_detect(GSS_CODE, "^E09"))%>%
  merge(.,
        bus_empsize_17,
        by.x=c("GSS_CODE","NAME"),
        by.y=c("code","area"),
        no.dups = TRUE)%>%
  distinct(.,GSS_CODE,
           .keep_all = TRUE)
#Map mergelbempsize17
#Define the map elements
empsize17_map <- tm_shape(mergelbempsize17, bbox = bbox_new)+
  tm_polygons(c("0-4 Employees","5-9 Employees","10-19 Employees","20-49 Employees"),
              style="pretty",
              palette="Pastel1",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) +
  tm_layout(title = "2017",
            title.size = 1,
            frame = FALSE,
            legend.show = TRUE,
            legend.width = 2) +
  tm_facets(sync = TRUE, ncol = 4) +
  #Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4) +
  tm_credits("(c) ONS (2020)", position=c("right", "bottom"))+
  tm_compass(position = c("left","bottom"),type = "arrow")+
  tm_scale_bar(position = c("left","bottom"))
#Print map
empsize17_map
            
#Merge lb_borough data with bus_empsize_18
mergelbempsize18 <- lb_borough%>%
  filter(str_detect(GSS_CODE, "^E09"))%>%
  merge(.,
        bus_empsize_18,
        by.x=c("GSS_CODE","NAME"),
        by.y=c("code","area"),
        no.dups = TRUE)%>%
  distinct(.,GSS_CODE,
           .keep_all = TRUE)
#Map mergelbempsize18
#Define the map elements
empsize18_map <- tm_shape(mergelbempsize18, bbox = bbox_new)+
  tm_polygons(c("0-4 Employees","5-9 Employees","10-19 Employees","20-49 Employees"),
              style="pretty",
              palette="Pastel1",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) + 
  tm_layout(title = "2018",
            title.size = 1,
            frame = FALSE,
            legend.show = TRUE,
            legend.width = 2) +
  tm_facets(sync = TRUE, ncol = 4) +
  #Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4) +
  tm_credits("(c) ONS (2020)", position=c("right", "bottom"))+
  tm_compass(position = c("left","bottom"),type = "arrow")+
  tm_scale_bar(position = c("left","bottom"))
#Print map
empsize18_map
            
#Merge lb_borough data with bus_empsize_19
mergelbempsize19 <- lb_borough%>%
  filter(str_detect(GSS_CODE, "^E09"))%>%
  merge(.,
        bus_empsize_19,
        by.x=c("GSS_CODE","NAME"),
        by.y=c("code","area"),
        no.dups = TRUE)%>%
  distinct(.,GSS_CODE,
           .keep_all = TRUE)
#Map mergelbempsize19
#Define the map elements
empsize19_map <- tm_shape(mergelbempsize19, bbox = bbox_new)+
  tm_polygons(c("0-4 Employees","5-9 Employees","10-19 Employees","20-49 Employees"),
              style="pretty",
              palette="Pastel1",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) +
  tm_layout(title = "2019",
            title.size = 1,
            frame = FALSE,
            legend.show = TRUE,
            legend.width = 2) +
  tm_facets(sync = TRUE, ncol = 4) +
  #Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4) +
  tm_credits("(c) ONS (2020)", position=c("right", "bottom"))+
  tm_compass(position = c("left","bottom"),type = "arrow")+
  tm_scale_bar(position = c("left","bottom"))
#Print map
empsize19_map
            
#Merge lb_borough data with bus_empsize_20
mergelbempsize20 <- lb_borough%>%
  filter(str_detect(GSS_CODE, "^E09"))%>%
  merge(.,
        bus_empsize_20,
        by.x=c("GSS_CODE","NAME"),
        by.y=c("code","area"),
        no.dups = TRUE)%>%
  distinct(.,GSS_CODE,
           .keep_all = TRUE)
#Map mergelbempsize20
#Define the map elements
empsize20_map <- tm_shape(mergelbempsize20, bbox = bbox_new)+
  tm_polygons(c("0-4 Employees","5-9 Employees","10-19 Employees","20-49 Employees"),
              style="pretty",
              palette="Pastel1",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) +
  tm_layout(title = "2020",
            title.size = 1,
            frame = FALSE,
            legend.show = TRUE,
            legend.width = 2) +
  tm_facets(sync = TRUE, ncol = 4) +
#Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4)+
  tm_credits("(c) ONS (2020)", position=c("right", "bottom"))+
  tm_compass(position = c("left","bottom"),type = "arrow")+
  tm_scale_bar(position = c("left","bottom"))
#Print map
empsize20_map

#Export maps to PNG (toggle the year in the file name)
tmap_save(empsize16_map, filename = "empsize16_map.png",
          width = 297,
          height = 200,
          units = "mm",
          dpi = 300)
tmap_save(empsize17_map, filename = "empsize17_map.png",
          width = 297,
          height = 200,
          units = "mm",
          dpi = 300)
tmap_save(empsize18_map, filename = "empsize18_map.png",
          width = 297,
          height = 200,
          units = "mm",
          dpi = 300)
tmap_save(empsize19_map, filename = "empsize19_map.png",
          width = 297,
          height = 200,
          units = "mm",
          dpi = 300)
tmap_save(empsize20_map, filename = "empsize20_map.png",
          width = 297,
          height = 200,
          units = "mm",
          dpi = 300)

