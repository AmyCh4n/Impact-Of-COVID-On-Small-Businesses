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
library(OpenStreetMap)
library(marmap)

#-----------------------ANCHOR INSTITUTION LOCATION MAPS---------------------#
#Create anchor institution location map
#Import London Boundary Borough data (lb_borough)
lb_borough <- st_read("ESRI/London_Borough_Excluding_MHW.shp")
lb_lsoa <- st_read("ESRI/LSOA_2011_London_gen_MHW.shp")

#Plot lb_borough using the qtm function
qtm(lb_borough)
summary(lb_borough)

#Import anchor institution location points
anchor_loc_uni <- st_read("universities.kml")
anchor_loc_coll <- st_read("college.kml")
anchor_loc_nhs <- st_read("nhs.kml")
#Plot lb_borough using the qtm function
qtm(anchor_loc_uni)
qtm(anchor_loc_coll)
qtm(anchor_loc_nhs)
summary(anchor_loc_uni)
summary(anchor_loc_coll)
summary(anchor_loc_nhs)

#Set tmap to interactive map view
tmap_mode("view")
##OR Set tmap to plot 
tmap_mode("plot")
#Plot where the anchor institutions are located (separate graphs for different 
#institution groups)
#Code for base map (run only if want osm base map)
lb_lsoa <- lb_lsoa %>% 
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL, alpha = 0.5)
#include following code prior to mapping pop or retail code
uni <- tm_shape(lb_lsoa)+
  tm_rgb()+
tm_shape(lb_borough) +
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(anchor_loc_uni) +
  tm_dots(col = "blue",
          size = 0.1,
          shape = 19,
          title = NA,
          legend.show = TRUE) +
  tm_add_legend(
    type = "symbol",
    labels = "Universities",
    size = 0.5,
    col = "blue",
    border.alpha = 0) +
  tm_layout(
    title = "University Locations",
    legend.outside = TRUE, 
    legend.outside.position = c("bottom"),
    frame = FALSE) +
  tm_credits("(c) Google Maps (2021)", position=c("right", "bottom"))+
  tm_compass(position = c("left","bottom"),type = "arrow")+
  tm_scale_bar(position = c("left","bottom"))

college <- tm_shape(lb_lsoa)+
  tm_rgb()+
  tm_shape(lb_borough) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(anchor_loc_coll) +
  tm_dots(col = "red",
          size = 0.1,
          shape = 19,
          title = NA,
          legend.show = TRUE) +
  tm_add_legend(
    type = "symbol",
    labels = "Higher Education Colleges",
    size = 0.5,
    col = "red",
    border.alpha = 0) +
  tm_layout(
    title = "Higher Education College Locations",
    legend.outside = TRUE, 
    legend.outside.position = c("bottom"),
    frame = FALSE) +
  tm_credits("(c) Google Maps (2021)", position=c("right", "bottom"))+
  tm_compass(position = c("left","bottom"),type = "arrow")+
  tm_scale_bar(position = c("left","bottom"))
  
nhs <- tm_shape(lb_lsoa)+
  tm_rgb()+
  tm_shape(lb_borough) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(anchor_loc_nhs) +
  tm_dots(col = "green",
          size = 0.1,
          shape = 19,
          title = NA,
          legend.show = TRUE) +
  tm_add_legend(
    type = "symbol",
    labels = "NHS Hospitals and locations",
    size = 0.5,
    col = "green",
    border.alpha = 0) +
  tm_layout(
    title = "NHS Locations",
    legend.outside = TRUE, 
    legend.outside.position = c("bottom"),
    frame = FALSE) +
  tm_credits("(c) Google Maps (2021)", position=c("right", "bottom"))+
  tm_compass(position = c("left","bottom"),type = "arrow")+
  tm_scale_bar(position = c("left","bottom"))
#Arrange the maps together
anch_inst_maps <- tmap_arrange(uni,college,nhs,
             ncol = 1,
             nrow = 3)

#Print out the map
tmap_save(anch_inst_maps, filename = "anch_inst_maps.png",
          width = 100,
          height = 425,
          units = "mm",
          dpi = 300)

#-------------------ANCHOR INSTITUTION CATCHMENT AREA MAPS---------------------#
#Convert the long and lat into sf and export as geojson for QGIS
anchor_loc_uni2 <- read_csv("universities.csv")
anchor_loc_coll2 <- read_csv("college.csv")
anchor_loc_nhs2 <- read_csv("nhs.csv")

anchor_loc_uni2 <- anchor_loc_uni2[,c(3,4)]
uni_points <- st_as_sf(anchor_loc_uni2, coords = c("longitude","latitude"), crs=4326)
st_write(uni_points,"uni_points.geojson")

anchor_loc_coll2 <- anchor_loc_coll2[,c(3,4)]
coll_points <- st_as_sf(anchor_loc_coll2, coords = c("longitude","latitude"), crs=4326)
st_write(coll_points,"coll_points.geojson")

anchor_loc_nhs2 <- anchor_loc_nhs2[,c(3,4)]
nhs_points <- st_as_sf(anchor_loc_nhs2, coords = c("longitude","latitude"), crs=4326)
st_write(nhs_points,"nhs_points.geojson")











