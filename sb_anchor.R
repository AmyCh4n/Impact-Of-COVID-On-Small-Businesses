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

#Separate out the data into separate sic code group datasets for QGIS
#Read in small business employment data
sb_raw <- read_csv("small-business-info.csv",
                   locale = locale(encoding = "latin1"),
                   na = "n/a")
class(sb_raw)
summary(sb_raw)
#Clean and rename columns
sb_raw <- clean_names(sb_raw)

#Read in FAME data
fame_raw <- read_csv("fame.csv",
                     locale = locale(encoding = "latin1"),
                     na = "n/a")
class(fame_raw)
summary(fame_raw)
#Select only relevant columns from fame
fame_raw <- fame_raw[,c(2:3,6:7)]
#Clean and rename columns in FAME dataset
fame_raw <- clean_names(fame_raw)
colnames(fame_raw)
names(fame_raw)[names(fame_raw) == "registered_number"] <- "company_number"

#Read in the postcode data with long lat
pc_raw <- read_csv("ukpostcodes.csv",
                   locale = locale(encoding = "latin1"),
                   na = "n/a")
class(pc_raw)
summary(pc_raw)
#Clean and rename columns
pc <- clean_names(pc_raw)
colnames(pc)
names(pc)[names(pc) == "postcode"] <- "r_o_full_postcode"

#Read in the postcode data with boroughs
pcb_raw <- read_csv("all_postcodes.csv",
                   locale = locale(encoding = "latin1"),
                   na = "n/a")
class(pcb_raw)
summary(pcb_raw)
#Take relevant columns
pcb <- pcb_raw[,c(3,21)]
#Clean and rename columns
pcb <- clean_names(pcb)
colnames(pcb)
names(pcb)[names(pcb) == "pcds"] <- "r_o_full_postcode"

#Merge FAME and sb data 
sb_location <- left_join(fame_raw,sb_raw,by=c("company_name","company_number"))
#Merge postcodes to get boroughs
sb_location <- left_join(sb_location,pcb,by="r_o_full_postcode")
#Merge postcodes to get long and lat
sb_location <- left_join(sb_location,pc,by="r_o_full_postcode")

#Take relevant columns
sb_location <- sb_location[,c(1:4,36,38:39)]
colnames(sb_location)
names(sb_location)[names(sb_location) == "primary_uk_sic_2007_code"] <- "sic_code"

#Make sure only london boroughs have been included 
sb_location <- sb_location %>% 
  filter(str_detect(ladcd,"^E09"))

#Separate out the data into separate datasets by general SIC code group
#Filter by general SIC code group
manufacturing <- sb_location %>% 
  filter(str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43"))
wholesale <- sb_location %>% 
  filter(str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53"))
services <- sb_location %>% 
  filter(str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96"))
other <- sb_location %>% 
  filter(str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))

#Remove NA values so that file can be converted into sf
manufacturing_na_removed <- manufacturing %>% 
  filter(!is.na(longitude)) %>% 
  filter(!is.na(latitude))

wholesale_na_removed <- wholesale %>% 
  filter(!is.na(longitude)) %>% 
  filter(!is.na(latitude))

services_na_removed <- services %>% 
  filter(!is.na(longitude)) %>% 
  filter(!is.na(latitude))

other_na_removed <- other %>% 
  filter(!is.na(longitude)) %>% 
  filter(!is.na(latitude))

#Convert into sf and export into goejson file
man_points <- st_as_sf(manufacturing_na_removed, 
                       coords = c("longitude","latitude"), 
                       crs=4326)
st_write(man_points,"man_points.geojson")

who_points <- st_as_sf(wholesale_na_removed, 
                       coords = c("longitude","latitude"), 
                       crs=4326)
st_write(who_points,"who_points.geojson")

ser_points <- st_as_sf(services_na_removed, 
                       coords = c("longitude","latitude"), 
                       crs=4326)
st_write(ser_points,"ser_points.geojson")

oth_points <- st_as_sf(other_na_removed, 
                       coords = c("longitude","latitude"), 
                       crs=4326)
st_write(oth_points,"oth_points.geojson")


