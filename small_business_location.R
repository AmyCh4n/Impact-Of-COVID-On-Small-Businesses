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

#-------------------------SMALL BUSINESS LOCATION MAPS------------------------#
#Create small business location map
#Import London Boundary Borough data (lb_borough)
lb_borough <- st_read("ESRI/London_Borough_Excluding_MHW.shp")
#Plot lb_borough using the qtm function
qtm(lb_borough)
summary(lb_borough)

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

#Read in the postcode data
pc_raw <- read_csv("all_postcodes.csv",
                   locale = locale(encoding = "latin1"),
                   na = "n/a")
class(pc_raw)
summary(pc_raw)
#Take relevant columns
pc <- pc_raw[,c(3,21)]
#Clean and rename columns
pc <- clean_names(pc)
colnames(pc)
names(pc)[names(pc) == "pcds"] <- "r_o_full_postcode"

#Merge FAME and sb data 
sb_location <- left_join(fame_raw,sb_raw,by=c("company_name","company_number"))
#Merge postcodes to get boroughs
sb_location <- left_join(sb_location,pc,by="r_o_full_postcode")
#Take relevant columns
sb_location <- sb_location[,c(1:4,36)]
colnames(sb_location)
names(sb_location)[names(sb_location) == "primary_uk_sic_2007_code"] <- "sic_code"

#Filter by borough
city <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000001"))
barkdag <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000002"))
barnet <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000003"))
bexley <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000004"))
brent <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000005"))
bromley <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000006"))
camden <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000007"))
croydon <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000008"))
ealing <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000009"))
enfield <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000010"))
greenwich <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000011"))
hackney <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000012"))
hamful <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000013"))
haringey <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000014"))
harrow <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000015"))
havering <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000016"))
hillingdon <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000017"))
hounslow <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000018"))
islington <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000019"))
kenchel <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000020"))
kingontham <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000021"))
lambeth <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000022"))
lewisham <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000023"))
merton <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000024"))
newham <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000025"))
redbridge <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000026"))
richontham <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000027"))
southwark <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000028"))
sutton <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000029"))
towerham <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000030"))
waltfor <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000031"))
wandsworth <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000032"))
westminster <- sb_location %>% 
  filter(str_detect(`ladcd`, "E09000033"))

#For each borough, count how many businesses per sic code 
city <- city %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "City of London"
area <- "E09000001"
city_siccount <- as.data.frame(borough)
city_siccount <- city_siccount %>% 
  mutate(area) %>%
  mutate(sum(city$manufacturing > 0)) %>% 
  mutate(sum(city$wholesale_trans_logist > 0)) %>% 
  mutate(sum(city$services > 0)) %>% 
  mutate(sum(city$other > 0))
colnames(city_siccount)
names(city_siccount)[names(city_siccount) == "sum(city$manufacturing > 0)"] <- "manufacturing"
names(city_siccount)[names(city_siccount) == "sum(city$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(city_siccount)[names(city_siccount) == "sum(city$services > 0)"] <- "services"
names(city_siccount)[names(city_siccount) == "sum(city$other > 0)"] <- "other"

barkdag <- barkdag %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Barking and Dagenham"
area <- "E09000002"
barkdag_siccount <- as.data.frame(borough)
barkdag_siccount <- barkdag_siccount %>% 
  mutate(area) %>%
  mutate(sum(barkdag$manufacturing > 0)) %>% 
  mutate(sum(barkdag$wholesale_trans_logist > 0)) %>% 
  mutate(sum(barkdag$services > 0)) %>% 
  mutate(sum(barkdag$other > 0))
colnames(barkdag_siccount)
names(barkdag_siccount)[names(barkdag_siccount) == "sum(barkdag$manufacturing > 0)"] <- "manufacturing"
names(barkdag_siccount)[names(barkdag_siccount) == "sum(barkdag$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(barkdag_siccount)[names(barkdag_siccount) == "sum(barkdag$services > 0)"] <- "services"
names(barkdag_siccount)[names(barkdag_siccount) == "sum(barkdag$other > 0)"] <- "other"

barnet <- barnet %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Barnet"
area <- "E09000003"
barnet_siccount <- as.data.frame(borough)
barnet_siccount <- barnet_siccount %>% 
  mutate(area) %>%
  mutate(sum(barnet$manufacturing > 0)) %>% 
  mutate(sum(barnet$wholesale_trans_logist > 0)) %>% 
  mutate(sum(barnet$services > 0)) %>% 
  mutate(sum(barnet$other > 0))
colnames(barnet_siccount)
names(barnet_siccount)[names(barnet_siccount) == "sum(barnet$manufacturing > 0)"] <- "manufacturing"
names(barnet_siccount)[names(barnet_siccount) == "sum(barnet$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(barnet_siccount)[names(barnet_siccount) == "sum(barnet$services > 0)"] <- "services"
names(barnet_siccount)[names(barnet_siccount) == "sum(barnet$other > 0)"] <- "other"

bexley <- bexley %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Bexley"
area <- "E09000004"
bexley_siccount <- as.data.frame(borough)
bexley_siccount <- bexley_siccount %>% 
  mutate(area) %>%
  mutate(sum(bexley$manufacturing > 0)) %>% 
  mutate(sum(bexley$wholesale_trans_logist > 0)) %>% 
  mutate(sum(bexley$services > 0)) %>% 
  mutate(sum(bexley$other > 0))
colnames(bexley_siccount)
names(bexley_siccount)[names(bexley_siccount) == "sum(bexley$manufacturing > 0)"] <- "manufacturing"
names(bexley_siccount)[names(bexley_siccount) == "sum(bexley$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(bexley_siccount)[names(bexley_siccount) == "sum(bexley$services > 0)"] <- "services"
names(bexley_siccount)[names(bexley_siccount) == "sum(bexley$other > 0)"] <- "other"

brent <- brent %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Brent"
area <- "E09000005"
brent_siccount <- as.data.frame(borough)
brent_siccount <- brent_siccount %>% 
  mutate(area) %>%
  mutate(sum(brent$manufacturing > 0)) %>% 
  mutate(sum(brent$wholesale_trans_logist > 0)) %>% 
  mutate(sum(brent$services > 0)) %>% 
  mutate(sum(brent$other > 0))
colnames(brent_siccount)
names(brent_siccount)[names(brent_siccount) == "sum(brent$manufacturing > 0)"] <- "manufacturing"
names(brent_siccount)[names(brent_siccount) == "sum(brent$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(brent_siccount)[names(brent_siccount) == "sum(brent$services > 0)"] <- "services"
names(brent_siccount)[names(brent_siccount) == "sum(brent$other > 0)"] <- "other"

bromley <- bromley %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Bromley"
area <- "E09000006"
bromley_siccount <- as.data.frame(borough)
bromley_siccount <- bromley_siccount %>% 
  mutate(area) %>%
  mutate(sum(bromley$manufacturing > 0)) %>% 
  mutate(sum(bromley$wholesale_trans_logist > 0)) %>% 
  mutate(sum(bromley$services > 0)) %>% 
  mutate(sum(bromley$other > 0))
colnames(bromley_siccount)
names(bromley_siccount)[names(bromley_siccount) == "sum(bromley$manufacturing > 0)"] <- "manufacturing"
names(bromley_siccount)[names(bromley_siccount) == "sum(bromley$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(bromley_siccount)[names(bromley_siccount) == "sum(bromley$services > 0)"] <- "services"
names(bromley_siccount)[names(bromley_siccount) == "sum(bromley$other > 0)"] <- "other"

camden <- camden %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Camden"
area <- "E09000007"
camden_siccount <- as.data.frame(borough)
camden_siccount <- camden_siccount %>% 
  mutate(area) %>%
  mutate(sum(camden$manufacturing > 0)) %>% 
  mutate(sum(camden$wholesale_trans_logist > 0)) %>% 
  mutate(sum(camden$services > 0)) %>% 
  mutate(sum(camden$other > 0))
colnames(camden_siccount)
names(camden_siccount)[names(camden_siccount) == "sum(camden$manufacturing > 0)"] <- "manufacturing"
names(camden_siccount)[names(camden_siccount) == "sum(camden$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(camden_siccount)[names(camden_siccount) == "sum(camden$services > 0)"] <- "services"
names(camden_siccount)[names(camden_siccount) == "sum(camden$other > 0)"] <- "other"

croydon <- croydon %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Croydon"
area <- "E09000008"
croydon_siccount <- as.data.frame(borough)
croydon_siccount <- croydon_siccount %>% 
  mutate(area) %>%
  mutate(sum(croydon$manufacturing > 0)) %>% 
  mutate(sum(croydon$wholesale_trans_logist > 0)) %>% 
  mutate(sum(croydon$services > 0)) %>% 
  mutate(sum(croydon$other > 0))
colnames(croydon_siccount)
names(croydon_siccount)[names(croydon_siccount) == "sum(croydon$manufacturing > 0)"] <- "manufacturing"
names(croydon_siccount)[names(croydon_siccount) == "sum(croydon$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(croydon_siccount)[names(croydon_siccount) == "sum(croydon$services > 0)"] <- "services"
names(croydon_siccount)[names(croydon_siccount) == "sum(croydon$other > 0)"] <- "other"

ealing <- ealing %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Ealing"
area <- "E09000009"
ealing_siccount <- as.data.frame(borough)
ealing_siccount <- ealing_siccount %>% 
  mutate(area) %>%
  mutate(sum(ealing$manufacturing > 0)) %>% 
  mutate(sum(ealing$wholesale_trans_logist > 0)) %>% 
  mutate(sum(ealing$services > 0)) %>% 
  mutate(sum(ealing$other > 0))
colnames(ealing_siccount)
names(ealing_siccount)[names(ealing_siccount) == "sum(ealing$manufacturing > 0)"] <- "manufacturing"
names(ealing_siccount)[names(ealing_siccount) == "sum(ealing$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(ealing_siccount)[names(ealing_siccount) == "sum(ealing$services > 0)"] <- "services"
names(ealing_siccount)[names(ealing_siccount) == "sum(ealing$other > 0)"] <- "other"

enfield <- enfield %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Enfield"
area <- "E09000010"
enfield_siccount <- as.data.frame(borough)
enfield_siccount <- enfield_siccount %>% 
  mutate(area) %>%
  mutate(sum(enfield$manufacturing > 0)) %>% 
  mutate(sum(enfield$wholesale_trans_logist > 0)) %>% 
  mutate(sum(enfield$services > 0)) %>% 
  mutate(sum(enfield$other > 0))
colnames(enfield_siccount)
names(enfield_siccount)[names(enfield_siccount) == "sum(enfield$manufacturing > 0)"] <- "manufacturing"
names(enfield_siccount)[names(enfield_siccount) == "sum(enfield$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(enfield_siccount)[names(enfield_siccount) == "sum(enfield$services > 0)"] <- "services"
names(enfield_siccount)[names(enfield_siccount) == "sum(enfield$other > 0)"] <- "other"

greenwich <- greenwich %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Greenwich"
area <- "E09000011"
greenwich_siccount <- as.data.frame(borough)
greenwich_siccount <- greenwich_siccount %>% 
  mutate(area) %>%
  mutate(sum(greenwich$manufacturing > 0)) %>% 
  mutate(sum(greenwich$wholesale_trans_logist > 0)) %>% 
  mutate(sum(greenwich$services > 0)) %>% 
  mutate(sum(greenwich$other > 0))
colnames(greenwich_siccount)
names(greenwich_siccount)[names(greenwich_siccount) == "sum(greenwich$manufacturing > 0)"] <- "manufacturing"
names(greenwich_siccount)[names(greenwich_siccount) == "sum(greenwich$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(greenwich_siccount)[names(greenwich_siccount) == "sum(greenwich$services > 0)"] <- "services"
names(greenwich_siccount)[names(greenwich_siccount) == "sum(greenwich$other > 0)"] <- "other"

hackney <- hackney %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Hackney"
area <- "E09000012"
hackney_siccount <- as.data.frame(borough)
hackney_siccount <- hackney_siccount %>% 
  mutate(area) %>%
  mutate(sum(hackney$manufacturing > 0)) %>% 
  mutate(sum(hackney$wholesale_trans_logist > 0)) %>% 
  mutate(sum(hackney$services > 0)) %>% 
  mutate(sum(hackney$other > 0))
colnames(hackney_siccount)
names(hackney_siccount)[names(hackney_siccount) == "sum(hackney$manufacturing > 0)"] <- "manufacturing"
names(hackney_siccount)[names(hackney_siccount) == "sum(hackney$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(hackney_siccount)[names(hackney_siccount) == "sum(hackney$services > 0)"] <- "services"
names(hackney_siccount)[names(hackney_siccount) == "sum(hackney$other > 0)"] <- "other"

hamful <- hamful %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Hammersmith and Fullham"
area <- "E09000013"
hamful_siccount <- as.data.frame(borough)
hamful_siccount <- hamful_siccount %>% 
  mutate(area) %>%
  mutate(sum(hamful$manufacturing > 0)) %>% 
  mutate(sum(hamful$wholesale_trans_logist > 0)) %>% 
  mutate(sum(hamful$services > 0)) %>% 
  mutate(sum(hamful$other > 0))
colnames(hamful_siccount)
names(hamful_siccount)[names(hamful_siccount) == "sum(hamful$manufacturing > 0)"] <- "manufacturing"
names(hamful_siccount)[names(hamful_siccount) == "sum(hamful$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(hamful_siccount)[names(hamful_siccount) == "sum(hamful$services > 0)"] <- "services"
names(hamful_siccount)[names(hamful_siccount) == "sum(hamful$other > 0)"] <- "other"

haringey <- haringey %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Haringey"
area <- "E09000014"
haringey_siccount <- as.data.frame(borough)
haringey_siccount <- haringey_siccount %>% 
  mutate(area) %>%
  mutate(sum(haringey$manufacturing > 0)) %>% 
  mutate(sum(haringey$wholesale_trans_logist > 0)) %>% 
  mutate(sum(haringey$services > 0)) %>% 
  mutate(sum(haringey$other > 0))
colnames(haringey_siccount)
names(haringey_siccount)[names(haringey_siccount) == "sum(haringey$manufacturing > 0)"] <- "manufacturing"
names(haringey_siccount)[names(haringey_siccount) == "sum(haringey$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(haringey_siccount)[names(haringey_siccount) == "sum(haringey$services > 0)"] <- "services"
names(haringey_siccount)[names(haringey_siccount) == "sum(haringey$other > 0)"] <- "other"

harrow <- harrow %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Harrow"
area <- "E09000015"
harrow_siccount <- as.data.frame(borough)
harrow_siccount <- harrow_siccount %>% 
  mutate(area) %>%
  mutate(sum(harrow$manufacturing > 0)) %>% 
  mutate(sum(harrow$wholesale_trans_logist > 0)) %>% 
  mutate(sum(harrow$services > 0)) %>% 
  mutate(sum(harrow$other > 0))
colnames(harrow_siccount)
names(harrow_siccount)[names(harrow_siccount) == "sum(harrow$manufacturing > 0)"] <- "manufacturing"
names(harrow_siccount)[names(harrow_siccount) == "sum(harrow$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(harrow_siccount)[names(harrow_siccount) == "sum(harrow$services > 0)"] <- "services"
names(harrow_siccount)[names(harrow_siccount) == "sum(harrow$other > 0)"] <- "other"

havering <- havering %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Havering"
area <- "E09000016"
havering_siccount <- as.data.frame(borough)
havering_siccount <- havering_siccount %>% 
  mutate(area) %>%
  mutate(sum(havering$manufacturing > 0)) %>% 
  mutate(sum(havering$wholesale_trans_logist > 0)) %>% 
  mutate(sum(havering$services > 0)) %>% 
  mutate(sum(havering$other > 0))
colnames(havering_siccount)
names(havering_siccount)[names(havering_siccount) == "sum(havering$manufacturing > 0)"] <- "manufacturing"
names(havering_siccount)[names(havering_siccount) == "sum(havering$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(havering_siccount)[names(havering_siccount) == "sum(havering$services > 0)"] <- "services"
names(havering_siccount)[names(havering_siccount) == "sum(havering$other > 0)"] <- "other"

hillingdon <- hillingdon %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Hillingdon"
area <- "E09000017"
hillingdon_siccount <- as.data.frame(borough)
hillingdon_siccount <- hillingdon_siccount %>% 
  mutate(area) %>%
  mutate(sum(hillingdon$manufacturing > 0)) %>% 
  mutate(sum(hillingdon$wholesale_trans_logist > 0)) %>% 
  mutate(sum(hillingdon$services > 0)) %>% 
  mutate(sum(hillingdon$other > 0))
colnames(hillingdon_siccount)
names(hillingdon_siccount)[names(hillingdon_siccount) == "sum(hillingdon$manufacturing > 0)"] <- "manufacturing"
names(hillingdon_siccount)[names(hillingdon_siccount) == "sum(hillingdon$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(hillingdon_siccount)[names(hillingdon_siccount) == "sum(hillingdon$services > 0)"] <- "services"
names(hillingdon_siccount)[names(hillingdon_siccount) == "sum(hillingdon$other > 0)"] <- "other"

hounslow <- hounslow %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Hounslow"
area <- "E09000018"
hounslow_siccount <- as.data.frame(borough)
hounslow_siccount <- hounslow_siccount %>% 
  mutate(area) %>%
  mutate(sum(hounslow$manufacturing > 0)) %>% 
  mutate(sum(hounslow$wholesale_trans_logist > 0)) %>% 
  mutate(sum(hounslow$services > 0)) %>% 
  mutate(sum(hounslow$other > 0))
colnames(hounslow_siccount)
names(hounslow_siccount)[names(hounslow_siccount) == "sum(hounslow$manufacturing > 0)"] <- "manufacturing"
names(hounslow_siccount)[names(hounslow_siccount) == "sum(hounslow$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(hounslow_siccount)[names(hounslow_siccount) == "sum(hounslow$services > 0)"] <- "services"
names(hounslow_siccount)[names(hounslow_siccount) == "sum(hounslow$other > 0)"] <- "other"

islington <- islington %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Islington"
area <- "E09000019"
islington_siccount <- as.data.frame(borough)
islington_siccount <- islington_siccount %>% 
  mutate(area) %>%
  mutate(sum(islington$manufacturing > 0)) %>% 
  mutate(sum(islington$wholesale_trans_logist > 0)) %>% 
  mutate(sum(islington$services > 0)) %>% 
  mutate(sum(islington$other > 0))
colnames(islington_siccount)
names(islington_siccount)[names(islington_siccount) == "sum(islington$manufacturing > 0)"] <- "manufacturing"
names(islington_siccount)[names(islington_siccount) == "sum(islington$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(islington_siccount)[names(islington_siccount) == "sum(islington$services > 0)"] <- "services"
names(islington_siccount)[names(islington_siccount) == "sum(islington$other > 0)"] <- "other"

kenchel <- kenchel %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Kensington and Chelsea"
area <- "E09000020"
kenchel_siccount <- as.data.frame(borough)
kenchel_siccount <- kenchel_siccount %>% 
  mutate(area) %>%
  mutate(sum(kenchel$manufacturing > 0)) %>% 
  mutate(sum(kenchel$wholesale_trans_logist > 0)) %>% 
  mutate(sum(kenchel$services > 0)) %>% 
  mutate(sum(kenchel$other > 0))
colnames(kenchel_siccount)
names(kenchel_siccount)[names(kenchel_siccount) == "sum(kenchel$manufacturing > 0)"] <- "manufacturing"
names(kenchel_siccount)[names(kenchel_siccount) == "sum(kenchel$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(kenchel_siccount)[names(kenchel_siccount) == "sum(kenchel$services > 0)"] <- "services"
names(kenchel_siccount)[names(kenchel_siccount) == "sum(kenchel$other > 0)"] <- "other"

kingontham <- kingontham %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Kingston Upon Thames"
area <- "E09000021"
kingontham_siccount <- as.data.frame(borough)
kingontham_siccount <- kingontham_siccount %>% 
  mutate(area) %>%
  mutate(sum(kingontham$manufacturing > 0)) %>% 
  mutate(sum(kingontham$wholesale_trans_logist > 0)) %>% 
  mutate(sum(kingontham$services > 0)) %>% 
  mutate(sum(kingontham$other > 0))
colnames(kingontham_siccount)
names(kingontham_siccount)[names(kingontham_siccount) == "sum(kingontham$manufacturing > 0)"] <- "manufacturing"
names(kingontham_siccount)[names(kingontham_siccount) == "sum(kingontham$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(kingontham_siccount)[names(kingontham_siccount) == "sum(kingontham$services > 0)"] <- "services"
names(kingontham_siccount)[names(kingontham_siccount) == "sum(kingontham$other > 0)"] <- "other"

lambeth <- lambeth %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Lambeth"
area <- "E09000022"
lambeth_siccount <- as.data.frame(borough)
lambeth_siccount <- lambeth_siccount %>% 
  mutate(area) %>%
  mutate(sum(lambeth$manufacturing > 0)) %>% 
  mutate(sum(lambeth$wholesale_trans_logist > 0)) %>% 
  mutate(sum(lambeth$services > 0)) %>% 
  mutate(sum(lambeth$other > 0))
colnames(lambeth_siccount)
names(lambeth_siccount)[names(lambeth_siccount) == "sum(lambeth$manufacturing > 0)"] <- "manufacturing"
names(lambeth_siccount)[names(lambeth_siccount) == "sum(lambeth$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(lambeth_siccount)[names(lambeth_siccount) == "sum(lambeth$services > 0)"] <- "services"
names(lambeth_siccount)[names(lambeth_siccount) == "sum(lambeth$other > 0)"] <- "other"

lewisham <- lewisham %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Lewisham"
area <- "E09000023"
lewisham_siccount <- as.data.frame(borough)
lewisham_siccount <- lewisham_siccount %>% 
  mutate(area) %>%
  mutate(sum(lewisham$manufacturing > 0)) %>% 
  mutate(sum(lewisham$wholesale_trans_logist > 0)) %>% 
  mutate(sum(lewisham$services > 0)) %>% 
  mutate(sum(lewisham$other > 0))
colnames(lewisham_siccount)
names(lewisham_siccount)[names(lewisham_siccount) == "sum(lewisham$manufacturing > 0)"] <- "manufacturing"
names(lewisham_siccount)[names(lewisham_siccount) == "sum(lewisham$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(lewisham_siccount)[names(lewisham_siccount) == "sum(lewisham$services > 0)"] <- "services"
names(lewisham_siccount)[names(lewisham_siccount) == "sum(lewisham$other > 0)"] <- "other"

merton <- merton %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Merton"
area <- "E09000024"
merton_siccount <- as.data.frame(borough)
merton_siccount <- merton_siccount %>% 
  mutate(area) %>%
  mutate(sum(merton$manufacturing > 0)) %>% 
  mutate(sum(merton$wholesale_trans_logist > 0)) %>% 
  mutate(sum(merton$services > 0)) %>% 
  mutate(sum(merton$other > 0))
colnames(merton_siccount)
names(merton_siccount)[names(merton_siccount) == "sum(merton$manufacturing > 0)"] <- "manufacturing"
names(merton_siccount)[names(merton_siccount) == "sum(merton$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(merton_siccount)[names(merton_siccount) == "sum(merton$services > 0)"] <- "services"
names(merton_siccount)[names(merton_siccount) == "sum(merton$other > 0)"] <- "other"

newham <- newham %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Newham"
area <- "E09000025"
newham_siccount <- as.data.frame(borough)
newham_siccount <- newham_siccount %>% 
  mutate(area) %>%
  mutate(sum(newham$manufacturing > 0)) %>% 
  mutate(sum(newham$wholesale_trans_logist > 0)) %>% 
  mutate(sum(newham$services > 0)) %>% 
  mutate(sum(newham$other > 0))
colnames(newham_siccount)
names(newham_siccount)[names(newham_siccount) == "sum(newham$manufacturing > 0)"] <- "manufacturing"
names(newham_siccount)[names(newham_siccount) == "sum(newham$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(newham_siccount)[names(newham_siccount) == "sum(newham$services > 0)"] <- "services"
names(newham_siccount)[names(newham_siccount) == "sum(newham$other > 0)"] <- "other"

redbridge <- redbridge %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Redbridge"
area <- "E09000026"
redbridge_siccount <- as.data.frame(borough)
redbridge_siccount <- redbridge_siccount %>% 
  mutate(area) %>%
  mutate(sum(redbridge$manufacturing > 0)) %>% 
  mutate(sum(redbridge$wholesale_trans_logist > 0)) %>% 
  mutate(sum(redbridge$services > 0)) %>% 
  mutate(sum(redbridge$other > 0))
colnames(redbridge_siccount)
names(redbridge_siccount)[names(redbridge_siccount) == "sum(redbridge$manufacturing > 0)"] <- "manufacturing"
names(redbridge_siccount)[names(redbridge_siccount) == "sum(redbridge$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(redbridge_siccount)[names(redbridge_siccount) == "sum(redbridge$services > 0)"] <- "services"
names(redbridge_siccount)[names(redbridge_siccount) == "sum(redbridge$other > 0)"] <- "other"

richontham <- richontham %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Richmond Upon Thames"
area <- "E09000027"
richontham_siccount <- as.data.frame(borough)
richontham_siccount <- richontham_siccount %>% 
  mutate(area) %>%
  mutate(sum(richontham$manufacturing > 0)) %>% 
  mutate(sum(richontham$wholesale_trans_logist > 0)) %>% 
  mutate(sum(richontham$services > 0)) %>% 
  mutate(sum(richontham$other > 0))
colnames(richontham_siccount)
names(richontham_siccount)[names(richontham_siccount) == "sum(richontham$manufacturing > 0)"] <- "manufacturing"
names(richontham_siccount)[names(richontham_siccount) == "sum(richontham$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(richontham_siccount)[names(richontham_siccount) == "sum(richontham$services > 0)"] <- "services"
names(richontham_siccount)[names(richontham_siccount) == "sum(richontham$other > 0)"] <- "other"

southwark <- southwark %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Southwark"
area <- "E09000028"
southwark_siccount <- as.data.frame(borough)
southwark_siccount <- southwark_siccount %>% 
  mutate(area) %>%
  mutate(sum(southwark$manufacturing > 0)) %>% 
  mutate(sum(southwark$wholesale_trans_logist > 0)) %>% 
  mutate(sum(southwark$services > 0)) %>% 
  mutate(sum(southwark$other > 0))
colnames(southwark_siccount)
names(southwark_siccount)[names(southwark_siccount) == "sum(southwark$manufacturing > 0)"] <- "manufacturing"
names(southwark_siccount)[names(southwark_siccount) == "sum(southwark$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(southwark_siccount)[names(southwark_siccount) == "sum(southwark$services > 0)"] <- "services"
names(southwark_siccount)[names(southwark_siccount) == "sum(southwark$other > 0)"] <- "other"

sutton <- sutton %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Sutton"
area <- "E09000029"
sutton_siccount <- as.data.frame(borough)
sutton_siccount <- sutton_siccount %>% 
  mutate(area) %>%
  mutate(sum(sutton$manufacturing > 0)) %>% 
  mutate(sum(sutton$wholesale_trans_logist > 0)) %>% 
  mutate(sum(sutton$services > 0)) %>% 
  mutate(sum(sutton$other > 0))
colnames(sutton_siccount)
names(sutton_siccount)[names(sutton_siccount) == "sum(sutton$manufacturing > 0)"] <- "manufacturing"
names(sutton_siccount)[names(sutton_siccount) == "sum(sutton$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(sutton_siccount)[names(sutton_siccount) == "sum(sutton$services > 0)"] <- "services"
names(sutton_siccount)[names(sutton_siccount) == "sum(sutton$other > 0)"] <- "other"

towerham <- towerham %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Tower Hamlets"
area <- "E09000030"
towerham_siccount <- as.data.frame(borough)
towerham_siccount <- towerham_siccount %>% 
  mutate(area) %>%
  mutate(sum(towerham$manufacturing > 0)) %>% 
  mutate(sum(towerham$wholesale_trans_logist > 0)) %>% 
  mutate(sum(towerham$services > 0)) %>% 
  mutate(sum(towerham$other > 0))
colnames(towerham_siccount)
names(towerham_siccount)[names(towerham_siccount) == "sum(towerham$manufacturing > 0)"] <- "manufacturing"
names(towerham_siccount)[names(towerham_siccount) == "sum(towerham$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(towerham_siccount)[names(towerham_siccount) == "sum(towerham$services > 0)"] <- "services"
names(towerham_siccount)[names(towerham_siccount) == "sum(towerham$other > 0)"] <- "other"

waltfor <- waltfor %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Waltham Forest"
area <- "E09000031"
waltfor_siccount <- as.data.frame(borough)
waltfor_siccount <- waltfor_siccount %>% 
  mutate(area) %>%
  mutate(sum(waltfor$manufacturing > 0)) %>% 
  mutate(sum(waltfor$wholesale_trans_logist > 0)) %>% 
  mutate(sum(waltfor$services > 0)) %>% 
  mutate(sum(waltfor$other > 0))
colnames(waltfor_siccount)
names(waltfor_siccount)[names(waltfor_siccount) == "sum(waltfor$manufacturing > 0)"] <- "manufacturing"
names(waltfor_siccount)[names(waltfor_siccount) == "sum(waltfor$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(waltfor_siccount)[names(waltfor_siccount) == "sum(waltfor$services > 0)"] <- "services"
names(waltfor_siccount)[names(waltfor_siccount) == "sum(waltfor$other > 0)"] <- "other"

wandsworth <- wandsworth %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Wandsworth"
area <- "E09000032"
wandsworth_siccount <- as.data.frame(borough)
wandsworth_siccount <- wandsworth_siccount %>% 
  mutate(area) %>% 
  mutate(sum(wandsworth$manufacturing > 0)) %>% 
  mutate(sum(wandsworth$wholesale_trans_logist > 0)) %>% 
  mutate(sum(wandsworth$services > 0)) %>% 
  mutate(sum(wandsworth$other > 0))
colnames(wandsworth_siccount)
names(wandsworth_siccount)[names(wandsworth_siccount) == "sum(wandsworth$manufacturing > 0)"] <- "manufacturing"
names(wandsworth_siccount)[names(wandsworth_siccount) == "sum(wandsworth$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(wandsworth_siccount)[names(wandsworth_siccount) == "sum(wandsworth$services > 0)"] <- "services"
names(wandsworth_siccount)[names(wandsworth_siccount) == "sum(wandsworth$other > 0)"] <- "other"

westminster <- westminster %>% 
  mutate(manufacturing = str_detect(sic_code, "^10|^11|^12|^13|^14|^15|^16|^17|^18|^19|^20|^21|^22|^23|^24|^25|^26|^27|^28|^29|^30|^31|^32|^33|^41|^42|^43")) %>% 
  mutate(wholesale_trans_logist = str_detect(sic_code, "^45|^46|^47|^49|^50|^51|^52|^53")) %>% 
  mutate(services = str_detect(sic_code, "^55|^56|^58|^59|^60|^61|^62|^63|^64|^65|^66|^68|^69|^70|^71|^72|^73|^74|^75|^77|^78|^79|^80|^81|^82|^85|^86|^87|^88|^90|^91|^92|^93|^94|^95|^96")) %>% 
  mutate(other = str_detect(sic_code, "^01|^02|^03|^05|^06|^07|^08|^09|^35|^36|^37|^38|^39|^84|^97|^98|^99"))
borough <- "Westminster"
area <- "E09000033"
westminster_siccount <- as.data.frame(borough)
westminster_siccount <- westminster_siccount %>% 
  mutate(area) %>% 
  mutate(sum(westminster$manufacturing > 0)) %>% 
  mutate(sum(westminster$wholesale_trans_logist > 0)) %>% 
  mutate(sum(westminster$services > 0)) %>% 
  mutate(sum(westminster$other > 0))
colnames(westminster_siccount)
names(westminster_siccount)[names(westminster_siccount) == "sum(westminster$manufacturing > 0)"] <- "manufacturing"
names(westminster_siccount)[names(westminster_siccount) == "sum(westminster$wholesale_trans_logist > 0)"] <- "wholesale_trans_logist"
names(westminster_siccount)[names(westminster_siccount) == "sum(westminster$services > 0)"] <- "services"
names(westminster_siccount)[names(westminster_siccount) == "sum(westminster$other > 0)"] <- "other"

#Join all the siccount tables into one (produces a table with the number of businesses categorised by sic code by borough)
siccount <- rbind(city_siccount,barkdag_siccount,barnet_siccount,bexley_siccount,brent_siccount,bromley_siccount,camden_siccount,croydon_siccount,ealing_siccount,enfield_siccount,greenwich_siccount,
                  hackney_siccount,hamful_siccount,haringey_siccount,harrow_siccount,havering_siccount,hillingdon_siccount,hounslow_siccount,islington_siccount,kenchel_siccount,kingontham_siccount,
                  lambeth_siccount,lewisham_siccount,merton_siccount,newham_siccount,redbridge_siccount,richontham_siccount,southwark_siccount,sutton_siccount,towerham_siccount,waltfor_siccount,
                  wandsworth_siccount,westminster_siccount)

#Plot the maps which show concentration of sb by sic code and borough
#Merge lb_borough data with siccount
tmap_mode("plot")
mergesiccount <- lb_borough%>%
  filter(str_detect(GSS_CODE, "^E09"))%>%
  merge(.,
        siccount,
        by.x=c("GSS_CODE"),
        by.y=c("area"),
        no.dups = TRUE)%>%
  distinct(.,NAME,
           .keep_all = TRUE)
#Create a bigger bounding box for the map (to fit the legend and title)
bbox_new <- st_bbox(mergesiccount)
xrange <- bbox_new$xmax - bbox_new$xmin 
yrange <- bbox_new$ymax - bbox_new$ymin
bbox_new[4] <- bbox_new[4] + (0.5 * yrange)
bbox_new <- bbox_new %>%
  st_as_sfc()
#Map mergesiccount
#Define the map elements
siccountmapm <- tm_shape(mergesiccount)+
  tm_polygons(c("manufacturing"),
              style="pretty",
              palette="Blues",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) +
  tm_layout(title = "Number of companies classifed under the manufacturing industry",
            title.size = 1,
            frame = FALSE) +
  #Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4) +
  tm_credits("(c) Companies House (2020), FAME (2021) and ONS (2020)", position=c("left", "bottom"))
#Print map
siccountmapm

siccountmapw <- tm_shape(mergesiccount)+
  tm_polygons(c("wholesale_trans_logist"),
              style="pretty",
              palette="Blues",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) +
  tm_layout(title = "Number of companies classifed under the wholesale, transport, and logistics industries",
            title.size = 1,
            frame = FALSE,
            legend.width = 0.5,
            legend.height = 0.5) +
  #Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4) +
  tm_credits("(c) Companies House (2020), FAME (2021) and ONS (2020)", position=c("left", "bottom"))
#Print map
siccountmapw

siccountmaps <- tm_shape(mergesiccount)+
  tm_polygons(c("services"),
              style="pretty",
              palette="Blues",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) +
  tm_layout(title = "Number of companies classifed under the service industry",
            title.size = 1,
            frame = FALSE) +
  #Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4) +
  tm_credits("(c) Companies House (2020), FAME (2021) and ONS (2020)", position=c("left", "bottom"))
#Print map
siccountmaps

siccountmapo <- tm_shape(mergesiccount)+
  tm_polygons(c("other"),
              style="pretty",
              palette="Blues",
              midpoint=NA,
              alpha = 1,
              border.alpha = 0) +
  tm_layout(title = "Number of companies classifed under other industries",
            title.size = 1,
            frame = FALSE,
            legend.width = 0.5,
            legend.height = 0.5) +
  #Layer borough outlines on top
  tm_shape(lb_borough)+
  tm_borders(col = "black",
             alpha = 0.4) +
  tm_credits("(c) Companies House (2020), FAME (2021) and ONS (2020)", position=c("left", "bottom"))
#Print map
siccountmapo

#Arrange the maps together 
siccountmap <- tmap_arrange(siccountmapm,siccountmapw,siccountmaps,siccountmapo,
  ncol = 2,
  nrow = 2)
siccountmap

#Print out the map
tmap_save(siccountmap, filename = "siccountmap.png",
          width = 425,
          height = 375,
          units = "mm",
          dpi = 300)




