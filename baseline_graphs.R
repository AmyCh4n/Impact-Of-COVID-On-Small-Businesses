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
library(ggrepel)
library(lemon)
library(patchwork)
library(ggpubr)

#-----------------------------------------------------------------------------#
#Read in no. of small businesses data - bpe
bpe <- read_csv("bpe.csv",
                locale = locale(encoding = "latin1"),
                na = "n/a", 
                col_types = 
                cols(
                  X1 = col_character(),
                  Businesses15 = col_double(),
                  'Employment15 (000)' = col_double(),
                  'Turnover15 (000,000)' = col_double(),
                  'Businesse%15' = col_double(),
                  'Employment%15' = col_double(),
                  'Turnover%15' = col_double(),
                  Businesses16 = col_double(),
                  'Employment16 (000)' = col_double(),
                  'Turnover16 (000,000)' = col_double(),
                  'Businesse%16' = col_double(),
                  'Employment%16' = col_double(),
                  'Turnover%16' = col_double(),
                  Businesses17 = col_double(),
                  'Employment17 (000)' = col_double(),
                  'Turnover17 (000,000)' = col_double(),
                  'Businesse%17' = col_double(),
                  'Employment%17' = col_double(),
                  'Turnover%17' = col_double(),
                  Businesses18 = col_double(),
                  'Employment18 (000)' = col_double(),
                  'Turnover18 (000,000)' = col_double(),
                  'Businesse%18' = col_double(),
                  'Employment%18' = col_double(),
                  'Turnover%18' = col_double(),
                  Businesses19 = col_double(),
                  'Employment19 (000)' = col_double(),
                  'Turnover19 (000,000)' = col_double(),
                  'Businesse%19' = col_double(),
                  'Employment%19' = col_double(),
                  'Turnover%19' = col_double(),
                  Businesses20 = col_double(),
                  'Employment20 (000)' = col_double(),
                  'Turnover20 (000,000)' = col_double(),
                  'Businesse%20' = col_double(),
                  'Employment%20' = col_double(),
                  'Turnover%20' = col_double()
                ))
class(bpe)
summary(bpe)

#Select columns
bpesbnum <- bpe[,c(1:2,8,14,20,26,32)]
#Select rows
bpesbnum <- bpesbnum[c(1:10,16:25,31:40,46:55,61:70,76:85,91:100,106:115,121:130,136:145,151:160,166:175,181:190,196:205,211:220,226:235,241:250),]
#Rename Columns
bpesbnum <- clean_names(bpesbnum)
colnames(bpesbnum)
names(bpesbnum)[names(bpesbnum) == "x1"] <- "employment_size"
names(bpesbnum)[names(bpesbnum) == "businesses15"] <- "2015"
names(bpesbnum)[names(bpesbnum) == "businesses16"] <- "2016"
names(bpesbnum)[names(bpesbnum) == "businesses17"] <- "2017"
names(bpesbnum)[names(bpesbnum) == "businesses18"] <- "2018"
names(bpesbnum)[names(bpesbnum) == "businesses19"] <- "2019"
names(bpesbnum)[names(bpesbnum) == "businesses20"] <- "2020"
bpesbnum

#Select columns
bpe_turnover <- bpe[,c(1,4,7,10,13,16,19,22,25,28,31,34,37)]
#Select rows
bpe_turnover <- bpe_turnover[c(1:10,16:25,31:40,46:55,61:70,76:85,91:100,106:115,121:130,136:145,151:160,166:175,181:190,196:205,211:220,226:235,241:250),]
#Clean and rename Columns
bpe_turnover <- clean_names(bpe_turnover)
colnames(bpe_turnover)
names(bpe_turnover)[names(bpe_turnover) == "x1"] <- "employment_size"
names(bpe_turnover)[names(bpe_turnover) == "turnover15_000_000"] <- "2015"
names(bpe_turnover)[names(bpe_turnover) == "turnover16_000_000"] <- "2016"
names(bpe_turnover)[names(bpe_turnover) == "turnover17_000_000"] <- "2017"
names(bpe_turnover)[names(bpe_turnover) == "turnover18_000_000"] <- "2018"
names(bpe_turnover)[names(bpe_turnover) == "turnover19_000_000"] <- "2019"
names(bpe_turnover)[names(bpe_turnover) == "turnover20_000_000"] <- "2020"
names(bpe_turnover)[names(bpe_turnover) == "turnover_percent_15"] <- "2015"
names(bpe_turnover)[names(bpe_turnover) == "turnover_percent_16"] <- "2016"
names(bpe_turnover)[names(bpe_turnover) == "turnover_percent_17"] <- "2017"
names(bpe_turnover)[names(bpe_turnover) == "turnover_percent_18"] <- "2018"
names(bpe_turnover)[names(bpe_turnover) == "turnover_percent_19"] <- "2019"
names(bpe_turnover)[names(bpe_turnover) == "turnover_percent_20"] <- "2020"
bpe_turnover

#--------------------------All Businesses--------------------------------------#
#Select All businesses
bpesbnum_all <- bpesbnum[c(4:10),]
#Pivot longer
bpesbnum_all <- bpesbnum_all%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
#Plot grouped line chart for number of businesses
#Create variable values
number_empgrp <- bpesbnum_all$employment_size
number_year <- bpesbnum_all$year
number_count <- bpesbnum_all$number
numberdf <- data.frame(number_empgrp,number_year,number_count)
numberdf_new <- numberdf
numberdf_new$number_empgrp <- factor(numberdf_new$number_empgrp, 
                                                 levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph
numbergraph <- ggplot(numberdf_new, aes(x=number_year, y=number_count, group=number_empgrp, color=number_empgrp)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("Number of Small Businesses from 2015 to 2020") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  labs(caption = "Data source: BPE 2020") +
  geom_smooth()
numbergraph

ggsave("Number of Small Businesses.png",
       width =  225,
       height = 150,
       units = "mm",
       dpi = 300)

#---------------------------------------------------------------------------#
#Plot annual turnover for all small businesses 
#Select rows
bpe_turnover_all <- bpe_turnover[c(4:10),]
#Select columns into t and tpct
bpe_turnover_all_tpct <- bpe_turnover_all[,c(1,3,5,7,9,11,13)]

#Plot scatter graph showing tpct of sb over the years 
#Pivot longer 
bpe_turnover_all_tpct <- bpe_turnover_all_tpct%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )

#Plot grouped line/bar chart for percent turnover
#Create variable values
turnoverpct_empgroup <- bpe_turnover_all_tpct$employment_size
turnoverpct_year <- bpe_turnover_all_tpct$year
turnoverpct_count <- bpe_turnover_all_tpct$pct_of_all_turnover
turnoverpct <- data.frame(turnoverpct_empgroup,turnoverpct_year,turnoverpct_count)
turnoverpct_new <- turnoverpct
turnoverpct_new$turnoverpct_empgroup <- factor(turnoverpct_new$turnoverpct_empgroup, 
                                               levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph for percent turnover
turnover <- ggplot(turnoverpct_new, aes(x=turnoverpct_year, y=turnoverpct_count, group=turnoverpct_empgroup, color=turnoverpct_empgroup)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("Turnover of Small Businesses as a Percentage of the Total Turnover from All Businesses") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  labs(caption = "Data source: BPE 2020") +
  geom_smooth()
turnover

ggsave("Annual Turnover All Businesses.png",
       width =  200,
       height = 150,
       units = "mm",
       dpi = 300)







