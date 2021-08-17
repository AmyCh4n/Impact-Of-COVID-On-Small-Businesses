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
library(tibbletime)
library(lubridate)
library(patchwork)
library(ggpubr)

#---------------------------------------------------------------------------#
#Read in bpe dataset 
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

#--------------------------Manufacturing--------------------------------------#
#Select Manufacturing businesses
bpesbnum_man <- bpesbnum[c(31:50),]
#Separate out C and F 
bpesbnum_manC <- bpesbnum_man[c(4:10),]
bpesbnum_manF <- bpesbnum_man[c(14:20),]
#Pivot longer
bpesbnum_manC <- bpesbnum_manC%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
#Pivot longer
bpesbnum_manF <- bpesbnum_manF%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
#Plot grouped line chart for C
#Create variable values
number_empgrpmC <- bpesbnum_manC$employment_size
number_yearmC <- bpesbnum_manC$year
number_countmC <- bpesbnum_manC$number
numberdfmC <- data.frame(number_empgrpmC,number_yearmC,number_countmC)
numberdfmC_new <- numberdfmC
numberdfmC_new$number_empgrpmC <- factor(numberdfmC_new$number_empgrpmC, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph
numbergraphmC <- ggplot(numberdfmC_new, aes(x=number_yearmC, y=number_countmC, group=number_empgrpmC, color=number_empgrpmC)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("C - Manufacturing") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  labs(caption = "Data source: BPE 2020") +
  geom_smooth()
numbergraphmC

#Plot grouped line chart for F
#Create variable values
number_empgrpmF <- bpesbnum_manF$employment_size
number_yearmF <- bpesbnum_manF$year
number_countmF <- bpesbnum_manF$number
numberdfmF <- data.frame(number_empgrpmF,number_yearmF,number_countmF)
numberdfmF_new <- numberdfmF
numberdfmF_new$number_empgrpmF <- factor(numberdfmF_new$number_empgrpmF, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph
numbergraphmF <- ggplot(numberdfmF_new, aes(x=number_yearmF, y=number_countmF, group=number_empgrpmF, color=number_empgrpmF)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("F - Construction") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  labs(caption = "Data source: BPE 2020") +
  geom_smooth()
numbergraphmF

#Arrange MAN graphs together
MAN_num_graphs <- ggarrange(numbergraphmC + theme(legend.position = "none"), 
                            numbergraphmF,
                            ncol = 2,
                            nrow = 1,
                            common.legend = TRUE,
                            legend = "right")
MAN_num_graphs

annotate_figure(MAN_num_graphs,
                top = text_grob("Number of Small Businesses (Manufacturing Industries)", color = "black", face = "bold", size = 12),
                bottom = text_grob("Data source: BPE 2020", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))


ggsave("Number of Small Manufacturing Businesses.png",
       width =  270,
       height = 150,
       units = "mm",
       dpi = 300)

#--------------------------------WLT------------------------------------------#
#Select relevant rows
bpesbnum_who <- bpesbnum[c(51:70),]
#Separate out into separate sic groups
bpesbnum_whoG <- bpesbnum_who[c(4:10),]
bpesbnum_whoH <- bpesbnum_who[c(14:20),]

#Pivot longer
bpesbnum_whoG <- bpesbnum_whoG%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
bpesbnum_whoH <- bpesbnum_whoH%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )

#Create variable values G
number_empgroupwG <- bpesbnum_whoG$employment_size
number_yearwG <- bpesbnum_whoG$year
number_countwG <- bpesbnum_whoG$number
numberwG <- data.frame(number_empgroupwG,number_yearwG,number_countwG)
numberwG_new <- numberwG
numberwG_new$number_empgroupwG <- factor(numberwG$number_empgroupwG, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph G
numberwG <- ggplot(numberwG_new, aes(x=number_yearwG, y=number_countwG, group=number_empgroupwG, color=number_empgroupwG)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("G - Wholesale and Retail Trade") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numberwG

#Create variable values H
number_empgroupwH <- bpesbnum_whoH$employment_size
number_yearwH <- bpesbnum_whoH$year
number_countwH <- bpesbnum_whoH$number
numberwH <- data.frame(number_empgroupwH,number_yearwH,number_countwH)
numberwH_new <- numberwH
numberwH_new$number_empgroupwH <- factor(numberwH$number_empgroupwH, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph H
numberwH <- ggplot(numberwH_new, aes(x=number_yearwH, y=number_countwH, group=number_empgroupwH, color=number_empgroupwH)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("H - Transportation and Storage") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numberwH

#Arrange WLT graphs together
WLT_num_graphs <- ggarrange(numberwG + theme(legend.position = "none"), 
                            numberwH, 
                            ncol = 2,
                            nrow = 1,
                            common.legend = TRUE,
                            legend = "right")
WLT_num_graphs

annotate_figure(WLT_num_graphs,
                top = text_grob("Number of Small Businesses (Wholesale, Logistics and Transportation Industries)", color = "black", face = "bold", size = 12),
                bottom = text_grob("Data source: BPE 2020", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))

ggsave("Number WLT Businesses.png",
       width =  270,
       height = 150,
       units = "mm",
       dpi = 300)


#-----------------------------Services graph----------------------------------#
#Select relevant rows 
bpesbnum_serv <- bpesbnum[c(71:80,131:170,81:130),]
#Separate out into separate sic groups
bpesbnum_servI <- bpesbnum_serv[c(4:10),]
bpesbnum_servP <- bpesbnum_serv[c(14:20),]
bpesbnum_servQ <- bpesbnum_serv[c(24:30),]
bpesbnum_servR<- bpesbnum_serv[c(34:40),]
bpesbnum_servS <- bpesbnum_serv[c(44:50),]
bpesbnum_servJ <- bpesbnum_serv[c(54:60),]
bpesbnum_servK <- bpesbnum_serv[c(64:70),]
bpesbnum_servL <- bpesbnum_serv[c(74:80),]
bpesbnum_servM <- bpesbnum_serv[c(84:90),]
bpesbnum_servN <- bpesbnum_serv[c(94:100),]
#Pivot longer
bpesbnum_servI <- bpesbnum_servI%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
bpesbnum_servP <- bpesbnum_servP%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
bpesbnum_servQ <- bpesbnum_servQ%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
bpesbnum_servR <- bpesbnum_servR%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
bpesbnum_servS <- bpesbnum_servS%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
bpesbnum_servJ <- bpesbnum_servJ%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
bpesbnum_servK <- bpesbnum_servK%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
bpesbnum_servL <- bpesbnum_servL%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
bpesbnum_servM <- bpesbnum_servM%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )
bpesbnum_servN <- bpesbnum_servN%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "number"
  )

#Create variable values I
number_empgroupsI <- bpesbnum_servI$employment_size
number_yearsI <- bpesbnum_servI$year
number_countsI <- bpesbnum_servI$number
numbersI <- data.frame(number_empgroupsI,number_yearsI,number_countsI)
numbersI_new <- numbersI
numbersI_new$number_empgroupsI <- factor(numbersI$number_empgroupsI, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph I
numbersI <- ggplot(numbersI_new, aes(x=number_yearsI, y=number_countsI, group=number_empgroupsI, color=number_empgroupsI)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("I - Accommodation and Food Service Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numbersI

#Create variable values P
number_empgroupsP <- bpesbnum_servP$employment_size
number_yearsP <- bpesbnum_servP$year
number_countsP <- bpesbnum_servP$number
numbersP <- data.frame(number_empgroupsP,number_yearsP,number_countsP)
numbersP_new <- numbersP
numbersP_new$number_empgroupsP <- factor(numbersP$number_empgroupsP, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph P
numbersP <- ggplot(numbersP_new, aes(x=number_yearsP, y=number_countsP, group=number_empgroupsP, color=number_empgroupsP)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("P - Education") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numbersP

#Create variable values Q
number_empgroupsQ <- bpesbnum_servQ$employment_size
number_yearsQ <- bpesbnum_servQ$year
number_countsQ <- bpesbnum_servQ$number
numbersQ <- data.frame(number_empgroupsQ,number_yearsQ,number_countsQ)
numbersQ_new <- numbersQ
numbersQ_new$number_empgroupsQ <- factor(numbersQ$number_empgroupsQ, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph Q
numbersQ <- ggplot(numbersQ_new, aes(x=number_yearsQ, y=number_countsQ, group=number_empgroupsQ, color=number_empgroupsQ)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("Q - Human Health and Social Work Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numbersQ

#Create variable values R
number_empgroupsR <- bpesbnum_servR$employment_size
number_yearsR <- bpesbnum_servR$year
number_countsR <- bpesbnum_servR$number
numbersR <- data.frame(number_empgroupsR,number_yearsR,number_countsR)
numbersR_new <- numbersR
numbersR_new$number_empgroupsR <- factor(numbersR$number_empgroupsR, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph R
numbersR <- ggplot(numbersR_new, aes(x=number_yearsR, y=number_countsR, group=number_empgroupsR, color=number_empgroupsR)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("R - Arts, Entertainment and Recreation") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numbersR

#Create variable values S
number_empgroupsS <- bpesbnum_servS$employment_size
number_yearsS <- bpesbnum_servS$year
number_countsS <- bpesbnum_servS$number
numbersS <- data.frame(number_empgroupsS,number_yearsS,number_countsS)
numbersS_new <- numbersS
numbersS_new$number_empgroupsS <- factor(numbersS$number_empgroupsS, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph S
numbersS <- ggplot(numbersS_new, aes(x=number_yearsS, y=number_countsS, group=number_empgroupsS, color=number_empgroupsS)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("S - Other Service Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numbersS

#Create variable values J
number_empgroupsJ <- bpesbnum_servJ$employment_size
number_yearsJ <- bpesbnum_servJ$year
number_countsJ <- bpesbnum_servJ$number
numbersJ <- data.frame(number_empgroupsJ,number_yearsJ,number_countsJ)
numbersJ_new <- numbersJ
numbersJ_new$number_empgroupsJ <- factor(numbersJ$number_empgroupsJ, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph J
numbersJ <- ggplot(numbersJ_new, aes(x=number_yearsJ, y=number_countsJ, group=number_empgroupsJ, color=number_empgroupsJ)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("J - Information and Communication") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numbersJ

#Create variable values K
number_empgroupsK <- bpesbnum_servK$employment_size
number_yearsK <- bpesbnum_servK$year
number_countsK <- bpesbnum_servK$number
numbersK <- data.frame(number_empgroupsK,number_yearsK,number_countsK)
numbersK_new <- numbersK
numbersK_new$number_empgroupsK <- factor(numbersK$number_empgroupsK, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph K
numbersK <- ggplot(numbersK_new, aes(x=number_yearsK, y=number_countsK, group=number_empgroupsK, color=number_empgroupsK)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("K - Financial and Insurance Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numbersK

#Create variable values L
number_empgroupsL <- bpesbnum_servL$employment_size
number_yearsL <- bpesbnum_servL$year
number_countsL <- bpesbnum_servL$number
numbersL <- data.frame(number_empgroupsL,number_yearsL,number_countsL)
numbersL_new <- numbersL
numbersL_new$number_empgroupsL <- factor(numbersL$number_empgroupsL, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph L
numbersL <- ggplot(numbersL_new, aes(x=number_yearsL, y=number_countsL, group=number_empgroupsL, color=number_empgroupsL)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("L - Real Estate Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numbersL

#Create variable values M
number_empgroupsM <- bpesbnum_servM$employment_size
number_yearsM <- bpesbnum_servM$year
number_countsM <- bpesbnum_servM$number
numbersM <- data.frame(number_empgroupsM,number_yearsM,number_countsM)
numbersM_new <- numbersM
numbersM_new$number_empgroupsM <- factor(numbersM$number_empgroupsM, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph M
numbersM <- ggplot(numbersM_new, aes(x=number_yearsM, y=number_countsM, group=number_empgroupsM, color=number_empgroupsM)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("M - Professional, Scientific and Technical Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numbersM

#Create variable values N
number_empgroupsN <- bpesbnum_servN$employment_size
number_yearsN <- bpesbnum_servN$year
number_countsN <- bpesbnum_servN$number
numbersN <- data.frame(number_empgroupsN,number_yearsN,number_countsN)
numbersN_new <- numbersN
numbersN_new$number_empgroupsN <- factor(numbersN$number_empgroupsN, 
                                         levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph N
numbersN <- ggplot(numbersN_new, aes(x=number_yearsN, y=number_countsN, group=number_empgroupsN, color=number_empgroupsN)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("N - Administrative and Support Service Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Number (Count)") +
  geom_smooth()
numbersN

#Arrange Service graphs together
service_num_graphs <- ggarrange(numbersI + theme(legend.position = "none"), 
                                numbersP + theme(legend.position = "none"), 
                                numbersQ + theme(legend.position = "none"), 
                                numbersR + theme(legend.position = "none"),
                                numbersS + theme(legend.position = "none"),
                                numbersJ, 
                                ncol = 2,
                                nrow = 3,
                                common.legend = TRUE,
                                legend = "right")
service_num_graphs

annotate_figure(service_num_graphs,
                top = text_grob("Number of Small Businesses (Service Industries)", color = "black", face = "bold", size = 12),
                bottom = text_grob("Data source: BPE 2020", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))

ggsave("Number Service Businesses.png",
       width =  300,
       height = 400,
       units = "mm",
       dpi = 300)

service_num_graphs1 <- ggarrange(numbersL + theme(legend.position = "none"), 
                                 numbersM + theme(legend.position = "none"), 
                                 numbersN+ theme(legend.position = "none"),
                                 numbersK,
                                 ncol = 2,
                                 nrow = 2,
                                 common.legend = TRUE,
                                 legend = "right")
service_num_graphs1

annotate_figure(service_num_graphs1,
                top = text_grob("Number of Small Businesses (Service Industries)", color = "black", face = "bold", size = 12),
                bottom = text_grob("Data source: BPE 2020", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))

ggsave("Number Service Businesses1.png",
       width =  300,
       height = 200,
       units = "mm",
       dpi = 300)

#------------------Manufacturing turnover graph--------------------------------# 
#Select relevant rows 
bpe_turnover_man <- bpe_turnover[c(31:50),]
#Select columns
bpe_turnover_man_t <- bpe_turnover_man[,c(1:2,4,6,8,10,12)]
bpe_turnover_man_tpct <- bpe_turnover_man[,c(1,3,5,7,9,11,13)]
#Separate out C and F 
bpe_turnover_man_tpctC <- bpe_turnover_man_tpct[c(4:10),]
bpe_turnover_man_tpctF <- bpe_turnover_man_tpct[c(14:20),]
#Pivot longer
bpe_turnover_man_tpctC <- bpe_turnover_man_tpctC%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
#Pivot longer
bpe_turnover_man_tpctF <- bpe_turnover_man_tpctF%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
#Plot grouped line chart for percent turnover C
#Create variable values
turnoverpct_empgroupmC <- bpe_turnover_man_tpctC$employment_size
turnoverpct_yearmC <- bpe_turnover_man_tpctC$year
turnoverpct_countmC <- bpe_turnover_man_tpctC$pct_of_all_turnover
turnoverpctmC <- data.frame(turnoverpct_empgroupmC,turnoverpct_yearmC,turnoverpct_countmC)
turnoverpctmC_new <- turnoverpctmC
turnoverpctmC_new$turnoverpct_empgroupmC <- factor(turnoverpctmC_new$turnoverpct_empgroupmC, 
                                               levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph
turnovermC <- ggplot(turnoverpctmC_new, aes(x=turnoverpct_yearmC, y=turnoverpct_countmC, group=turnoverpct_empgroupmC, color=turnoverpct_empgroupmC)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("C - Manufacturing") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnovermC

#Plot grouped line chart for percent turnover F
#Create variable values
turnoverpct_empgroupmF <- bpe_turnover_man_tpctF$employment_size
turnoverpct_yearmF <- bpe_turnover_man_tpctF$year
turnoverpct_countmF <- bpe_turnover_man_tpctF$pct_of_all_turnover
turnoverpctmF <- data.frame(turnoverpct_empgroupmF,turnoverpct_yearmF,turnoverpct_countmF)
turnoverpctmF_new <- turnoverpctmF
turnoverpctmF_new$turnoverpct_empgroupmF <- factor(turnoverpctmF_new$turnoverpct_empgroupmF, 
                                                 levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph
turnovermF <- ggplot(turnoverpctmF_new, aes(x=turnoverpct_yearmF, y=turnoverpct_countmF, group=turnoverpct_empgroupmF, color=turnoverpct_empgroupmF)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("F - Construction") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnovermF

#Arrange MAN graphs together
MAN_graphs <- ggarrange(turnovermC + theme(legend.position = "none"), 
                        turnovermF,
                            ncol = 2,
                            nrow = 1,
                            common.legend = TRUE,
                            legend = "right")
MAN_graphs

annotate_figure(MAN_graphs,
                top = text_grob("Turnover of Small Businesses (Manufacturing Industries) as a Percentage of the Total Turnover from All Manufacturing Businesses", color = "black", face = "bold", size = 12),
                bottom = text_grob("Data source: BPE 2020", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))


ggsave("Annual Turnover Manufacturing Businesses.png",
       width =  270,
       height = 150,
       units = "mm",
       dpi = 300)

#----------------Wholesale, logistics and transport graph turnover-------------#
#Select relevant rows
bpe_turnover_who <- bpe_turnover[c(51:70),]
#Select columns
bpe_turnover_who_t <- bpe_turnover_who[,c(1:2,4,6,8,10,12)]
bpe_turnover_who_tpct <- bpe_turnover_who[,c(1,3,5,7,9,11,13)]
#Separate out into separate sic groups
bpe_turnover_whoG <- bpe_turnover_who_tpct[c(4:10),]
bpe_turnover_whoH <- bpe_turnover_who_tpct[c(14:20),]
#Pivot longer
bpe_turnover_whoG <- bpe_turnover_whoG%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
bpe_turnover_whoH <- bpe_turnover_whoH%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )

#Plot grouped line chart for percent turnover
#Create variable values G
turnoverpct_empgroupwG <- bpe_turnover_whoG$employment_size
turnoverpct_yearwG <- bpe_turnover_whoG$year
turnoverpct_countwG <- bpe_turnover_whoG$pct_of_all_turnover
turnoverpctwG <- data.frame(turnoverpct_empgroupwG,turnoverpct_yearwG,turnoverpct_countwG)
turnoverpctwG_new <- turnoverpctwG
turnoverpctwG_new$turnoverpct_empgroupwG <- factor(turnoverpctwG$turnoverpct_empgroupwG, 
                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph G
turnoverwG <- ggplot(turnoverpctwG_new, aes(x=turnoverpct_yearwG, y=turnoverpct_countwG, group=turnoverpct_empgroupwG, color=turnoverpct_empgroupwG)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("G - Wholesale and Retail Trade") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoverwG

#Create variable values H
turnoverpct_empgroupwH <- bpe_turnover_whoH$employment_size
turnoverpct_yearwH <- bpe_turnover_whoH$year
turnoverpct_countwH <- bpe_turnover_whoH$pct_of_all_turnover
turnoverpctwH <- data.frame(turnoverpct_empgroupwH,turnoverpct_yearwH,turnoverpct_countwH)
turnoverpctwH_new <- turnoverpctwH
turnoverpctwH_new$turnoverpct_empgroupwH <- factor(turnoverpctwH$turnoverpct_empgroupwH, 
                                                 levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph H
turnoverwH <- ggplot(turnoverpctwH_new, aes(x=turnoverpct_yearwH, y=turnoverpct_countwH, group=turnoverpct_empgroupwH, color=turnoverpct_empgroupwH)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("H - Transportation and Storage") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoverwH

#Arrange WLT graphs together
WLT_graphs <- ggarrange(turnoverwG + theme(legend.position = "none"), 
                        turnoverwH,
                        ncol = 2,
                        nrow = 1,
                        common.legend = TRUE,
                        legend = "right")
WLT_graphs

annotate_figure(WLT_graphs,
                top = text_grob("Turnover of Small Businesses (Wholesale, Logistics and Transportation (WLT) Industries) as a Percentage of the Total Turnover from All WLT Businesses", color = "black", face = "bold", size = 12),
                bottom = text_grob("Data source: BPE 2020", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))

ggsave("Annual Turnover WLT Businesses.png",
       width =  270,
       height = 150,
       units = "mm",
       dpi = 300)

#-----------------------------Services graph turnover-------------------------#
#Select relevant rows 
bpe_turnover_serv <- bpe_turnover[c(71:80,131:170,81:130),]
#Select columns
bpe_turnover_serv_t <- bpe_turnover_serv[,c(1:2,4,6,8,10,12)]
bpe_turnover_serv_tpct <- bpe_turnover_serv[,c(1,3,5,7,9,11,13)]
#Separate out into separate sic groups
bpe_turnover_servI <- bpe_turnover_serv_tpct[c(4:10),]
bpe_turnover_servP <- bpe_turnover_serv_tpct[c(14:20),]
bpe_turnover_servQ <- bpe_turnover_serv_tpct[c(24:30),]
bpe_turnover_servR <- bpe_turnover_serv_tpct[c(34:40),]
bpe_turnover_servS <- bpe_turnover_serv_tpct[c(44:50),]
bpe_turnover_servJ <- bpe_turnover_serv_tpct[c(54:60),]
bpe_turnover_servK <- bpe_turnover_serv_tpct[c(64:70),]
bpe_turnover_servL <- bpe_turnover_serv_tpct[c(74:80),]
bpe_turnover_servM <- bpe_turnover_serv_tpct[c(84:90),]
bpe_turnover_servN <- bpe_turnover_serv_tpct[c(94:100),]
#Pivot longer
bpe_turnover_servI <- bpe_turnover_servI%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
bpe_turnover_servP <- bpe_turnover_servP%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
bpe_turnover_servQ <- bpe_turnover_servQ%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
bpe_turnover_servR <- bpe_turnover_servR%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
bpe_turnover_servS <- bpe_turnover_servS%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
bpe_turnover_servJ <- bpe_turnover_servJ%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
bpe_turnover_servK <- bpe_turnover_servK%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
bpe_turnover_servL <- bpe_turnover_servL%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
bpe_turnover_servM <- bpe_turnover_servM%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )
bpe_turnover_servN <- bpe_turnover_servN%>% 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "pct_of_all_turnover"
  )

#Plot grouped line chart for percent turnover
#Create variable values I
turnoverpct_empgroupsI <- bpe_turnover_servI$employment_size
turnoverpct_yearsI <- bpe_turnover_servI$year
turnoverpct_countsI <- bpe_turnover_servI$pct_of_all_turnover
turnoverpctsI <- data.frame(turnoverpct_empgroupsI,turnoverpct_yearsI,turnoverpct_countsI)
turnoverpctsI_new <- turnoverpctsI
turnoverpctsI_new$turnoverpct_empgroupsI <- factor(turnoverpctsI$turnoverpct_empgroupsI, 
                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph I
turnoversI <- ggplot(turnoverpctsI_new, aes(x=turnoverpct_yearsI, y=turnoverpct_countsI, group=turnoverpct_empgroupsI, color=turnoverpct_empgroupsI)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("I - Accommodation and Food Service Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoversI

#Plot grouped line chart for percent turnover
#Create variable values P
turnoverpct_empgroupsP <- bpe_turnover_servP$employment_size
turnoverpct_yearsP <- bpe_turnover_servP$year
turnoverpct_countsP <- bpe_turnover_servP$pct_of_all_turnover
turnoverpctsP <- data.frame(turnoverpct_empgroupsP,turnoverpct_yearsP,turnoverpct_countsP)
turnoverpctsP_new <- turnoverpctsP
turnoverpctsP_new$turnoverpct_empgroupsP <- factor(turnoverpctsP$turnoverpct_empgroupsP, 
                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph P
turnoversP <- ggplot(turnoverpctsP_new, aes(x=turnoverpct_yearsP, y=turnoverpct_countsP, group=turnoverpct_empgroupsP, color=turnoverpct_empgroupsP)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("P - Education") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoversP

#Plot grouped line chart for percent turnover
#Create variable values Q
turnoverpct_empgroupsQ <- bpe_turnover_servQ$employment_size
turnoverpct_yearsQ <- bpe_turnover_servQ$year
turnoverpct_countsQ <- bpe_turnover_servQ$pct_of_all_turnover
turnoverpctsQ <- data.frame(turnoverpct_empgroupsQ,turnoverpct_yearsQ,turnoverpct_countsQ)
turnoverpctsQ_new <- turnoverpctsQ
turnoverpctsQ_new$turnoverpct_empgroupsQ <- factor(turnoverpctsQ$turnoverpct_empgroupsQ, 
                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph Q
turnoversQ <- ggplot(turnoverpctsQ_new, aes(x=turnoverpct_yearsQ, y=turnoverpct_countsQ, group=turnoverpct_empgroupsQ, color=turnoverpct_empgroupsQ)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("Q - Human Health and Social Work Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoversQ

#Create variable values R
turnoverpct_empgroupsR <- bpe_turnover_servR$employment_size
turnoverpct_yearsR <- bpe_turnover_servR$year
turnoverpct_countsR <- bpe_turnover_servR$pct_of_all_turnover
turnoverpctsR <- data.frame(turnoverpct_empgroupsR,turnoverpct_yearsR,turnoverpct_countsR)
turnoverpctsR_new <- turnoverpctsR
turnoverpctsR_new$turnoverpct_empgroupsR <- factor(turnoverpctsR$turnoverpct_empgroupsR, 
                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph R
turnoversR <- ggplot(turnoverpctsR_new, aes(x=turnoverpct_yearsR, y=turnoverpct_countsR, group=turnoverpct_empgroupsR, color=turnoverpct_empgroupsR)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("R - Arts, Entertainment and Recreation") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoversR

#Create variable values S
turnoverpct_empgroupsS <- bpe_turnover_servS$employment_size
turnoverpct_yearsS <- bpe_turnover_servS$year
turnoverpct_countsS <- bpe_turnover_servS$pct_of_all_turnover
turnoverpctsS <- data.frame(turnoverpct_empgroupsS,turnoverpct_yearsS,turnoverpct_countsS)
turnoverpctsS_new <- turnoverpctsS
turnoverpctsS_new$turnoverpct_empgroupsS <- factor(turnoverpctsS$turnoverpct_empgroupsS, 
                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph S
turnoversS <- ggplot(turnoverpctsS_new, aes(x=turnoverpct_yearsS, y=turnoverpct_countsS, group=turnoverpct_empgroupsS, color=turnoverpct_empgroupsS)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("S - Other Service Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoversS

#Create variable values J
turnoverpct_empgroupsJ <- bpe_turnover_servJ$employment_size
turnoverpct_yearsJ <- bpe_turnover_servJ$year
turnoverpct_countsJ <- bpe_turnover_servJ$pct_of_all_turnover
turnoverpctsJ <- data.frame(turnoverpct_empgroupsJ,turnoverpct_yearsJ,turnoverpct_countsJ)
turnoverpctsJ_new <- turnoverpctsJ
turnoverpctsJ_new$turnoverpct_empgroupsJ <- factor(turnoverpctsJ$turnoverpct_empgroupsJ, 
                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph J
turnoversJ <- ggplot(turnoverpctsJ_new, aes(x=turnoverpct_yearsJ, y=turnoverpct_countsJ, group=turnoverpct_empgroupsJ, color=turnoverpct_empgroupsJ)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("J - Information and Communication") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoversJ

#Create variable values K
#turnoverpct_empgroupsK <- bpe_turnover_servK$employment_size
#turnoverpct_yearsK <- bpe_turnover_servK$year
#turnoverpct_countsK <- bpe_turnover_servK$pct_of_all_turnover
#turnoverpctsK <- data.frame(turnoverpct_empgroupsK,turnoverpct_yearsK,turnoverpct_countsK)
#turnoverpctsK_new <- turnoverpctsK
#turnoverpctsK_new$turnoverpct_empgroupsK <- factor(turnoverpctsK$turnoverpct_empgroupsK, 
#                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph K
#turnoversK <- ggplot(turnoverpctsK_new, aes(x=turnoverpct_yearsK, y=turnoverpct_countsK, group=turnoverpct_empgroupsK, color=turnoverpct_empgroupsK)) +
#  geom_line() +
#  theme_ipsum(base_size = 15) +
#  scale_color_discrete("Number of Employees") +
#ggtitle("K - Financial and Insurance Activities") +
#  theme(plot.title = element_text(size = 10)) +
#  xlab("Year")+
#  ylab("Turnover (%)") +
#  geom_smooth()
#turnoversK

#Create variable values L
turnoverpct_empgroupsL <- bpe_turnover_servL$employment_size
turnoverpct_yearsL <- bpe_turnover_servL$year
turnoverpct_countsL <- bpe_turnover_servL$pct_of_all_turnover
turnoverpctsL <- data.frame(turnoverpct_empgroupsL,turnoverpct_yearsL,turnoverpct_countsL)
turnoverpctsL_new <- turnoverpctsL
turnoverpctsL_new$turnoverpct_empgroupsL <- factor(turnoverpctsL$turnoverpct_empgroupsL, 
                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph L
turnoversL <- ggplot(turnoverpctsL_new, aes(x=turnoverpct_yearsL, y=turnoverpct_countsL, group=turnoverpct_empgroupsL, color=turnoverpct_empgroupsL)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("L - Real Estate Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoversL

#Create variable values M
turnoverpct_empgroupsM <- bpe_turnover_servM$employment_size
turnoverpct_yearsM <- bpe_turnover_servM$year
turnoverpct_countsM <- bpe_turnover_servM$pct_of_all_turnover
turnoverpctsM <- data.frame(turnoverpct_empgroupsM,turnoverpct_yearsM,turnoverpct_countsM)
turnoverpctsM_new <- turnoverpctsM
turnoverpctsM_new$turnoverpct_empgroupsM <- factor(turnoverpctsM$turnoverpct_empgroupsM, 
                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph M
turnoversM <- ggplot(turnoverpctsM_new, aes(x=turnoverpct_yearsM, y=turnoverpct_countsM, group=turnoverpct_empgroupsM, color=turnoverpct_empgroupsM)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("M - Professional, Scientific and Technical Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoversM

#Create variable values N
turnoverpct_empgroupsN <- bpe_turnover_servN$employment_size
turnoverpct_yearsN <- bpe_turnover_servN$year
turnoverpct_countsN <- bpe_turnover_servN$pct_of_all_turnover
turnoverpctsN <- data.frame(turnoverpct_empgroupsN,turnoverpct_yearsN,turnoverpct_countsN)
turnoverpctsN_new <- turnoverpctsN
turnoverpctsN_new$turnoverpct_empgroupsN <- factor(turnoverpctsN$turnoverpct_empgroupsN, 
                                                   levels = c("0 (unregistered)","0 (registered)","1","2-4","5-9","10-19","20-49"))
#Grouped Line Graph N
turnoversN <- ggplot(turnoverpctsN_new, aes(x=turnoverpct_yearsN, y=turnoverpct_countsN, group=turnoverpct_empgroupsN, color=turnoverpct_empgroupsN)) +
  geom_line() +
  theme_ipsum(base_size = 15) +
  scale_color_discrete("Number of Employees") +
  ggtitle("N - Administrative and Support Service Activities") +
  theme(plot.title = element_text(size = 10)) +
  xlab("Year")+
  ylab("Turnover (%)") +
  geom_smooth()
turnoversN

#Arrange Service graphs together
service_graphs <- ggarrange(turnoversI + theme(legend.position = "none"), 
                            turnoversP + theme(legend.position = "none"), 
                            turnoversQ + theme(legend.position = "none"), 
                            turnoversR + theme(legend.position = "none"),
                            turnoversS + theme(legend.position = "none"),
                            turnoversJ,
                        ncol = 2,
                        nrow = 3,
                        common.legend = TRUE,
                        legend = "right")
service_graphs

annotate_figure(service_graphs,
                top = text_grob("Turnover of Small Businesses (Service Industries) as a Percentage of the Total Turnover from All Service Businesses", color = "black", face = "bold", size = 12),
                bottom = text_grob("Data source: BPE 2020", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))

ggsave("Annual Turnover Service Businesses.png",
       width =  300,
       height = 400,
       units = "mm",
       dpi = 300)

service_graphs1 <- ggarrange(turnoversL + theme(legend.position = "none"), 
                             turnoversM + theme(legend.position = "none"), 
                             turnoversN,
                             ncol = 2,
                             nrow = 2,
                             common.legend = TRUE,
                             legend = "right")
service_graphs1

annotate_figure(service_graphs1,
                top = text_grob("Turnover of Small Businesses (Service Industries) as a Percentage of the Total Turnover from All Service Businesses", color = "black", face = "bold", size = 12),
                bottom = text_grob("Data source: BPE 2020", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))

ggsave("Annual Turnover Service Businesses1.png",
       width =  300,
       height = 200,
       units = "mm",
       dpi = 300)



