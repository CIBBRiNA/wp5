library(tidyr)
library(dplyr)
library(stringr)

datapath <- "Q:\\20-forskning\\20-dfad\\data\\Data\\FDI_data_dissemination\\FDI 2025 DC\\"
outpath <- "Q:/20-forskning/20-dfad/users/joeg/home/VMS/250318_CIBBRiNA/Outputs/"

# Read FDI effort data
effort_EU <- read.csv(paste(datapath,"Effort\\FDI Effort EU.csv",sep=""))
effort_rect <- read.csv(paste(datapath, "Spatial EU27\\spatial_effort_tableau_pts_EU27_2012-2024.csv", sep=""))
effort_MS <- read.csv(paste(datapath,"Effort\\FDI Effort by country.csv",sep=""))

prepare_effort_data <- function(df, metier_col, subregion_col) {
  df$area <- tolower(df[[subregion_col]])
  df$gear_lvl4 <- substr(df[[metier_col]], 1, 3)
  df$metier_lvl5 <- substr(df[[metier_col]], 1, 7)
  return(df)
}

clean_C_values <- function(df, cols) {
  for (col in cols) {
    df[[col]][df[[col]] == "C"] <- 0
    df[[col]] <- as.numeric(df[[col]])
  }
  return(df)
}

effort_EU <- prepare_effort_data(effort_EU, "Metier", "Sub.region")
effort_MS <- prepare_effort_data(effort_MS, "Metier", "Sub.region")
effort_rect <- prepare_effort_data(effort_rect, "metier", "sub_region")

effort_EU$area[effort_EU$area=="34.01.02"] <- "34.1.2" 

# Define case studies
gear_cs1 <- c("GND","GNS","GTR","GTN")
gear_cs2 <- c("GND","GNS","GTR")
gear_cs3 <- c("GNS","GTR")
gear_cs4 <- c("LLD","LLS")
gear_cs5 <- c("LLD")
gear_cs6 <- c("LL","LLS","LLD")
gear_cs7 <- c("OTM","PTM")
gear_cs8 <- c("OTB","OTT")

area_cs1 <- c("27.5.a","27.4.a","27.4.b","27.4.c","27.3.a.20","27.3.a.21","27.3.b.23","27.3.c.22","27.3.d.24","27.3.d.25","27.3.d.26")
area_cs2 <- c("27.8.c","27.9.a")
area_cs3 <- c("27.4.a","27.4.b","27.4.c","27.6.a","27.6.b","27.7.d","27.7.e","27.7.f","27.7.g","27.7.h","27.7.j")
area_cs4 <- c("34.1.2","27.9.a")
area_cs5 <- c("34.1.2")
area_cs6 <- c("27.6.a","27.7.b","27.7.j","27.4.a")
area_cs7 <- c("27.4.a","27.4.b","27.4.c","27.6.a","27.6.b","27.7.a","27.7.b","27.7.c","27.7.d","27.7.e","27.7.f","27.7.g","27.7.h","27.7.j","27.7.k","27.8.a")
area_cs8 <- c("27.3.a.20","27.4.b","27.4.c","27.7.d")

# Apply case studies

effort_MS$CS <- ''
effort_EU$CS <- ''
effort_rect$CS <- ''

#CS1 def
effort_MS <- effort_MS %>%  mutate(CS = ifelse(area %in% area_cs1 & gear_lvl4 %in% gear_cs1 & Country %in% c("Denmark","Sweden","Poland","Germany"),
                                                    "CS1",CS))
effort_EU <- effort_EU %>%  mutate(CS = ifelse(area %in% area_cs1 & gear_lvl4 %in% gear_cs1,"CS1",CS))
effort_rect <- effort_rect %>%  mutate(CS = ifelse(area %in% area_cs1 & gear_lvl4 %in% gear_cs1,"CS1",CS))

#CS2 def
effort_MS <- effort_MS %>%  mutate(CS = ifelse(area %in% area_cs2 & gear_lvl4 %in% gear_cs2 & Country %in% c("Spain"),"CS2",CS))
effort_EU <- effort_EU %>%  mutate(CS = ifelse(area %in% area_cs2 & gear_lvl4 %in% gear_cs2,"CS2",CS))
effort_rect <- effort_rect %>%  mutate(CS = ifelse(area %in% area_cs2 & gear_lvl4 %in% gear_cs2,"CS2",CS))

#CS3 def
effort_MS <- effort_MS %>%  mutate(CS = ifelse(area %in% area_cs3 & gear_lvl4 %in% gear_cs3 & Country %in% c("United Kingdom"),"CS3",CS))
effort_EU <- effort_EU %>%  mutate(CS = ifelse(area %in% area_cs3 & gear_lvl4 %in% gear_cs3,"CS3",CS))
effort_rect <- effort_rect %>%  mutate(CS = ifelse(area %in% area_cs3 & gear_lvl4 %in% gear_cs3,"CS3",CS))

#CS4 def
#Mainland
effort_MS <- effort_MS %>%  mutate(CS = ifelse(area == "27.9.a" & metier_lvl5 =="LLS_DWS" & Country %in% c("Portugal"),"CS4_ML",CS))
effort_EU <- effort_EU %>%  mutate(CS = ifelse(area == "27.9.a" & metier_lvl5 =="LLS_DWS","CS4_ML",CS))
effort_rect <- effort_rect %>%  mutate(CS = ifelse(area == "27.9.a" & metier_lvl5 =="LLS_DWS","CS4_ML",CS))
#Madeira
effort_MS <- effort_MS %>%  mutate(CS = ifelse(area == "34.1.2" & metier_lvl5 =="LLD_DWS" & Country %in% c("Portugal"),"CS4_MD",CS))
effort_EU <- effort_EU %>%  mutate(CS = ifelse(area == "34.1.2" & metier_lvl5 =="LLD_DWS","CS4_MD",CS))
effort_rect <- effort_rect %>%  mutate(CS = ifelse(area == "34.1.2" & metier_lvl5 =="LLD_DWS","CS4_MD",CS))

#CS5 def
effort_MS <- effort_MS %>%  mutate(CS = ifelse(area == "34.1.2" & metier_lvl5 =="LLD_LPF" & Country %in% c("Portugal"),"CS5",CS))
effort_EU <- effort_EU %>%  mutate(CS = ifelse(area == "34.1.2" & metier_lvl5 =="LLD_LPF","CS5",CS))
effort_rect <- effort_rect %>%  mutate(CS = ifelse(area == "34.1.2" & metier_lvl5 =="LLD_LPF","CS5",CS))

#CS6 def
effort_MS <- effort_MS %>%  mutate(CS = ifelse(area %in% area_cs6 & gear_lvl4 %in% gear_cs6 & Country %in% c("United Kingdom"),"CS6",CS))
effort_EU <- effort_EU %>%  mutate(CS = ifelse(area %in% area_cs6 & gear_lvl4 %in% gear_cs6,"CS6",CS))
effort_rect <- effort_rect %>%  mutate(CS = ifelse(area %in% area_cs6 & gear_lvl4 %in% gear_cs6,"CS6",CS))

#CS7 def
effort_MS <- effort_MS %>%  mutate(CS = ifelse(area %in% area_cs7 & gear_lvl4 %in% gear_cs7 & Country %in% c("United Kingdom","Denmark","Ireland","Netherlands"),"CS7",CS))
effort_EU <- effort_EU %>%  mutate(CS = ifelse(area %in% area_cs7 & gear_lvl4 %in% gear_cs7,"CS7",CS))
effort_rect <- effort_rect %>%  mutate(CS = ifelse(area %in% area_cs7 & gear_lvl4 %in% gear_cs7,"CS7",CS))

#CS8 def
effort_MS <- effort_MS %>%  mutate(CS = ifelse(area %in% area_cs8 & gear_lvl4 %in% gear_cs8 & Country %in% c("Belgium","Netherlands"),"CS8",CS))
effort_EU <- effort_EU %>%  mutate(CS = ifelse(area %in% area_cs8 & gear_lvl4 %in% gear_cs8,"CS8",CS))
effort_rect <- effort_rect %>%  mutate(CS = ifelse(area %in% area_cs8 & gear_lvl4 %in% gear_cs8,"CS8",CS))

# Filter data
effort_MS1 <- effort_MS %>% filter(CS != "")
effort_EU1 <- effort_EU %>% filter(CS != "")
effort_rect1 <- effort_rect %>% filter(CS != "")

effort_MS1$year <- effort_MS1$Year
effort_EU1$year <- effort_EU1$Year
effort_MS1$quarter <- effort_MS1$Quarter
effort_EU1$quarter <- effort_EU1$Quarter

effort_rect1$statisticalRectangle <- effort_rect1$icesname

cols_to_clean <- c("Total.days.at.sea", "Total.Fishing.Days", "Total.kW.days.at.Sea", "Total.kW.fishing.days")
effort_MS1 <- clean_C_values(effort_MS1, cols_to_clean)

###########################################################################
#Aggregation level 1
#Aggregated level by CS, area and year

#Filtered by CS Area&Gear
FDI_EU_agg1 <- effort_EU1 %>%
  group_by(CS, year, area) %>%
  summarise(DaysAtSea=sum(Total.days.at.sea, na.rm=T), FishingDays=sum(Total.Fishing.Days, na.rm=T),
            kWDaS=sum(Total.kW.days.at.Sea, na.rm=T), kWFD=sum(Total.kW.fishing.days, na.rm=T))

write.csv(FDI_EU_agg1, paste0(outpath,"CIBBRiNA_FDI_EU_agg1.csv"), row.names=F)

#Filtered by CS Area&Gear&Country
FDI_MS_agg1 <- effort_MS1 %>%
  group_by(CS, year, area) %>%
  summarise(DaysAtSea=sum(Total.days.at.sea, na.rm=T), FishingDays=sum(Total.Fishing.Days, na.rm=T),
            kWDaS=sum(Total.kW.days.at.Sea, na.rm=T), kWFD=sum(Total.kW.fishing.days, na.rm=T))

write.csv(FDI_MS_agg1, paste0(outpath,"CIBBRiNA_FDI_MS_agg1.csv"), row.names=F)

###########################################################################

###########################################################################
#Aggregation level 2a
#Detailed level by quarter - total over all countries.
#Filtered by CS Area&Gear
FDI_EU_agg2a <- effort_EU1 %>%
  group_by(CS, year, area, quarter) %>%
  summarise(DaysAtSea=sum(Total.days.at.sea, na.rm=T), FishingDays=sum(Total.Fishing.Days, na.rm=T),
            kWDaS=sum(Total.kW.days.at.Sea, na.rm=T), kWFD=sum(Total.kW.fishing.days, na.rm=T))

write.csv(FDI_EU_agg2a, paste0(outpath,"CIBBRiNA_FDI_EU_agg2a.csv"), row.names=F)

#Filtered by CS Area&Gear&Country
FDI_MS_agg2a <- effort_MS1 %>%
  group_by(CS, year, area, quarter) %>%
  summarise(DaysAtSea=sum(Total.days.at.sea, na.rm=T), FishingDays=sum(Total.Fishing.Days, na.rm=T),
            kWDaS=sum(Total.kW.days.at.Sea, na.rm=T), kWFD=sum(Total.kW.fishing.days, na.rm=T))

write.csv(FDI_MS_agg2a, paste0(outpath,"CIBBRiNA_FDI_MS_agg2a.csv"), row.names=F)

###########################################################################

###########################################################################
#Aggregation level 2b
#Detailed level by retangle and gear - total over all countries.
#Filtered by CS Area&Gear
FDI_EU_agg2b <- effort_rect1 %>%
  group_by(CS, year, area, statisticalRectangle, gear_lvl4) %>%
  summarise(FishingDays=sum(totfishdays, na.rm=T))

write.csv(FDI_EU_agg2b, paste0(outpath,"CIBBRiNA_FDI_EU_agg2b.csv"), row.names=F)

###########################################################################