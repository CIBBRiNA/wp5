library(tidyr)
library(dplyr)
library(stringr)

vessel_limit <- 3

#Insert data path
datapath <- " "

#Insert out path
outpath <- " "
gear_gillnet <- c("GND","GNS","GTR","GTN")
gear_longline <- c("LHM","LHP","LLD","LLS", "LNB","LTL")
gear_pelagic <- c("OTM","PTM")
gear_demersal <- c("OTB","PTB","TBB")

load_effort_data <- function(zip_path, file_name = "CommercialEffort.csv", gear_filter = NULL) {
  # Load and subset relevant columns
  effort_data <- read.csv(unz(zip_path, file_name)) %>%
      mutate(
      Metier_lvl5 = substr(CEmetier6, 1, 7),
      Gear_lvl4 = substr(CEmetier6, 1, 3)
    )
}

effort_cs1 <- load_effort_data(file.path(datapath, "RDBES CE CS1 DK DE IS NO PL SE 3.20-26 4 5a2.zip"), gear_filter = gear_gillnet)
effort_cs2 <- load_effort_data(file.path(datapath, "RDBES CE CS2 ES 8c 9a.zip"), gear_filter = gear_gillnet)
effort_cs3 <- load_effort_data(file.path(datapath, "RDBES CE CS3 UK 4 6 7d-j.zip"), gear_filter = gear_gillnet)
effort_cs6 <- load_effort_data(file.path(datapath, "RDBES CE CS6 UK 4a 6a 7bj.zip"), gear_filter = gear_longline)
effort_cs7 <- load_effort_data(file.path(datapath, "RDBES CE CS7 UK DK NL 4 6 7 8a.zip"), gear_filter = gear_pelagic)
effort_cs8 <- load_effort_data(file.path(datapath, "RDBES CE CS8 NL 3a 4bc 7d.zip"), gear_filter = gear_demersal)

effort_cs1$CS <- 'CS1'
effort_cs2$CS <- 'CS2'
effort_cs3$CS <- 'CS3'
effort_cs6$CS <- 'CS6'
effort_cs7$CS <- 'CS7'
effort_cs8$CS <- 'CS8'

effort_cs1$level4 <- substr(effort_cs1$CEmetier6,1,3)
unique(effort_cs1$level4)
effort_cs1 <- effort_cs1 %>% filter(level4 %in% c("GNS","GND","GTR"))

effort_cs2$level4 <- substr(effort_cs2$CEmetier6,1,3)
unique(effort_cs2$level4)
effort_cs2 <- effort_cs2 %>% filter(level4 %in% c("GNS","GND","GTR"))

effort_cs3$level4 <- substr(effort_cs3$CEmetier6,1,3)
unique(effort_cs3$level4)
effort_cs3 <- effort_cs3 %>% filter(level4 %in% c("GNS","GTR"))

effort_cs3$CEvesselFlagCountry <- 'GB'

effort_cs6$level4 <- substr(effort_cs6$CEmetier6,1,3)
unique(effort_cs6$level4)
effort_cs6 <- effort_cs6 %>% filter(level4 %in% c("LLS","LLD"))

effort_cs6$CEvesselFlagCountry <- 'GB'

effort_cs7$level4 <- substr(effort_cs7$CEmetier6,1,3)
unique(effort_cs7$level4)
effort_cs7 <- effort_cs7 %>% filter(level4 %in% c("OTM","PTM"))
unique(effort_cs7$CEvesselFlagCountry)
effort_cs7$CEvesselFlagCountry[effort_cs7$CEvesselFlagCountry %in% c("GB-SCT","GB-ENG","GB-WLS","GB-NIR")] <- 'GB'

effort_cs8$level4 <- substr(effort_cs8$CEmetier6,1,3)
unique(effort_cs8$level4)
effort_cs8 <- effort_cs8 %>% filter(level4 %in% c("OTB","OTT"))

RDBES_cs <- rbind(effort_cs1, effort_cs2, effort_cs3, effort_cs6, effort_cs7, effort_cs8)
RDBES_cs$area <- RDBES_cs$CEarea
RDBES_cs$year <- RDBES_cs$CEyear
RDBES_cs$statisticalRectangle <- RDBES_cs$CEstatisticalRectangle
RDBES_cs$quarter <- RDBES_cs$CEquarter

###########################################################################
#Aggregation level 1
#Aggregated level by CS, area and year
RDBES_agg1 <- RDBES_cs %>%
  group_by(CS, year, area,  CEencryptedVesselIds ) %>%
  summarise(DaysAtSea=sum(CEscientificDaysAtSea, na.rm=T), FishingDays=sum(CEscientificFishingDays, na.rm=T),
            kWDaS=sum(CEscientifickWDaysAtSea, na.rm=T), kWFD=sum(CEscientifickWFishingDays, na.rm=T))

RDBES_agg1_tot <- RDBES_cs %>%
  group_by(CS, year, area) %>%
  summarise(DaysAtSea=sum(CEscientificDaysAtSea, na.rm=T), FishingDays=sum(CEscientificFishingDays, na.rm=T),
            kWDaS=sum(CEscientifickWDaysAtSea, na.rm=T), kWFD=sum(CEscientifickWFishingDays, na.rm=T))

# Separate multiple vessel IDs
RDBES_agg1_ves <- RDBES_agg1 %>%
  separate_rows(CEencryptedVesselIds, sep = "\\s*;\\s*|\\s*/\\s*")

# Group and count distinct vessels
RDBES_agg1_ves_count <- RDBES_agg1_ves %>%
  group_by(CS, area, year) %>%
  summarise(n_vessels = n_distinct(CEencryptedVesselIds), .groups = "drop")

RDBES_agg1_m <- merge(RDBES_agg1_tot, RDBES_agg1_ves_count, by=c("CS", "area","year"))
RDBES_agg1_m$anon <- ifelse(RDBES_agg1_m$n_vessels<vessel_limit, 'Y','N')

RDBES_agg1_coverage <- RDBES_agg1_m %>%
  group_by(CS, year, anon) %>%
  summarise(DaysAtSea=sum(DaysAtSea, na.rm=T), FishingDays=sum(FishingDays, na.rm=T),kWDaS=sum(kWDaS, na.rm=T), kWFD=sum(kWFD, na.rm=T))

RDBES_agg1_pct <- RDBES_agg1_coverage %>%
  group_by(CS, year) %>%
  summarise(
    DaysAtSea_pct = round(100 * sum(DaysAtSea[anon == "Y"], na.rm = TRUE) /
                            sum(DaysAtSea, na.rm = TRUE),2),
    FishingDays_pct = round(100 * sum(FishingDays[anon == "Y"], na.rm = TRUE) /
                              sum(FishingDays, na.rm = TRUE),2),
    kWDaS_pct = round(100 * sum(kWDaS[anon == "Y"], na.rm = TRUE) /
                        sum(kWDaS, na.rm = TRUE),2),
    kWFD_pct = round(100 * sum(kWFD[anon == "Y"], na.rm = TRUE) /
                       sum(kWFD, na.rm = TRUE),2)
  )

RDBES_agg1_masked <- RDBES_agg1_m[RDBES_agg1_m$anon=='N',]
RDBES_agg1_masked <- RDBES_agg1_masked %>% select(-anon)

write.csv(RDBES_agg1_masked, paste0(outpath,"CIBBRiNA_RDBES_agg1.csv"), row.names=F)
write.csv(RDBES_agg1_pct, paste0(outpath,"CIBBRiNA_RDBES_agg1_pct.csv"), row.names=F)

###########################################################################

###########################################################################
#Aggregation level 2a
#Detailed level by quarter - total over all countries.
RDBES_agg2a <- RDBES_cs %>%
  group_by(CS, year, area, quarter, CEencryptedVesselIds ) %>%
  summarise(DaysAtSea=sum(CEscientificDaysAtSea, na.rm=T), FishingDays=sum(CEscientificFishingDays, na.rm=T),
            kWDaS=sum(CEscientifickWDaysAtSea, na.rm=T), kWFD=sum(CEscientifickWFishingDays, na.rm=T))

RDBES_agg2a_tot <- RDBES_cs %>%
  group_by(CS, year , area, quarter) %>%
  summarise(DaysAtSea=sum(CEscientificDaysAtSea, na.rm=T), FishingDays=sum(CEscientificFishingDays, na.rm=T),
            kWDaS=sum(CEscientifickWDaysAtSea, na.rm=T), kWFD=sum(CEscientifickWFishingDays, na.rm=T))

# Separate multiple vessel IDs
RDBES_agg2a_ves <- RDBES_agg2a %>%
  separate_rows(CEencryptedVesselIds, sep = "\\s*;\\s*|\\s*/\\s*")

# Group and count distinct vessels
RDBES_agg2a_ves_count <- RDBES_agg2a_ves %>%
  group_by(CS, year, area, quarter) %>%
  summarise(n_vessels = n_distinct(CEencryptedVesselIds), .groups = "drop")

RDBES_agg2a_m <- merge(RDBES_agg2a_tot, RDBES_agg2a_ves_count, by=c("CS", "year", "area", "quarter"))
RDBES_agg2a_m$anon <- ifelse(RDBES_agg2a_m$n_vessels<vessel_limit, 'Y','N')

RDBES_agg2a_coverage <- RDBES_agg2a_m %>%
  group_by(CS, year, area, anon) %>%
  summarise(DaysAtSea=sum(DaysAtSea, na.rm=T), FishingDays=sum(FishingDays, na.rm=T),kWDaS=sum(kWDaS, na.rm=T), kWFD=sum(kWFD, na.rm=T))

RDBES_agg2a_pct <- RDBES_agg2a_coverage %>%
  group_by(CS, year, area) %>%
  summarise(
    DaysAtSea_pct = round(100 * sum(DaysAtSea[anon == "Y"], na.rm = TRUE) /
      sum(DaysAtSea, na.rm = TRUE),2),
    FishingDays_pct = round(100 * sum(FishingDays[anon == "Y"], na.rm = TRUE) /
      sum(FishingDays, na.rm = TRUE),2),
    kWDaS_pct = round(100 * sum(kWDaS[anon == "Y"], na.rm = TRUE) /
      sum(kWDaS, na.rm = TRUE),2),
    kWFD_pct = round(100 * sum(kWFD[anon == "Y"], na.rm = TRUE) /
      sum(kWFD, na.rm = TRUE),2)
  )

RDBES_agg2a_masked <- RDBES_agg2a_m[RDBES_agg2a_m$anon=='N',]
RDBES_agg2a_masked <- RDBES_agg2a_masked %>% select(-anon)

write.csv(RDBES_agg2a_masked, paste0(outpath,"CIBBRiNA_RDBES_agg2a.csv"), row.names=F)
write.csv(RDBES_agg2a_pct, paste0(outpath,"CIBBRiNA_RDBES_agg2a_pct.csv"), row.names=F)

###########################################################################

#Aggregation level 2b
#Detailed level by statistical rectangle and gear - total over all countries.
RDBES_agg2b <- RDBES_cs %>%
  group_by(CS, year, area, statisticalRectangle, Gear_lvl4, CEencryptedVesselIds ) %>%
  summarise(DaysAtSea=sum(CEscientificDaysAtSea, na.rm=T), FishingDays=sum(CEscientificFishingDays, na.rm=T),
            kWDaS=sum(CEscientifickWDaysAtSea, na.rm=T), kWFD=sum(CEscientifickWFishingDays, na.rm=T))

RDBES_agg2b_tot <- RDBES_cs %>%
  group_by(CS, year , area, statisticalRectangle, Gear_lvl4) %>%
  summarise(DaysAtSea=sum(CEscientificDaysAtSea, na.rm=T), FishingDays=sum(CEscientificFishingDays, na.rm=T),
            kWDaS=sum(CEscientifickWDaysAtSea, na.rm=T), kWFD=sum(CEscientifickWFishingDays, na.rm=T))

# Separate multiple vessel IDs
RDBES_agg2b_ves <- RDBES_agg2b %>%
  separate_rows(CEencryptedVesselIds, sep = "\\s*;\\s*|\\s*/\\s*")

# Group and count distinct vessels
RDBES_agg2b_ves_count <- RDBES_agg2b_ves %>%
  group_by(CS, year, area, statisticalRectangle, Gear_lvl4) %>%
  summarise(n_vessels = n_distinct(CEencryptedVesselIds), .groups = "drop")

RDBES_agg2b_m <- merge(RDBES_agg2b_tot, RDBES_agg2b_ves_count, by=c("CS", "year", "area", "statisticalRectangle", "Gear_lvl4"))
RDBES_agg2b_m$anon <- ifelse(RDBES_agg2b_m$n_vessels<vessel_limit, 'Y','N')

RDBES_agg2b_coverage <- RDBES_agg2b_m %>%
  group_by(CS, year, area, anon) %>%
  summarise(DaysAtSea=sum(DaysAtSea, na.rm=T), FishingDays=sum(FishingDays, na.rm=T),kWDaS=sum(kWDaS, na.rm=T), kWFD=sum(kWFD, na.rm=T))

RDBES_agg2b_pct <- RDBES_agg2b_coverage %>%
  group_by(CS, year, area) %>%
  summarise(
    DaysAtSea_pct = round(100 * sum(DaysAtSea[anon == "Y"], na.rm = TRUE) /
                            sum(DaysAtSea, na.rm = TRUE),2),
    FishingDays_pct = round(100 * sum(FishingDays[anon == "Y"], na.rm = TRUE) /
                              sum(FishingDays, na.rm = TRUE),2),
    kWDaS_pct = round(100 * sum(kWDaS[anon == "Y"], na.rm = TRUE) /
                        sum(kWDaS, na.rm = TRUE),2),
    kWFD_pct = round(100 * sum(kWFD[anon == "Y"], na.rm = TRUE) /
                       sum(kWFD, na.rm = TRUE),2)
  )

RDBES_agg2b_masked <- RDBES_agg2b_m[RDBES_agg2b_m$anon=='N',]
RDBES_agg2b_masked <- RDBES_agg2b_masked %>% select(-anon)

write.csv(RDBES_agg2b_masked, paste0(outpath,"CIBBRiNA_RDBES_agg2b.csv"), row.names=F)
write.csv(RDBES_agg2b_pct, paste0(outpath,"CIBBRiNA_RDBES_agg2b_pct.csv"), row.names=F)

###########################################################################