library(tidyr)
library(dplyr)
library(stringr)
library(csquares)
library(vmstools)
library(sf)

#Area shapefile
area <- st_read("Q:\\20-forskning\\12-gis\\Dynamisk\\GEOdata2020\\BasicLayers\\Boundaries\\ICES\\ICES_Areas_20160601_cut_dense_3857.shp", quiet = TRUE)

datapath <- "H:/c-users/CIBBRiNA/CIBBRINA_VMS_Request_2025/"
outpath <- "Q:/20-forskning/20-dfad/users/joeg/home/VMS/250318_CIBBRiNA/Outputs/"

load(paste(datapath, "CS1_Northern_Gillnets.Rdata", sep = ""))
load(paste(datapath, "CS2_Southern_Gillnets.Rdata", sep = ""))
load(paste(datapath, "CS3_UK_Static_Nets.Rdata", sep = ""))
load(paste(datapath, "CS6_UK_Longlines.Rdata", sep = ""))
load(paste(datapath, "CS7_Pelagic_Trawl_Fisheries.Rdata", sep = ""))
cs7.data.output <- cs7.data.output[cs7.data.output$country != 'IE',]
load(paste(datapath, "CS8_Demersal_Trawl_Fisheries.Rdata", sep = ""))
cs8.data.output <- cs8.data.output[cs8.data.output$country != 'BE',]

vms_cs1 <- cs1.data.output
vms_cs2 <- cs2.data.output
vms_cs3 <- cs3.data.output
vms_cs6 <- cs6.data.output
vms_cs7 <- cs7.data.output
vms_cs8 <- cs8.data.output

vms_cs1$CS <- 'CS1'
vms_cs2$CS <- 'CS2'
vms_cs3$CS <- 'CS3'
vms_cs6$CS <- 'CS6'
vms_cs7$CS <- 'CS7'
vms_cs8$CS <- 'CS8'

vms_cs8 <- vms_cs8 %>% filter(gearCode %in% c("OTB","OTT"))

#unique(vms_cs8$gearCode)

vms <- rbind(vms_cs1, vms_cs2, vms_cs3, vms_cs6, vms_cs7, vms_cs8)

vms$Gear_lvl4 = vms$gearCode

vms$quarter[vms$month %in% c(1,2,3)] <- 1
vms$quarter[vms$month %in% c(4,5,6)] <- 2
vms$quarter[vms$month %in% c(7,8,9)] <- 3
vms$quarter[vms$month %in% c(10,11,12)] <- 4

vms$statisticalRectangle <- ices_from_csquares(vms$cSquare)
vms$coords <- CSquare2LonLat(vms$cSquare, 0.05)
vms$lat  <- vms$coords$SI_LATI
vms$lon  <- vms$coords$SI_LONG

vms_sf <- st_as_sf(vms,coords = c("lon", "lat"),crs = 4326)  # WGS84
area_wgs <- st_transform(area, 4326)
area_wgs <- st_make_valid(area_wgs)
vms_joined <- st_join(vms_sf, area_wgs["Area_Full"], join = st_within)
vms_joined$area <- vms_joined$Area_Full

vms <- st_drop_geometry(vms_joined)

tjek <- vms %>%
  filter(anonVessels=='not_required')

###########################################################################
#Aggregation level 1
not_required_flags <- vms %>%
  group_by(CS, year, area) %>%
  summarise(not_required_flag = if (all(is.na(anonVessels))) {
      NA                      
    } else {
      any(anonVessels == "not_required", na.rm = TRUE)
    },
    .groups = "drop"
  )

VMS_agg1 <- vms %>%
  mutate(anonVessels = na_if(anonVessels, "not_required") ) %>%
  group_by(CS, year, area, anonVessels) %>%
  summarise(fishingHours= sum(fishingHours, na.rm = TRUE),kwFishinghours  = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

VMS_agg1_tot <- vms %>%
  group_by(CS, year, area) %>%
  summarise(fishingHours   = sum(fishingHours, na.rm = TRUE),kwFishinghours = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

# Separate multiple vessel IDs
VMS_agg1_ves <- VMS_agg1 %>%
  separate_rows(anonVessels,
                sep = "\\s*;\\s*|\\s*/\\s*")

# Group and count distinct vessels
VMS_agg1_ves_count <- VMS_agg1_ves %>%
  group_by(CS, year, area) %>%
  summarise(
    n_vessels = n_distinct(anonVessels, na.rm = TRUE),
    .groups = "drop"
  )

VMS_agg1_m <- VMS_agg1_tot %>%
  left_join(VMS_agg1_ves_count,     by = c("CS", "year", "area")) %>%
  left_join(not_required_flags,     by = c("CS", "year", "area"))

VMS_agg1_m <- VMS_agg1_m %>%
  mutate(
    anon = case_when(
      not_required_flag ~ "N",          # TRUE ≥3 vessels not sensitive
      n_vessels < 3      ~ "Y",          # <3 vessels  anonymise
      TRUE               ~ "N"           # All other cases not sensitive
    )
  )

VMS_agg1_coverage <- VMS_agg1_m %>%
  group_by(CS, year, anon) %>%
  summarise(fishingHours= sum(fishingHours, na.rm = TRUE),kwFishinghours  = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

VMS_agg1_pct <- VMS_agg1_coverage %>%
  group_by(CS, year) %>%
  summarise(
    fishingHours_pct = round(100 * sum(fishingHours[anon == "Y"], na.rm = TRUE) /
                            sum(fishingHours, na.rm = TRUE),2),
    kwFishinghours_pct = round(100 * sum(kwFishinghours[anon == "Y"], na.rm = TRUE) /
                              sum(kwFishinghours, na.rm = TRUE),2),
    )

VMS_agg1_masked <- VMS_agg1_m[VMS_agg1_m$anon=='N',]
VMS_agg1_masked <- VMS_agg1_masked %>% select(-c(anon,not_required_flag ))

write.csv(VMS_agg1_masked, paste0(outpath,"CIBBRiNA_VMS_agg1.csv"), row.names=F)
write.csv(VMS_agg1_pct, paste0(outpath,"CIBBRiNA_VMS_agg1_pct.csv"), row.names=F)

###########################################################################
#Aggregation level 2a

not_required_flags2a <- vms %>%
  group_by(CS, year, area, quarter) %>%
  summarise(not_required_flag = if (all(is.na(anonVessels))) {
    NA                      
  } else {
    any(anonVessels == "not_required", na.rm = TRUE)
  },
  .groups = "drop"
  )

VMS_agg2a <- vms %>%
  mutate(anonVessels = na_if(anonVessels, "not_required") ) %>%
  group_by(CS, year, area, quarter, anonVessels) %>%
  summarise(fishingHours= sum(fishingHours, na.rm = TRUE),kwFishinghours  = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

VMS_agg2a_tot <- vms %>%
  group_by(CS, year, area, quarter) %>%
  summarise(fishingHours   = sum(fishingHours, na.rm = TRUE),kwFishinghours = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

# Separate multiple vessel IDs
VMS_agg2a_ves <- VMS_agg2a %>%
  separate_rows(anonVessels,
                sep = "\\s*;\\s*|\\s*/\\s*")

# Group and count distinct vessels
VMS_agg2a_ves_count <- VMS_agg2a_ves %>%
  group_by(CS, year, area, quarter) %>%
  summarise(
    n_vessels = n_distinct(anonVessels, na.rm = TRUE),
    .groups = "drop"
  )

VMS_agg2a_m <- VMS_agg2a_tot %>%
  left_join(VMS_agg2a_ves_count,     by = c("CS","year","area","quarter")) %>%
  left_join(not_required_flags2a,     by = c("CS","year","area","quarter"))

VMS_agg2a_m <- VMS_agg2a_m %>%
  mutate(
    anon = case_when(
      not_required_flag ~ "N",          # TRUE ≥3 vessels not sensitive
      n_vessels < 3      ~ "Y",          # <3 vessels anonymise
      TRUE               ~ "N"           # All other cases not sensitive
    )
  )

VMS_agg2a_coverage <- VMS_agg2a_m %>%
  group_by(CS, year, area, anon) %>%
  summarise(fishingHours= sum(fishingHours, na.rm = TRUE),kwFishinghours  = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

VMS_agg2a_pct <- VMS_agg2a_coverage %>%
  group_by(CS, year, area) %>%
  summarise(
    fishingHours_pct = round(100 * sum(fishingHours[anon == "Y"], na.rm = TRUE) /
                               sum(fishingHours, na.rm = TRUE),2),
    kwFishinghours_pct = round(100 * sum(kwFishinghours[anon == "Y"], na.rm = TRUE) /
                                 sum(kwFishinghours, na.rm = TRUE),2),
  )

VMS_agg2a_masked <- VMS_agg2a_m[VMS_agg2a_m$anon=='N',]
VMS_agg2a_masked <- VMS_agg2a_masked %>% select(-c(anon,not_required_flag ))

write.csv(VMS_agg2a_masked, paste0(outpath,"CIBBRiNA_VMS_agg2a.csv"), row.names=F)
write.csv(VMS_agg2a_pct, paste0(outpath,"CIBBRiNA_VMS_agg2a_pct.csv"), row.names=F)
###########################################################################

###########################################################################
#Aggregation level 2b

not_required_flags2b <- vms %>%
  group_by(CS, year, area, statisticalRectangle, Gear_lvl4) %>%
  summarise(not_required_flag = if (all(is.na(anonVessels))) {
    NA                      
  } else {
    any(anonVessels == "not_required", na.rm = TRUE)
  },
  .groups = "drop"
  )

VMS_agg2b <- vms %>%
  mutate(anonVessels = na_if(anonVessels, "not_required") ) %>%
  group_by(CS, year, area, statisticalRectangle, Gear_lvl4, anonVessels) %>%
  summarise(fishingHours= sum(fishingHours, na.rm = TRUE),kwFishinghours  = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

VMS_agg2b_tot <- vms %>%
  group_by(CS, year, area, statisticalRectangle, Gear_lvl4) %>%
  summarise(fishingHours   = sum(fishingHours, na.rm = TRUE),kwFishinghours = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

# Separate multiple vessel IDs
VMS_agg2b_ves <- VMS_agg2b %>%
  separate_rows(anonVessels,
                sep = "\\s*;\\s*|\\s*/\\s*")

# Group and count distinct vessels
VMS_agg2b_ves_count <- VMS_agg2b_ves %>%
  group_by(CS, year, area, statisticalRectangle, Gear_lvl4) %>%
  summarise(
    n_vessels = n_distinct(anonVessels, na.rm = TRUE),
    .groups = "drop"
  )

VMS_agg2b_m <- VMS_agg2b_tot %>%
  left_join(VMS_agg2b_ves_count,     by = c("CS","year","area","statisticalRectangle","Gear_lvl4")) %>%
  left_join(not_required_flags2b,     by = c("CS","year","area","statisticalRectangle","Gear_lvl4"))

VMS_agg2b_m <- VMS_agg2b_m %>%
  mutate(
    anon = case_when(
      not_required_flag ~ "N",          # TRUE ≥3 vessels not sensitive
      n_vessels < 3      ~ "Y",          # <3 vessels anonymise
      TRUE               ~ "N"           # All other cases not sensitive
    )
  )

VMS_agg2b_coverage <- VMS_agg2b_m %>%
  group_by(CS, year, area, anon) %>%
  summarise(fishingHours= sum(fishingHours, na.rm = TRUE),kwFishinghours  = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

VMS_agg2b_pct <- VMS_agg2b_coverage %>%
  group_by(CS, year, area) %>%
  summarise(
    fishingHours_pct = round(100 * sum(fishingHours[anon == "Y"], na.rm = TRUE) /
                               sum(fishingHours, na.rm = TRUE),2),
    kwFishinghours_pct = round(100 * sum(kwFishinghours[anon == "Y"], na.rm = TRUE) /
                                 sum(kwFishinghours, na.rm = TRUE),2),
  )

VMS_agg2b_masked <- VMS_agg2b_m[VMS_agg2b_m$anon=='N',]
VMS_agg2b_masked <- VMS_agg2b_masked %>% select(-c(anon,not_required_flag ))

write.csv(VMS_agg2b_masked, paste0(outpath,"CIBBRiNA_VMS_agg2b.csv"), row.names=F)
write.csv(VMS_agg2b_pct, paste0(outpath,"CIBBRiNA_VMS_agg2b_pct.csv"), row.names=F)
###########################################################################

###########################################################################
#Aggregation level 3 (csquare 0.05 degrees)

not_required_flags3 <- vms %>%
  group_by(CS, year, area, statisticalRectangle, cSquare, Gear_lvl4) %>%
  summarise(not_required_flag = if (all(is.na(anonVessels))) {
    NA                      
  } else {
    any(anonVessels == "not_required", na.rm = TRUE)
  },
  .groups = "drop"
  )

VMS_agg3 <- vms %>%
  mutate(anonVessels = na_if(anonVessels, "not_required") ) %>%
  group_by(CS, year, area, statisticalRectangle,cSquare, Gear_lvl4, anonVessels) %>%
  summarise(fishingHours= sum(fishingHours, na.rm = TRUE),kwFishinghours  = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

VMS_agg3_tot <- vms %>%
  group_by(CS, year, area, statisticalRectangle,cSquare, Gear_lvl4) %>%
  summarise(fishingHours   = sum(fishingHours, na.rm = TRUE),kwFishinghours = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

# Separate multiple vessel IDs
VMS_agg3_ves <- VMS_agg3 %>%
  separate_rows(anonVessels,
                sep = "\\s*;\\s*|\\s*/\\s*")

# Group and count distinct vessels
VMS_agg3_ves_count <- VMS_agg3_ves %>%
  group_by(CS, year, area, statisticalRectangle, cSquare,Gear_lvl4) %>%
  summarise(
    n_vessels = n_distinct(anonVessels, na.rm = TRUE),
    .groups = "drop"
  )

VMS_agg3_m <- VMS_agg3_tot %>%
  left_join(VMS_agg3_ves_count,     by = c("CS","year","area","statisticalRectangle","cSquare","Gear_lvl4")) %>%
  left_join(not_required_flags3,     by = c("CS","year","area","statisticalRectangle","cSquare","Gear_lvl4"))

VMS_agg3_m <- VMS_agg3_m %>%
  mutate(
    anon = case_when(
      not_required_flag ~ "N",          # TRUE ≥3 vessels not sensitive
      n_vessels < 3      ~ "Y",          # <3 vessels anonymise
      TRUE               ~ "N"           # All other cases not sensitive
    )
  )

VMS_agg3_coverage <- VMS_agg3_m %>%
  group_by(CS, year, area,  anon) %>%
  summarise(fishingHours= sum(fishingHours, na.rm = TRUE),kwFishinghours  = sum(kwFishinghours, na.rm = TRUE),.groups = "drop")

VMS_agg3_pct <- VMS_agg3_coverage %>%
  group_by(CS, year, area) %>%
  summarise(
    fishingHours_pct = round(100 * sum(fishingHours[anon == "Y"], na.rm = TRUE) /
                               sum(fishingHours, na.rm = TRUE),2),
    kwFishinghours_pct = round(100 * sum(kwFishinghours[anon == "Y"], na.rm = TRUE) /
                                 sum(kwFishinghours, na.rm = TRUE),2),
  )

VMS_agg3_masked <- VMS_agg3_m[VMS_agg3_m$anon=='N',]
VMS_agg3_masked <- VMS_agg3_masked %>% select(-c(anon,not_required_flag ))

write.csv(VMS_agg3_masked, paste0(outpath,"CIBBRiNA_VMS_agg3.csv"), row.names=F)
write.csv(VMS_agg3_pct, paste0(outpath,"CIBBRiNA_VMS_agg3_pct.csv"), row.names=F)
###########################################################################
