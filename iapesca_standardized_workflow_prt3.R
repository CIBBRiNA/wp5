#devtools::install_git("https://gitlab.ifremer.fr/iapesca/r-packages_iapesca")
library(iapesca)
library(data.table)
library(mapview)
library(ggplot2)
library(rpart)
library(vmstools)
library(stars)
library(RANN)
ls(getNamespace("iapesca"), all.names=TRUE) |> head()

sf::sf_use_s2(F)

#necessary files, can be downloaded from "https://gis.ices.dk/sf/"
ices <- st_read("data/ices/ICES_Areas_20160601_cut_dense_3857.shp")%>%
  st_transform(4326)
ices2 <- st_drop_geometry(ices)

rect <- st_read("data/ices/StatRec_map_Areas_Full_20170124.shp")|>
  st_transform(4326) |>
  st_drop_geometry()
names(rect)[3] <- "statisticalRectangle"

## data
lst <- list.files("results/ship_effort/", full.names = T)

options(warn=-1)
suppressMessages(
  for (x in 1){
    fun <- function(i) {
      
      Setting.rfMod <- readRDS(lst[i])
      
      res <- Setting.rfMod$traj
      nets <- Setting.rfMod$nets
      
      res$Setting_GearId[is.na(res$Setting_GearId)] <- ""
      res$Hauling_GearId[is.na(res$Hauling_GearId)] <- ""
      
      haul <- setDT(res)
      soak <- setDT(st_drop_geometry(nets[, c("Net", "length", "hauling.start", 
                                              "hauling.end", "soaking.time.hours")]))
      
      haul$hauling.start <- haul$DATE_TIME
      haul$hauling.end <- haul$DATE_TIME
      soak$hauling.start <- as.POSIXct(soak$hauling.start, tz = "UTC")
      soak$hauling.end <- as.POSIXct(soak$hauling.end, tz = "UTC")
      
      setkey(haul, hauling.start, hauling.end)
      setkey(soak, hauling.start, hauling.end)
      
      haul <- foverlaps(haul, soak)
      
      
      #by point
      nets <- data.table(haul[! is.na(haul$Net), ])
      nets[ , ptsNetlength:= length/.N, by = .(Net)] 
      
      
      #for soaktime it should be the mean pr trip, to account for nets being slit up falsly
      nets[ , soaking.time.hours:= mean(soaking.time.hours), by = .(FISHING_TRIP_FK)] 
      nets[ , ptsSoaktime:= soaking.time.hours/.N, by = .(Net)] 
      
      nets$haulNr <- nets$Net
      nets$netLength <- as.numeric(nets$length)
      nets$netSoaktime <- nets$soaking.time.hours
      
      ##
      nets$netSoaktime[nets$netSoaktime <= 0] <- NA
      nets <- nets[nets$netLength > 0, ]
      
      #############################################################################################      
      dat <- st_as_sf(nets) 
      dat$LONG <- st_coordinates(dat)[, "X"]
      dat$LATI <- st_coordinates(dat)[, "Y"]
      
      #add other info # square and area
      dat$year <-  year(dat$DATE_TIME)
      dat$quarter <-  quarter(dat$DATE_TIME)
      
      dat$statisticalRectangle <- 
        ICESrectangle(data.frame(SI_LONG = dat$LONG,
                                 SI_LATI = dat$LATI))
      
      
      overlap <- st_intersects(dat, ices)
      dat$idd <- 1:nrow(dat)
      
      #insert fake area to points too close to land..
      xx <- dat[rep(seq(nrow(dat)), lengths(overlap)), ]
      xx$Area_27 <- ices2[unlist(overlap), ]$Area_27
      
      yy <- dat[! dat$idd %in% xx$idd, ]
      
      if (nrow(yy) > 0){
        #if too close to land, take area square relation
        yy <- merge(yy, rect[, c("statisticalRectangle", "Area_27")])
        dat <- rbind(xx, yy)
      } else {
        dat <- xx
      }
      
      dat <- dat[order(dat$idd), ]
      
      ### c square
      Csq.grd <- CSquare_Grids(dat, Csq = 0.05, extent = NULL, 
                               class.output = "ras")
      
      Csq.grd <- Csq.grd[["Csq.rast"]]
      
      Csq.grd <- st_as_stars(Csq.grd) %>%
        st_as_sf(as_points = FALSE, merge = F)
      
      #make id
      Csq.grd$idx <- round(st_coordinates(st_centroid(Csq.grd))[,1], 4)
      Csq.grd$idy <- round(st_coordinates(st_centroid(Csq.grd))[,2], 4)
      
      #drop sf
      dat <- st_drop_geometry(dat)
      Csq.grd <- st_drop_geometry(Csq.grd)
      Csq.grd$CSquare <- as.character(Csq.grd$CSquare)
      
      #find nearest neighbor in the grid for summation by point
      index <- nn2(data = Csq.grd[, c("idx", "idy")],
                   query = dat[, c("LONG", "LATI")], k = 1)
      
      dat$CSquare <- Csq.grd[index[["nn.idx"]], "CSquare"]
      
      
      ### summarize
      dat$area <- dat$Area_27
      dat$CS <- "CS1" #<- change if relevent
      
      setDT(dat)
      out <- dat[ ,. (Gear_lvl4 = "GNS",
                      LengthHauled = sum(as.numeric(ptsNetlength), na.rm = T),
                      SoakTime = sum(ptsSoaktime, na.rm = T),
                      LengthXSkt = sum(as.numeric(ptsNetlength), na.rm = T) * 
                        sum(ptsSoaktime, na.rm = T),
                      fishingHours = (sum(DIFFTIME.secs)/(60*60)), na.rm = T),
                  by = .(VESSEL_FK, quarter, year, CS,
                         area, statisticalRectangle, CSquare)]
      
    }
    ship_effort <- lapply(1:length(lst), fun)
    
  }
  
)
options(warn=0)

#put into the final CIBBRiNA format
setDT(ship_effort)
iapesca_agg3 <- ship_effort[ ,. (Gear_lvl4 = "GNS",
                        LengthHauled = sum(LengthHauled),
                        SoakTime = sum(SoakTime),
                        LengthXSkt = sum(LengthXSkt),
                        fishingHours = sum(fishingHours),
                        nVessls = length(unique(VESSEL_FK))),
                    by = .(year, quarter, CS,
                           area, statisticalRectangle, CSquare)]

iapesca_agg3$conf_flag <- ifelse(iapesca_agg3$nVessls >= 3, 0, 1)
iapesca_agg3_sum <- iapesca_agg3[ ,. (fishingHours_pct=sum(fishingHours),
                                      LengthHauled_pct=sum(LengthHauled),
                                      SoakTime_pct=sum(SoakTime), 
                                      LengthXSkt_pct=sum(LengthXSkt)),
                                  by = .(CS, year, area, conf_flag)]

iapesca_agg3_sum[, fishingHours_pct := round((fishingHours_pct/ sum(fishingHours_pct))*100, 2),
                 by = .(CS, year, area)]
iapesca_agg3_sum[, LengthHauled_pct := round((LengthHauled_pct/ sum(LengthHauled_pct))*100, 2),
                 by = .(CS, year, area)]
iapesca_agg3_sum[, SoakTime_pct := round((SoakTime_pct/ sum(SoakTime_pct))*100, 2),
                 by = .(CS, year, area)]
iapesca_agg3_sum[, LengthXSkt_pct := round((LengthXSkt_pct/ sum(LengthXSkt_pct))*100, 2),
                 by = .(CS, year, area)]

write.csv(iapesca_agg3_sum[iapesca_agg3_sum$conf_flag == 0, ], 
          "results/CIBBRINA_aggregation_lvl3.csv", 
          quote = F, row.names = F)

write.csv(iapesca_agg3[iapesca_agg3$conf_flag == 0, ], 
          "results/CIBBRINA_aggregation_lvl3.csv", 
          quote = F, row.names = F)

######
iapesca_agg2b <- ship_effort[ ,. (Gear_lvl4 = "GNS",
                        LengthHauled = sum(LengthHauled),
                        SoakTime = sum(SoakTime),
                        LengthXSkt = sum(LengthXSkt),
                        FishingHours = sum(FishingHours),
                        nVessls = length(unique(VESSEL_FK))),
                    by = .(year, 
                           Area, statisticalRectangle)]

iapesca_agg2b$conf_flag <- ifelse(iapesca_agg2b$nVessls >= 3, 0, 1)
iapesca_agg2b_sum <- iapesca_agg2b[ ,. (fishingHours_pct=sum(fishingHours),
                                      LengthHauled_pct=sum(LengthHauled),
                                      SoakTime_pct=sum(SoakTime), 
                                      LengthXSkt_pct=sum(LengthXSkt)),
                                  by = .(CS, year, area, conf_flag)]

iapesca_agg2b_sum[, fishingHours_pct := round((fishingHours_pct/ sum(fishingHours_pct))*100, 2),
                 by = .(CS, year, area)]
iapesca_agg2b_sum[, LengthHauled_pct := round((LengthHauled_pct/ sum(LengthHauled_pct))*100, 2),
                 by = .(CS, year, area)]
iapesca_agg2b_sum[, SoakTime_pct := round((SoakTime_pct/ sum(SoakTime_pct))*100, 2),
                 by = .(CS, year, area)]
iapesca_agg2b_sum[, LengthXSkt_pct := round((LengthXSkt_pct/ sum(LengthXSkt_pct))*100, 2),
                 by = .(CS, year, area)]

write.csv(iapesca_agg2b_sum[iapesca_agg2b_sum$conf_flag == 0, ], 
          "results/CIBBRINA_aggregation_lvl2b.csv", 
          quote = F, row.names = F)

write.csv(iapesca_agg2b[iapesca_agg2b$conf_flag == 0, ], 
          "results/CIBBRINA_aggregation_lvl2b.csv", 
          quote = F, row.names = F)

#
iapesca_agg2a <- ship_effort[ ,. (Gear_lvl4 = "GNS",
                        LengthHauled = sum(LengthHauled),
                        SoakTime = sum(SoakTime),
                        LengthXSkt = sum(LengthXSkt),
                        FishingHours = sum(FishingHours),
                        nVessls = length(unique(VESSEL_FK))),
                    by = .(year, quarter, 
                           Area)]

iapesca_agg2a$conf_flag <- ifelse(iapesca_agg2a$nVessls >= 3, 0, 1)
iapesca_agg2a_sum <- iapesca_agg2a[ ,. (fishingHours_pct=sum(fishingHours),
                                        LengthHauled_pct=sum(LengthHauled),
                                        SoakTime_pct=sum(SoakTime), 
                                        LengthXSkt_pct=sum(LengthXSkt)),
                                    by = .(CS, year, area, conf_flag)]

iapesca_agg2a_sum[, fishingHours_pct := round((fishingHours_pct/ sum(fishingHours_pct))*100, 2),
                  by = .(CS, year, area)]
iapesca_agg2a_sum[, LengthHauled_pct := round((LengthHauled_pct/ sum(LengthHauled_pct))*100, 2),
                  by = .(CS, year, area)]
iapesca_agg2a_sum[, SoakTime_pct := round((SoakTime_pct/ sum(SoakTime_pct))*100, 2),
                  by = .(CS, year, area)]
iapesca_agg2a_sum[, LengthXSkt_pct := round((LengthXSkt_pct/ sum(LengthXSkt_pct))*100, 2),
                  by = .(CS, year, area)]

write.csv(iapesca_agg2a_sum[iapesca_agg2a_sum$conf_flag == 0, ], 
          "results/CIBRINA_aggregation_lvl2a.csv", 
          quote = F, row.names = F)

write.csv(iapesca_agg2a[iapesca_agg2a$conf_flag == 0, ], 
          "results/CIBRINA_aggregation_lvl2a.csv", 
          quote = F, row.names = F)

#
iapesca_agg1 <- ship_effort[ ,. (Gear_lvl4 = "GNS",
                        LengthHauled = sum(LengthHauled),
                        SoakTime = sum(SoakTime),
                        LengthXSkt = sum(LengthXSkt),
                        FishingHours = sum(FishingHours),
                        nVessls = length(unique(VESSEL_FK))),
                    by = .(year, Area)]

iapesca_agg1 <- ship_effort[ ,. (Gear_lvl4 = "GNS",
                                  LengthHauled = sum(LengthHauled),
                                  SoakTime = sum(SoakTime),
                                  LengthXSkt = sum(LengthXSkt),
                                  FishingHours = sum(FishingHours),
                                  nVessls = length(unique(VESSEL_FK))),
                              by = .(year, quarter, 
                                     Area)]

iapesca_agg1$conf_flag <- ifelse(iapesca_agg1$nVessls >= 3, 0, 1)
iapesca_agg1_sum <- iapesca_agg1[ ,. (fishingHours_pct=sum(fishingHours),
                                        LengthHauled_pct=sum(LengthHauled),
                                        SoakTime_pct=sum(SoakTime), 
                                        LengthXSkt_pct=sum(LengthXSkt)),
                                    by = .(CS, year, conf_flag)]

iapesca_agg1_sum[, fishingHours_pct := round((fishingHours_pct/ sum(fishingHours_pct))*100, 2),
                  by = .(CS, year)]
iapesca_agg1_sum[, LengthHauled_pct := round((LengthHauled_pct/ sum(LengthHauled_pct))*100, 2),
                  by = .(CS, year)]
iapesca_agg1_sum[, SoakTime_pct := round((SoakTime_pct/ sum(SoakTime_pct))*100, 2),
                  by = .(CS, year)]
iapesca_agg1_sum[, LengthXSkt_pct := round((LengthXSkt_pct/ sum(LengthXSkt_pct))*100, 2),
                  by = .(CS, year)]

write.csv(iapesca_agg1_sum[iapesca_agg1_sum$conf_flag == 0, ], 
          "results/CIBBRINA_aggregation_lvl1.csv", 
          quote = F, row.names = F)

write.csv(iapesca_agg1[iapesca_agg1$conf_flag == 0, ], 
          "results/CIBBRINA_aggregation_lvl1.csv", 
          quote = F, row.names = F)


