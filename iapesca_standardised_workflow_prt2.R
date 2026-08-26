#devtools::install_git("https://gitlab.ifremer.fr/iapesca/r-packages_iapesca")
library(iapesca)
library(data.table)
library(mapview)
library(ggplot2)
library(rpart)
ls(getNamespace("iapesca"), all.names=TRUE) |> head()

sf::sf_use_s2(F)

#support files
havne <- st_read("Q:/20-forskning/12-gis//Dynamisk/GEOdata/BasicLayers/Boundaries/Havnepolygoner/havnepolygoner.shp")%>%
  st_transform(4326)

#load in trained RF model, needed to determine fishing from positions data
rf <- readRDS("results/Forrest_model_all_data.rds")
rf <- rf[["mod.rf"]]

#positions data e.g AIS data
aisFiles <- list.files("data/AIS_data",
                       full.names = T, pattern = "non_IMO")

#her it fits as one file, otherwise loop over e.g. year
ais <- data.frame(plyr::rbind.fill(lapply(aisFiles, read.csv2)))
ais <- ais[ais$country == "Sweden" &
             ! is.na(ais$callsign), ]

ships <- unique(ais$callsign)

options(warn=-1)
suppressMessages(
  for (x in 1) {
    
    #get positions data
    fun <- function(i) {
      
      print(paste0(i, " of ", length(ships)))
      ship <- ships[i]
      
      pos <- ais[ais$callsign == ship, ]
      
      ## format data
      pos$postime <- as.POSIXct(strptime(pos$timestamp_pretty, 
                                         "%d/%m/%Y %H:%M:%S"), tz = "UTC")
      
      ##add trip from harbor shape file
      pos <- pos[order(pos$postime), ]
      
      pos$lat <- as.numeric(pos$lat)
      pos$long <- as.numeric(pos$long)
      
      #tripid
      
      pos <- pos[!is.na(pos$long) & !is.na(pos$lat), ] %>%
        sf::st_as_sf(coords = c("long","lat"), remove = F) %>%
        sf::st_set_crs(4326)
      
      pos$SI_HARB <- lengths(st_intersects(pos, havne)) > 0
      
      setDT(pos)
      pos <- pos[order(pos$postime), ]
      pos$trip_id<-paste0("pos_", ship, "_", rleid(pos$SI_HARB))
      
      #remove non sailing
      pos <- pos[pos$SI_HARB == 0, ]
      
      #remove spurius trips i.e. in and out of harbour too quicly to fish
      length <- pos[, .N, keyby=trip_id]
      length <- length[length$N > 10, ]$trip_id
      pos <- pos[pos$trip_id %in% length, ]
      ##
      
      # Fit data to the CIBBRINA format
      pos$Activity.type <- NA
      pos$Hauling <- NA
      pos$Setting <- NA
      
      process.PosCIBBRiNA <- Positions2CIBBRiNA(pos,
                                                VESSEL_ID = "callsign",
                                                TRIP_ID = "trip_id",
                                                DATE_TIME = "postime",
                                                LONGITUDE = "long",
                                                LATITUDE = "lat",
                                                GEAR = "GNS",
                                                PASSIVE_GEAR = TRUE,
                                                ACTIVITY = "Activity.type",
                                                ACTIVITY.fishing = NULL,
                                                ACTIVITY.hauling = "Hauling",
                                                ACTIVITY.setting = "Setting",
                                                SPEED = NULL,
                                                COURSE = NULL,
                                                HEADING = NULL,
                                                QUALITY = NULL,
                                                MaxSpeed = 25,
                                                epsg = 4326,
                                                parallelize = FALSE,
                                                nCores = NULL,
                                                keep.columns = FALSE,
                                                as.sf = FALSE,
                                                as.df = FALSE)
      
      process.PosCIBBRiNA$positions.CIBBRiNA |> head()
      assign_lsTabs(process.PosCIBBRiNA)
      c("positions.CIBBRiNA", "FT.desc") %in% ls()
      rm(process.PosCIBBRiNA)
      
      #add covariates
      posForMl <- do.call(rbind,
                          lapply(FT.desc$FT_ID,
                                 function(trip){
                                   Process_TripPositions(
                                     trip.path = df2sfp(
                                       positions.CIBBRiNA[ positions.CIBBRiNA$FT_ID %in% trip, ]),
                                     MaxSpeed = 25,
                                     resampling = 60,
                                     create.paths = FALSE,
                                     columns.ref = c("VESSEL_ID", "FT_ID"),
                                     CalcFeatures = TRUE,
                                     movingWindow = 1)$trip.path
                                 }))

      #
      posForMl2 <- st_drop_geometry(posForMl)
      
      pred.cart <- predict(rf, posForMl2, )$predictions
      pred.cart.hauling <- apply(pred.cart, 1, function(x){
        return(colnames(pred.cart)[ which.max(x)])
      })
      
      posForMl$prediction <- pred.cart.hauling
      
      #setup net finding/ setting from geospatial computation
      Nets.rfMod <- Create_NetsByBoat(traj = posForMl,
                                      Col.Fop = "prediction",
                                      Fop.category = "hauling",
                                      parallelize = FALSE,
                                      nCores = 4,
                                      length.lim = c(60, 13000),
                                      verbose = TRUE)
      
      
      
      Setting.rfMod <- Retrieve_SettingOperations(
        traj = posForMl,
        ls.nets = Nets.rfMod$Nets,
        Col.Fop = "prediction",
        Setting.category = "setting",
        Hauling.category = "hauling",
        update.Fop = TRUE,
        remove.orphans = TRUE,
        tol.soaktime = c(1, 120))
      
      
      saveRDS(Setting.rfMod, paste0("results/ship_effort/", ship,"_nets.rds"))
          
         
    }
    lapply(1:length(fids), fun)
  }
  
)
options(warn=0)

