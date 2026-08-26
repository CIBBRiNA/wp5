#devtools::install_git("https://gitlab.ifremer.fr/iapesca/r-packages_iapesca")
library(iapesca)
library(data.table)
library(mapview)
library(ggplot2)
library(rpart)
library(plyr)
ls(getNamespace("iapesca"), all.names=TRUE) |> head()

# read in national training data
data <- readRDS("data/annoteted_data/tst_data.rds")

#subset for faster example run
dat <- data[data$encryptedID %in% unique(data$encryptedID)[1:3], ]

# translate some key variables
dat$Activity.type <- as.character(dat$Activity.type)
dat$Activity.type[dat$Activity.type == "Gear in"] <- "Hauling"
dat$Activity.type[dat$Activity.type == "Gear out"] <- "Setting"
dat$Activity.type[dat$Activity.type %in% c("sailing", "Not in EM data")] <- "not_fishing"

dat$Latitude <- as.numeric(dat$Latitude)
dat$Longitude <- as.numeric(dat$Longitude)
dat$speed <- as.numeric(dat$speed)
dat$head_deg <- as.numeric(dat$head_deg)

# Fit data to the CIBBRINA format
dat$FOP <- dat$Activity.type
process.PosCIBBRiNA <- Positions2CIBBRiNA(dat,
                                          VESSEL_ID = "encryptedID",
                                          TRIP_ID = "trip_id",
                                          DATE_TIME = "time_stamp",
                                          LONGITUDE = "Longitude",
                                          LATITUDE = "Latitude",
                                          GEAR = "GNS",
                                          PASSIVE_GEAR = TRUE,
                                          ACTIVITY = "Activity.type",
                                          ACTIVITY.fishing = NULL,
                                          ACTIVITY.hauling = "Hauling",
                                          ACTIVITY.setting = "Setting",
                                          SPEED = "Speed",
                                          COURSE = NULL,
                                          HEADING = "head_deg",
                                          QUALITY = NULL,
                                          MaxSpeed = 25,
                                          epsg = 4326,
                                          parallelize = FALSE,
                                          nCores = NULL,
                                          keep.columns = "FOP",
                                          as.sf = FALSE,
                                          as.df = FALSE)


process.PosCIBBRiNA$positions.CIBBRiNA |> head()

assign_lsTabs(process.PosCIBBRiNA)
c("positions.CIBBRiNA", "FT.desc") %in% ls()
rm(process.PosCIBBRiNA)


# Setup random forest training
res.secs <- round((max(FT.desc$AVG_INTV_S))/60) * 60
res.secs |> print()

FT.desc <- FT.desc[!FT.desc$FT_ID %like% "NA", ]

posForMl <- do.call(rbind.fill,
                    lapply(FT.desc$FT_ID,
                           function(trip){
                             print(trip)
                             Process_TripPositions(
                               trip.path = df2sfp(
                                 positions.CIBBRiNA[ positions.CIBBRiNA$FT_ID %in% trip, ]),
                               MaxSpeed = 25,
                               resampling = res.secs,
                               keep.var = c("ACTIVITY", "FOP"),
                               create.paths = FALSE,
                               columns.ref = c("VESSEL_ID", "FT_ID"),
                               CalcFeatures = TRUE,
                               movingWindow = 1)$trip.path
                           }))


covars <- c( "SPEED.kn", "Acceleration", "ProximityIndex",
             "Jerk", "BearingRate", "SpeedChange",
             "Straigthness", "Sinuosity", "TurningAngle",
             "DirectionChange")

col.index <- sapply(1:length(covars), function(k){ grep(colnames(posForMl), pattern = covars[k])})
covars.model <- colnames(posForMl)[col.index]
covars.model |> print()

nnai <- apply(posForMl[, covars.model], 1, function(x){ !anyNA(x) })
nnai|> summary()

posForMl$ACTIVITY <- factor(as.character(posForMl$ACTIVITY))

#train the model on parts pf the data set, and keep a part for testing
train <- posForMl[ nnai & !is.na(posForMl$ACTIVITY) &
                     posForMl$VESSEL_FK %in% unique(posForMl$VESSEL_FK)[1:2], ]

test <- posForMl[ nnai & !is.na(posForMl$ACTIVITY) &
                     posForMl$VESSEL_FK %in% unique(posForMl$VESSEL_FK)[3], ]

# run the forest
optim.rf <- tune_RF(formula = CreateFormula("ACTIVITY", covars.model),
                    data = train,
                    num.threads = 4)

# adjust the parametors, depending on data and model results, reiterate.
mod.rf <- ranger::ranger(CreateFormula("ACTIVITY", covars.model),
                         train,
                         importance = "impurity",
                         mtry = optim.rf$mtry,
                         min.node.size = optim.rf$min.node.size,
                         probability = TRUE,
                         num.threads = 1,
                         num.trees = 500,
                         write.forest = TRUE)


# Look into the model results
mod.cart <- rpart::rpart(CreateFormula("ACTIVITY", covars.model),
                         train)
plot_CART_tree(mod.cart)

#
rf.diagnosis <- RF_autodiagnosis(mod.rf,
                                 train,
                                 plot.output = FALSE)

barplot(rf.diagnosis$var.imp[1:6], cex.names = 0.5, cex.axis = 0.8,
      ylab = "Variance of response", main = "Variable importance" )

# prediction accuracy, here tested on a part of the dataset not used in traing 
mod.cart <- rpart(CreateFormula("ACTIVITY", covars),
                  test)
pred.cart <- predict(mod.cart)
pred.cart.hauling <- apply(pred.cart, 1, function(x){
  return(colnames(pred.cart)[ which.max(x)])
})


test$ACTIVITY <- ifelse(test$ACTIVITY == "hauling", "hauling", "not_fishing")
test$prediction <- pred.cart.hauling

table(test$ACTIVITY, test$prediction)
overall_err_test <- table(test$ACTIVITY, test$prediction)
(sum(diag(overall_err_test))/(sum(overall_err_test)))*100 # nice accuracy



########
#after testing the model a functional model is trained on all the data
# adjust the parametors, depending on data and model results, reiterate.
mod.rf <- ranger::ranger(CreateFormula("ACTIVITY", covars.model),
                         posForMl[nnai & !is.na(posForMl$ACTIVITY), ],
                         importance = "impurity",
                         mtry = optim.rf$mtry,
                         min.node.size = optim.rf$min.node.size,
                         probability = TRUE,
                         num.threads = 1,
                         num.trees = 500,
                         write.forest = TRUE)

saveRDS( list(optim.rf = optim.rf, mod.rf = mod.rf), 
         file= "results/Forrest_model_all_data.rds")




















