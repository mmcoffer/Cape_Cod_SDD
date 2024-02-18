

## Script to generate random forest models, scatter plots, and calculate agreement statistics


## Code accompanying "Satellite imagery as a management tool for monitoring water clarity across freshwater 
## ponds in Cape Cod, Massachusetts" (Coffer et al., 2024, Journal of Environmental Management). 
## R code written by first author Megan Coffer.


## Load required packages 
require(randomForest)


## Set main working directory 
main.dir <- "..."


## Read in depth data 
depth <- read.csv(paste0(main.dir, "Input_Data/Pond_Depth/Pond_Depth_formatted.csv"), header = T)


## Define function to create random forest model and compute correlations 
rf.corr.fxn <- function(satellite){
  
  ## Read in data 
  sat.csv <- read.csv(paste0(main.dir,"Input_Data/Field_Data/", satellite, "_Rrs0010C.csv"))
  sat.depth <- merge(sat.csv, depth[, c("CCC_GIS_ID", "Name", "Acres", "Town", "Maximum_Depth__m_")], by = "CCC_GIS_ID")
  ## Subset to just the months of April through October 
  sat.depth$day_str <- as.Date(sat.depth$day_str, "%Y-%m-%d")
  sat.depth <- subset(sat.depth, months(day_str) %in% c("April","May","June","July","August","September","October"))
  
  ## Determine the spectral bands of interest based on the satellite 
  if(satellite %in% c("L8","L9")){
    sat.bands <- c("B1_mean","B2_mean","B3_mean","B4_mean","B5_mean","B6_mean","B7_mean")
    sat.depth$NDVI <- (sat.depth$B5_mean - sat.depth$B4_mean)/(sat.depth$B5_mean + sat.depth$B4_mean)
  }else if(satellite %in% c("L5","L7")){
    sat.bands <- c("B1_mean","B2_mean","B3_mean","B4_mean","B5_mean","B7_mean")
    sat.depth$NDVI <- (sat.depth$B4_mean - sat.depth$B3_mean)/(sat.depth$B4_mean + sat.depth$B3_mean)
  }else if(satellite == "S2"){
    sat.bands <- c("B2_mean","B3_mean","B4_mean","B5_mean","B6_mean","B7_mean","B8_mean","B11_mean","B12_mean")
    sat.depth$NDVI <- (sat.depth$B8_mean - sat.depth$B4_mean)/(sat.depth$B8_mean + sat.depth$B4_mean)
  }
  
  ## Discard rows that do not have complete observations for the columns of interest 
  sat.cc <- sat.depth[complete.cases(sat.depth[,c("SDD..m.", sat.bands, "Maximum_Depth__m_")]),]
  ## Remove observations that may have floating vegetation 
  sat.cc <- subset(sat.cc, NDVI <= 0.3)
  
  ## Create an empty dataframe to populate with results 
  day.offsets <- c(1:5)
  corr.df <- as.data.frame(matrix(nrow = length(day.offsets), ncol = 9))
  colnames(corr.df) <- c("SENSOR","OFFSET","TRAIN","TRAIN_N","TEST","TEST_N","ASSOCIATION","MAE","BIAS")
  corr.df$SENSOR <- satellite
  corr.df$OFFSET <- paste(day.offsets, "day(s)")
  
  ## Loop through each day.offset in day.offsets
  for(day.offset in day.offsets){
      
    ## Subset to just observations collected within n days of a satellite overpass
    sat.subset <- subset(sat.cc, abs(lagDays_mean) <= day.offset)
    ## Set seed so results are replicable 
    set.seed(200)
    ## Split observations into training and testing subsets 
    sat.sample.index <- sample(2, nrow(sat.subset), replace = T, prob = c(0.7, 0.3))
    sat.train <- sat.subset[sat.sample.index == 1,]
    sat.test <- sat.subset[sat.sample.index == 2,]
    ## Generate a random forest 
    rf.formula <- as.formula(paste("SDD..m. ~ ", paste(sat.bands, collapse = " + "), "+ Maximum_Depth__m_"))
    #rf.formula <- as.formula(paste("SDD..m. ~ ", paste(sat.bands, collapse = " + ")))
    sat.rf <- randomForest(rf.formula, data = sat.train, proximity = T)
    ## Predict training observations using l8.rf 
    sat.train.predict <- predict(sat.rf, sat.train)
    ## Predict testing observations using l8.rf 
    sat.test.predict <- predict(sat.rf, sat.test)
    ## Set satellite-predicted SDD that's greater than maximum pond depth to the maximum pond depth
    sat.train.predict[which(sat.train.predict > sat.train$Maximum_Depth__m_)] <- sat.train$Maximum_Depth__m_[which(sat.train.predict > sat.train$Maximum_Depth__m_)]
    if(length(which(sat.test.predict > sat.test$Maximum_Depth__m_)) > 0){
      sat.test.predict[which(sat.test.predict > sat.test$Maximum_Depth__m_)] <- sat.test$Maximum_Depth__m_[which(sat.test.predict > sat.test$Maximum_Depth__m_)]
    }
    ## Compute correlation 
    corr.df$TRAIN[day.offset] <- round(cor(sat.train.predict, sat.train$SDD..m., method = "spearman"), digits = 2)
    corr.df$TRAIN_N[day.offset] <- length(sat.train.predict)
    corr.df$TEST[day.offset] <- round(cor(sat.test.predict, sat.test$SDD..m., method = "spearman"), digits = 2)
    corr.df$TEST_N[day.offset] <- length(sat.test.predict)
    sat.cor <- corr.df$TEST[day.offset]
    corr.df$ASSOCIATION[day.offset] <- ifelse(sat.cor < 0.2, "negligible", ifelse(sat.cor < 0.4, "weak", ifelse(sat.cor < 0.6, "moderate", ifelse(sat.cor < 0.8, "strong", "very strong"))))
    corr.df$MAE[day.offset] <- round(Metrics::mae(actual = sat.test$SDD..m., predicted = sat.test.predict), digits = 2)
    corr.df$BIAS[day.offset] <- round(Metrics::bias(sat.test$SDD..m., sat.test.predict), digits = 2)
    
  }
  
  ## Return correlation results 
  return(corr.df)
  
}


## Apply sat.rf.fxn function across all satellite sensors 
r.corr.df <- lapply(c("L9","L8","L7","L5","S2"), rf.corr.fxn)
all.corr.df <- do.call(rbind, r.corr.df)


## Loop through each unique date and find highest correlation among sensors 
for(i in 1:length(unique(all.corr.df$OFFSET))){
  
  ## Subset to i day offset 
  s.corr.df <- subset(all.corr.df, OFFSET == unique(all.corr.df$OFFSET)[i])
  ## Find weighted average and association 
  s.avg.train <- round(weighted.mean(as.numeric(s.corr.df$TRAIN), as.numeric(s.corr.df$TRAIN_N)), digits = 2)
  s.avg.test <- round(weighted.mean(as.numeric(s.corr.df$TEST), as.numeric(s.corr.df$TEST_N)), digits = 2)
  s.asc <- ifelse(s.avg.test <= 0.2, "negligible", ifelse(s.avg.test <= 0.4, "weak", ifelse(s.avg.test <= 0.6, "moderate", ifelse(s.avg.test <= 0.8, "strong", "very strong"))))
  s.mae <- round(weighted.mean(as.numeric(s.corr.df$MAE), as.numeric(s.corr.df$TEST_N)), digits = 2)
  s.bias <- round(weighted.mean(as.numeric(s.corr.df$BIAS), as.numeric(s.corr.df$TEST_N)), digits = 2)
  ## Add row to dataframe 
  s.row <- c("avg", unique(all.corr.df$OFFSET)[i], s.avg.train, sum(as.numeric(s.corr.df$TRAIN_N)), s.avg.test, sum(as.numeric(s.corr.df$TEST_N)), s.asc, s.mae, s.bias)
  all.corr.df <- rbind(all.corr.df, s.row)
  
}
## Find day offset with highest correlation
offset.max <- which.max(subset(all.corr.df, SENSOR == "avg")$TEST)


## Export CSV file 
dir.create(file.path(main.dir, "Output_Data"), showWarnings = F)
dir.create(file.path(main.dir, "Output_Data/Correlation"), showWarnings = F)
write.csv(all.corr.df, paste0(main.dir,"Output_Data/Correlation/RF_Corr_allSensors_allOffsets.csv"), row.names = F)


## Define a function to create random forest model 
rf.create.fxn <- function(satellite, day.offset = offset.max){
  
  ## Read in data 
  sat.csv <- read.csv(paste0(main.dir,"Input_Data/Field_Data/", satellite, "_Rrs0010C.csv"))
  sat.depth <- merge(sat.csv, depth[, c("CCC_GIS_ID", "Name", "Acres", "Town", "Maximum_Depth__m_")], by = "CCC_GIS_ID")
  ## Subset to just the months of April through October 
  sat.depth$day_str <- as.Date(sat.depth$day_str, "%Y-%m-%d")
  sat.depth <- subset(sat.depth, months(day_str) %in% c("April","May","June","July","August","September","October"))
  
  ## Determine the spectral bands of interest based on the satellite 
  if(satellite %in% c("L8","L9")){
    sat.bands <- c("B1_mean","B2_mean","B3_mean","B4_mean","B5_mean","B6_mean","B7_mean")
    sat.depth$NDVI <- (sat.depth$B5_mean - sat.depth$B4_mean)/(sat.depth$B5_mean + sat.depth$B4_mean)
  }else if(satellite %in% c("L5","L7")){
    sat.bands <- c("B1_mean","B2_mean","B3_mean","B4_mean","B5_mean","B7_mean")
    sat.depth$NDVI <- (sat.depth$B4_mean - sat.depth$B3_mean)/(sat.depth$B4_mean + sat.depth$B3_mean)
  }else if(satellite == "S2"){
    sat.bands <- c("B1_mean","B2_mean","B3_mean","B4_mean","B5_mean","B6_mean","B7_mean","B8_mean","B9_mean","B10_mean","B11_mean","B12_mean")
    sat.depth$NDVI <- (sat.depth$B8_mean - sat.depth$B4_mean)/(sat.depth$B8_mean + sat.depth$B4_mean)
  }
  
  ## Discard rows that do not have complete observations for the columns of interest 
  sat.cc <- sat.depth[complete.cases(sat.depth[,c("SDD..m.", sat.bands, "Maximum_Depth__m_")]),]
  ## Remove observations that may have floating vegetation 
  sat.cc <- subset(sat.cc, NDVI <= 0.3)
  
  ## Subset to just observations collected within n days of a satellite overpass
  sat.subset <- subset(sat.cc, abs(lagDays_mean) <= day.offset)
  ## Set seed so results are replicable 
  set.seed(200)
  ## Split observations into training and testing subsets 
  sat.sample.index <- sample(2, nrow(sat.subset), replace = T, prob = c(0.7, 0.3))
  sat.train <- sat.subset[sat.sample.index == 1,]
  sat.test <- sat.subset[sat.sample.index == 2,]
  ## Generate a random forest 
  rf.formula <- as.formula(paste("SDD..m. ~ ", paste(sat.bands, collapse = " + "), "+ Maximum_Depth__m_"))
  sat.rf <- randomForest(rf.formula, data = sat.train, proximity = T)
  dir.create(file.path(main.dir, "Output_Data/Random_Forest"), showWarnings = F)
  save(sat.rf, file = paste0(main.dir,"Output_Data/Random_Forest/", satellite, "_Random_Forest.RData"))
  ## Predict training observations using sat.rf 
  sat.train.predict <- predict(sat.rf, sat.train)
  ## Predict testing observations using sat.rf 
  sat.test.predict <- predict(sat.rf, sat.test)
  ## Set satellite-predicted SDD that's greater than maximum pond depth to the maximum pond depth
  sat.train.predict[which(sat.train.predict > sat.train$Maximum_Depth__m_)] <- sat.train$Maximum_Depth__m_[which(sat.train.predict > sat.train$Maximum_Depth__m_)]
  sat.test.predict[which(sat.test.predict > sat.test$Maximum_Depth__m_)] <- sat.test$Maximum_Depth__m_[which(sat.test.predict > sat.test$Maximum_Depth__m_)]

}


## Apply sat.rf.fxn function across all satellite sensors 
sat.rf.create.output <- sapply(c("L9","L8","L7","L5","S2"), rf.create.fxn)

