

## Script to generate timeseries of estimated SDD from all satellite imagery


## Code accompanying "Satellite imagery as a management tool for monitoring water clarity across freshwater 
## ponds in Cape Cod, Massachusetts" (Coffer et al., 2024, Journal of Environmental Management). 
## R code written by first author Megan Coffer.


## Load required packages 
require(randomForest)


## Set main working directory 
main.dir <- "..."


## Read in depth data 
depth <- read.csv(paste0(main.dir, "Input_Data/Pond_Depth/Pond_Depth_formatted.csv"), header = T)


## Define function to create timeseries 
sat.ts.fxn <- function(satellite){
  
  ## Read in random forest model 
  sat.rf <- get(load(paste0(main.dir,"Output_Data/Random_Forest/", satellite, "_Random_Forest.RData")))
  ## Read in reflectance data 
  sat.filenames <- list.files(paste0(main.dir,"Input_Data/Reflectance_Timeseries/", satellite),"*.csv$", full.names = T)
  sat.csvs <- lapply(sat.filenames, read.csv)
  
  ## Determine the spectral bands of interest based on the satellite 
  if(satellite %in% c("L8","L9")){
    sat.bands <- c("B1_mean","B2_mean","B3_mean","B4_mean","B5_mean","B6_mean","B7_mean")
   }else if(satellite %in% c("L5","L7")){
    sat.bands <- c("B1_mean","B2_mean","B3_mean","B4_mean","B5_mean","B7_mean")
  }else if(satellite == "S2"){
    sat.bands <- c("B1_mean","B2_mean","B3_mean","B4_mean","B5_mean","B6_mean","B7_mean","B8_mean","B9_mean","B10_mean","B11_mean","B12_mean")
  }
  

  ## Loop through each CCC_GIS_ID and generate timeseries
  for(id in 1:length(sat.csvs)){
    
    ## Join depth and subset to complete cases
    sat.depth <- merge(sat.csvs[[id]], depth[, c("CCC_GIS_ID", "Name", "Acres", "Town", "Maximum_Depth__m_")], by = "CCC_GIS_ID")
    sat.cc <- sat.depth[complete.cases(sat.depth[,c(sat.bands, "Maximum_Depth__m_")]),]
    #test[[id]] <- ifelse(length(unique(sat.depth$CCC_GIS_ID)) > 0, unique(sat.depth$CCC_GIS_ID), NA)
    ## Remove observations that may have floating vegetation
    if(satellite %in% c("L8","L9")){
      sat.cc$NDVI <- (sat.cc$B5_mean - sat.cc$B4_mean)/(sat.cc$B5_mean + sat.cc$B4_mean)
    }else if(satellite %in% c("L5","L7")){
      sat.cc$NDVI <- (sat.cc$B4_mean - sat.cc$B3_mean)/(sat.cc$B4_mean + sat.cc$B3_mean)
    }else if(satellite == "S2"){
      sat.cc$NDVI <- (sat.cc$B8_mean - sat.cc$B4_mean)/(sat.cc$B8_mean + sat.cc$B4_mean)
    }
    sat.ndvi <- subset(sat.cc, NDVI <= 0.3)
    ## Subset to just summer observations
    sat.ndvi$day_str <- as.Date(sat.ndvi$day_str, "%Y-%m-%d")
    sat.season <- subset(sat.ndvi, months(sat.ndvi$day_str) %in% c("April","May","June","July","August","September","October"))

    ## If there is more than 0 rows in sat.season, continue processing
    if(nrow(sat.season) > 0){

      ## Apply random forest and set satellite-predicted SDD that's greater than maximum pond depth to the maximum pond depth
      sat.season$SDD_PREDICT <- predict(sat.rf, sat.season)
      sat.season$SDD_PREDICT[which(sat.season$SDD_PREDICT > sat.season$Maximum_Depth__m_)] <- sat.season$Maximum_Depth__m_[which(sat.season$SDD_PREDICT > sat.season$Maximum_Depth__m_)]
      ## Output timeseries as CSV file
      dir.create(file.path(main.dir, "Output_Data/SDD_Timeseries"), showWarnings = F)
      dir.create(file.path(main.dir, "Output_Data/SDD_Timeseries/", satellite), showWarnings = F)
      write.csv(sat.season, paste0(main.dir,"Output_Data/SDD_Timeseries/", satellite, "/", unique(sat.cc$CCC_GIS_ID), "_", satellite, "_SDD_Timeseries.csv"))

    }
  }
}


## Apply sat.ts.fxn function across all satellite sensors 
sapply(c("L9","L8","L7","L5","S2"), sat.ts.fxn)




