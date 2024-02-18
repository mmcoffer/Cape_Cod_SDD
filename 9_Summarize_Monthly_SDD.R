
## Summarize monthly SDD for each year  


## Code accompanying "Satellite imagery as a management tool for monitoring water clarity across freshwater 
## ponds in Cape Cod, Massachusetts" (Coffer et al., 2024, Journal of Environmental Management). 
## R code written by first author Megan Coffer.


## Load required packages 
require(stringr)


## Set main working directory 
main.dir <- "..."
## Set the year of interest
sat.year <- "2023"


## Read in satellite-predicted SDD for relevant sensors 
## Landsat 5
l5.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/L5/"), "*.csv$", full.names = T)
if(length(l5.list.files) > 0){
  l5 <- lapply(lapply(l5.list.files, read.csv), function(x){subset(x, substr(x$day_str,1,4) == sat.year)})
} else{
  l5 <- NA
}
## Landsat 7 
l7.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/L7/"), "*.csv$", full.names = T)
if(length(l7.list.files) > 0){
  l7 <- lapply(lapply(l7.list.files, read.csv), function(x){subset(x, substr(x$day_str,1,4) == sat.year)}) 
} else{
  l7 <- NA
}
## Landsat 8 
l8.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/L8/"), "*.csv$", full.names = T)
if(length(l8.list.files) > 0){
  l8 <- lapply(lapply(l8.list.files, read.csv), function(x){subset(x, substr(x$day_str,1,4) == sat.year)})
} else{
  l8 <- NA
}
## Landsat 9
l9.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/L9/"), "*.csv$", full.names = T)
if(length(l9.list.files) > 0){
  l9 <- lapply(lapply(l9.list.files, read.csv), function(x){subset(x, substr(x$day_str,1,4) == sat.year)})
} else{
  l9 <- NA
}
## Sentinel-2
s2.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/S2/"), "*.csv$", full.names = T)
if(length(s2.list.files) > 0){
  s2 <- lapply(lapply(s2.list.files, read.csv), function(x){subset(x, substr(x$day_str,1,4) == sat.year)})
} else{
  s2 <- NA
}
## Find unique ponds IDs
ponds <- unique(c(sapply(str_split(basename(l5.list.files), "_"), "[[", 1), sapply(str_split(basename(l7.list.files), "_"), "[[", 1), sapply(str_split(basename(l8.list.files), "_"), "[[", 1), sapply(str_split(basename(l9.list.files), "_"), "[[", 1), sapply(str_split(basename(s2.list.files), "_"), "[[", 1)))


## Create a dataframe to populate with results 
df <- as.data.frame(matrix(nrow = max(c(length(l5), length(l7), length(l8), length(l9), length(s2))), ncol = 13))
colnames(df) <- c("CCC_GIS_ID","Waterbody_Name","Waterbody_Town","Surface_Area_Acres","Waterbody_Latitude","Waterbody_Longitude","Apr_SDD","May_SDD","Jun_SDD","Jul_SDD","Aug_SDD","Sep_SDD","Oct_SDD")


## Loop through each row and populate with results 
for(pond in 1:nrow(df)){
  
  ## Combine all sensors for the given pond 
  ## Columns of interest
  df.columns <- c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT","Acres","latitude","longitude")
  ## Landsat 5
  if(length(l5) > 1){
    l5.pond <- l5[[which(sapply(str_split(basename(l5.list.files), "_"), "[[", 1) == ponds[pond])]]
  } else{
    l5.pond <- NA
  }
  ## Landsat 7
  if(length(l7) > 1){
    l7.pond <- l7[[which(sapply(str_split(basename(l7.list.files), "_"), "[[", 1) == ponds[pond])]]  
  } else{
    l7.pond <- NA
  }
  ## Sentinel-2
  if(length(s2) > 1){
    s2.pond <- s2[[which(sapply(str_split(basename(s2.list.files), "_"), "[[", 1) == ponds[pond])]]
  } else{
    s2.pond <- NA
  }
  ## Landsat 8 and 9
  if(!(ponds[pond] %in% sapply(str_split(basename(l8.list.files), "_"), "[[", 1)) & ponds[pond] %in% sapply(str_split(basename(l9.list.files), "_"), "[[", 1)){
    l9.pond <- l9[[which(sapply(str_split(basename(l9.list.files), "_"), "[[", 1) == ponds[pond])]]
    pond.df <- rbind(l5.pond[df.columns], l7.pond[df.columns], l9.pond[df.columns], s2.pond[df.columns])
  } else if(ponds[pond] %in% sapply(str_split(basename(l8.list.files), "_"), "[[", 1) & !(ponds[pond] %in% sapply(str_split(basename(l9.list.files), "_"), "[[", 1))){
    l8.pond <- l8[[which(sapply(str_split(basename(l8.list.files), "_"), "[[", 1) == ponds[pond])]]
    pond.df <- rbind(l5.pond[df.columns], l7.pond[df.columns], l8.pond[df.columns], s2.pond[df.columns])
  } else{
    l8.pond <- l8[[which(sapply(str_split(basename(l8.list.files), "_"), "[[", 1) == ponds[pond])]]
    l9.pond <- l9[[which(sapply(str_split(basename(l9.list.files), "_"), "[[", 1) == ponds[pond])]]
    pond.df <- rbind(l5.pond[df.columns], l7.pond[df.columns], l8.pond[df.columns], l9.pond[df.columns], s2.pond[df.columns])
  }
  
  s2.pond <- s2[[which(sapply(str_split(basename(s2.list.files), "_"), "[[", 1) == ponds[pond])]]
  df.columns <- c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT","Acres","latitude","longitude")
  if(!(ponds[pond] %in% sapply(str_split(basename(l8.list.files), "_"), "[[", 1)) & ponds[pond] %in% sapply(str_split(basename(l9.list.files), "_"), "[[", 1)){
    l9.pond <- l9[[which(sapply(str_split(basename(l9.list.files), "_"), "[[", 1) == ponds[pond])]]
    pond.df <- rbind(l9.pond[df.columns], s2.pond[df.columns])
  } else if(ponds[pond] %in% sapply(str_split(basename(l8.list.files), "_"), "[[", 1) & !(ponds[pond] %in% sapply(str_split(basename(l9.list.files), "_"), "[[", 1))){
    l8.pond <- l8[[which(sapply(str_split(basename(l8.list.files), "_"), "[[", 1) == ponds[pond])]]
    pond.df <- rbind(l8.pond[df.columns], s2.pond[df.columns])
  } else{
    l8.pond <- l8[[which(sapply(str_split(basename(l8.list.files), "_"), "[[", 1) == ponds[pond])]]
    l9.pond <- l9[[which(sapply(str_split(basename(l9.list.files), "_"), "[[", 1) == ponds[pond])]]
    pond.df <- rbind(l8.pond[df.columns], l9.pond[df.columns], s2.pond[df.columns])
  }
  ## Reduce to only complete cases of pond.df 
  pond.df <- pond.df[complete.cases(pond.df),]
  
  ## If there is at least one observation, proceed
  if(nrow(pond.df) > 0){
    
    ## Populate df with results 
    df$CCC_GIS_ID[pond] <- unique(pond.df$CCC_GIS_ID)
    df$Waterbody_Name[pond] <- unique(pond.df$Name)
    df$Waterbody_Town[pond] <- unique(pond.df$Town)
    df$Surface_Area_Acres[pond] <- unique(pond.df$Acres)
    df$Waterbody_Latitude[pond] <- unique(pond.df$latitude)
    df$Waterbody_Longitude[pond] <- unique(pond.df$longitude)
    df$Apr_SDD[pond] <- median(subset(pond.df, substr(day_str,6,7) == "04")$SDD_PREDICT)
    df$May_SDD[pond] <- median(subset(pond.df, substr(day_str,6,7) == "05")$SDD_PREDICT)
    df$Jun_SDD[pond] <- median(subset(pond.df, substr(day_str,6,7) == "06")$SDD_PREDICT)
    df$Jul_SDD[pond] <- median(subset(pond.df, substr(day_str,6,7) == "07")$SDD_PREDICT)
    df$Aug_SDD[pond] <- median(subset(pond.df, substr(day_str,6,7) == "08")$SDD_PREDICT)
    df$Sep_SDD[pond] <- median(subset(pond.df, substr(day_str,6,7) == "09")$SDD_PREDICT)
    df$Oct_SDD[pond] <- median(subset(pond.df, substr(day_str,6,7) == "10")$SDD_PREDICT)
    
  }
}


## Export df as a CSV file 
dir.create(file.path(main.dir, "Output_Data/Monthly_SDD"), showWarnings = F)
write.csv(df, paste0(main.dir,"Output_Data/Monthly_SDD/", sat.year, "_monthly_SDD.csv"), row.names = F)

