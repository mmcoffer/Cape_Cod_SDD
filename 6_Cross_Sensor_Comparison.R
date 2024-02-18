

## Cross-sensor assessment 


## Code accompanying "Satellite imagery as a management tool for monitoring water clarity across freshwater 
## ponds in Cape Cod, Massachusetts" (Coffer et al., 2024, Journal of Environmental Management). 
## R code written by first author Megan Coffer.


## Load required packages 
require(stringr)


## Set main working directory 
main.dir <- "..."


## Create an empty dataframe to populate with results 
df <- as.data.frame(matrix(nrow = 5, ncol = 9))
colnames(df) <- c("SAT1","SAT2","START_YEAR","END_YEAR","rs","n","ASSOCIATION","MAE","BIAS")
df$SAT1 <- c("L5","L7","L8","L8","L9")
df$SAT2 <- c("L7","L8","L9","S2","S2")


## Define a function to compute cross-sensor comparison 
for(i in 1:nrow(df)){
  
  ## List csv files for each satellite
  sat1.list <- list.files(paste0(main.dir, "Output_Data/SDD_Timeseries/", df$SAT1[i]), "*.csv$", full.names = T)
  sat2.list <- list.files(paste0(main.dir, "Output_Data/SDD_Timeseries/", df$SAT2[i]), "*.csv$", full.names = T)
  
  ## If there are observations for both sensors, proceed 
  if(length(sat1.list) > 0 & length(sat2.list) > 0){
    
    ## Find matching CCC_GIS_IDs
    i.ccc.ids <- intersect(sapply(str_split(substr(basename(sat1.list), 1, 7), "_"), "[[", 1), sapply(str_split(substr(basename(sat2.list), 1, 7), "_"), "[[", 1))
    ## Read in data 
    sat1.df <- do.call(rbind, lapply(sat1.list[which(sapply(str_split(substr(basename(sat1.list), 1, 7), "_"), "[[", 1) %in% i.ccc.ids)], read.csv))
    sat2.df <- do.call(rbind, lapply(sat2.list[which(sapply(str_split(substr(basename(sat2.list), 1, 7), "_"), "[[", 1) %in% i.ccc.ids)], read.csv))
    ## Average for each month 
    sat1.df$YEARMONTH <- paste0(substr(sat1.df$day_str, 1, 4), substr(sat1.df$day_str, 6, 7))
    sat2.df$YEARMONTH <- paste0(substr(sat2.df$day_str, 1, 4), substr(sat2.df$day_str, 6, 7))
    sat1.agg <- aggregate(SDD_PREDICT ~ YEARMONTH + CCC_GIS_ID, sat1.df, FUN = mean)
    sat2.agg <- aggregate(SDD_PREDICT ~ YEARMONTH + CCC_GIS_ID, sat2.df, FUN = mean)
    
    ## Find matching PRODUCT_IDs and months
    i.sat.ids <- intersect(unique(paste(sat1.agg$YEARMONTH, sat1.agg$CCC_GIS_ID, sep = "__")), unique(paste(sat2.agg$YEARMONTH, sat2.agg$CCC_GIS_ID, sep = "__")))
    i.sat.ids.yearmonth <- sapply(str_split(i.sat.ids, "__"), "[[", 1)
    i.sat.ids.cccgisid <- sapply(str_split(i.sat.ids, "__"), "[[", 2)
    ## Subset satellite1.df.agg and satellite2.df.agg to just matching i.sat.ids
    sat1.subset <- subset(sat1.agg, YEARMONTH %in% i.sat.ids.yearmonth & CCC_GIS_ID %in% i.sat.ids.cccgisid)
    sat2.subset <- subset(sat2.agg, YEARMONTH %in% i.sat.ids.yearmonth & CCC_GIS_ID %in% i.sat.ids.cccgisid)
    ## Merge to a single dataframe  
    sat.merge <- merge(sat1.subset, sat2.subset, by = c("YEARMONTH", "CCC_GIS_ID"))
    
    ## Populate dataframe with results 
    df$START_YEAR[i] <- min(unique(as.numeric(substr(sat.merge$YEARMONTH, 1, 4))))
    df$END_YEAR[i] <- max(unique(as.numeric(substr(sat.merge$YEARMONTH, 1, 4))))
    df$rs[i] <- round(cor(sat.merge$SDD_PREDICT.x, sat.merge$SDD_PREDICT.y, method = "spearman"), digits = 2)
    df$n[i] <- nrow(sat.merge)
    df$ASSOCIATION[i] <-  ifelse(df$rs[i] < 0.2, "negligible", ifelse(df$rs[i] < 0.4, "weak", ifelse(df$rs[i] < 0.6, "moderate", ifelse(df$rs[i] < 0.8, "strong", "very strong"))))
    df$MAE[i] <- round(Metrics::mae(sat.merge$SDD_PREDICT.x, sat.merge$SDD_PREDICT.y), digits = 2)
    df$BIAS[i] <- round(Metrics::bias(sat.merge$SDD_PREDICT.x, sat.merge$SDD_PREDICT.y), digits = 2)
    
  }
}


## Export df as CSV 
write.csv(df, paste0(main.dir, "Output_Data/SDD_Timeseries/Cross-Sensor_comparison.csv"), row.names = F)

