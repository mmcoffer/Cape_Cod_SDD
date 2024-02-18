

## Investigate year-over-year changes in SDD 


## Code accompanying "Satellite imagery as a management tool for monitoring water clarity across freshwater 
## ponds in Cape Cod, Massachusetts" (Coffer et al., 2024, Journal of Environmental Management). 
## R code written by first author Megan Coffer.


## Load required packages 
require(lubridate)
require(stringr)


## Set main working directory 
main.dir <- "..."
## Set years to be analyzed (this script is intended for recent time periods, not L5 and L7)
year1 <- "2021"
year2 <- "2022"


## Read in time series for recent sensors 
## Landsat 8 
if((as.numeric(year1) & as.numeric(year2) > 2012)){
  l8.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/L8/"), "*.csv$", full.names = T)
  l8 <- lapply(l8.list.files, read.csv)
  l8.ponds <- sapply(str_split(basename(l8.list.files), "_"), "[[", 1)
} else{
  l8 <- NA
  l8.ponds <- NA
}
## Landsat 9
if((as.numeric(year1) & as.numeric(year2) > 2021)){
  l9.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/L9/"), "*.csv$", full.names = T)
  l9 <- lapply(l9.list.files, read.csv)
  l9.ponds <- sapply(str_split(basename(l9.list.files), "_"), "[[", 1)
} else{
  l9 <- NA
  l9.ponds <- NA
}
## Sentinel-2
if((as.numeric(year1) & as.numeric(year2) > 2016)){
  s2.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/S2/"), "*.csv$", full.names = T)
  s2 <- lapply(s2.list.files, read.csv)
  s2.ponds <- sapply(str_split(basename(s2.list.files), "_"), "[[", 1)
} else{
  s2 <- NA
  s2.ponds <- NA
}
## Get unique ponds across sensors 
ponds <- unique(c(l8.ponds, l9.ponds, s2.ponds))


## Create an empty dataframe to populate with results 
df <- as.data.frame(matrix(nrow = length(ponds), ncol = 10))
colnames(df) <- c("CCC_GIS_ID","NAME","TOWN","N_YEAR1","MED_YEAR1","N_YEAR2","MED_YEAR2","W","PVALUE","CHANGE")
df$CCC_GIS_ID <- ponds


## Loop through each pond 
for(pond in ponds){
  
  ## Combine all sensors for the given pond 
  df.columns <- c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")
  s2.pond <- s2[[which(sapply(str_split(basename(s2.list.files), "_"), "[[", 1) == pond)]]
  if(!(pond %in% sapply(str_split(basename(l8.list.files), "_"), "[[", 1)) & pond %in% sapply(str_split(basename(l9.list.files), "_"), "[[", 1)){
    l9.pond <- l9[[which(sapply(str_split(basename(l9.list.files), "_"), "[[", 1) == pond)]]
    sat.pond <- rbind(l9.pond[df.columns], s2.pond[df.columns])
  } else if(pond %in% sapply(str_split(basename(l8.list.files), "_"), "[[", 1) & !(pond %in% sapply(str_split(basename(l9.list.files), "_"), "[[", 1))){
    l8.pond <- l8[[which(sapply(str_split(basename(l8.list.files), "_"), "[[", 1) == pond)]]
    sat.pond <- rbind(l8.pond[df.columns], s2.pond[df.columns])
  } else{
    l8.pond <- l8[[which(sapply(str_split(basename(l8.list.files), "_"), "[[", 1) == pond)]]
    l9.pond <- l9[[which(sapply(str_split(basename(l9.list.files), "_"), "[[", 1) == pond)]]
    sat.pond <- rbind(l8.pond[df.columns], l9.pond[df.columns], s2.pond[df.columns])
  }
  
  
  ## Subset to satellite data for the given pond 
  s2.pond <- s2[[which(sapply(str_split(basename(s2.list.files), "_"), "[[", 1) == pond)]]
  if(pond %in% sapply(str_split(basename(l8.list.files), "_"), "[[", 1)){
    l8.pond <- l8[[which(sapply(str_split(basename(l8.list.files), "_"), "[[", 1) == pond)]]
    sat.pond <- rbind(l8.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], s2.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")])
  }else{
    sat.pond <- s2.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")]
  }
  
  ## Format date column 
  sat.pond$day_str <- as.Date(sat.pond$day_str, "%Y-%m-%d")
  sat.pond$YEAR <- year(sat.pond$day_str)
  ## Populate dataframe with pond name and town  
  df$NAME[which(df$CCC_GIS_ID == pond)] <- unique(sat.pond$Name)
  df$TOWN[which(df$CCC_GIS_ID == pond)] <- unique(sat.pond$Town)
  df$N_YEAR1[which(df$CCC_GIS_ID == pond)] <- length(which(!(is.na(subset(sat.pond, YEAR == as.numeric(year1))$SDD_PREDICT))))
  df$MED_YEAR1[which(df$CCC_GIS_ID == pond)] <- median(subset(sat.pond, YEAR == as.numeric(year1))$SDD_PREDICT, na.rm = T)
  df$N_YEAR2[which(df$CCC_GIS_ID == pond)] <- length(which(!(is.na(subset(sat.pond, YEAR == as.numeric(year2))$SDD_PREDICT))))
  df$MED_YEAR2[which(df$CCC_GIS_ID == pond)] <- median(subset(sat.pond, YEAR == as.numeric(year2))$SDD_PREDICT, na.rm = T)
  
  ## Check that there are at least 5 observations per year following Mundry and Fischer (1998)
  if(df$N_YEAR1[which(df$CCC_GIS_ID == pond)] >= 5 & df$N_YEAR2[which(df$CCC_GIS_ID == pond)] >= 5){
    
    ## Check that there at least half the data aren't ties 
    all.sdd <- c(subset(sat.pond, YEAR == as.numeric(year1))$SDD_PREDICT, subset(sat.pond, YEAR == as.numeric(year2))$SDD_PREDICT)
    if(length(unique(all.sdd)) > (length(all.sdd) * 0.5)){
      
      ## Apply Mann Whitney U test
      mwu.pond <- wilcox.test(subset(sat.pond, YEAR == as.numeric(year1))$SDD_PREDICT, subset(sat.pond, YEAR == as.numeric(year2))$SDD_PREDICT, alternative = "two.sided", digits.rank = 7)
      df$W[which(df$CCC_GIS_ID == pond)] <- as.numeric(mwu.pond$statistic)
      df$PVALUE[which(df$CCC_GIS_ID == pond)] <- as.numeric(mwu.pond$p.value)
      df$CHANGE[which(df$CCC_GIS_ID == pond)] <- ifelse(df$MED_YEAR1[which(df$CCC_GIS_ID == pond)] > df$MED_YEAR2[which(df$CCC_GIS_ID == pond)] & df$PVALUE[which(df$CCC_GIS_ID == pond)] < 0.05, "Deteriorating", ifelse(df$MED_YEAR1[which(df$CCC_GIS_ID == pond)] < df$MED_YEAR2[which(df$CCC_GIS_ID == pond)] & df$PVALUE[which(df$CCC_GIS_ID == pond)] < 0.05, "Improving", "No change"))
      
    }
  }
}


## Output results as a CSV file 
write.csv(df, paste0(main.dir, "Output_Data/Output_Trend_Data/Short-term_change.csv"))


