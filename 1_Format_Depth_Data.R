

## Format depth data for Cape Cod using information compiled by the Cape Cod Commission (CCC)


## Code accompanying "Satellite imagery as a management tool for monitoring water clarity across freshwater 
## ponds in Cape Cod, Massachusetts" (Coffer et al., 2024, Journal of Environmental Management). 
## R code written by first author Megan Coffer.


## Set main working directory 
main.dir <- "..."


## Pond depth, retrieved from: https://gis-cccommission.opendata.arcgis.com/datasets/CCCommission::ponds/about
depth.csv.all <- read.csv(paste0(main.dir, "Input_Data/Pond_Depth/CCC_PondDepths_06June2023.csv"))
## Add additional pond depth data from CCC
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "FA-948")] <- 5.25
## Add pond depth data from the Orleans Pond Coalition (note, some of this overrides the PALS data)
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "OR-253")] <- 6.07
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "OR-262")] <- 20.01
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "OR-256")] <- 5.74
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "OR-174")] <- 15.09
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "OR-264")] <- 10.83
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "OR-247")] <- 12.80 
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "OR-142")] <- 18.04
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "OR-136")] <- 18.04 
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "OR-249")] <- 16.90 
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "OR-176")] <- 30.84 
## Add pond depth data from 2003 Cape Cod Pond and Lake Atlas 
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "BA-756")] <- 4
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "BA-594")] <- 6
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "BA-411")] <- 16
## Add pond depth data from Portnoy et al. (2001)
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "WE-66")] <- 13 
## Add pond depth data from Cape Cod Nautical Map
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "BA-914")] <- 6
## One pond is included in this dataset but isn't a hectare according to the CCC shapefile and needs to be excluded 
depth.csv.all$Maximum_Depth__ft_[which(depth.csv.all$CCC_GIS_ID == "BO-495")] <- NA


## Subset to ponds with depth data 
depth.csv <- subset(depth.csv.all, !is.na(Maximum_Depth__ft_))
## Subset to ponds that are at least a hectare in size 
depth <- subset(depth.csv, Acres >= 2.4710538146717)
## Convert depth from feet to meters 
depth$Maximum_Depth__m_ <- depth$Maximum_Depth__ft_ * 0.3048000


## Export depth data as CSV file 
write.csv(depth, paste0(main.dir, "Input_Data/Pond_Depth/Pond_Depth_formatted.csv"), row.names = F)

