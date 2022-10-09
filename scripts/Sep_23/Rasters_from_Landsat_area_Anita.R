# by Anita Giraldo 5 April 2022
# last modified by Anita Giraldo 5 april 2022

# This script makes rasters of mean kelp biomass for each year of Landsat data 
# from a .csv file


# libraries ----
library(here)
library(dplyr)
library(stringr)
library(car)
library(Amelia)
library(sp)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(rgeos)
library(spdep)
library(tidyr)
library(ggplot2)
library(viridis)
library(beepr)



## Rasters of kelp area for each year ----
# To do this, I aggregated from 30m cells to ~200m (loop can change this)
# so the final raster stack would no be so huge

# NORTH COAST mean kelp ----

# Clear environment ----
rm(list = ls())


# Directories ----
w.dir <- here()
d.dir <- here('data')
r.dir <- here('spatial_data/rasters')
d2.dir <- "G:/Shared drives/California Kelp Restoration Project - Seagrant/R_Projects/Kelp_Landsat/data"

o.dir <- "G:/Shared drives/Anita's Drive UCSB/SURE_Project/Spatial_data/Landsat_rasters"

# load csv ----

df <- read.csv(paste(d2.dir, "NC_Landsat_kelp_area_1984_2021.csv", sep ='/')) %>%
  glimpse()


## make factors ---
df2 <- df %>%
  mutate(latlon = paste(lat, lon, sep = '_')) %>%
  mutate_at(vars(year, quarter, latlon), list(as.factor)) %>%
  glimpse()

# 1. Calculate annual summaries---- adapt > make sure you only use summer (Q3)

df3 <- df2 %>%
  # get only summer (Q3)
  filter(quarter == 3) %>%
  # get the average kelp for each year 
  # group_by(year, latlon) %>%
  # summarise(mean_area = mean(area, na.rm = TRUE),
  #           max_area = max(area, na.rm = TRUE),
  #           sd_area = sd(area, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(latitude = sub("_.*", "", latlon)) %>% # Extract characters before pattern _
  mutate(longitude = sub(".*_", "", latlon)) %>% # Extract characters after pattern _
  mutate_at(vars(latitude, longitude), list(as.numeric)) %>%
  glimpse() # Rows: 1,437,084


## vector of years --
years <- paste(1984:2021)
length(years) # 38

# load blank raster ----
blank <- raster(paste(r.dir, "NC_blank.tif", sep ='/'))
extent(blank)

# choose cell size fot aggregate factor --
agg.fact <- 200/30 # for cell size ~ 200m
res <- "30m"
# agg.fact <- 500/30 # for cell size ~ 500m

# make empty raster stack to save --
k.years <- stack()

# summary stat being calulated --
var <- "mean_kelp_area"

coast <- "NC"

# 2. Loop to get raster and aggregate ----

# get year 1984
kelp.year <- df3 %>%
  dplyr::filter(year == 1984) %>%
  droplevels() %>%
  glimpse()

# make spatial points
sp.year <- kelp.year
coordinates(sp.year) <- ~longitude + latitude
proj4string(sp.year) <- "+proj=longlat +datum=WGS84 +no_defs"

# make new blank
blank.year <- blank
blank.year

# rasterize the mean kelp year
raster.year <- rasterize(sp.year, blank.year, field = "area", fun = mean, update = TRUE)


######

  
    
for(i in 1:length(1:length(years))) {
  
  # get year 
  year.x <- years[i]
  kelp.year <- df3 %>%
    dplyr::filter(year == print(year.x)) %>%
    droplevels() %>%
    glimpse()
  
  # make spatial points
  sp.year <- kelp.year
  coordinates(sp.year) <- ~longitude + latitude
  proj4string(sp.year) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  # make new blank
  blank.year <- blank
  blank.year
  
  # rasterize the mean kelp year  
  raster.year <- rasterize(sp.year, blank.year, field = "area", fun = mean, update = TRUE)
  
  # name raster layer with year
  name.r <- paste(var, year.x, sep = '_')
  names(raster.year) <- name.r
  
  # aggregate raster
  # sum for the total kelp area in the area of the cell
  #raster.year.agg <- raster::aggregate(raster.year, fact = agg.fact, fun = sum) 
  
  # add to raster stack
  #k.years <- stack(k.years, raster.year)
  
  file.name <- paste(coast, var, year.x, res, sep = '_')
  writeRaster(raster.year, paste(o.dir, paste(file.name, "tif", sep ='.'), sep = '/'), overwrite = TRUE)
  
}
