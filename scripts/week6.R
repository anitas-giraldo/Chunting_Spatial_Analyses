# load libraries
library(sp)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(pander)
library(magrittr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(here)

# compare spatial predictions of kelp to 'in situ' survey data
# compare each year and location

# set a directory
w.dir <- here()
d.dir <- here('data')
r.dir <- here('spatial_data/sp_predictions_5.1.1_V2')

# read and transform the observed data to the log scale
df <- read.csv(paste(d.dir, 
                     'RCCA_kelp_inverts_NC_depth-zones_wave_clim_temp_nit_subs_orbvel_npp.csv', 
                     sep = '/')) %>%
  dplyr::select(site_name, year, transect, zone, latitude, longitude, den_NERLUE) %>%
  mutate_at(vars(year, transect, zone, site_name), list(as.factor)) %>%
  mutate(log_den_NERLUE = log(den_NERLUE)) 

filter(df1, den_NERLUE == 0) %>% count() # 719 0's

# log(0), -inf, 0, NA
df$log_den_NERLUE <- replace(df$log_den_NERLUE, df$log_den_NERLUE == -Inf, 0)
head(df)

df2 <- df %>% 
  group_by(site_name, year, zone) %>%
  summarise_at(vars(log_den_NERLUE), list(log_den_NERLUE = mean), na.rm = TRUE)
head(df2)

# read the .csv file
site <- read.csv(paste(d.dir, 
                       'RCCA_North_Coast_sites.csv', 
                       sep = '/')) 
# convert from .csv to .shp
site_shp <- st_as_sf(csv, coords = c('longitude', 'latitude'), crs = 'EPSG:4326')
class(site_shp)
# write the file
st_write(site_shp, paste0(d.dir, '/RCCA_North_Coast_sites.shp'), append = FALSE)

# extract the predicted log kelps density to every year (2004-2021) for each site
# in the North Coast

# 2006
rast_2006 <- rast(paste0(r.dir, '/2006_Nereo_preds_NC_V4_5.1.1_V2.tif'))
terra::extract(rast_2006, vect(site_shp$geometry)) %>% 
  mutate(site_name = site$site_name, .before = fit)
