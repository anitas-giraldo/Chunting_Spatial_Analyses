
library(here)
library(dplyr)


w.dir <- here()
d.dir <- here('data')
dir(d.dir)

df <- read.csv(paste(d.dir, "RCCA_kelp_inverts_NC_depth-zones_wave_clim_temp_nit_subs_orbvel_npp.csv", sep ='/')) %>%
  glimpse()

df2 <- df %>%
  dplyr::select(site_name, year, transect, zone, latitude, longitude, den_NERLUE) %>%
  mutate_at(vars(year, transect, zone, site_name), list(as.factor)) %>%
  glimpse()

levels(df2$year)
levels(df2$site_name)
