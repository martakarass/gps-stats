#' @description 
#' Script to test reading gpx data in R

rm(list = ls())
library(tidyverse)
library(here)
# library(plotKML)
# https://rpubs.com/ials2un/gpx1
library(XML)
# library(OpenStreetMap)
library(lubridate)
library(ggmap)
# library(raster)
# library(sp)
library(geosphere)
here()

options(digits = 10)


# ------------------------------------------------------------------------------
# PART 1 -- Based on https://rpubs.com/ials2un/gpx1
# ------------------------------------------------------------------------------

# parse GPX file
path_tmp <- file.path(here(), "data", "Morning_Run_2022-01-01.gpx")
parsed <- htmlTreeParse(file = path_tmp, useInternalNodes = T)

# get values via via the respective xpath
coords <- xpathSApply(parsed, path = "//trkpt", xmlAttrs)
elev   <- xpathSApply(parsed, path = "//trkpt/ele", xmlValue)
ts_chr <- xpathSApply(parsed, path = "//trkpt/time", xmlValue)

# combine into df 
dat_df <- data.frame(
  ts_chr = ts_chr,
  lat = as.numeric(coords["lat",]), 
  lon = as.numeric(coords["lon",]), 
  elev = as.numeric(elev)
)

# compute distance (in meters) between subsequent GPS points
# distm documentation: first one is longitude, second is latitude
dat_df <- 
  dat_df %>%
  # compute distance (in meters) between subsequent GPS points
  mutate(lat_lead = lead(lat)) %>%
  mutate(lon_lead = lead(lon)) %>%
  rowwise() %>%
  mutate(dist_lead_m = distm(c(lon, lat), c(lon_lead, lat_lead), fun = distHaversine)[1,1]) %>%
  ungroup()

# compute time elapsed (in seconds) between subsequent GPS points
dat_df <- 
  dat_df %>%
  mutate(ts_POSIXct = ymd_hms(ts_chr, tz = "EST")) %>% # define time as POSIXct
  mutate(ts_POSIXct_lead = lead(ts_POSIXct)) %>%
  mutate(ts_diff_s = as.numeric(difftime(ts_POSIXct_lead, ts_POSIXct, units = "secs"))) 

# compute metres per seconds, kilometres per hour 
dat_df <- 
  dat_df %>%
  mutate(speed_m_per_sec = dist_lead_m / ts_diff_s) %>%
  mutate(speed_km_per_h = speed_m_per_sec * 3.6)

# -----------------------------------------------------------------
# PLOT 

ggplot(dat_df, aes(x = ts_POSIXct, y = elev)) + 
  geom_line() + labs(x = "Time", y = "Elevation [m]") + 
  theme_minimal(base_size = 10)

ggplot(dat_df, aes(x = ts_POSIXct, y = speed_km_per_h)) + 
  geom_line() + labs(x = "Time", y = "Speed [km/h]") + 
  theme_minimal(base_size = 10)


dat_df_agg <- 
  dat_df %>%
  mutate(ts_POSIXct_floor = floor_date(ts_POSIXct, "5 seconds")) %>%
  group_by(ts_POSIXct_floor) %>%
  summarise(ts_diff_s = sum(ts_diff_s),
            dist_lead_m = sum(dist_lead_m)) %>%
  ungroup() %>%
  mutate(speed_m_per_sec = dist_lead_m / ts_diff_s) %>%
  mutate(speed_km_per_h = speed_m_per_sec * 3.6)

ggplot(dat_df_agg, aes(x = ts_POSIXct_floor, y = speed_km_per_h)) + 
  geom_line() + labs(x = "Time", y = "Speed [km/h]") + 
  theme_minimal(base_size = 10)


# https://cran.r-project.org/web/packages/ggmap/readme/README.html

# Plot the track without any map, the shape of the track is already visible.
plot(rev(dat_df$lon), rev(dat_df$lat), 
     type = "l", col = "red", lwd = 3, bty = "n", 
     ylab = "Latitude", xlab = "Longitude")

# plot ggplo2 
bbox <- make_bbox(range(dat_df$lon), range(dat_df$lat))
# dat_df_map <- get_map(bbox, maptype = "toner-lite", source = "stamen")
# dat_df_map <- get_googlemap(bbox)
dat_df_map <- get_map(bbox, maptype = "roadmap", source = "google")
dat_df_map <- get_googlemap(center = c(mean(range(dat_df$lon)), mean(range(dat_df$lat))), zoom = 15)

# add distance marks
dat_df_dist_marks <- 
  dat_df %>% 
  mutate(dist_m_cumsum = cumsum(dist_lead_m)) %>%
  mutate(dist_m_cumsum_km_floor = floor(dist_m_cumsum / 1000)) %>%
  group_by(dist_m_cumsum_km_floor) %>%
  filter(row_number() == 1, dist_m_cumsum_km_floor > 0) 

# Plot
ggmap(dat_df_map) + 
  geom_point(data = dat_df, aes(lon, lat, col = elev),
             size = 1, alpha=0.7) +
  geom_label(data = dat_df_dist_marks, aes(lon, lat, label = dist_m_cumsum_km_floor),
             size = 3) +
  labs(x = "Longitude", y = "Latitude", color = "Elev. [m]",
       title="Track of one Marta's run on 2022-01-01")



