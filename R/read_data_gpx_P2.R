#' @description 
#' Script to test reading gpx data in R

rm(list = ls())
library(tidyverse)
library(here)
# library(plotKML)
# https://rpubs.com/ials2un/gpx1
library(XML)
library(OpenStreetMap)
library(lubridate)
library(ggmap)
library(raster)
library(sp)
library(geosphere)
here()

options(digits = 10)


# ------------------------------------------------------------------------------
# PART 2 
# ------------------------------------------------------------------------------


# reading in the GPX file
path_tmp <- file.path(here(), "data", "Jan_4,_2022_6_16_22_PM.gpx")
# Parse the GPX file
pfile <- htmlTreeParse(file = path_tmp, useInternalNodes = TRUE)

# Get all elevations, times and coordinates via the respective xpath
elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)





/Users/martakaras/Dropbox/_PROJECTS/gps-stats/data/Jan_4,_2022_6_16_22_PM.gpx

# combine into df 
geodf <- data.frame(
  lat =  as.numeric(coords["lat",]), 
  lon = as.numeric(coords["lon",]), 
  ele = elevations, 
  t_vec = times)
rm(list=c("elevations", "pfile", "times", "coords"))

# compute distance (in meters) between subsequent GPS points
geodf <- geodf %>%
  mutate(lat_lead = lead(lat)) %>%
  mutate(lon_lead = lead(lon)) %>%
  rowwise() %>%
  # distm documentation: first one is longitude, second is latitude
  mutate(dist_lead_m = distm(c(lon, lat), c(lon_lead, lat_lead), fun = distHaversine)[1,1]) %>%
  ungroup()

# compute time elapsed (in seconds) between subsequent GPS points
geodf <- geodf %>%
  mutate(t_vec_f = strptime(t_vec, format = "%Y-%m-%dT%H:%M:%OS")) %>%
  mutate(t_vec_f_lead = lead(t_vec_f)) %>%
  mutate(time_diff_lead_s = as.numeric(difftime(t_vec_f_lead, t_vec_f, units = "secs"))) 

# calculate metres per seconds, kilometres per hour 
geodf <- geodf %>%
  mutate(speed.m.per.sec = dist_lead_m / time_diff_lead_s) %>%
  mutate(speed.km.per.h = speed.m.per.sec * 3.6) %>%
  mutate(speed.km.per.h = ifelse(is.na(speed.km.per.h), 0, speed.km.per.h))

head(as.data.frame(geodf))

# compute lowess smoothers 
geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y



## PLOTS

# Plot elevations and smoother
plot(geodf$ele, type = "l", bty = "n", xaxt = "n", ylab = "Elevation", xlab = "", col = "grey40")
lines(geodf$lowess.ele, col = "red", lwd = 3)
legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "red"), lwd = c(1,3), bty = "n")

# Plot speeds and smoother
plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "",
     col = "grey40")
lines(geodf$lowess.speed, col = "blue", lwd = 3)
legend(x="bottom", legend = c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
abline(h = mean(geodf$speed.km.per.h), lty = 2, col = "blue")

# Plot the track without any map, the shape of the track is already visible.
plot(rev(geodf$lon), rev(geodf$lat), type = "l", col = "red", lwd = 3, bty = "n", 
     ylab = "Latitude", xlab = "Longitude")


library(ggmap)
lat <- c(min(geodf$lat), max(geodf$lat))
lat
lon <- c(min(geodf$lon), max(geodf$lon))
lon
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")

ggmap(b1) + 
  geom_point(data = geodf, aes(lon,lat,col = ele),
             size=1, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude", 
       title="Track of one Sascha run through Stuttgart")

