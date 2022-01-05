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
# path_tmp <- file.path(here(), "data", "Jan_4,_2022_6_16_22_PM.gpx")
path_tmp <- file.path(here(), "data", "Jan_4,_2022_10_40_30_PM.gpx")
# Parse the GPX file
pfile <- htmlTreeParse(file = path_tmp, useInternalNodes = TRUE)

# Get all elevations, times and coordinates via the respective xpath
elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)

# combine into df 
geodf <- data.frame(
  lat =  as.numeric(coords["lat",]), 
  lon = as.numeric(coords["lon",]), 
  ele = elevations, 
  t_vec = times)
head(geodf)


