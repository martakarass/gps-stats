#' @description 
#' Process JSON data from Google Fit application into CSV

rm(list = ls())
library(tidyverse)
library(here)
library(jsonlite)
library(nanotime)
library(stringr)
library(lubridate)

options(scipen = 100)
options(digits.secs = 3)

# open the project in RStudio via clicking <project name>.Rproj
# to have here() pointing to the project directory path 
here()

# read JSON from Google Fit dump
json_fname <- "derived_com.google.location.sample_com.google.(2).json"
json_fpath <- file.path(here(), "data_raw", json_fname)
json_dat <- fromJSON(json_fpath)
if (all(names(json_dat) == c("Data Source", "Data Points"))){
  json_dat <- json_dat[["Data Points"]]
}

# parse nanoseconds
timestamp_POSIXct <- as.POSIXct(as.nanotime(json_dat$startTimeNanos))
timestamp_POSIXct_int <- as.integer(timestamp_POSIXct)
timestamp_chr <- as.character(timestamp_POSIXct)
# check 
c(head(timestamp_POSIXct, 1), tail(timestamp_POSIXct, 1))
c(head(timestamp_chr, 1), tail(timestamp_chr, 1))

# convert JSON values to data frame 
# rename data frame columns (https://developers.google.com/fit/datatypes/location)
dat_l <- lapply(json_dat$fitValue, unlist)
dat_df <- as_tibble(do.call("rbind", dat_l))
names(dat_df) <- c("latitude", "longitude", "accuracy", "altitude")

# add timestamp info
dat_df <- dat_df %>%
  mutate(timestamp_POSIXct = timestamp_POSIXct, 
         timestamp_POSIXct_int = timestamp_POSIXct_int, 
         .before = everything()) %>%
  arrange(timestamp_POSIXct)

# average number of observations per day 
dat_df_sub <- 
  dat_df %>% 
  filter(as_date(timestamp_POSIXct) == as_date("2022-01-01"))

as_date(dat_df$timestamp_POSIXct[1]) == as_date("2022-01-01")

plt_df2 <- 
  dat_df %>% 
  mutate(day_date = as.Date(timestamp_POSIXct)) %>%
  group_by(day_date) %>%
  summarise(cnt = n())
ggplot(plt_df2, aes(x = day_date, y = cnt)) + geom_point()
summary(plt_df2$cnt)

# save whole to file 
rdata_fname <- "google_fit_sample.rdata"
rdata_fpath <- file.path(here(), "data_raw", rdata_fname)
saveRDS(dat_df, rdata_fname)

# save subset to file 
rdata_fname <- "google_fit_sample.rdata"
rdata_fpath <- file.path(here(), "data_raw", rdata_fname)
saveRDS(dat_df, rdata_fname)
