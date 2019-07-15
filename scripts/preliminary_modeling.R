#Basic SDM modeling for monarchs
#Keaton Wilson
#keatonwilson@me.com
#2019-07-09

#packages
library(tidyverse)
library(SSDM)
library(raster)
library(lubridate)

#Environmental Variables
bioclim.data <- raster::getData(name = "worldclim",
                                var = "bio",
                                res = 2.5,
                                path = "./data/")

data = read_csv("./data/mw_flowering_data.csv")

#Do we have enough data to build a month-by-month SDM?
data %>%
  mutate(month = month(DateIdentified)) %>%
  group_by(month) %>%
  summarize(n = n())

#Some NAs, let's get rid of this
data = data %>%
  mutate(month = month(DateIdentified)) %>%
  filter(!is.na(month))

#Trimming bioclim data
max_lat = max(data$latitude)
min_lat = min(data$latitude)
max_lon = max(data$longitude)
min_lon = min(data$longitude)
geographic_extent = extent(c(min_lon + 10, max_lon + 10, min_lat + 10, max_lat))

bioclim.data = crop(bioclim.data, geographic_extent)

#Modeling
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)


#First model - let's iterate by month, but treat all species as one species
#Turning the data into a list to feed into the for loop below
data_list = data %>% 
  group_by(month) %>%
  do(vals = data.frame(.)) %>%
  dplyr::select(vals) %>%
  lapply(function(x) {(x)})

mod_list = list()

for (x in 1:length(data_list$vals)) {
mod_list[x] = modelling(algorithm = 'MAXENT', Occurrences = as.data.frame(data_list$vals[[x]]),
                Env = bioclim.data,
                cv = 'k-fold',
                cv.param = c(5,5),
                Xcol = 'longitude', 
                Ycol = 'latitude')
}
