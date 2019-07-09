#Basic SDM modeling for monarchs
#Keaton Wilson
#keatonwilson@me.com
#2019-07-09

#packages
library(tidyverse)
library(SSDM)
library(raster)

#Environmental Variables
bioclim.data <- raster::getData(name = "worldclim",
                                var = "bio",
                                res = 2.5,
                                path = "./data/")

data = read_csv("./data/mw_flowering_data.csv")

#Trimming bioclim data
max_lat = max(data$latitude)
min_lat = min(data$latitude)
max_lon = max(data$longitude)
min_lon = min(data$longitude)
geographic_extent = extent(c(min_lon + 10, max_lon + 10, min_lat + 10, max_lat))

bioclim.data = crop(bioclim.data, geographic_extent)

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)

mod = stack_modelling(algorithms = 'GLM', Occurrences = as.data.frame(data),
                Env = bioclim.data, 
                Xcol = 'longitude', 
                Ycol = 'latitude', 
                Spcol = 'name')
