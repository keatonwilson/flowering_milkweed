#Initial data exploration from milkweed mapper data
#Keaton Wilson
#keatonwilson@me.com
#2019-07-08

#libraries
library(tidyverse)
library(mapr)
library(ggmap)
library(readxl)
library(lubridate)

register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")
#Reading in the data (data downloaded on 2019-07-08)
mwm = read_csv("./data/wmmm_20190708_193105_875693.csv")

glimpse(mwm)

#paring it down
mwm = mwm %>%
  select(Genus, SpeciesSubSpecies, 
         latitude = Latitude, longitude = Longitude, 
         HabitatType, 
         State, County, RecordYear, DateIdentified)

mwm = mwm %>%
  mutate(name = paste(Genus, SpeciesSubSpecies, sep = " "))

#Lots of different species - let's pare it down
mwm_few = mwm %>%
  group_by(name) %>%
  summarize(n = n()) %>%
  filter(n < 5) %>%
  print(n = 30) 

#Lots of species with just one record of flowering - not super helpful. 
'%!in%' <- function(x,y)!('%in%'(x,y))
blacklist = mwm_few$name

mwm_sub = mwm %>%
  filter(name %!in% blacklist)

mwm_sub = mwm_sub %>%
  mutate(quarter = lubridate::quarter(DateIdentified))

#faceted map by species
base_map = get_map(location = "Nevada", zoom = 5)
ggmap(base_map) +
  geom_point(data = mwm_sub %>%
               filter(!is.na(quarter)), aes(x = longitude, 
                                            y = latitude, 
                                            color = name), 
             size = 2, 
             alpha = 0.7) +
  facet_wrap(~ quarter)

#Generating summary data frame to plot medians
summary_data = mwm_sub %>%
  group_by(name) %>%
  summarize(mean_month = mean(month(DateIdentified), na.rm = TRUE))

#median plot
mwm_sub %>%
  ggplot(aes(x = month(DateIdentified), color = name)) +
  geom_vline(data = summary_data, aes(xintercept = mean_month), lty = 2) +
  geom_density() +
  facet_wrap(~ name) +
  theme_classic()

#Categorizing as early, late or mid
sum_mwm_data = summary_data %>%
  mutate(flowering_cat = ifelse(mean_month <= 6.5, "early", 
                                ifelse(mean_month >= 7.5, "late", "mid"))) %>%
  left_join(mwm_sub, by = "name")

sum_mwm_data %>%
  group_by(flowering_cat, name) %>%
  summarize(n = n())

write_csv(sum_mwm_data, "./data/mw_flowering_data.csv")
