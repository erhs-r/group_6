library(tidyverse)
library(sp)
library(tigris)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)
library(maps)
library(ggmap)

#Import our datasets----
#policy <- read.csv("raw_data/state_policy_updates_20201122_0721.csv")
#covid_cases <- read.csv("raw_data/covid_outbreak.csv") # no geometry here
#covid_deaths <- read.csv("raw_data/covid_deaths.csv") #city avail here

#working with ses data for this spatial test
#pull in dataset
#it seems to be pretty tidy already
ses <- read.csv("raw_data/Income_Poverty__Census_Tracts_.csv")

#are FIPS and Tract_FIPS the same? YES
compare_fips <- ses %>%
  select(FIPS, Tract_FIPS) %>%
  mutate(compare = ifelse(as.numeric(FIPS) == as.numeric(Tract_FIPS), TRUE, FALSE))

#subset Larimer county from other CO counties?
larimer_ses <- ses %>%
  select(Tract_FIPS:Poverty_Per_Capita_Income) %>%
  filter(County == "LARIMER") 

#NOT SURE How to do this?
#trying to create geometry for our data by combining larimer data from 
#tigris census by fips to get lat lon
larimer_pov <- larimer_ses %>%
  mutate(fips= Tract_FIPS)


# example using tigris census to create maps with fips codes----
co_counties <- counties(state= "CO", cb = TRUE, class = "sf")
class(co_counties)

ggplot() +
  geom_sf(data = co_counties, aes(fill = ALAND)) +
  ggtitle("Land acres of Colorado conties") +
  theme_dark() +
  scale_fill_viridis("Land area")

larimer <- co_counties %>%
  filter(NAME == "Larimer")

ggplot() +
  geom_sf(data=co_counties, color = "lightgray") +
  geom_sf(data=larimer, fill = "darkcyan") +
  geom_sf_text(data=larimer, aes(label = NAME), color = "white") +
  theme_dark() + labs(x= "", y="")


