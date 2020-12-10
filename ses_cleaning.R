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

#example from Bree
larimer_tracts <- tracts(state = 08, county =069, 
                         cb = TRUE, class = "sf")

larimer_income <- ses %>%
  filter(County == "LARIMER")

larimer_income$FIPS %in% larimer_tracts$GEOID # All 73 in tracts

larimer_info <- full_join(larimer_income, larimer_tracts, 
                          by = list(x = "FIPS", y = "GEOID"))


library(viridis)
library(scales)
#plot larimer cities 

larimer_short <- larimer_info %>%
  select(Population_Total:Poverty_Per_Capita_Income, TRACTCE)

larimer_tracts %>% left_join(.,larimer_short, by = "TRACTCE") %>%
  ggplot() +
  geom_sf(aes(fill = Population_Total)) +
  geom_point(data = larimer_gps %>%
               filter(City!="Cedaredge"), aes(x=Lon, y=Lat)) +
  geom_text_repel(data = larimer_gps %>%
              filter(City!="Cedaredge"), 
            aes(x=Lon, y=Lat, 
                label = City), color = "white",
            show.legend=NA, 
            nudge_x = .1, nudge_y = .1,
            angle = 25) +
  ggtitle("Population Totals for Larimer County")

  
ggplot() +
  geom_sf(data = larimer_tracts) 

install.packages("OpenStreetMaps")
library(OpenStreetMap)

head(larimer)



