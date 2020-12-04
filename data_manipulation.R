library(tidyverse)
library(tigris)

pos_pcr <- read_csv("raw_data/pcr-positive-by-day.csv")
state_policy <- read_csv("raw_data/state_policy_updates_20201122_0721.csv")
LC_covid <- read_csv("raw_data/LC-COVID-casesdata.csv")
covid_deaths <- read_csv("raw_data/covid_deaths.csv")
income <- read_csv("raw_data/Income_Poverty__Census_Tracts_.csv")

# https://geocompr.robinlovelace.net/spatial-operations.html
# https://journal.r-project.org/archive/2016/RJ-2016-043/RJ-2016-043.pdf

larimer_tracts <- tracts(state = 08, county =069, 
                        cb = TRUE, class = "sf")

larimer_income <- income %>%
  filter(County == "LARIMER")

larimer_income$FIPS %in% larimer_tracts$GEOID # All 73 in tracts


  






