library(tidyverse)
library(tigris)
library(na.tools)

state_policy <- read_csv("raw_data/state_policy_updates_20201122_0721.csv")
LC_covid <- read_csv("raw_data/LC-COVID-casesdata.csv")
covid_deaths <- read_csv("raw_data/covid_deaths.csv")
income <- read_csv("raw_data/Income_Poverty__Census_Tracts_.csv")

# https://geocompr.robinlovelace.net/spatial-operations.html
# https://journal.r-project.org/archive/2016/RJ-2016-043/RJ-2016-043.pdf

### Pull census data for Larimer county 

larimer_tracts <- tracts(state = 08, county =069, 
                        cb = TRUE, class = "sf")

### Filter the income data to just larimer county

larimer_income <- income %>%
  filter(County == "LARIMER")

### Verified that FIPS and GEOID match in both data sets

larimer_income$FIPS %in% larimer_tracts$GEOID # All 73 in tracts

### Join the data sets 

larimer_info <- full_join(larimer_income, larimer_tracts, 
                          by = list(x = "FIPS", y = "GEOID"))

### Make all city names in title format to join data sets

LC_covid <- LC_covid %>% 
  mutate(City = str_to_title(City))

### Create vectors of gps coordinates of cities from information collected from
### the internet

Lat <- c(40.559167, 40.404789, 40.284667, 40.377117, 40.336944,40.807820, 
         40.529718, 40.702324, 40.477222, 40.633808, 40.625679, 40.431269,
         40.453740, 40.794319, 40.487171, 38.894137)
Lon <- c(-105.078056, -105.085868, -104.965504, -105.525514, -104.912222,
         -105.578641, -104.981654, -105.005497, -104.911944, -105.148819,
         -105.171089, -105.339661, -105.448837, -105.216579, -105.210056,
         -107.925498)

City <- c("Fort Collins", "Loveland", "Berthoud", "Estes Park", "Johnstown",
            "Red Feather Lakes", "Timnath", "Wellington", "Windsor", "Laporte", 
            "Bellvue", "Drake", "Glen Haven", "Livermore", "Masonville", 
            "Cedaredge")

### Combine the vectors into a data frame and then join the data sets so cities
### have coordinates associated with them for mapping and remove NA values

larimer_gps <- data.frame(City, Lat, Lon)

LC_covid_gps <- full_join(larimer_gps, LC_covid, by = "City") %>% na.rm()

#FC: 40.559167, -105.078056, GEOID:0806991330
#Loveland: 40.404789, −105.085868, GEOID: 0806992337
#Berthoud: 40.284667, -104.965504, GEOID: 0806990285
#Estes Park: 40.377117, -105.525514, GEOID:0806991235
#Johnstown: 40.336944, -104.912222, GEOID: 0812391881
#Red Feather: 40.807820, -105.578641, GEOID:
#Timnath: 40.529718, -104.981654, GEOID: 0806993610
#Wellington: 40.702324, -105.005497, GEOID: 0806993610
#Windsor: 40.477222, -104.911944, GEOID: 0812393895
#Laporte: 40.633808, -105.148819, GEOID:
#Bellvue:40.625679,-105.171089, GEOID:
#Drake: 40.431269, -105.339661, GEOID:
#Glen Haven: 40.453740,-105.448837, GEOID:
#Livermore: 40.794319,-105.216579, GEOID: 0806992299
#Masonville: 40.487171,-105.210056, GEOID:
#Cedaredge: 38.894137, -107.925498, GEOID: 





