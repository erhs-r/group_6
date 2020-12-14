library(tidyverse)
library(tigris)
library(na.tools)
library(lubridate)
library(viridis)
library(leaflet)
library(readxl)
library(stringr)

LC_covid <- read_csv("raw_data/LC-COVID-casesdata.csv")

### Make all city names in title format to join data sets

LC_covid <- LC_covid %>% 
  mutate(City = str_to_title(City))

### Count the total number of COVID cases per city

total_covid_per_city <- LC_covid %>% 
  group_by(City) %>% 
  count()

### Create vectors of gps coordinates of cities from information collected from
### the internet

Lat <- c(40.625679, 40.284667, 40.431269, 40.377117, 40.559167, 
         40.453740, 40.336944, 40.633808, 40.794319, 40.404789, 40.487171,
         40.807820, 40.529718, 40.702324, 40.477222)

Lon <- c( -105.171089, -104.965504, -105.339661, -105.525514,
          -105.078056, -105.448837, -104.912222, -105.148819, -105.216579,
          -105.085868, -105.210056,-105.578641, -104.981654, 
          -105.005497, -104.911944)

City <- c("Bellvue", "Berthoud", "Drake", "Estes Park",
          "Fort Collins", "Glen Haven", "Johnstown", "Laporte", "Livermore",
          "Loveland", "Masonville", "Red Feather Lakes", "Timnath", 
          "Wellington", "Windsor")

larimer_pop <- c(1708, 9094, 1135, 6426, 170243, 126, 15198, 2450, 2360, 78877,
                 0, 443, 4998, 10437, 30477)

### Combine the vectors into a data frame and then join the data sets so cities
### have coordinates associated with them for mapping and remove NA values

larimer_gps <- data.frame(City, Lat, Lon, larimer_pop)

class(LC_covid_gps$ReportedDate)

total_covid <- full_join(total_covid_per_city, larimer_gps, by = "City") %>% 
  na.rm()

LC_covid_gps <- full_join(total_covid, LC_covid, by = "City") %>% 
  na.rm()

LC_covid_gps <- LC_covid_gps %>% 
  mutate(ReportedDate = mdy(ReportedDate)) 

city_count <- LC_covid_gps %>% 
  group_by(City, ReportedDate) %>% 
  count(name = "num_of_cases")

city_count <- full_join(city_count, LC_covid_gps, 
                        by = (list(x = c("City", "ReportedDate"),
                                   y = c("City", "ReportedDate"))))

total_city_count <- city_count %>% 
  group_by(City) %>% 
  count()

total_city_count <- city_count %>% 
  group_by(City) %>% 
  summarize(total_count = n(),
            mean_age = mean(Age, na.rm = TRUE),
            .groups = "drop")

city_count %>% 
  group_by(City, ReportedDate) %>% 
  ggplot(aes(x = ReportedDate, 
             y = num_of_cases, 
             color = larimer_pop)) +
  geom_point(alpha = 0.5) +
  labs(x = "Month of test", y = "Number of postive individuals") +
  ggtitle("Positive COVID-19 cases") +
  scale_color_viridis(name = "Total population", option = "viridis") +
  facet_wrap(~City) +
  theme_bw() +
  ggsave("positive_cases.png", 
         device = "png", 
         width = 10, 
         height = 10,
         limitsize = FALSE)

library(tidyverse)
library(tigris)
library(na.tools)
library(lubridate)
library(viridis)
library(leaflet)
library(readxl)
library(stringr)
library(knitr)
library(kableExtra)

LC_covid <- read_csv("raw_data/LC-COVID-casesdata.csv")
covid_deaths <- read_csv("raw_data/covid_deaths.csv")

### Make all city names in title format to join data sets

LC_covid <- LC_covid %>% 
  mutate(City = str_to_title(City))

LC_covid_count <- LC_covid %>% 
  group_by(ReportedDate) %>% count()

total_covid_per_city <- LC_covid %>% 
  group_by(City) %>% 
  count()

### Create vectors of gps coordinates of cities from information collected from
### the internet

Lat <- c(40.625679, 40.284667, 40.431269, 40.377117, 40.559167, 
         40.453740, 40.336944, 40.633808, 40.794319, 40.404789, 40.487171,
         40.807820, 40.529718, 40.702324, 40.477222)

Lon <- c( -105.171089, -104.965504, -105.339661, -105.525514,
          -105.078056, -105.448837, -104.912222, -105.148819, -105.216579,
          -105.085868, -105.210056,-105.578641, -104.981654, 
          -105.005497, -104.911944)

City <- c("Bellvue", "Berthoud", "Drake", "Estes Park",
          "Fort Collins", "Glen Haven", "Johnstown", "Laporte", "Livermore",
          "Loveland", "Masonville", "Red Feather Lakes", "Timnath", 
          "Wellington", "Windsor")


### Combine the vectors into a data frame and then join the data sets so cities
### have coordinates associated with them for mapping and remove NA values

larimer_gps <- data.frame(City, Lat, Lon)

total_covid <- full_join(total_covid_per_city, larimer_gps, by = "City") %>% 
  na.rm()

LC_covid_gps <- full_join(larimer_gps, LC_covid, by = "City") %>% 
  na.rm()

LC_covid_gps <- LC_covid_gps %>% 
  mutate(ReportedDate = mdy(ReportedDate)) 

city_count <- LC_covid_gps %>% 
  group_by(City, ReportedDate) %>% 
  count(name = "num_of_cases")

total_city_count <- city_count %>% 
  group_by(City) %>% 
  summarize(total_count = n(),
            mean_age = mean(Age, na.rm = TRUE),
            .groups = "drop")

Lat <- c(40.625679, 40.284667, 40.431269, 40.377117, 40.559167, 
         40.453740, 40.336944, 40.633808, 40.794319, 40.404789, 40.487171,
         40.807820, 40.529718, 40.702324, 40.477222)

Lon <- c( -105.171089, -104.965504, -105.339661, -105.525514,
          -105.078056, -105.448837, -104.912222, -105.148819, -105.216579,
          -105.085868, -105.210056,-105.578641, -104.981654, 
          -105.005497, -104.911944)

City <- c("Bellvue", "Berthoud", "Drake", "Estes Park",
          "Fort Collins", "Glen Haven", "Johnstown", "Laporte", "Livermore",
          "Loveland", "Masonville", "Red Feather Lakes", "Timnath", 
          "Wellington", "Windsor")

larimer_gps <- data.frame(City, Lat, Lon)

covid_deaths <- full_join(larimer_gps, covid_deaths, 
                          by = list(x = "City", y = "city")) %>% na.rm()

covid_deaths_total <- covid_deaths %>% 
  group_by(City) %>% 
  count()

mean_age_deaths <- covid_deaths %>% 
  group_by(City) %>% 
  summarize(mean_age_death = mean(age), .groups = "drop")

mean_age_deaths <- full_join(mean_age_deaths, covid_deaths_total, by = "City")

covid_table <- full_join(total_city_count, mean_age_deaths, by = "City")

kable(covid_table) 




