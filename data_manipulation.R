library(tidyverse)
library(tigris)
library(na.tools)
library(lubridate)
library(viridis)
library(leaflet)

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

Lat <- c(40.559167, 40.404789, 40.284667, 40.377117, 40.336944, 40.807820, 
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

total_covid <- full_join(total_covid_per_city, larimer_gps, by = "City") %>% 
  na.rm()
  
LC_covid_gps <- full_join(larimer_gps, LC_covid, by = "City") %>% 
  na.rm()

LC_covid_gps <- LC_covid_gps %>% 
  mutate(ReportedDate = mdy(ReportedDate)) 

LC_covid_gps <- LC_covid_gps %>%
  mutate(popup_info = paste("<b>Location:</b>", City, "<br/>",
                            "<b>Sex:</b>", Sex, "<br/>",
                            "<b>Age:</b>", Age, "<br/>",
                            "<b>Date:</b>", ReportedDate, "<br/>",
                            "<b>Total cases reported:</b>", CaseCount))

### Provide covid deaths with coordinates and calculate mean age of death

covid_deaths <- full_join(larimer_gps, covid_deaths, 
                          by = list(x = "City", y = "city")) %>% na.rm()

covid_deaths_total <- covid_deaths %>% 
  group_by(City) %>% 
  count()

mean_age_deaths <- covid_deaths %>% 
  group_by(City) %>% 
  summarize(mean_age = mean(age), .groups = "drop")

mean_age_deaths <- full_join(mean_age_deaths, covid_deaths_total, by = "City")

mean_age_deaths <- full_join(larimer_gps, mean_age_deaths, by = "City") %>% 
  na.rm() %>% 
  mutate(popup_info = paste("<b>Location:</b>", City, "<br/>",
                            "<b>Number of deaths:</b>", n, "<br/>",
                            "<b>Mean age of death:</b>", mean_age))
City <- c("Bellvue", "Berthoud", "Cedaredge", "Drake", "Estes Park", 
          "Fort Collins", "Glen Haven", "Johnstown", "Laporte", "Livermore",
          "Loveland", "Masonville", "Red Feather Lakes", "Timnath", 
          "Wellington", "Windsor")


         
    
### Prepare palette info for legend add-on

obit_icon <- makeIcon("images/icons8-obituary-30.png", 
                      iconWidth = 15, iconHeight = 15)

covid_icon <- makeIcon("images/icons8-coronavirus-64.png",
                       iconWidth = 15, iconHeight = 15)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = larimer_tracts, color = "#000000", 
              fillColor = "969696", weight = 2, 
              group = "tracts") %>% 
  addMarkers(data = total_covid, 
             lat = ~Lat, 
             lng = ~Lon,
             icon = ~covid_icon,
             popup = ~popup_info,
             popupOptions(width(1200))) %>% 
  addMarkers(data = mean_age_deaths,
             lng = ~ Lon, 
             lat = ~ Lat,
             popup = ~ popup_info, 
             icon = ~obit_icon)

### put individual popup data per city in popup column
city_count <- LC_covid_gps %>% 
  group_by(City, ReportedDate) %>% 
  count()

FC <- LC_covid_gps %>% 
  mutate(FC = City == "Fort Collins") %>% 
  filter(FC) %>% 
  group_by(ReportedDate) %>% 
  count()

FC %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Fort Collins COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("FC.svg",
         device = "svg",
         width = 6, 
         height = 6,
         limitsize = FALSE)

Loveland <- LC_covid_gps %>% 
  mutate(Loveland = City == "Loveland") %>% 
  filter(Loveland) %>% 
  group_by(ReportedDate) %>% 
  count()

Loveland %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Loveland COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Loveland.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

Berthoud <- LC_covid_gps %>% 
  mutate(Berthoud = City == "Berthoud") %>% 
  filter(Berthoud) %>% 
  group_by(ReportedDate) %>% 
  count()

Berthoud %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Berthoud COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Berthoud.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

EP <- LC_covid_gps %>% 
  mutate(FC = City == "Estes Park") %>% 
  filter(FC) %>% 
  group_by(ReportedDate) %>% 
  count()

EP %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Estes Park COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("EP.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

Johnstown <- LC_covid_gps %>% 
  mutate(Johnstown = City == "Johnstown") %>% 
  filter(Johnstown) %>% 
  group_by(ReportedDate) %>% 
  count()

Johnstown %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Johnstown COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Johnstown.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

RFL <- LC_covid_gps %>% 
  mutate(RFL = City == "Red Feather Lakes") %>% 
  filter(RFL) %>% 
  group_by(ReportedDate) %>% 
  count()

RFL %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Red Feather COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("RFL.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

Timnath <- LC_covid_gps %>% 
  mutate(Timnath = City == "Timnath") %>% 
  filter(Timnath) %>% 
  group_by(ReportedDate) %>% 
  count()

Timnath %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Timnath COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Timnath.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

Wellington <- LC_covid_gps %>% 
  mutate(Wellington = City == "Wellington") %>% 
  filter(Wellington) %>% 
  group_by(ReportedDate) %>% 
  count()

Wellington %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Wellington COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Wellington.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

Windsor <- LC_covid_gps %>% 
  mutate(Windsor = City == "Windsor") %>% 
  filter(Windsor) %>% 
  group_by(ReportedDate) %>% 
  count()

Windsor %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Windsor COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Windsor.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

Laporte <- LC_covid_gps %>% 
  mutate(Laporte = City == "Laporte") %>% 
  filter(Laporte) %>% 
  group_by(ReportedDate) %>% 
  count()

Laporte %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Laporte COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Laporte.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)
  
Bellvue <- LC_covid_gps %>% 
  mutate(Bellvue = City == "Bellvue") %>% 
  filter(Bellvue) %>% 
  group_by(ReportedDate) %>% 
  count()

Bellvue %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Bellvue COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Bellvue.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

Drake <- LC_covid_gps %>% 
  mutate(Drake = City == "Drake") %>% 
  filter(Drake) %>% 
  group_by(ReportedDate) %>% 
  count()

Drake %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Drake COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Drake.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

GH <- LC_covid_gps %>% 
  mutate(GH = City == "Glen Haven") %>% 
  filter(GH) %>% 
  group_by(ReportedDate) %>% 
  count()

GH %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Glen Haven COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("GH.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

Livermore <- LC_covid_gps %>% 
  mutate(Livermore = City == "Livermore") %>% 
  filter(Livermore) %>% 
  group_by(ReportedDate) %>% 
  count()

Livermore %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Livermore COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Livermore.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

Masonville <- LC_covid_gps %>% 
  mutate(Masonville = City == "Masonville") %>% 
  filter(Masonville) %>% 
  group_by(ReportedDate) %>% 
  count()

Masonville %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Masonville COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme_bw() +
  ggsave("Masonville.svg", 
         device = "svg", 
         width = 6, 
         height = 6,
         limitsize = FALSE)

popup_info <- c("<img src = images/Bellvue.svg>", 
                "<img src = images/Berthoud.svg>",
                "<img src = images/Drake.svg>",
                "<img src = images/EP.svg>",
                "<img src = images/FC.svg>",
                "<img src = images/GH.svg>",
                "<img src = images/Johnstown.svg>",
                "<img src = images/Laporte.svg>",
                "<img src = images/Livermore.svg>",
                "<img src = images/Loveland.svg>",
                "<img src = images/Masonville.svg>",
                "<img src = images/RFL.svg>",
                "<img src = images/Timnath.svg>",
                "<img src = images/Wellington.svg>",
                "<img src = images/Windsor.svg>")

plots <- data.frame(City, popup_info)

total_covid <- full_join(total_covid, plots, by = "City")

Cedaredge <- LC_covid_gps %>% 
  mutate(Cedaredge = City == "Cedaredge") %>% 
  filter(Cedaredge) %>% 
  group_by(ReportedDate) %>% 
  count()

Cedaredge %>% 
  ggplot(aes(x = ReportedDate, y = n, color = n)) + 
  geom_point() +
  labs(x = "Date of test", y = "Number of postive individuals") +
  ggtitle("Positive Cedaredge COVID-19 cases") +
  scale_color_gradientn(colors = rainbow(5),
                        name = "Total infected") +
  theme(element_text(size = 100)) +
  theme_bw() +
  ggsave("Cedaredge.svg", 
         device = "svg", 
         width = 10, 
         height = 10,
         limitsize = FALSE)


leaflet() %>%
  addTiles() %>%
  addPolygons(data = larimer_tracts, color = "#000000", 
              fillColor = "969696", weight = 2, 
              group = "tracts") %>%
  addCircleMarkers(data = LC_covid_gps, radius = 2,
                   lng = ~ Lon, lat = ~ Lat,
                   popup = ~ popup_info, opacity = 0.9,
                   color = pal(mean_age_deaths$mean_age), 
                   group = "cases") %>% 
  addLayersControl(baseGroups = c("base map"), 
                   overlayGroups = c("tracts", "cases"))


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








