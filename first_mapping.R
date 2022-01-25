####################################
### FP Incidence Preliminary Mapping ###
####################################

## The below was to enable rnaturalearth to function
#devtools::install_github("ropenscilabs/rnaturalearth")
#devtools::install_github("ropenscilabs/rnaturalearthdata")
#install.packages("rnaturalearthhires",
               #  repos = "http://packages.ropensci.org",
                # type = "source")
#install.packages("magick")

library(magick)
library(tidyverse)
library(rnaturalearth)
library(mapview)
library(sf)


## Directory fetch
#getwd()
#setwd("/Users/aidanperez/Documents/FP_incidence_CR/Scripts")

### load csv

capture_data <- read.csv("2021-10_Cm_capture_data.csv")

# inspect
summary(capture_data)
glimpse(capture_data)

# select only crystal river turtles & filter NA from Balazs score
table(capture_data$Site)

cd2 <- capture_data %>%
  filter(str_detect (string = Site, pattern = "Crystal River")) %>%
  filter(!is.na(FP.Balazs.Score)) # filter out "NA" values in Balazs score

#check filter
table(cd2$Site)
table(cd2$FP.Balazs.Score)

##uspoly
states <- ne_states(country = "United States of America", returnclass = "sf")

## fl shapefile

florida <- st_read("/Users/aidanperez/Documents/FP_incidence_CR/Mapping/fl_shapefile/Florida_Shoreline_(1_to_40%2C000_Scale).shp")


### Viz and interactive map 

cm.sf <- cd2 %>%
  drop_na(GPS_X, GPS_Y) %>% 
  st_as_sf(., coords = c("GPS_X", "GPS_Y"), crs = 4326)

# mapview(cm.sf, zcol = 'FP.Balazs.Score')

### plots

# turtles captured by year 
ggplot()+
  geom_sf(data = florida, fill = "grey") +
  geom_point(data = cd2, aes(x = GPS_X, y = GPS_Y, color = factor(Year)), size = 1) +
  ggtitle("Captured Turtles Distribution per Year") +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  theme_bw() +
  coord_sf()

# balazs score total
ggplot()+
  geom_sf(data = florida, fill = "grey") +
  geom_point(data = cd2, aes(x = GPS_X, y = GPS_Y, color = factor(FP.Balazs.Score)), size = 1) +
  ggtitle("Total Balazs Score Geospatial Dist. 2016-2021") +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  theme_bw() +
  coord_sf() 

### balazs score by year 

#filtering years
balazs2016 <- filter(cd2, Year == "2016")
balazs2017 <- filter(cd2, Year == "2017")
balazs2018 <- filter(cd2, Year == "2018")
balazs2019 <- filter(cd2, Year == "2019")
balazs2020 <- filter(cd2, Year == "2020")
balazs2021 <- filter(cd2, Year == "2021")

#Balazs 2016
ggplot()+
  geom_sf(data = florida, fill = "grey") +
  geom_point(data = balazs2016, aes(x = GPS_X, y = GPS_Y, color = factor(FP.Balazs.Score)), size = 1) +
  ggtitle("Balazs Score Geospatial Distribution - 2016") +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  theme_bw() +
  coord_sf()

#Balazs 2017
ggplot()+
  geom_sf(data = florida, fill = "grey") +
  geom_point(data = balazs2017, aes(x = GPS_X, y = GPS_Y, color = factor(FP.Balazs.Score)), size = 1) +
  ggtitle("Balazs Score Geospatial Distribution - 2017") +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  theme_bw() +
  coord_sf()

#Balazs 2018
ggplot()+
  geom_sf(data = florida, fill = "grey") +
  geom_point(data = balazs2018, aes(x = GPS_X, y = GPS_Y, color = factor(FP.Balazs.Score)), size = 1) +
  ggtitle("Balazs Score Geospatial Distribution - 2018") +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  theme_bw() +
  coord_sf()

#Balazs 2019
ggplot()+
  geom_sf(data = florida, fill = "grey") +
  geom_point(data = balazs2019, aes(x = GPS_X, y = GPS_Y, color = factor(FP.Balazs.Score)), size = 1) +
  ggtitle("Balazs Score Geospatial Distribution - 2019") +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  theme_bw() +
  coord_sf()

#Balazs 2020
ggplot()+
  geom_sf(data = florida, fill = "grey") +
  geom_point(data = balazs2020, aes(x = GPS_X, y = GPS_Y, color = factor(FP.Balazs.Score)), size = 1) +
  ggtitle("Balazs Score Geospatial Distribution - 2020") +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  theme_bw() +
  coord_sf()

#Balazs 2021
ggplot()+
  geom_sf(data = florida, fill = "grey") +
  geom_point(data = balazs2021, aes(x = GPS_X, y = GPS_Y, color = factor(FP.Balazs.Score)), size = 1) +
  ggtitle("Balazs Score Geospatial Distribution - 2021") +
  xlim(-82.82,-82.67) +
  ylim(28.7, 28.85) + 
  theme_bw() +
  coord_sf()

### stack gif balazs score maps 

#list files and read them
balazs_stack <- list.files(path = "/Users/aidanperez/Documents/FP_incidence_CR/Mapping/prelim_maps/balazs_scores", full.names = TRUE)
balazs_list <- lapply(balazs_stack, image_read)

#join these images together
balazs_joined <- image_join(balazs_list)

#animate at 1fps 
balazs_anim <- image_animate(balazs_joined, fps = 1)

#view
balazs_anim

#save gif
image_write(image = balazs_anim,
            path = "/Users/aidanperez/Documents/FP_incidence_CR/Mapping/prelim_maps/balazs_scores/animated_balazs_map.gif")


## Cordinate map Balazs score incidence per year 
dat<- capture_data %>%
  filter(Fibropapilloma.Visible == 'Yes')


ggplot(dat, aes(GPS_X, GPS_Y, fill = factor(Year), size = FP.Balazs.Score)) +
  geom_point(alpha = 0.7, shape = 21, color = 'black') +
  scale_fill_brewer('Year', palette = "OrRd") +
  xlim(-82.9, -82.6) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12, face = 'bold')) +
  coord_equal() +
  facet_wrap(~ Year, ncol = 2)  # This is main feature; everything else is for aesthetics
