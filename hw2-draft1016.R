library(tidyverse)
library(sf)
library(RSocrata)
library(viridis)
library(spatstat)
library(raster)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)

# functions
root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


policeDistricts <- 
  st_read("http://data-phl.opendata.arcgis.com/datasets/62ec63afb8824a15953399b1fa819df2_0.geojson") %>%
  st_transform('ESRI:102728') %>%
  dplyr::select(District = DIST_NUM)

phillyBoundary <- 
  st_read("http://data.phl.opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson") %>%
  st_transform('ESRI:102728') 

shoot <- 
  st_read('https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings&filename=shootings&format=geojson&skipfields=cartodb_id') %>% 
  filter(year == "2018") %>%
  na.omit() %>%
  st_transform('ESRI:102728')  

#vacant_land <-
#  st_read('http://data.phl.opendata.arcgis.com/datasets/c71d6fdda9c24be1bfb755147bb43691_0.geojson') %>% 
#  na.omit() %>%
#  st_transform('ESRI:102728') 

tobacco <-
  st_read('http://data-phl.opendata.arcgis.com/datasets/853a1421e738471b8cc0d6ff755d47ff_0.geojson') %>% 
  na.omit() %>%
  st_transform('ESRI:102728')

building <-
  st_read('https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+demolitions&filename=demolitions&format=geojson&skipfields=cartodb_id') %>% 
  na.omit() %>%
  st_transform('ESRI:102728')

# ========================================================

# uses grid.arrange to organize indpendent plots
grid.arrange(ncol=2,
ggplot() + 
  geom_sf(data = phillyBoundary) +
  geom_sf(data = shoot, colour="red", size=0.1, show.legend = "point") +
  labs(title= "Shootings, Philadelphia, 2018") +
  mapTheme(title_size = 14),
             
ggplot() + 
  geom_sf(data = phillyBoundary, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(shoot)), 
                  aes(X, Y, fill = ..level.., alpha = ..level..),
                  size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_viridis() +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Shootings") +
  mapTheme(title_size = 14) + theme(legend.position = "none"))

# Creating a fishnet grid
philly_fishnet <- 
  st_make_grid(phillyBoundary,
               cellsize = 500, 
               square = TRUE) %>%
  .[phillyBoundary] %>% 
  st_sf() %>%
  mutate(uniqueID = rownames(.))

# Aggregate points to the fishnet
shooting_net <- 
  dplyr::select(shoot) %>% 
  mutate(countshootings = 1) %>% 
  aggregate(., philly_fishnet, sum) %>%
  mutate(countshootings = replace_na(countshootings, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(philly_fishnet) / 24), 
                       size=nrow(philly_fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = shooting_net, aes(fill = countshootings), color = NA) +
  scale_fill_viridis() +
  labs(title = "Count of Shootings for the fishnet") +
  mapTheme()

# Modeling Spatial Features
# How we aggregate a feature to our fishnet
vars_net <- tobacco %>%
  st_join(., philly_fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID) %>%
  summarize(count = n()) %>%
  full_join(philly_fishnet, by = "uniqueID") %>%
  spread(count, fill=0) %>%
  st_sf() %>%
  na.omit() %>%
  ungroup()

# convinience to reduce length of function names.
st_c <- st_coordinates
st_coid <- st_centroid

## create NN from abandoned cars
vars_net <- vars_net %>%
  mutate(tobacco.nn = nn_function(st_c(st_coid(vars_net)), 
                                         st_c(tobacco),
                                         k = 3))

## Visualize the NN feature
vars_net.long.nn <- 
  dplyr::select(vars_net, ends_with(".nn")) %>%
  gather(Variable, value, -geometry)

ggplot() +
  geom_sf(data = vars_net.long.nn, aes(fill=value), colour=NA) +
  scale_fill_viridis(name="NN Distance") +
  labs(title="Tobacco Retailer NN Distance") +
  mapTheme()
