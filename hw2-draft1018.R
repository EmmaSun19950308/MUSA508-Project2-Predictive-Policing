# Load Libraries
library(sf)
library(tidyverse)
# install.packages('mapview')
library(mapview)
library(spdep)
library(caret)
library(ckanr) # for opening data APIs built on CKAN technology
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(jtools)     # for regression model plots
library(stargazer) # for creating table
library(broom)
library(tufte)
library(rmarkdown)
library(kableExtra)
library(tidycensus)
# new in Lab6
library(RSocrata)
library(viridis)
library(spatstat) # make kernel density map
library(raster)
library(knitr)
library(rgdal)


# Identify functions
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 15,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(color = "darkred", size=15, face="bold"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}


# Load hexadecimal color palette

palette <- c('#feedde', '#fdbe85', '#fd8d3c', '#e6550d', '#a63603')

# for calculating average nearest neighbor distance.

nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    dplyr::summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull() # pull() is similar to $. It's mostly useful because it looks a little nicer in pipes, it also works with remote data frames, and it can optionally name the output.
  
  return(output)  
}

# =====================
# DATA

# polygon
phillypoliceDistricts <- 
  st_read("http://data-phl.opendata.arcgis.com/datasets/62ec63afb8824a15953399b1fa819df2_0.geojson") %>%
  st_transform('ESRI:102728') %>%
  dplyr::select(District = DIST_NUM)

phillyBoundary <- 
  st_read("http://data.phl.opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson") %>%
  st_transform('ESRI:102728') 

# 2018 Shooting Victims
shootings <- 
  st_read('https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings&filename=shootings&format=geojson&skipfields=cartodb_id') %>% 
  filter(year == "2018") %>%
  na.omit() %>%
  st_transform('ESRI:102728')  

# 2018 theft
#theft <-
  #st_read('https://opendata.arcgis.com/datasets/abe39f44c8af4bfb8bfb2ec7d233d920_0.geojson') %>% 
  #filter(TEXT_GENERAL_CODE== "Thefts") %>%
  #mutate(year = substr(DISPATCH_DATE,1,4)) %>%
  #filter(year == '2018') %>%
  #na.omit() %>%
  #st_transform('ESRI:102728')



# ========================================================

# 1. A map of shooting crime in 2018, Philadelphia
grid.arrange(ncol=2,
             ggplot() + 
               geom_sf(data = phillyBoundary) +
               geom_sf(data = shootings, colour="darkred", size=0.5, show.legend = "point") +
               labs(title= "Shootings, Philadelphia, in 2018\n",
                    caption = 'Figure 1.1') +
               mapTheme() +
               plotTheme(),
             
             ggplot() + 
               geom_sf(data = phillyBoundary, fill = "#E5E5E5") +
               stat_density2d(data = data.frame(st_coordinates(shootings)), 
                              aes(X, Y, fill = ..level.., alpha = ..level..),
                              size = 0.01, bins = 40, geom = 'polygon') +
               scale_fill_viridis_c(option = "plasma") +
               scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
               labs(title = "Density of Shootings\n",
                    caption = 'Figure 1.2') +
               mapTheme() + 
               theme(legend.position = "none") +
               plotTheme())

# OR Theft? 不用了。
grid.arrange(ncol=2,
             ggplot() + 
               geom_sf(data = phillyBoundary) +
               geom_sf(data = theft, colour="darkred", size=0.1, show.legend = "point") +
               labs(title= "Thefts, Philadelphia, in 2018\n") +
               mapTheme() +
               plotTheme(),
             
             ggplot() + 
               geom_sf(data = phillyBoundary, fill = "#E5E5E5") +
               stat_density2d(data = data.frame(st_coordinates(theft)), 
                              aes(X, Y, fill = ..level.., alpha = ..level..),
                              size = 0.01, bins = 40, geom = 'polygon') +
               scale_fill_viridis_c(option = "plasma") +
               scale_alpha(range = c(0.5, 1), guide = FALSE) +
               labs(title = "Density of Thefts\n") +
               mapTheme() + 
               theme(legend.position = "none") +
               plotTheme())




## 2. A map of your outcome joined to the fishnet
# 2.1 Creating a fishnet grid

philly_fishnet <- 
  st_make_grid(phillyBoundary,
               cellsize = 500, 
               square = TRUE) %>%
  .[phillyBoundary] %>% 
  st_sf() %>%
  mutate(uniqueID = rownames(.))

# 2.2 Aggregate points to the fishnet
shooting_net <- 
  dplyr::select(shootings) %>% 
  mutate(countshootings = 1) %>% 
  aggregate(., philly_fishnet, sum) %>%
  mutate(countshootings = replace_na(countshootings, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(philly_fishnet) / 24), 
                       size=nrow(philly_fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = shooting_net, aes(fill = countshootings), color = NA) +
  scale_fill_viridis_c(option = "plasma",
                       name = 'Shooting Counts') +
  labs(title = "Count of Shootings for the fishnet\n",
       caption = 'Figure 2') +
  mapTheme() +
  plotTheme()


# OR theft_net 不用了
theft_net <- 
  dplyr::select(theft) %>% 
  mutate(counttheft = 1) %>% 
  aggregate(., philly_fishnet, sum) %>%
  mutate(counttheft = replace_na(counttheft, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(philly_fishnet) / 24), 
                       size=nrow(philly_fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = theft_net, aes(fill = counttheft), color = NA) +
  scale_fill_viridis() +
  labs(title = "Count of Thefts Joined to the Fishnet\n") +
  mapTheme() +
  plotTheme()


# 3. A small multiple map of your risk factors in the fishnet

## 3.1 Feature counts in fishnet
# Philly neighborhood

phillyneigh <-
  st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson") %>%
  st_transform('ESRI:102728') %>%
  st_transform(st_crs(philly_fishnet)) 
  
building_demolition <-
  st_read('https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+demolitions&filename=demolitions&format=geojson&skipfields=cartodb_id') %>% 
  mutate(year = substr(start_date,1,4)) %>%
  filter(year == '2018') %>%
  st_transform('ESRI:102728') %>%
  st_transform(st_crs(philly_fishnet)) %>%
  mutate(Legend = "Building Demolition") %>%
  dplyr::select(geometry, Legend)

vacant_land <-
  st_read('https://opendata.arcgis.com/datasets/b990222a527849229b4192feb4c42dc0_0.geojson') %>% 
  filter(VACANT_FLAG == "Land") %>%
  na.omit() %>%
  st_transform('ESRI:102728') %>%
  st_transform(st_crs(philly_fishnet)) %>%
  mutate(Legend = "Vacant Land") %>%
  dplyr::select(geometry, Legend)

tobacco <-
  st_read('http://data-phl.opendata.arcgis.com/datasets/853a1421e738471b8cc0d6ff755d47ff_0.geojson') %>% 
  st_transform('ESRI:102728') %>%
  st_transform(st_crs(philly_fishnet)) %>%
  mutate(Legend = "Tobacco") %>%
  dplyr::select(geometry, Legend) %>%
  na.omit() 

tobacco_violation <-
  st_read('https://opendata.arcgis.com/datasets/25b43fb8cae84e8a89d74d8707bbb5f2_0.geojson') %>% 
  filter(COMP_CHK_Y == '2018')%>%
  na.omit() %>%
  st_transform('ESRI:102728') %>%
  st_transform(st_crs(philly_fishnet)) %>%
  mutate(Legend = "Tobacco Violation") %>%
  dplyr::select(geometry, Legend)

affordable_housing <-
  st_read('https://opendata.arcgis.com/datasets/ca8944190b604b2aae7eff17c8dd9ef5_0.geojson') %>% 
  filter(FISCAL_YEAR_COMPLETE <= "2018") %>%
  st_transform('ESRI:102728') %>%
  st_transform(st_crs(philly_fishnet)) %>%
  mutate(Legend = "Affordable Housing") %>%
  dplyr::select(geometry, Legend)

# All variables in fishnet 
vars_net <- 
  rbind(building_demolition, tobacco, affordable_housing, tobacco_violation,
        vacant_land) %>%
  st_join(., philly_fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(philly_fishnet, by = "uniqueID") %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  na.omit() %>% 
  dplyr::select(-`<NA>`) %>%
  ungroup()

### Multiple map for feature counts in fishnet
vars_net.long <- 
  gather(vars_net, Variable, value, -geometry, -uniqueID)

vars <- unique(vars_net.long$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis_c(option = "plasma",
                         name = " ") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList, ncol =2, top = "Risk Factors by Fishnet\n"))



## 3.2 Nearest Neighbor Feature
# convenience to reduce length of function names.
st_c <- st_coordinates
st_coid <- st_centroid

## create NN from abandoned cars, k = 5
'%!in%' <- function(x,y)!('%in%'(x,y))

vars_net$tobacco.nn <- 
  nn_function(st_c(st_coid(vars_net)), 
              st_c(tobacco),
              k = 3)
vars_net$building_demolition.nn <-
           nn_function(st_c(st_coid(vars_net)), 
                                           st_c(building_demolition),
                                           k = 3)

vars_net$affordable_housing.nn <- 
           nn_function(st_c(st_coid(vars_net)), 
                                         st_c(affordable_housing %>%
                                                filter(geometry %!in% 'c(NaN, NaN)')),
                                         k = 3)
vars_net$tobacco_violation.nn <- 
           nn_function(st_c(st_coid(vars_net)), 
                                 st_c(tobacco_violation),
                                 k = 3)
vars_net$vacant_land.nn <-
           nn_function(st_c(st_coid(vars_net)), 
                                               st_c(vacant_land),
                                               k = 3)

## Visualize the nearest three features
vars_net.long.nn <- 
  dplyr::select(vars_net, ends_with(".nn")) %>%
  gather(Variable, value, -geometry)

vars <- unique(vars_net.long.nn$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long.nn, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis_c(option = "plasma",
                         name = " ") +
    labs(title=i) +
    mapTheme() +
    plotTheme()}

do.call(grid.arrange,c(mapList, ncol = 2, top = "Nearest Neighbor risk Factors by Fishnet\n"))


# IV and DVs all in fishnet
philly_final_net <-
  left_join(shooting_net, st_drop_geometry(vars_net), by="uniqueID") 

philly_final_net <-
  st_centroid(philly_final_net) %>%
  st_join(dplyr::select(phillyneigh, mapname), by = "uniqueID") %>%
  st_join(dplyr::select(phillypoliceDistricts, District), by = "uniqueID") %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(philly_final_net, geometry, uniqueID)) %>%
  st_sf() %>%
  na.omit()


# 4. Local Moran's I-related small multiple map of your outcome
## 4.1 spatial weights matrix

philly_final_net.nb <- poly2nb(as_Spatial(philly_final_net), queen=TRUE)
philly_final_net.weights <- nb2listw(philly_final_net.nb, style="W", zero.policy=TRUE)


philly_final_net.localMorans <- 
  cbind(
    as.data.frame(localmoran(philly_final_net$countshootings, philly_final_net.weights)),
    as.data.frame(philly_final_net)) %>% 
  st_sf() %>%
  dplyr::select(Shooting_Count = countshootings, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z > 0)`) %>%
  mutate(Significant_Hotspots = ifelse(P_Value <= 0.05, 1, 0)) %>%
  gather(Variable, Value, -geometry)

vars <- unique(philly_final_net.localMorans$Variable)
varList <- list()

for(i in vars){
  varList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(philly_final_net.localMorans, Variable == i), 
            aes(fill = Value), colour=NA) +
    scale_fill_viridis_c(option = "plasma",
                         name = " ") +
    labs(title=i) +
    mapTheme() + theme(legend.position="bottom") +
    plotTheme()}

do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics, Burglary"))

### Distance to highly significant shooting hotpot
philly_final_net <-
  philly_final_net %>% 
  mutate(shooting.isSig = 
           ifelse(localmoran(philly_final_net$countshootings, 
                             philly_final_net.weights)[,5] <= 0.0000001, 1, 0)) %>%
  mutate(shooting.isSig.dist = 
           nn_function(st_coordinates(st_centroid(philly_final_net)),
                       st_coordinates(st_centroid(
                         filter(philly_final_net, shooting.isSig == 1))), 1))

# 5. A small multiple scatterplot with correlations
philly.correlation.long <-
  st_drop_geometry(philly_final_net) %>%
  dplyr::select(-uniqueID, -cvID,-starts_with('mapname'),-starts_with('District')) %>%
  gather(Variable, Value, -countshootings)

philly.correlation.cor <-
  philly.correlation.long %>%
  group_by(Variable) %>%
  summarize(philly.correlation = cor(Value, countshootings, use = "complete.obs"))

ggplot(philly.correlation.long, aes(Value, countshootings)) +
  geom_point(size = 0.1) +
  geom_text(data = philly.correlation.cor, aes(label = paste("r =", round(philly.correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Shooting count as a function of risk factors") +
  plotTheme()

# 6. A histogram of dependent variable

# Given shooting is a relatively rare event, it is reasonable for most grid cells to contain no crime events.
# And this distribution is Poisson distribution

ggplot(data = philly_final_net) +
  geom_histogram(aes(x = countshootings), fill = 'orange') +
  labs(title="Histogram of Dependent Variable: Count of Shootings\n",
       caption = "Figure 6") +
  xlab('Count of Shootings') +
  ylab('Count') +
  plotTheme()

## 7. A small multiple map of model errors by random k-fold and spatial cross validation.

# 这是这个问题的回答。A well generalized crime predictive model learns the crime risk 'experience' at both citywide and local spatial scales. The best way to test for this is to hold out one local area, train the model on the remaining n - 1 areas, predict for the hold out, and record the goodness of fit.
# For a given risk factor, I selected feature counts in each fishnet and distance to shooting hotpots as well as distance to significant shooting hotpots as features used in the model to avoid collinerity. I also added Local Moran's I spatial process features based on the result in Question 4. Therefore, we have **seven** features in total.
# `reg.ss.cv` performs random k-fold cross validation using spatial process features, while `reg.ss.spatialCV` performs LOGO-CV, spatial cross-validation on neighborhood name, using the same features.

## define the variables 
reg.ss.vars <- c("Affordable Housing" , "Building Demolition","Tobacco" ,"Tobacco Violation","Vacant Land", 
                 "shooting.isSig" ,"shooting.isSig.dist")

## Define crossValidate function
crossValidate <- function(dataset, id, dependentVariable, indVariables) {
  
  allPredictions <- data.frame()
  cvID_list <- unique(dataset[[id]])
  
  for (i in cvID_list) {
    
    thisFold <- i
    cat("This hold out fold is", thisFold, "\n")
    
    fold.train <- filter(dataset, dataset[[id]] != thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    fold.test  <- filter(dataset, dataset[[id]] == thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    
    regression <-
      glm(countshootings ~ ., family = "poisson", 
          data = fold.train %>% 
            dplyr::select(-geometry, -id))
    
    thisPrediction <- 
      mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
    
    allPredictions <-
      rbind(allPredictions, thisPrediction)
    
  }
  return(st_sf(allPredictions))
}

# 注意：514 和524 两个model差不多需要20分钟才能run 完，我们只run一次，不浪费时间。
# 所以，我们到交作业的时候再knit，在此之前，我们都只run 特定code chunk.
# 你每次关闭R时候，记得保存你的Rproj
###  create random k-fold cv
reg.ss.cv <- crossValidate(
  dataset = philly_final_net,
  id = "cvID",
  dependentVariable = "countshootings",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = cvID, countshootings, Prediction, geometry)


###  create spatial cross validation
reg.ss.spatialCV <- crossValidate(
  dataset = philly_final_net,
  id = "mapname",                            ### !!! <- really important line
  dependentVariable = "countshootings",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = mapname, countshootings, Prediction, geometry)

# Bind two CVs together
reg.summary <- 
  rbind(
    mutate(reg.ss.cv,        Error = Prediction - countshootings,
           Regression = "Random k-fold CV: Spatial Process"),
    mutate(reg.ss.spatialCV, Error = Prediction - countshootings,
           Regression = "Spatial LOGO-CV: Spatial Process")) %>%
  st_sf()


# calculate and visualize MAE for each fold 
error_by_reg_and_fold <- 
  reg.summary %>%
  group_by(Regression, cvID) %>% 
  summarize(Mean_Error = mean(Prediction - countshootings, na.rm = T),
            MAE = mean(abs(Mean_Error), na.rm = T),
            SD_MAE = mean(abs(Mean_Error), na.rm = T)) %>%
  ungroup()

#### 7.1 Distribution of MAE
error_by_reg_and_fold %>%
  ggplot(aes(MAE)) + 
  geom_histogram(bins = 30, colour="black", fill = "#5dccb9") +
  facet_wrap(~Regression) +  
  geom_vline(xintercept = 0) + 
  scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  labs(title="Distribution of MAE", 
       subtitle = "k-fold cross validation vs. LOGO-CV\n",
       caption = "Figure 7.1",
       x="Mean Absolute Error", y="Count") +
  plotTheme()

##### 7.2 visualizes the random k-fold and LOGO-CV errors spatially. 
error_by_reg_and_fold %>%
  ggplot() +
  geom_sf(aes(fill = MAE)) +
  facet_wrap(~Regression) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Shooting Errors by Rondom k-fold and LOGO-CV Regression\n",
       caption = 'Figure 7.2') +
  mapTheme() + theme(legend.position="bottom") +
  plotTheme()

##### 7.3 Predicted shooting and observed shooting
# Interestingly, Figure 7.3 below shows that all models over-predict in low shooting areas and under-predict in hot spot areas. Over-predictions in lower shooting areas may highlight areas of latent risk. Under-prediction in higher shooting areas may reflect difficulty predicting the hotspots.
st_drop_geometry(reg.summary) %>%
  group_by(Regression) %>%
  mutate(burglary_Decile = ntile(countshootings, 10)) %>%
  group_by(Regression, burglary_Decile) %>%
  summarize(meanObserved = mean(countshootings, na.rm=T),
            meanPrediction = mean(Prediction, na.rm=T)) %>%
  gather(Variable, Value, -Regression, -burglary_Decile) %>%          
  ggplot(aes(burglary_Decile, Value, shape = Variable)) +
  geom_point(size = 2) + 
  geom_path(aes(group = burglary_Decile), colour = "black") +
  scale_shape_manual(values = c(2, 17)) +
  facet_wrap(~Regression) + xlim(0,10) +
  labs(title = "Predicted and Observed Shootings by Observed Burglary Decile\n",
       caption = 'Figure 7.3') +
  plotTheme()

## 8. A table of MAE and standard deviation MAE by regression.
st_drop_geometry(error_by_reg_and_fold) %>%
  group_by(Regression) %>% 
  summarize(Mean_MAE = round(mean(MAE), 2),
            SD_MAE = round(sd(MAE), 2)) %>%
  kable(caption = "Table 8. MAE by regression") %>%
  kable_styling("striped", full_width = F) 



# 9. A table of raw errors by race context for a random k-fold vs. spatial cross validation regression.

## 9.1 Fetch census data
# View(load_variables(2018,'acs5',cache = TRUE))
tracts18 <- 
  get_acs(geography = "tract", variables = c("B00001_001E","B02001_002E"), 
          year=2018, state=42, county=101, geometry=T, output="wide") %>%
  st_transform('ESRI:102728') %>%
  rename(TotalPop = B00001_001E, 
         Whites = B02001_002E) %>%
  mutate(percentWhite = Whites / TotalPop,
         raceContext = ifelse(percentWhite > .5, "Majority_White", "Majority_Non_White")) %>%
  .[phillyneigh,]

ggplot() + 
  geom_sf(data = na.omit(tracts18), aes(fill = raceContext)) +
  scale_fill_manual(values = c("#25CB10", "#ff9966"), name="Race Context") +
  labs(title = "Race Context in Philly\n",
       caption = 'Figure 9') +
  mapTheme() + 
  theme(legend.position="bottom") +
  plotTheme()

# The model on average, under-predicts in Majority_Non_White neighborhoods and over-predicts in Majority_White neighborhoods. 
# It looks like this algorithm generalizes well with respect to race.
reg.summary %>% 
  st_centroid() %>%
  st_join(tracts18) %>%
  na.omit() %>%
  st_drop_geometry() %>%
  group_by(Regression, raceContext) %>%
  summarize(mean.Error = mean(Error, na.rm = T)) %>%
  spread(raceContext, mean.Error) %>%
  kable(caption = "Table 9. Mean Error by neighborhood racial context") %>%
  kable_styling("striped", full_width = F) 

## 10. The map comparing kernel density to risk predictions for the next year's crime.

### 10.1 Make Kernel Density Map
sho_ppp <- as.ppp(st_coordinates(shootings), W = st_bbox(philly_final_net))
sho_KD.1000 <- spatstat::density.ppp(sho_ppp, 1000)
sho_KD.1500 <- spatstat::density.ppp(sho_ppp, 1500)
sho_KD.2000 <- spatstat::density.ppp(sho_ppp, 2000)
sho_KD.df <- rbind(
  mutate(data.frame(rasterToPoints(mask(raster(sho_KD.1000), as(phillyneigh, 'Spatial')))), Legend = "1000 Ft."),
  mutate(data.frame(rasterToPoints(mask(raster(sho_KD.1500), as(phillyneigh, 'Spatial')))), Legend = "1500 Ft."),
  mutate(data.frame(rasterToPoints(mask(raster(sho_KD.2000), as(phillyneigh, 'Spatial')))), Legend = "2000 Ft.")) 

sho_KD.df$Legend <- factor(sho_KD.df$Legend, levels = c("1000 Ft.", "1500 Ft.", "2000 Ft."))

ggplot(data = sho_KD.df, aes(x = x, y = y)) +
  geom_raster(aes(fill=layer)) + 
  facet_wrap(~Legend) +
  coord_sf(crs=st_crs(philly_final_net)) + 
  scale_fill_viridis_c(option = "plasma",
                       name = "Density") +
  labs(title = "Kernel Density with 3 Different Search Radius Scales\n",
       caption = 'Figure 10.1') +
  mapTheme() +
  plotTheme()

### 10.2 Fetch 2019 Shooting Victims Data
shootings19 <- 
  st_read('https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings&filename=shootings&format=geojson&skipfields=cartodb_id') %>% 
  filter(year == "2019") %>%
  na.omit() %>%
  st_transform('ESRI:102728')   %>%
  distinct() %>%
  .[philly_fishnet,] 

### 10.3 Prediction by Kernel Density Model
sho_KDE_sf <- as.data.frame(sho_KD.1000) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(philly_final_net)) %>%
  aggregate(., philly_final_net, mean) %>%
  mutate(label = "Kernel Density",
         Risk_Category = ntile(value, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(shootings19) %>% mutate(shootingCount = 1), ., sum) %>%
      mutate(shootingCount = replace_na(shootingCount, 0))) %>%
  dplyr::select(label, Risk_Category, shootingCount)

##### 10.4 Prediction by Risk Prediction Model
sho_risk_sf <-
  reg.ss.spatialCV %>%
  mutate(label = "Risk Predictions",
         Risk_Category = ntile(Prediction, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(shootings19) %>% mutate(shootingCount = 1), ., sum) %>%
      mutate(shootingCount = replace_na(shootingCount, 0))) %>%
  dplyr::select(label,Risk_Category, shootingCount)

##### 10.5 Comparison Maps
# Below a map is generated of the risk categories for both model types with a sample of shooting19 points overlaid. A strongly fit model should show that the highest risk category is uniquely targeted to places with a high density of shooting points.
rbind(sho_KDE_sf, sho_risk_sf) %>%
  na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category, -geometry) %>%
  ggplot() +
  geom_sf(aes(fill = Risk_Category), colour = NA) +
  geom_sf(data = sample_n(shootings19, 1130), size = .5, colour = "black") +
  facet_wrap(~label, ) +
  scale_fill_viridis_d(option = "plasma",
                       name = 'Risk Category') + 
  labs(title="Comparison of Kernel Density and Risk Predictions",
       subtitle="Bottom layer is 2019 predicted shooting counts.\nDot is observed 2019 shooting counts.\n",
       caption = 'Figure 10.2') +
  mapTheme() +
  plotTheme()

## 11. The bar plot making this comparison.
# The risk prediction model narrowly edges out the Kernel Density in the highest risk categories, while the business-as-usual hot spot approach performs much better on the second highest risk catogory. Hence, based on the model I created so far, it is hardly to say which one is completely better.
rbind(sho_KDE_sf, sho_risk_sf) %>%
  st_set_geometry(NULL) %>% 
  na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category) %>%
  group_by(label, Risk_Category) %>%
  summarize(countShootings = sum(Value)) %>%
  ungroup() %>%
  group_by(label) %>%
  mutate(Rate_of_test_set_crimes = countShootings / sum(countShootings)) %>%
  ggplot(aes(Risk_Category,Rate_of_test_set_crimes)) +
  geom_bar(aes(fill=label), position="dodge", stat="identity") +
  scale_fill_viridis_d(option = "plasma",
                       name = 'Model') +
  labs(title = "Risk Prediction vs. Kernel Density, 2019 Shooing Crime\n",
       caption = 'Figure 11',
       x = 'Risk Category',
       y = 'Rate of Test Set Shooting Crimes') +
  plotTheme()


a <- st_read('Neighborhoods_Philadelphia.shp')
a <- st_read("Neighborhoods_Philadelphia.shp")

# ============
# Census
acs <-
  get_acs(geography = "tract", variables = c("B25002_003E", "B25001_001E", "B19013_001E", "B01001A_001E", "B01003_001E", "B07013_002E", "B07013_003E", "B06009_002E", 
                                             "B05003_008E", "B05003_019E", "B06012_002", "B21005_006E", "B21005_011E", 
                                             "B01001_006E", "B01001_007E", "B01001_008E", "B01001_009E","B01001_010E", "B01001_011E", "B01001_012E",
                                             "B01001_031E", "B01001_032E", "B01001_033E", "B01001_034E", "B01001_035E", "B01001_036E"
  ), year=2018, state=17, county=031, geometry=T) %>%
  st_transform(st_crs(fishnet))
acs <-         #filter for chicago tracts
  rbind(
    st_centroid(acs)[chicagoBound,] %>%
      st_drop_geometry() %>%
      left_join(acs) %>%
      st_sf() %>%
      mutate(inChicago = "YES"),
    st_centroid(acs)[chicagoBound, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(acs) %>%
      st_sf() %>%
      mutate(inChicago = "NO")) %>%
  filter(inChicago == "YES") %>%
  dplyr::select(-inChicago)
#long to wide form
acs <-
  acs %>%
  dplyr::select(-moe, -GEOID) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(vacantUnits = B25002_003,
         totalUnits = B25001_001,
         medHHInc = B19013_001,
         white = B01001A_001,
         population = B01003_001,
         ownerOcc = B07013_002,
         renterOcc = B07013_003,
         noHsDegree = B06009_002,
         maleAdult = B05003_008,
         femaleAdult = B05003_019,
         poverty = B06012_002,
         youthUnempVet = B21005_006,
         youthUnempNonVet = B21005_011,
         male1517 = B01001_006,
         male1819 = B01001_007,
         male20 = B01001_008,
         male21 = B01001_009,
         male2224 = B01001_010,
         male2529 = B01001_011,
         male3034 = B01001_012,
         female1819 = B01001_031,
         female20 = B01001_032,
         female21 = B01001_033,
         female2224 = B01001_034,
         female2529 = B01001_035,
         female3034 = B01001_036)
acs <- 
  acs %>%
  mutate(pctVacant = ifelse(totalUnits > 0, vacantUnits / totalUnits, 0),
         pctWhite = ifelse(population > 0, white / population, 0), 
         pctRenterOcc = renterOcc/ (renterOcc + ownerOcc),
         pctNoHS = noHsDegree/ (maleAdult + femaleAdult),
         pctPoverty = ifelse(population > 0, poverty / population, 0),
         youthUnemploy = (youthUnempVet + youthUnempNonVet) / (male1819 + male20 + male21 + male2224 + male2529 + male3034 + female1819 + female20 + female21 + female2224 + female2529 + female3034),
         pctMaleYouth = ifelse(population > 0, (male1517 + male1819 + male20 + male21 + male2224 + male2529 + male3034) / population, 0)) %>%
  dplyr::select(-totalUnits,-vacantUnits,-white,-renterOcc,-ownerOcc, -noHsDegree, -maleAdult, -femaleAdult, -youthUnempVet, -youthUnempNonVet, -male1517, -male1819, -male20, -male21, -male2224, -male2529, -male3034, -female1819, -female20, -female21, -female2224, -female2529, -female3034, -poverty)



# ATTACH VARIABLES TO FISHNET
vars_net <- 
  rbind(abandonCars,streetLightsOut,abandonBuildings,
        liquorRetail, graffiti, sanitation, busStops, buildingViolation, affordableRentHousing, alleyLightsOut, garbageOpen, policeStations) %>%
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet) %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()
vars_net <- vars_net %>%
  st_join(., acs)
