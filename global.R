

library(readr)
library(leaflet)
library(dplyr)
library(plotly)
library(highcharter)
library(lubridate)
library(geosphere)
library(deckgl)
library(RCurl)


#-----------------------------> DATA ------------------------------------>



#data <- read_csv("ships_data/ships.csv")
web <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")
data  <- read.csv(text = web)


#--------------------------> SHIP TYPES --------------------------------->

df <- data %>% 
  select(ship_type) %>% 
  distinct(ship_type)

    

