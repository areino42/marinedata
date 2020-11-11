

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


#--------------------------> SHIP TYPES --------------------------------->

df <- data %>% 
  select(ship_type) %>% 
  distinct(ship_type)

    

