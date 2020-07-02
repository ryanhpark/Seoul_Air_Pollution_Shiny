library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(tidyverse)
library(DT)

# Reading the CSV Files used 
sum_df = read.csv("sum_df.csv")
station_info = read.csv("measurement_station_info.csv")
item_info = read.csv("measurement_item_info.csv")

# Customizing group color for PM10
getColor1 <- function(colors) {
  if (colors <= 30) {"blue"} 
  else if (colors <= 80) {"green"} 
  else if (colors <= 150) {"orange"} 
  else {"red"}
}

# Customizing group color for PM2.5
getColor2 <- function(colors) {
    if (colors <= 15) {"blue"} 
    else if (colors <= 35) {"green"} 
    else if (colors <= 70) {"orange"} 
    else {"red"}}