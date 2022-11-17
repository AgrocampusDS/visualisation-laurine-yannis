rm(list=ls())

data <- read.csv("./dataset/HDI.csv", sep=",", header = TRUE)

names <- names(data)

countries_coordinates <- read.csv("./dataset/countries_coordinates.csv", sep=",", header = TRUE)

data2 <- merge(data, countries_coordinates, by="Country")

## First graph with map test

library('tidyverse')
library('ggplot2')
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")

world %>% ggplot() +geom_sf()

## Data pre-processing : rename of certain value
data[data$Country == "Congo (Democratic Republic of the)", "Country"] <- "Democratic Republic of the Congo"
data[data$Country == "Congo", "Country"] <- "Republic of Congo"
data[data$Country == "Tanzania (United Republic of)", "Country"] <- "Tanzania"
data[data$Country == "Bolivia (Plurinational State of)", "Country"] <- "Bolivia"
data[data$Country == "Korea (Republic of)", "Country"] <- "Republic of Korea" 

complete_data <- world %>% rename(Country = name_long) %>% inner_join(data, by="Country")

complete_data %>% ggplot() + geom_sf(aes(fill=HDI))



