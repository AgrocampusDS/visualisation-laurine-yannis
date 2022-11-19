rm(list=ls())

data <- read.csv("./dataset/HDI.csv", sep=",", header = TRUE)

names <- names(data)

countries_coordinates <- read.csv("./dataset/countries_coordinates.csv", sep=",", header = TRUE)

data2 <- merge(data, countries_coordinates, by="Country")

## First graph with map test

library('tidyverse')
library('ggplot2')
library("ggthemes")
library("gridExtra")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggnewscale")

world <- ne_countries(scale = "medium", returnclass = "sf")

world %>% ggplot() +geom_sf()

## Data pre-processing : rename of certain value
data[data$Country == "Congo (Democratic Republic of the)", "Country"] <- "Democratic Republic of the Congo"
data[data$Country == "Congo", "Country"] <- "Republic of Congo"
data[data$Country == "Tanzania (United Republic of)", "Country"] <- "Tanzania"
data[data$Country == "Bolivia (Plurinational State of)", "Country"] <- "Bolivia"
data[data$Country == "Korea (Republic of)", "Country"] <- "Republic of Korea" 

complete_data <- world %>% rename(Country = name_long) %>% inner_join(data, by="Country")

complete_data %>% ggplot() + 
  geom_sf(aes(fill=HDI))  +
  scale_fill_gradient(low="red", high="blue")

## Focus on africa

africa_data <- complete_data %>% filter(continent == "Africa")
africa_data %>% ggplot() + 
  geom_sf(aes(fill=HDI))  +
  scale_fill_gradient(low="red", high="blue") +
  theme_tufte() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

## extract centroid 
sf_use_s2(FALSE)
countries_centroid <- st_centroid(africa_data$geometry)

africa_data <- cbind(africa_data, centroid = st_centroid(africa_data$geometry))

# Represent mean years of school and life expectancy
## Creation of class for mean years : 

africa_data <- africa_data %>% drop_na(Mean.years.of.schooling)

meanClass <- apply(africa_data, MARGIN = 1, FUN= function(x){
  if(as.numeric(x["Mean.years.of.schooling"]) <= 2.5){
    return("0-2.5")
  }
  if((as.numeric(x["Mean.years.of.schooling"]) > 2.5) && (as.numeric(x["Mean.years.of.schooling"]) <=5)){
    return("2.5-5")
  }
  if((as.numeric(x["Mean.years.of.schooling"]) > 5) && (as.numeric(x["Mean.years.of.schooling"]) <=7.5)){
    return("5-7.5")
  }
  if(as.numeric(x["Mean.years.of.schooling"]) > 7.5){
    return("7.5-10")
  }
})

africa_data$meanClass <- meanClass

map <- africa_data %>%  ggplot() + 
  geom_sf(aes(fill=Life.expectancy)) +
  scale_fill_gradientn("Espérance de vie", colors=c("#DFD0BD", "#5B4A54")) +
  
  #new_scale_color() +
  
  geom_sf(data = africa_data$geometry.1, size=6, aes(color=africa_data$meanClass)) +
  scale_color_manual("Année d'école", values=c("#E64814", "#E9CB0D", "#9CDE3B", "#2D5E5C"), 
                     breaks=c("0-2.5", "2.5-5", "5-7.5", "7.5-10")) +

  theme_tufte() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# test on agricultural employment

africa_data %>% rename(Agriculture = Employment.in.agriculture....of.total.employment..2010.2014) %>%  ggplot() + 
  geom_sf(aes(fill=Agriculture))  +
  geom_sf(data = africa_data$geometry.1) +
  scale_fill_gradient(low="red", high="blue") +
  theme_tufte() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# test multiple plots 
t <- function(title, bgcolor){
  #fonction pour créer un titre custom
  return(ggplot() +                      # Draw ggplot2 plot with text only
           annotate("text",
                    x = 1,
                    y = 0,
                    size = 4,
                    label = title,
                    colour="black") + 
           theme_void()+
           theme(plot.background = element_rect(fill=bgcolor, color=bgcolor))
  )
}

test <- t('Titre', "white")

l <- list(map, test)

grid.arrange(
  grobs = l,
  widths = c(3),
  heights = c(3, 1),
  layout_matrix = rbind(c(1),
                        c(2))
)
