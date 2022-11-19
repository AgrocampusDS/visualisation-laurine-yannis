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
africa_data$clusters <- clusters

# Plot first Map, Africa HDI and School
map_HDI_School <- africa_data %>%  ggplot() + 
  #labs(title="Les limites de l'IDH",
   #       subtitle = "Absence de prise en compte des inégalités") +
  
  geom_sf(aes(fill=HDI)) +
  scale_fill_gradientn("IDH", colors=c("#DFD0BD", "#5B4A54")) +

  geom_sf(aes(geometry = geometry.1, color=clusters, size=Inequality.in.income....)) +

  scale_color_manual("Classes IDH", values=c("#E64814", "#E9CB0D", "#9CDE3B"), 
                     breaks=c("1", "2", "3"), labels=c("Bas", "Moyen", "Élevé")) +

  scale_size(name="Inégalité d'income") +
  theme_tufte() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = guide_colourbar(order=1),
         color = guide_legend(order=2),
         size = guide_legend(order=3))

map_HDI_School

grid.arrange(
  map_HDI_School, 
  bottom="Les limites de l'IDH \n Absence de prise en compte des inégalités (exemple du salaire)"
)

#Fonction Title
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

l <- list(map_HDI_School, test)

grid.arrange(
  grobs = l,
  widths = c(3),
  heights = c(3, 1),
  layout_matrix = rbind(c(1),
                        c(2))
)


# Second graph IDH/IDHI

africa_data$RatioIDHI_IDH <- africa_data$Inequality.adjusted.HDI..IHDI. / africa_data$HDI

africa_data %>% drop_na(Inequality.adjusted.HDI..IHDI.) %>% 
  ggplot(aes(x = reorder(Country, HDI), y=RatioIDHI_IDH, fill=clusters)) +
  geom_col(width = 0.9)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 50, hjust=1))
  
