d1=read.table("./dataset/student-mat.csv",sep=",",header=TRUE)
d2=read.table("./dataset/student-por.csv",sep=",",header=TRUE)

allnames <- names(d1)

factornames <- allnames[! names(d1) %in% c("age","G1", "G2", "G3", "absences")]
d1[,factornames] <- lapply(d1[, factornames], factor)


d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

## Anova sur les données portugais 
require(car)

dataAnova <- d2[, c("G1", "G2", "G3", "Dalc", "Walc")]

## Modèle avec moyenne générale

dataAnova["Moy"] <- round((dataAnova$G1 + dataAnova$G2 + dataAnova$G3)/3,2)

regLin <- lm(Moy~Dalc+Walc, data = dataAnova)
anova(regLin)

regLinDalc <- lm(Moy~Dalc, data = dataAnova)
anova(regLin, regLinDalc)

summary(regLinDalc)

## Modèles par année

regLinG1 <- lm(G1~Dalc+Walc, data = dataAnova)
Anova(regLinG1, type="III")

regLinG2 <- lm(G2~Dalc+Walc, data = dataAnova)
Anova(regLinG2, type="III")

regLinG3 <- lm(G3~Dalc+Walc, data = dataAnova)
Anova(regLinG3, type="III")

## seulement la consommation 

regLinG1Dalc <- lm(G1~Dalc, data = dataAnova)
Anova(regLinG1Dalc, type="III")

regLinG2Dalc <- lm(G2~Dalc, data = dataAnova)
Anova(regLinG2Dalc, type="III")

regLinG3Dalc <- lm(G3~Dalc, data = dataAnova)
Anova(regLinG3Dalc, type="III")

summary(regLinG1Dalc)
summary(regLinG2Dalc)
summary(regLinG3Dalc)

## first graph

library(ggplot2)
library(wesanderson)
library(ggthemes)

dataAnova$MoyClass <- rep("Dino", 649)

MoyClass <- apply(dataAnova, MARGIN = 1, FUN= function(x){
  if(as.numeric(x["Moy"]) <= 5){
   return("0-5")
  }
  if((as.numeric(x["Moy"]) > 5) && (as.numeric(x["Moy"]) <=10)){
    return("5-10")
  }
  if((as.numeric(x["Moy"]) > 10) && (as.numeric(x["Moy"]) <=15)){
    return("10-15")
  }
  if(as.numeric(x["Moy"]) > 15){
    return("15-20")
  }
})

dataAnova$MoyClass <- MoyClass

plot(density(dataAnova$Moy))

#essai 1

ggplot(dataAnova) + aes(x = Dalc, y = Walc, color=MoyClass) +
  labs(title = "Notes Moyennes en fonction de la consommation d'alcool", 
       x="Consommation Quotidienne", 
       y="Consommation le Week-end") +
  #theme_tufte()+
  geom_jitter(size=0.8, alpha=0.8, width = 0.15)+
  scale_color_manual(values = c("#E53C0E", "#E9CB0D", "#9CDE3B", "#16A069"),
                     breaks = c("0-5", "5-10", "10-15", "15-20")
                     ) +
  theme_light()+
  theme_minimal()
  
# essai 2: 

ggplot(dataAnova) + aes(x = Dalc, y = Walc, color=MoyClass) +
  labs(title = "Notes Moyennes en fonction de la consommation d'alcool", 
       x="Consommation Quotidienne", 
       y="Consommation le Week-end") +
  #theme_tufte()+
  geom_jitter(size=0.8, alpha=0.8, width = 0.15)+
  scale_color_manual(values = c("#E53C0E", "#E9CB0D", "#9CDE3B", "#16A069"),
                     breaks = c("0-5", "5-10", "10-15", "15-20")
  ) +
  theme_light()+
  theme_minimal()
