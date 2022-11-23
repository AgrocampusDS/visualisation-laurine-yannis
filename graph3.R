#graph3

# step1 : find the right countries to compare

dta1 <- data.frame(africa_data[,c("HDI", 
                          "Inequality.adjusted.HDI..IHDI.")])[,-3]

dta1 <- data.frame(c(dta1, diff = dta1[1]-dta1[2]))

rownames(dta1) <- africa_data$Country

# dta1[which.max(dta1$diff),] # Botswana (5)
dta1[which.min(dta1$diff),] # Niger (33)
dta1[46,] # South Africa (46)

dta.NGR <- data.frame(Group = c("HDI", "IHDI"))
dta.RSA <- data.frame(Group = c("HDI", "IHDI"))

# step2: recalculate sub indexes 

# dta.hdi <- data.frame(africa_data[,c("Life.expectancy", 
#                                      "Mean.years.of.schooling",
#                                      "Gross.national.income..GNI..per.capita")])[,-4]
# rownames(dta.hdi) <- africa_data$Gross.national.income..GNI..per.capita
# 
# dta.hdi <- dta.hdi[c(33, 46),]
# 
# 
# dta.hdi <- data.frame(c(dta.hdi, 
#                         IESP = (dta.hdi[1] - 25)/(85 - 25),
#                         INI = dta.hdi[2]/15, 
#                         IPIB = (log(dta.hdi[3])-log(100)) /(log(40000) -log(100))))
# dta.hdi <- data.frame(c(dta.hdi, 
#                         IDH.calc = (dta.hdi[4] + dta.hdi[5] + dta.hdi[6]) /3))




dta.hdi <- data.frame(africa_data[,c("Life.expectancy", 
                                     "Mean.years.of.schooling",
                                     "Gross.national.income..GNI..per.capita", 
                                     "Inequality.adjusted.life.expectancy.index", 
                                     "Inequality.adjusted.education.index",
                                     "Inequality.adjusted.income.index")])[,-7]
rownames(dta.hdi) <- africa_data$Gross.national.income..GNI..per.capita
dta.hdi <- dta.hdi[c(33, 46),]

dta.hdi <- data.frame(c(dta.hdi, 
                        IESP = (dta.hdi[1] - 25)/(85 - 25),
                        INI = dta.hdi[2]/15, 
                        IPIB = (log(dta.hdi[3])-log(100)) /(log(40000) -log(100))))

names(dta.hdi[,(4:6)])<-names(dta.hdi[,(1:3)])


HDI.Niger <- dta.hdi[1,7:9]
HDI.RSA <- dta.hdi[2,7:9]
IHDI.Niger <- dta.hdi[1,4:6]
IHDI.RSA <- dta.hdi[2,4:6]

names(HDI.Niger) <- c("Life expectancy", "Education", "Income")
names(IHDI.Niger) <- c("Life expectancy", "Education", "Income")
names(HDI.RSA) <- c("Life expectancy", "Education", "Income")
names(IHDI.RSA) <- c("Life expectancy", "Education", "Income")

# rbind(HDI.Niger, IHDI.Niger)
# rbind(HDI.RSA, IHDI.RSA)

dta.Niger <- data.frame(Group = c("HDI", "IHDI"), rbind(HDI.Niger, IHDI.Niger))
dta.RSA <- data.frame(Group = c("HDI", "IHDI"), rbind(HDI.RSA, IHDI.RSA))

library(tidyverse)
library(ggplot2)
library(ggradar)

dta.radar <- dta.hdi[4:6]
dta.radar <- data.frame(Country = c("Botswana", "Niger"), dta.radar)
dta.radar <- data.frame(t(dta.radar))

ggradar(dta.radar)

ggradar(dta.Niger, 
        axis.labels = c("Life expectancy index", "Education index", "Income index"),
        axis.label.size = 4,
        values.radar = c("0", "0.5", "1"))
ggradar(dta.RSA)

#https://r-charts.com/ranking/radar-chart/


library(fmsb)


dta.Niger.fmsb <- dta.Niger[-1]
names(dta.Niger.fmsb) <- c("Life expectancy", "Education", "Income")
min.max = data.frame(rbind (c(1,1,1), c(0,0,0)))
names(min.max) <- c("Life expectancy", "Education", "Income")
dta.Niger.fmsb <- rbind(data.frame(), min.max, dta.Niger.fmsb)

dta.RSA.fmsb <- dta.RSA[-1]
names(dta.RSA.fmsb) <- c("Life expectancy", "Education", "Income")
min.max = data.frame(rbind (c(1,1,1), c(0,0,0)))
names(min.max) <- c("Life expectancy", "Education", "Income")
dta.RSA.fmsb <- rbind(data.frame(), min.max, dta.RSA.fmsb)



data.frame (c(1,1,1), c(2,2,2))

# choix des couleurs
areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 0, 1, 0.25))

radarchart(dta.Niger.fmsb, 
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = c(2,4),   # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas, 
           title = "Décomposition des indicateurs IDH et IDHI du Niger", 
           maxmin = TRUE, 
           paxislab = 1:5,
           seg = 4,
           axislabcol = "grey", 
           axistype = 4, 
           caxislabels = c("0.0","0.25", "0.50", "0.75","1.0"))

legend("topright",
       legend = c("HDI", "IHDI"),
       bty = "n", pch = 20, col = areas,
       text.col = "grey25", pt.cex = 2)


# RSA
radarchart(dta.RSA.fmsb, 
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = c(2,4),   # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas, 
           title = "Décomposition des indicateurs IDH et IDHI du Niger", 
           maxmin = TRUE, 
           paxislab = 1:5,
           seg = 4,
           axislabcol = "grey", 
           axistype = 4, 
           caxislabels = c("0.0","0.25", "0.50", "0.75","1.0"))

legend("topright",
       legend = c("HDI", "IHDI"),
       bty = "n", pch = 20, col = areas,
       text.col = "grey25", pt.cex = 2)


# radarchart(dat, 
#            axistype=1,
#            seg=5, 
#            plty=1, 
#            vlabels=c("Total\nQOL", "Physical\naspects", 
#                      "Phychological\naspects", "Social\naspects", "Environmental\naspects"), 
#            title="(axis=1, 5 segments, with specified vlabels)", 
#            vlcex=0.5)
