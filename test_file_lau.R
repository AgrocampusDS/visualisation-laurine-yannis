v_tot <- colnames(data2)

# Regroupements de variables
# ==========================

v_basic <- c("Country",
             "X", 
             "Id", 
             "ISO.3166.Country.Code", 
             "Latitude", 
             "Longitude"
)

v_HDI <- c("HDI.Rank", 
           "HDI", 
           "Change.in.HDI.rank.2010.2015", 
           "Average.annual.HDI.growth.1990.2000", 
           "Average.annual.HDI.growth.2000.2010", 
           "Average.annual.HDI.growth.2010.2015",
           "Average.annual.HDI.growth.1990.2015"
)

v_edu <- c("Mean.years.of.schooling",
           "Mean.years.of.schooling.Female",                                               
           "Mean.years.of.schooling.Male",
           "Population.with.at.least.some.secondary.education....2005.2015..Female",       
           "Population.with.at.least.some.secondary.education....2005.2015..Male", 
           "Inequality.in.education...",                                                   
           "Inequality.adjusted.education.index"
)


lm <- lm(data = africa_data[which(HCPC_2$data.clust$clust == 1),], Internet.users ~ Gender.Development.Index.value)
plot(lm)
anova(lm)
plot(data = africa_data[which(HCPC_2$data.clust$clust == 1),], Internet.users ~ Gender.Development.Index.value)

v_gender <- c("Gender.Development.Index.value",                                               
              "Gender.Development.Index.Group",                                               
              "Human.Development.Index..HDI..Female",                                         
              "Human.Development.Index..HDI..Male",                                           
              "Life.expectancy.at.birth.Female",                                              
              "Life.expectancy.at.birth.Male",
              "Mean.years.of.schooling.Female",                                               
              "Mean.years.of.schooling.Male",                                                 
              "Estimated.gross.national.income.per.capita.Female",                            
              "Estimated.gross.national.income.per.capita.Male",                              
              "Share.of.seats.in.parliament....held.by.women.",                               
              "Population.with.at.least.some.secondary.education....2005.2015..Female",       
              "Population.with.at.least.some.secondary.education....2005.2015..Male"         
              
              )  



# ACP et clustering
# =================

library(FactoMineR)
library(Factoshiny)


## --- Evolution de l'HDI 

data_ACP_1 <- data.frame(africa_data[,c("Average.annual.HDI.growth.1990.2000", 
                               "Average.annual.HDI.growth.2000.2010", 
                               "Average.annual.HDI.growth.2010.2015",
                               "HDI", 
                               "Gender.Development.Index.Group")])[,-6]

rownames(data_ACP_1) <- africa_data$Country

ACP_1 <- PCA(data_ACP_1, quanti.sup = 4, quali.sup = 5, graph = F)
Factoshiny(ACP_1)


## --- HDI Classique 

data_ACP_2 <-data.frame(africa_data[,c("Life.expectancy", 
                             "Mean.years.of.schooling",
                             "gdp_md_est",
                             "HDI", 
                             "Inequality.adjusted.HDI..IHDI.",
                             "Inequality.in.education...")])[,-7]

rownames(data_ACP_2) <- africa_data$Country

ACP_2 <- PCA(data_ACP_2, quanti.sup = 4:6, graph = F)
# Factoshiny(ACP_2_clust)
HCPC_2 <- HCPC(ACP_2, nb.clust = 3, consol = T, metric = "euclidean", method = "ward")


clusters <- HCPC_2$data.clust$clust #liste du numéro de cluster de chaque pays 

clust1 <- data_ACP_2[which(HCPC_2$data.clust$clust == 1),]
clust2 <- data_ACP_2[which(HCPC_2$data.clust$clust == 2),]
clust3 <- data_ACP_2[which(HCPC_2$data.clust$clust == 3),]


ACP_2_clust <- PCA(data.frame(data_ACP_2, cluster = as.factor(HCPC_2$data.clust$clust)), 
                   quanti.sup = 4:6, 
                   quali.sup = 7, 
                   graph = F)

plot.PCA(ACP_2_clust,
         choix='var',
         title="Graphe des variables de l'ACP",
         col.quanti.sup='#0000FF')

plot.PCA(ACP_2_clust,
         invisible=c('ind.sup'),
         habillage=7,
         title="Graphe des individus de l'ACP (IDH)",
         label =c('ind','quali'),
         col.hab = c("#f3a815", "#0c4f50", "#de2c1e"))


## --- IHDI

data_ACP_3 <-data.frame(africa_data[,c("Inequality.adjusted.life.expectancy.index", 
                                       "Inequality.adjusted.education.index",
                                       "Inequality.adjusted.income.index",
                                       "HDI", 
                                       "Inequality.adjusted.HDI..IHDI.",
                                       "Inequality.in.education...")])[,-7]

rownames(data_ACP_3) <- africa_data$Country

ACP_3 <- PCA(data_ACP_3, quanti.sup = 4:6, graph = F)
HCPC_3 <- HCPC(ACP_3, nb.clust = 3, consol = T, metric = "euclidean", method = "ward")

clusters_i <- HCPC_3$data.clust$clust #liste du numéro de cluster de chaque pays 

clust1_i <- data_ACP_3[which(HCPC_3$data.clust$clust == 1),]
clust2_i <- data_ACP_3[which(HCPC_3$data.clust$clust == 2),]
clust3_i <- data_ACP_3[which(HCPC_3$data.clust$clust == 3),]


ACP_3_clust <- PCA(data.frame(data_ACP_3, cluster = as.factor(HCPC_3$data.clust$clust)), 
                   quanti.sup = 4:6, 
                   quali.sup = 7, 
                   graph = F)
Factoshiny(ACP_3_clust)

plot.PCA(ACP_3_clust,
         choix='var',
         title="Graphe des variables de l'ACP (IDHI)",
         col.quanti.sup='#0000FF')

plot.PCA(ACP_3_clust,
         invisible=c('ind.sup'),
         habillage=7,
         title="Graphe des individus de l'ACP",
         label =c('ind','quali'), 
         col.hab = c("#f3a815", "#0c4f50", "#de2c1e"))
