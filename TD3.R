setwd(dir="L:/BUT/SD/Promo 2024/tmargerand/U1/programmation statistique/TD3")
install.packages("readxl")
library(readxl)

pokemon <- read_excel(path = "pokemon.xlsx",sheet = "pokemon")
View(pokemon)
dim(pokemon)
nrow(pokemon)
ncol(pokemon)

summary(pokemon)
pokemon$is_legendary <-as.factor(pokemon$is_legendary)
pokemon$generation <-as.factor(pokemon$generation)
pokemon$type <-as.factor(pokemon$type)
summary(pokemon)

pokemon$attack_group=ifelse(test=pokemon$attack>mean1,yes="attack+",no="attack-")

pokemon$water_fire=ifelse(test=pokemon$type %in% c("water","fire"),yes="yes",no="no")
pokemon$water_fire=as.factor(pokemon$water_fire)
summary(pokemon)

a3=quantile(pokemon$attack,probs=0.75)
d3=quantile(pokemon$defense, probs=0.75)
v3=quantile(pokemon$speed, probs=0.75)
pokemon$best=ifelse(test=pokemon$attack>a3 & pokemon$defense>d3 & pokemon$speed>v3,yes="yes",no="no")
pokemon$best=as.factor(pokemon$best)
summary(pokemon)

requete = subset(pokemon, is.na(weight_kg))
View(requete)

requete$weight_kg=mean(pokemon$weight_kg)
View(requete)
requete2 = subset(pokemon, !is.na(weight_kg))
View(requete2)

med_weight=median(pokemon$weight_kg,na.rm=TRUE)
med_height=median(pokemon$height_m,na.rm=TRUE)
pokemon$weight_kg_NA=ifelse(test=is.na(pokemon$weight_kg),med_weight,pokemon$weight_kg)
pokemon$height_m_NA=ifelse(is.na(pokemon$height_m),med_height,pokemon$height_m)

pokemon$weightgroup=cut(pokemon$weight_kg,breaks=3,labels=c("léger","moyen","lourd"))
View(pokemon)

pokemon$height_m_group = cut(pokemon$height_m,
                             breaks = c(0,1,2,3,
                                        max(pokemon$height_m,
                                            na.rm = TRUE)))
View(pokemon)

pokemon$defense_group = cut(pokemon$defense,
                            breaks = quantile(pokemon$defense,
                                              na.rm = TRUE),
                            include.lowest = TRUE)
summary(pokemon$defense_group)

aggregate(x = attack ~ type, 
          data = pokemon,
          FUN = function(x) mean(x))

aggregate(x = attack ~ generation + type,
          data = pokemon, 
          FUN = function(x) median(x))
aggregate(x=pokedex_number~type,pokemon,FUN = function(x) length(x))

aggregate(speed ~ generation + type,
          data = pokemon, 
          FUN = function(x) c(moy = mean(x),
                              med = median(x),
                              eff = length(x) ) )