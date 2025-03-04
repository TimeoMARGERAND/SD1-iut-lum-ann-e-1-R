setwd("L:/BUT/SD/Promo 2024/tmargerand/U1/programmation statistique/TP2")
df<-read.csv("fao.csv", sep=";", dec=",", header = TRUE)

nrow(df)
summary(df)

mean(df$Dispo_alim, na.rm = TRUE)
mean(df$Population, na.rm = TRUE)
sum(df$Population, na.rm = TRUE)
sd(df$Export_viande, na.rm = TRUE)
sd(df$Import_viande, na.rm = TRUE)
median(df$Prod_viande, na.rm = TRUE)
quantile(df$Dispo_alim, probs = c(0.25,0.5))'pour choisir les quantile, utiliser un vecteur'
quantile(df$Import_viande, probs = seq(0,1,0.1))

rang = order(df$Population)
q1 = head(df[rang,],5)
View(q1)

rang2 = order(df$Population, decreasing = TRUE)
q2 = head(df[rang2,],5)
View(q2)

rang3 = order(df$Prod_viande, decreasing = TRUE)
q3 = head(df[rang3,], n=5)
View(q3)

rang4 = order(df$Import_viande)
q4 = head(df[rang4,], n = 5)
View(q4)

q5 = subset(df, df$Dispo_alim >= 2300)
View(q5)

q6 = subset(df, df$Dispo_alim>=3500 & df$Import_viande>1000)
View(q6)

q7 = subset(df,df$Nom %in% c("France", "Belgique"))
View(q7)

df$Partexport = df$Export_viande/df$Prod_viande

df$dispo_alim_pays = df$Dispo_alim*df$Population

write.table(df, file = "ExportTp2.csv" )

q8 = sum(df$dispo_alim_pays, na.rm = T)
q8

q9 = q8/2300
q9

plot(x = df$Prod_viande, y = df$Export_viande, main = "lien entre production et exportation de viande")

matriceCor = cor(df[ , - 1] , use = "complete.obs")
matriceCor = round(matriceCor , 2)
View(matriceCor)

install.packages("corrplot")
library(corrplot)
corrplot(matricecor, method ="circle")