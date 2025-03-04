getwd()
setwd(dir ="L:/BUT/SD/Promo 2024/tmargerand/U1/programmation statistique/TD2")
getwd()
bodies_karts=read.csv(file = "bodies_karts.csv", header = TRUE, sep =";",dec = ",")
drivers=read.csv(file = "drivers.csv", header = TRUE, sep =";",dec = ",")
gliders=read.csv(file = "gliders.csv", header = TRUE, sep ="|",dec = ".")
tires=read.csv(file = "tires.csv", header = TRUE, sep = "\t",dec = ",")

dim(bodies_kart)
dim(tires)
dim(gliders)
dim(drivers)

summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)

cor(x=drivers$Weight,y=drivers$Acceleration)
cov(x=drivers$Weight,y=drivers$Acceleration)/(sd(drivers$Weight)*sd(drivers$Acceleration))
coefCorr = cor(x = drivers$Weight,
               y = drivers$Acceleration)
coefDeter = coefCorr^2
print(coefDeter)

matriceCor = cor(drivers[ , - 1])
matriceCor = round(matriceCor , 2)
View(matriceCor)

install.packages("corrplot")
library(corrplot)
corrplot(matriceCor, method = "circle")

matricecorr2 = cor(tires[,-1])
matricecorr2 = round(matricecorr2,2)
corrplot(matricecorr2, method = "circle")

install.packages("corrplot")
matricecorr3 = cor(gliders[,-1])
matricecorr3 = round(matricecorr3,1)
corrplot(matricecorr3, method = "circle")

resultat = drivers[ , c("Driver" , "Weight")]
View(resultat)

resultat = drivers[ 1:10 , c("Driver" , "Acceleration")]
View(resultat)

resultat = drivers[ , -c(5,7,9)]
View(resultat)

resultat = drivers[ , -c(2,3)]
View(resultat)

resultat = drivers[ c(32,3,12) , ]
View(resultat)

rang = order(drivers$Weight)
resultat = drivers[ rang  , c("Driver", "Weight") ]
View(resultat)
help(subset)
topDriver = subset(x = drivers,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Driver","Acceleration"))
topGlider = subset(x = gliders,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Glider","Acceleration"))