#=========================================================================================================
# Proyecto Probabilidad y estadistica 1
# Giancarlo Gonzalez
#=========================================================================================================

#se carga la base de datos y se lee
library(readxl)
library(aplpack)
library(ggplot2)
library(car)
library(boot)
library(QuantPsyc)
library(tidyverse)
datos_proyecto <- read_excel("datos_proyecto.xlsx")
View(datos_proyecto)


#-------------
#Obtencion de datos por columnas

nombre <- datos_proyecto[,2]
equipo <- datos_proyecto[,3]
edad <- datos_proyecto[,4]
estatura <- datos_proyecto[,5]
peso <- datos_proyecto[,6]
universidad <- datos_proyecto[,7]
pais <- datos_proyecto[,8]

draft_year <- datos_proyecto[,9]
draft_round <- datos_proyecto[,10]
draft_number <- datos_proyecto[,11]

gp <- datos_proyecto[,12]
pts <- datos_proyecto[,13]
reb <- datos_proyecto[,14]
ast <- datos_proyecto[,15]

net_rating <- datos_proyecto[,16]
oreb_pct <- datos_proyecto[,17]
dreb_pct <- datos_proyecto[,18]
usg_pct <- datos_proyecto[,19]
ts_pct <- datos_proyecto[,20]
ast_pct <- datos_proyecto[,21]

season <- datos_proyecto[,22]

#-------------------------------------------
#Análisis estadístico

#resumen
summary(datos_proyecto)

#tablas
table(datos_proyecto$team_abbreviation, datos_proyecto$age)

options(max.print=999999)
table(datos_proyecto$team_abbreviation, datos_proyecto$college)



#----------------------------------
#diagramas de barras
#edad
barplot(table(edad))



#---------------------------------
#tortas
#torta de nacionalidades
hist(table(datos_proyecto$country))

#----------------------------------
#histogramas
#peso
hist(datos_proyecto$player_weight)

#estatura
hist(datos_proyecto$player_height)

#pts
hist(datos_proyecto$pts)

#reb
hist(datos_proyecto$reb)

#asis
hist(datos_proyecto$ast)

#net-rating
hist(datos_proyecto$`net_rating(eficienia def-att)`)

#oreb-pct
hist(datos_proyecto$`oreb_pct rebs ofensivos`)

#dreb-pct
hist(datos_proyecto$`dreb_pct rebs def`)

#usg-pct
hist(datos_proyecto$`usg_pct uso`)

#ts-pct
hist(datos_proyecto$`ts_pct tiro real`)

#ast-pct
hist(datos_proyecto$`ast_pct porcentaje asis`)

#----------------------------------
#Plot

#altura-peso
plot(datos_proyecto$player_height, datos_proyecto$player_weight)

#plot edad - patidos jugados
plot(datos_proyecto$age, datos_proyecto$`gp-partidos jugados`, xlab="edad", ylab="partidos jugados")

#edad
plot(datos_proyecto$age, datos_proyecto$pts)

#Caras de Chernoff primeros 4 años 
caras1<-sample

# Estimadores

# Media
attach(datos_proyecto)
me_age=mean(age)
me_height=mean(player_height)
me_weight=mean(player_weight)
me_gp=mean(gp)
me_pts=mean(pts)
me_reb=mean(reb)
me_ast=mean(ast)

# Varianza
va_age=var(age)
va_height=var(player_height)
va_weight=var(player_weight)
va_gp=var(gp)
va_pts=var(pts)
va_reb=var(reb)
va_ast=var(ast)

# Desviacion Estandar
sd_age=sd(age)
sd_height=sd(player_height)
sd_weight=sd(player_weight)
sd_gp=sd(gp)
sd_pts=sd(pts)
sd_reb=sd(reb)
sd_ast=sd(ast)

# Intervalos de confianza bilateral a un nivel de significancia del .95
# Significa que con una prob del 0.95 voy a encontrar mi media en ese intervalo
t.test(age, conf.level=0.95)$conf.int
t.test(player_height, conf.level=0.95)$conf.int
t.test(player_weight, conf.level=0.95)$conf.int
t.test(gp, conf.level=0.95)$conf.int
t.test(pts, conf.level=0.95)$conf.int
t.test(reb, conf.level=0.95)$conf.int
t.test(ast, conf.level=0.95)$conf.int

# Pruebas de hipotesis
# H0: M1-M0=0      Ha: M1-M2 != 0 don M es la media de puntos de la n esima temporada
# nivel de significancia 0.05
M1<-datos_proyecto[sample(1:440,400),6]
M2<-datos_proyecto[sample(10632:11145,400),6]
# desviacion estadar de las muestras 
sd_M1<-sd(M1)
sd_M2<-sd(M2)
# media de las muestras
me_M1<-mean(M1)
me_M2<-mean(M2)
# Estadistico de prueba
z<-(me_M1-me_M2)/(sqrt(((sd_M1)^2/400)+((sd_M2)^2/400)))
# alpha= 1.96


# Regresion lineal simple

re_m1<-datos_proyecto[sample(1:440,400),]
re_m2<-datos_proyecto[sample(10632:11145,400),]
# Modelo 1996-altura-puntos
modelo96=lm(pts~player_height, data=re_m1)
grafica96<-ggplot(re_m1, aes(player_height,pts))
grafica96 + geom_point() + geom_smooth(method = "lm",color="Red")
summary(modelo96)
# modelo 2019-altura-puntos
modelo20=lm(pts~player_height, data=re_m2)
grafica20<-ggplot(re_m2, aes(player_height,pts))
grafica20 + geom_point() + geom_smooth(method = "lm",color="Blue")
summary(modelo20)
# Modelo 1996-altura-rebotes
modelo96_1=lm(reb~player_height, data=re_m1)
grafica96_1<-ggplot(re_m1, aes(player_height,reb))
grafica96_1 + geom_point() + geom_smooth(method = "lm",color="Red")
summary(modelo96_1)
# modelo 2019-altura-rebotes
modelo20_1=lm(reb~player_height, data=re_m2)
grafica20_1<-ggplot(re_m2, aes(player_height,reb))
grafica20_1 + geom_point() + geom_smooth(method = "lm",color="Blue")
summary(modelo20_1)
# Modelo 1996-altura-asistencias
modelo96_2=lm(ast~player_height, data=re_m1)
grafica96_2<-ggplot(re_m1, aes(player_height,ast))
grafica96_2 + geom_point() + geom_smooth(method = "lm",color="Red")
summary(modelo96_2)
# modelo 2019-altura-asistencias
modelo20_2=lm(ast~player_height, data=re_m2)
grafica20_2<-ggplot(re_m2, aes(player_height,ast))
grafica20_2 + geom_point() + geom_smooth(method = "lm",color="Blue")
summary(modelo20_2)
# R-squared = 0.25
# el modelo explica el 25% de la variabilidad de Y
# por lo que el modelo no es bueno 