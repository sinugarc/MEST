#Transformaciones

#El análisis de residuos da problemas cuando la relación entre respuesta y covariables no es lineal.(Podemos comprobarlo graficando)
#El objetivo es hacer transformaciones (en las variables explicativas o en la variable respuesta) con el objetivo de linealizar el modelo.
#Una vez aplicada una transformación, debemos comprobar que el problema se ha resuelto.


#Colinealidad

#Decimos que hay colinealidad exacta si t(X)%*%X es singular (determinante=0 -> no tiene inversa)
#Hay multicolinealidad cuando t(X)%*%X es prácticamente singular (determinante muy próximo a 0)
#k -> Me indica la colinealidad
#Si k entre 30 y 100 -> dependencia moderada
#Si k mayor que 100 -> graves problemas de colinealidad 
#Índice de condición: Sirve para ver dónde está el problema de colinealidad (me indica la variable con el problema)
#Si k_j entre 30 y 100 -> Colinealidad moderada a fuerte
#Si k_j entre 500 y 1000 -> Problema de colinealidad grave
#También podemos utilizar el R^2 (R-> Coeficiente de correlación lineal de Pearson)
#Factor de inflación de varianza -> VIF


#Transformaciones para la no normalidad y varianzas distintas del error (o heterocedasticidad)

#Tenemos varianzas desconocidas e iguales. Vamos a utilizar otro estadístico (el que estábamos usando no sirve porque habíamos supuesto que las varianzas eran conocidas e iguales)
#Transformación por el método de máxima verosimilitud: Primero hay que estimar el valor lambda. Para ello, utilizamos boxcox() para el modelo, y obtenemos el valor de lambda. Si lambda=0 tomamos lnY, si no, tomamos la otra fórmula del PDF.


#EJERCICIOS EN R

#Ejemplo de cómo importar los datos a R

getwd() #Nos devuelve el directorio de trabajo
read <-read.csv("Nombre del archivo.csv")

#Observaciones influyentes (Ejercicio 6 de la hoja)

  #v)
install.packages("ggplot2")
install.packages("PASWR2")
install.packages("leaps")
install.packages("car")
library(leaps)
library(PASWR2)
library(car)
library(ggplot2)
model3.lm <- lm(hwfat~ age + abs + triceps, data=HSWRESTLER)
head(HSWRESTLER)

#Utilizar la función fortify()

fmod <- fortify(model3.lm)
head(fmod, n=3) #Si quitamos el n, tendríamos tantos valores como observaciones. Ahora solo le pedimos que nos saque los 3 primeros

x <- fmod$fitted
y <- fmod$stdresid
plot(x,y, xlab="Valores ajustados", ylab="Residuos studentalizados", ylim=c(-2,3), xlim=c(5,20))
segments(5,0,20,0)
sort(abs(rstandard(model3.lm)), decreasing=TRUE)[1:3]
#Otra forma
sort(abs(fmod$.stdresid), decreasing=TRUE)[1:3]
BCV <- qt(1-0.2/(2*78),73)  #Bonferroni Critic Value
BCV

sum(abs(rstudent(model3.lm))>BCV) #Número de residuos studentalizados mayores que BCV

max(abs(rstudent(model3.lm)))
which.max(abs(rstudent(model3.lm)))

library(car)
outlierTest(model3.lm)
influenceIndexPlot(model3.lm) #Las observaciones influyentes son (22,27,32,35 y 60). Ahora hay que hacer el modelo sin esas observaciones, porque son outliers

#x)
install.packages("ISLR")
library(ISLR)
install.packages("PASWR")
install.packages("leaps")
library(leaps)
library(PASWR)
obs.out <- c(22,27,32,35,60) #Observaciones a quitar
HSW <- HSWRESTLER[-obs.out, 1:7]  #Deberíamos quitarlas de 1 en 1 y no todas de golpe
model3sin <- lm(hwfat~age+abs+triceps, data=HSW)
model3sin
summary(model3sin)
summary(model3.lm)
modelall <- lm(hwfat~., data=HSW)
modelall
set.seed(5)
train <- sample(c(TRUE, FALSE), size=nrow(HSWRESTLER[]))




