##Validación del modelo##

#Siempre hay que validar el modelo con un conjunto distinto al de entrenamiento (con el conjunto de test).



##PASOS PARA EL TRABAJO##

#Elegimos el conjunto de datos. Hay que describir el número de observaciones, variables y de dónde hemos sacado los datos.
#Podemos describir solo las variables que entraron en nuestro modelo final, podemos obviar la descripción de las demás.
#Pre-proceso de datos (miramos que las variables sean consistentes). Estudio descriptivo
#Boxplot de las variables. Si me salen cosas raras, hago un summary de la variable y veo si hay valores extraños y de qué tipo. Corregimos valores extraños.
#Creamos el modelo de regresión.
#Vamos a utilizar validación cruzada de orden k, con k primo. (También se podría hacer validación cruzada dejando uno fuera en cada caso, y el error de test sería la media de los errores de test obtenidos al dejar cada vez uno fuera)
#Separamos en conjunto de entrenamiento y conjunto de test. (70-30%, 75-25%)
#Para cada trozo, la validación cruzada nos va a decir qué conjunto es mejor. Lo vamos a ver con el método por pasos o por criterios.
#Tras eso, nos vamos a quedar con el mejor/mejores conjuntos de los obtenidos en el paso anterior.
#Al hacer el error de test, nos quedamos con el/los que menor error de test tenga/tengan. Si dos tienen el mismo error de test, me quedo con el que menos variables tenga.
#Comprobamos la normalidad, homocedasticidad y la colinealidad.
#Si alguna de las anteriores no se tiene (las 2 primeras) o hay colinealidad, vemos si podemos hacer transformaciones para resolver el problema del paso anterior.
#Tras aplicar las transformaciones pertinentes, volvemos a pasar el modelo.
#Outlier test e InfluenceIndexPlot. Si hay observaciones influyentes, las quitamos y volvemos a pasar el modelo.
#Aquí ya habríamos terminado la validación cruzada.
#Pueden pasar 2 cosas: todas las variables son significativas o no.



##EJERCICIO 6##

#x)
install.packages("ISLR")
library(ISLR)
install.packages("PASWR")
install.packages("leaps")
library(leaps)
library(PASWR)
obs.out <- c(22,27,32,35,60) #Observaciones a quitar
HSW <- HSWRESTLER[-obs.out, 1:7]  #Deberíamos quitarlas de 1 en 1 y no todas de golpe
set.seed(5)
train <- sample(c(TRUE, FALSE), size=nrow(HSwrestler[-obs.out,]), replace=TRUE, prob=c(0.7, 0.3))
prop.table(table(train))
test <- (!train)
prop.table(table(test))

#x1)

#model.exh <- regsubsets(HWFAT~., data=HSwrestler[train, 1:7], method=)


