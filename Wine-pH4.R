#Buscamos el directorio de trabajo
getwd()

#Utilizamos la ruta desde el directorio de trabajo
datos <- read.csv("/Users/alicia/Práctica 1/winequality-red.csv")

#Instalamos los paquetes necesarios
install.packages("leaps")
library(leaps)
install.packages("MASS") 
library(MASS)
install.packages("ggplot2") 
library(ggplot2)

#Información básica de nuestro conjunto de datos

head(datos) #Nos devuelve las primeras seis filas de nuestro conjunto de datos
str(datos)  #Nos devuelve los tipos de las variables contenidas en nuestro conjunto de datos
summary(datos)  #Nos devuelve un resumen de algunas características importantes de las variables de nuestro conjunto de datos

#Número de observaciones y número de variables
n=1599 #Número de observaciones
p=12 #Número de variables

##Análisis descriptivo##

#Asignamos las variables

f.a <- datos$fixed.acidity
v.a <- datos$volatile.acidity
c.a <- datos$citric.acid
r.s <- datos$residual.sugar
chlorides <- datos$chlorides
f.s.d <- datos$free.sulfur.dioxide
t.s.d <- datos$total.sulfur.dioxide
density <- datos$density
pH <- datos$pH #La tomamos como variable respuesta, por ser continua y distribuirse siguiendo una distribución normal.
sulphates <- datos$sulphates
alcohol <- datos$alcohol
quality <- datos$quality  #No sirve como variable respuesta por ser discreta.

#Realizamos un pequeño estudio de cada una de las variables

layout(matrix(1:4,ncol=2,byrow=TRUE))

#fixed acidity
summary(f.a)
hist(f.a, main="Histograma de la variable fixed acidity")
plot(f.a,main="Gráfica de dispersión de la variable fixed acidity") 
na_f.a=is.na(f.a)

#volatile acidity
summary(v.a)
hist(v.a, main="Histograma de la variable volatile acidity")
plot(v.a, main="Gráfica de dispersión de la variable volatile acidity") 
na_v.a=is.na(v.a)

layout(matrix(1:4,ncol=2,byrow=TRUE))

#citric acid
summary(c.a)
hist(c.a, main="Histograma de la variable citric acid")
plot(c.a, main="Gráfica de dispersión de la variable citric acid") 
na_c.a=is.na(c.a)

#residual sugar
summary(r.s)
hist(r.s, main="Histograma de la variable residual sugar")
plot(r.s, main="Gráfica de dispersión de la variable residual sugar") 
na_r.s=is.na(r.s)

layout(matrix(1:4,ncol=2,byrow=TRUE))

#chlorides
summary(chlorides)
hist(chlorides, main="Histograma de la variable chlorides")
plot(chlorides, main="Gráfica de dispersión de la variable chlorides") 
na_chlorides=is.na(chlorides)

#free sulfur dioxide
summary(f.s.d)
hist(f.s.d, main="Histograma de la variable free sulfur dioxide")
plot(f.s.d, main="Gráfica de dispersión de la variable free sulfur dioxide") 
na_f.s.d=is.na(f.s.d)

layout(matrix(1:4,ncol=2,byrow=TRUE))

#total sulfur dioxide
summary(t.s.d)
hist(t.s.d, main="Histograma de la variable total sulfur dioxide")
plot(t.s.d, main="Gráfica de dispersión de la variable total sulfur dioxide") 
na_t.s.d=is.na(t.s.d)

#density
summary(density)
hist(density, main="Histograma de la variable density")
plot(density, main="Gráfica de dispersión de la variable density") 
na_density=is.na(density)

layout(matrix(1:4,ncol=2,byrow=TRUE))

#pH
summary(pH)
hist(pH, main="Histograma de la variable pH")
plot(pH, main="Gráfica de dispersión de la variable pH") 
na_pH=is.na(pH)

#sulphates
summary(sulphates)
hist(sulphates, main="Histograma de la variable sulphates")
plot(sulphates, main="Gráfica de dispersión de la variable sulphates") 
na_sulphates=is.na(sulphates)

layout(matrix(1:4,ncol=2,byrow=TRUE))

#alcohol
summary(alcohol)
hist(alcohol, main="Histograma de la variable alcohol")
plot(alcohol, main="Gráfica de dispersión de la variable alcohol") 
na_alcohol=is.na(alcohol)

#quality
summary(quality)
hist(quality, main="Histograma de la variable quality")
plot(quality, main="Gráfica de dispersión de la variable quality") 
na_quality=is.na(quality)


#Train & Test

#Utilizamos el 70% del conjunto de datos como el conjunto de entrenamiento y el 30% restante como test

#Semilla 1

set.seed(1)
train1 <- sample(c(TRUE, FALSE), nrow(datos), replace=TRUE, prob=c(0.7, 0.3))
test1 <- (!train1)
#prop.table(table(train1))    #La tabla es demasiado grande para que funcione el comando
model.exh1 <- regsubsets(pH ~ ., data=train1)
summary(model.exh1)

#Semilla 2

set.seed(2)
train2 <- sample(c(TRUE, FALSE), nrow(datos), replace=TRUE, prob=c(0.7, 0.3))
test2 <- (!train2)
#prop.table(table(train2))
model.exh2 <- regsubsets(pH ~ ., data=train2)
summary(model.exh2)

#Semilla 3

set.seed(3)
train3 <- sample(c(TRUE, FALSE), nrow(datos), replace=TRUE, prob=c(0.7, 0.3))
test3 <- (!train3)
#prop.table(table(train3))
model.exh3 <- regsubsets(pH ~ ., data=train3)
summary(model.exh3)

#Semilla 4

set.seed(4)
train4 <- sample(c(TRUE, FALSE), nrow(datos), replace=TRUE, prob=c(0.7, 0.3))
test4 <- (!train4)
#prop.table(table(train4))
model.exh4 <- regsubsets(pH ~ ., data=train4)
summary(model.exh4)

#Semilla 5

set.seed(5)
train5 <- sample(c(TRUE, FALSE), nrow(datos), replace=TRUE, prob=c(0.7, 0.3))
test5 <- (!train5)
#prop.table(table(train5))
model.exh5 <- regsubsets(pH ~ ., data=train5)
summary(model.exh5)



##2. ESTUDIO Y EVALUACIÓN DEL MODELO COMPLETO##

#Creamos el modelo

model.completo <- lm(pH ~ ., data=datos)

#Tabla de resultados con el estudio de sus p-valores

summary(model.completo)

#Correlaciones

#cor.test(peso, long)   #Correlación entre 2 variables
#cov2cor()  #Matriz de varianzas-covarianzas


##3. SELECCIÓN DEL MEJOR MODELO##

#MÉTODOS POR PASOS

#Backward

#Comenzamos con un modelo que incorpora todas las variables explicativas potencialmente influyentes, model.completo.
summary(model.completo) #Miramos los p-valores correspondientes a los t-estadísticos de los bi_hat
drop1(model.completo, test="F") #Alternativa a summary. Como el p-valor asociado a volatile.acidity es el mayor, y es mayor que alpha=0.05, esa variable no es significativa y podemos excluirla del modelo
#Fijando el nivel de significación alpha=0.05, eliminamos del modelo la variable con un p-valor mayor, en este caso v.a (volatile acidity)
model_NO_v.a <- update(model.completo,.~.-volatile.acidity) #Modelo excluyendo v.a
drop1(model_NO_v.a, test="F")  #Ya hemos conseguido de todas las variables sean significativas a un nivel de significación alpha=0.05

#No necesitamos extraer más variables del modelo

#Forward

#Comenzamos con una única variable explicativa (la que menor p-valor tiene, y que es menor que 0.05; o valor de F más grande)
#Como el mínimo p-valor es el asociado a la variable f.a, es la primera que introducimos en el modelo (He sacado los cálculos aparte, voy a cambiarlo por la función add1())

#Stepwise



#MÉTODOS POR CRITERIOS

#R^2 ajustado

MR2adj <- summary(models)$adjr2   #Devuelve el R2 ajustado de los 8
MR2adj
which.max(MR2adj) #Devuelve el que mayor R2 ajustado tiene de los 8
plot(models, scale="adjr2")

#Cp de Mallows

Cp <- summary(models)$cp  #Devuelve el Cp de Mallows de los 8
Cp 
which.min(Cp) #Devuelve el que menor Cp tenga
plot(models, scale="Cp")

#Criterio de Información de Bayes (BIC)

MBIC <- summary(models)$bic #Devuelve el MBIC de los 8
MBIC
which.min(MBIC) #Devuelve el que menor MBIC tenga
plot(models, scale="bic")

#Criterio de Información de Akaike (AIC)

#El AIC es equivalente al Cp de Mallows


##4. DIAGNÓSTICO##

#Comprobación de cumplimiento de las hipótesis del modelo de RLM

  #LINEALIDAD
  #NORMALIDAD
  #HOMOCEDASTICIDAD
  #AUTOCORRELACIÓN
  #ESTUDIO DE OUTLIERS, INFLUYENTES Y LEVERAGE
  #COLINEALIDAD
  #POSIBLES TRANSFORMACIONES

##5. ERROR DEL TEST##

##Conclusión##

#Modelo seleccionado

model <- lm()

#Errores estándar

#p-valores

#R^2 ajustado

MR2adj_model <- summary()$adjr2   #Devuelve el R2 ajustado de los 8
MR2adj_model
which.max(MR2adj_model)

#ANOVA del modelo seleccionado

anova()



##EXTRA##

models <- regsubsets(pH ~ ., data=datos)  #Representa los modelos de i variables, y marca con un asterisco el que menor Cp de Mallows tiene.
summary(models) #Es exhaustivo


#Cálculo de los coeficientes

#A mano

Y <- as.matrix(pH, nrow=length(f.a))
X <- cbind(rep(1, length(f.a)), f.a, v.a, c.a, r.s, chlorides, f.s.d, t.s.d, density, sulphates, alcohol, quality)
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%Y
beta0_hat <- beta_hat[1,1]
beta1_hat <- beta_hat[2,1]
beta2_hat <- beta_hat[3,1]
beta3_hat <- beta_hat[4,1]
beta4_hat <- beta_hat[5,1]
beta5_hat <- beta_hat[6,1]
beta6_hat <- beta_hat[7,1]
beta7_hat <- beta_hat[8,1]
beta8_hat <- beta_hat[9,1]
beta9_hat <- beta_hat[10,1]
beta10_hat <- beta_hat[11,1]
beta11_hat <- beta_hat[12,1]

#Con la función lm() de R

model.completo$coefficients