#Residuos=Error (buscamos que sea lo más pequeño posible). Residuo=yi-(b0+b1x1+...+bnxn)
#El error se distribuye como una normal
#Tenemos que comprobar el error del conjunto de test y del conjunto de entrenamiento.
#Si el conjunto de entrenamiento tiene un error muy grande, el de test será grande también. Si el de test tiene un error muy pequeño, el de entrenamiento funcionará bien.
#Modelizamos para poder estimar los valores de los betas
#Selección de variables (2 formas, por pasos (forward, backward y stepwise) y por criterios)
#Forward: Tomamos un modelo inicial constante. Añadimos la variable con el p-valor más pequeño o el f más grande (van al revés). Vamos añadiendo 1 a una, buscando que siempre sean significativas
#Backward: Partimos del modelo completo. De las que no sean estadísticamente significativas, quitamos la que mayor p-valor tenga. Repetimos el proceso hasta que todas sean significativas.
#Por pasos: De entre todos los de 1 variable, se queda con el mejor (por ejemplo, si el criterio es el Cp de Mallows, se queda con el que menor Cp de Mallows tenga). Repetimos con 2, 3, ... variables, y se queda con los mejores de cada tamaño. De entre ellos, decide el mejor/los mejores modelos. Después, seleccionado el modelo/modelos, comprobamos si cumple la normalidad (test de Shapiro-Wilk) y la homocedasticidad (o igualdad de varianzas. Utilizamos el test de Levenne o el test de Breusch-Pagan...bptest()).
#Homocedasticidad: H0 ~ homocedasticidad, H1 ~ heterocedasticidad
#Pruebas de autocorrelación de los coeficientes: Test de Durbin-Watson (H0: p=0 (autocorrelación), H1: p!=0 (no hay autocorrelación))
#summary(modelo) o boxplot de las variables <- Me dan información sobre los outliers y observaciones inusuales, y nos proporcionan un estudio inicial del conjunto de datos.
#Si tenemos valores missing o outliers, hacemos el punto medio, la moda, la mediana o combinación lineal de los datos próximos para salvar esos casos.
#Debemos hacer comprobaciones previas al estudio de los modelos referentes a los valores missing y outliers. Hay que intentar eliminar el menor número posible de filas.
#Residuos estandarizados: Buscamos tipificar los residuos (con media 0 y desviación típica=(var^(1/2))). Calcular con rstandard()
#p<-número de valores+1
#Leverage inusual: 2*(p/n) ó 3*(p/n)
#Los residuos estandarizados se grafican en el scatterplot como una nube de puntos alrededor de la recta y=x si hay normalidad
#Residuos estudentizados: student()


#Ejercicio 6

install.packages("PASWR2")
install.packages("leaps")
install.packages("car")
library(leaps)
library(PASWR2)
library(car)

#t) Normalidad
model4.lm <- lm(HWFAT ~ AGE + HT + ABS + TRICEPS, data=HSWRESTLER)
durbinWatsonTest(model4.lm)
shapiro.test(resid(model4.lm))  #No confundir con homocedasticidad. Puede entrar en el examen para interpretar el código.
#Como p-valor mayor que 0.05 -> Aceptamos p=0 -> No hay autocorrelación. Aceptamos la hipótesis nula

#u) 
model1.lm <- lm(HWFAT ~ ABS, data=HSWRESTLER)
plot(model1.lm)
#To see next plot, press enter (when it says to hit return to see next plot)

