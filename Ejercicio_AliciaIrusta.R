##HOJA 3##

#EJERCICIO 1

set.seed(1)
x=rnorm (100)
y=2*x+rnorm (100)

#a)

##Corrección de Rosa
X <- cbind(rep(1,length(x)), x)
betahat <- solve(t(x)%*%x)%*%t(x)%*%y
betahat
model <- lm(y~x)
summary(model)$coef
##
b1hat <- cov(x,y)/var(x)
b0hat <- mean(y)-b1hat*mean(x)
model <- lm(y~x)
summary(model)$coef[,1]

#b)
plot(x,y, main="Gráfica del modelo")
abline(model, col="red")

#c)
model1 <- lm(y~x+0)
summary(model1)$coef
#El modelo model3 no es estadísticamente significativo pues el p-valor es mayor que 0.05.
#Como el error estimado del model1 es ligeramente menor que en model, el modelo obtenido es una aproximación ligeramente mejor. 
#Al ser el t-valor un poco más grande en el nuevo modelo, tenemos que la región de aceptación de la hipótesis nula es un poco mayor que en el modelo original.
#Como el p-valor obtenido sigue siendo inferior a alpha=0.05, tenemos que es significativo.
#Por tanto, el modelo model1 es ligeramente mejor que model.

#d)
model2 <- lm(x~y)
summary(model2)$coef[,1]
model3 <- lm(x~y+0)
summary(model3)$coef
#Vamos a tener la misma conclusión que en el apartado c)
##Como el error estimado del model3 es ligeramente menor que en model2, el modelo obtenido es una aproximación ligeramente mejor. 
#Al ser el t-valor un poco más grande en el nuevo modelo, tenemos que la región de aceptación de la hipótesis nula es un poco mayor que en el modelo original.
#Como el p-valor obtenido sigue siendo inferior a alpha=0.05, tenemos que es significativo.
#Por tanto, el modelo model3 es ligeramente mejor que model2.

#e)
#Vamos a obtener el mismo resultado en ambos casos (apartados c) y d))

#f)
#Al hacer b1=0, el modelo únicamente depende de b0, por lo que nos quedamos sin modelo.
model4 <- lm(y~1)
summary(model4)$coef
model5 <- lm(x~1)
summary(model5)$coef


#EJERCICIO 2

#a)
set.seed(2)
X <- rnorm(100, 0, 1)
  
#b)
eps <- rnorm(100, 0, 0.5)

#c)
Y <- -1+0.5*X+eps
length(Y)

model <- lm(Y~X)
summary(model)$coef[,1]

#d)
plot(X,Y, main="Gráfica de dispersión del modelo")

#e)
b1_hat <- sum((X-mean(X))*(Y-mean(Y)))/(sum((X-mean(X))**2))
b0_hat <- mean(Y)-b1_hat*mean(X)
#El modelo obtenido por mínimos cuadrados es una buena aproximación del modelo original, pues b0=b0_hat y b1=b1_hat

#f)
abline(model, col="red")
legend()

#g)
