#Ejemplo 13/02/2023   (Pág 28 - PDF MEST)

install.packages("PASWR2")
install.packages("leaps")
library(leaps)
library(PASWR2)
head(HSWRESTLER)

Y <- HSWRESTLER$hwfat
X1 <- HSWRESTLER$abs
X2 <- HSWRESTLER$triceps
X3 <- HSWRESTLER$subscap
X4 <- HSWRESTLER$age
X5 <- HSWRESTLER$wt
X6 <- HSWRESTLER$tanfat
X7 <- HSWRESTLER$skfat

models <- regsubsets(hwfat ~ ., data=HSWRESTLER)
summary(models)

#Que el algoritmo sea exhaustivo significa que nunca va a haber dos de cada tipo. Generalmente se queda con el primero que aparece (el más pequeño), no devuelve varios valores


MR2adj <- summary(models)$adjr2 #Que me imprima el R2 ajustado de los 8
MR2adj
which.max(MR2adj) #Que me devuelva el mayor de todos (es el que nos interesa para el modelo)
cp <- summary(models)$cp  #Teniendo en cuenta el Cp de Mallows
cp
which.min(cp) #Nos interesa el que menor cp tenga
MBIC <- summary(models)$bic
MBIC
which.min(MBIC) #Nos interesa el que menor MBIC tenga

#Análogo para el AIC (es equivalente al Cp de Mallows, también tendrías que quedarnos con el mínimo, y tendría que coincidir con el cp)

#Para el AIC, hay que instalar el paquete MASS, que utilizaremos para otras cosas más adelante
