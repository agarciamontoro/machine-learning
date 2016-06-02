## ----setup, include=FALSE------------------------------------------------
rm(list=ls())
library(knitr)
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
set.seed(1992121)

# Definimos una función para hacer las pausas entre los ejercicios
# Tomada de http://stackoverflow.com/a/15272920/3248221
pausa <- function(){
    print("Presiona [Enter] para continuar...")
    line <- readLines(con = "stdin", n=1)
}

## ------------------------------------------------------------------------
# Cargamos la librería necesaria para usar la base de datos Auto
# Para usarla, hay que instalar con la orden
# install.packages('ISLR')
library(ISLR)

# Usamos Auto por defecto, evitando así poner el prefijo Auto$
# siempre que queramos acceder a una característica de esa base de datos
attach(Auto)

## ----eval=FALSE----------------------------------------------------------
class(Auto)
dim(Auto)
colnames(Auto)

## ------------------------------------------------------------------------
# Eliminamos la última columna
Auto <- Auto[,seq(ncol(Auto)-1)]

## ------------------------------------------------------------------------
# Visualizamos la relación entre todos los pares de variables
pairs(Auto, pch=20, cex=0.2, col="steelblue")

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Plot para mpg-displacement
par(mfrow=c(1,2))
plot(displacement, mpg, pch=20, col="steelblue",
     main="Cilindrada")
boxplot(mpg~displacement)

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Plot para mpg-horsepower
plot(horsepower, mpg, pch=20, col="steelblue",
     main="Potencia")
boxplot(mpg~horsepower)

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Plot para mpg-weight
plot(weight, mpg, pch=20, col="steelblue",
     main="Peso")
boxplot(mpg~weight)
par(mfrow=c(1,1))

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Tomamos los valores absolutos de la correlación entre mpg y todas las demás variables,
# sin incluirse a sí misma
corr <- abs(cor(Auto))["mpg",-1]

# Visualizamos el grado de correlación en un gráfico de barras.
# Creamos el gráfico.
bp <- barplot(corr, axes = FALSE, axisnames = FALSE, col = "steelblue",
              main="Correlación entre mpg y las demás variables")

# Añadimos el texto, girado 45 grados.
text(bp, par("usr")[3]-0.02, labels = colnames(Auto[-1]),
     srt = 45, adj = 1, xpd = TRUE)

# Dibujamos los ejes.
axis(2)

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Plot para mpg-cylinders
plot(cylinders, mpg, pch=20, col="steelblue",
     main="Número de cilindors")

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
selec <- c("displacement", "horsepower", "weight")

## ------------------------------------------------------------------------
# Vector de índices para la muestra de entrenamiento (80%)
trainIdx  <- sample(nrow(Auto), size=0.8*nrow(Auto))

# Vector de índices para la muestra de test
testIdx   <- setdiff(1:nrow(Auto), trainIdx)

## ------------------------------------------------------------------------
# Creamos una nueva variable booleana, mpg01, en función de la mediana,
# y la añadimos a la base de datos
mpg01 <- ifelse(mpg > median(mpg), T, F)
Auto <- data.frame(mpg01, Auto)

# Obtenemos las muestras de entrenamiento y de test
Auto.train <- Auto[trainIdx,]
Auto.test  <- Auto[testIdx,]

## ------------------------------------------------------------------------
# Ajustamos el modelo con los datos de entrenamiento
mod.lin = glm(mpg01~displacement+horsepower+weight, data=Auto.train, family = binomial)

## ------------------------------------------------------------------------
# Usamos el modelo lineal ajustado para predecir con la muestra de test
mod.lin.pred <- predict(mod.lin, Auto.test, type = "response")

## ------------------------------------------------------------------------
# Si la predicción tiene probabilidad mayor que el 50%,
# asignamos el valor Verdad; en otro caso, asignamos el valor Falso
mod.lin.mpg01 <- ifelse(mod.lin.pred > 0.5, T, F)

## ------------------------------------------------------------------------
# Calculamos el error como el porcentaje de muestras mal clasificadas
mod.lin.error <- mean(Auto.test$mpg01 != mod.lin.mpg01)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de la regresión lineal:", 100*mod.lin.error, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
tab <- table(Real=Auto.test$mpg01, Predecido=mod.lin.mpg01)
kable(tab, caption="Regresión lineal")

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Escalado de la muestra de entrenamiento sin la variable mpg01
norm.train <- scale(Auto.train[,-1])

# Escalado de la muesrta de test sin mpg01 y en base al escalado anterior
norm.test <- scale(Auto.test[,-1],
                   center = attributes(norm.train)$"scaled:center",
                   scale = attributes(norm.train)$"scaled:scale")

# Creamos una variable con ambas muestras y la reordenamos según el nombre (número) de las filas
norm.full <- rbind(norm.train, norm.test)
norm.full <- norm.full[order(as.numeric(row.names(norm.full))),]

## ------------------------------------------------------------------------
# Cargamos la librería class, que contiene los modelos del knn
library(class)

# Predecimos de la forma más directa posible y con el típico valor de k=3
knn.pred <- knn(norm.train, norm.test, as.factor(Auto.train$mpg01), k=3, prob=T)

# Calculamos el error
knn.error <- mean(Auto.test$mpg01 != knn.pred)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error del vecino más cercano:", 100*knn.error, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Necesitamos la librería e1071, que podemos instalar con la orden
# install.packages("e1071")
library(e1071)

# Para la validación cruzada usamos el conjunto completo de datos
# y el conjunto completo de clases
knn.cross <- tune.knn(x = norm.full, y = as.factor(Auto$mpg01), k = 1:20,
                      tunecontrol = tune.control(sampling = "cross"), cross=10)

# Podemos estudiar visualmente la búsqueda del mejor k:
plot(knn.cross)

# Nos quedamos con el mejor parámetro
knn.k <- knn.cross$best.parameters$k

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Parámetro ajustado del vecino más cercano: k = ", knn.k)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Predecimos con el k calculado
knn.pred <- knn(norm.train, norm.test, as.factor(Auto.train$mpg01), k = knn.k, prob = T)

# Calculamos de nuevo el error
knn.k.error <- mean(Auto.test$mpg01 != knn.pred)

# Vemos una tabla que resume los resultados
tab <- table(Real=Auto.test$mpg01, Predecido=knn.pred)
kable(tab, caption="kNN")

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error del vecino más cercano con k = ", knn.k, "-> ", 100*knn.k.error, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# install.packages("ROCR")
library ( ROCR )

curveROC <- function (probabilities, labels, model.knn=F, ...) {
  if(model.knn){
    prob <- attributes(probabilities)$"prob"
    probabilities <- ifelse(probabilities == F, 1-prob, prob)
  }

  prediction <- prediction(probabilities, labels)

  # Calculamos la curva para las tasas de verdaderos y falsos positivos
  performance <- performance(prediction, "tpr", "fpr")
  plot(performance, ...)

  # Devolvemos el estimador AUC - el área bajo la curva
  return(attributes(performance(prediction, "auc"))$"y.values"[[1]])
}

mod.lin.auc <- curveROC(mod.lin.pred, Auto.test$mpg01,
                        col="darkolivegreen",lwd=1.5, main="Curvas ROC")
knn.auc <- curveROC(knn.pred, Auto.test$mpg01, model.knn=T, add=T,
                    col="steelblue", lwd=1.5)

legend("bottomright", c("Regresión lineal", "Vecino más cercano"),
       col=c("darkolivegreen", "steelblue"), lwd=1.5)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("AUC del kNN = ", knn.auc, ". AUC de la regresión lineal = ", mod.lin.auc)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Cargamos la librería boot, que puede ser instalada con
# install.packages("boot")
library(boot)

# Hacemos validación cruzada
mod.lin.cv <- cv.glm(mod.lin, data=Auto.train, K=5)

# Tomamos la estimación del error de predicción
mod.lin.cv.err <- mod.lin.cv$delta[1]

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de cv con la regresión logística: ", 100*mod.lin.cv.err, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Predecimos con el k calculado anteriormente y con leave-one-out cross-validation
knn.cv.pred <- knn.cv(norm.full, as.factor(Auto$mpg01), k = knn.k)

# Calculamos de nuevo el error
knn.cv.error <- mean(Auto$mpg01 != knn.cv.pred)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de cv con vecino más cercano: ", 100*knn.cv.error, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Ajustamos el modelo con los datos de entrenamiento
lm.hdw <- lm(mpg ~ horsepower+displacement+weight, data=Auto.train)

# Cálculo de errores
lm.hdw.err <- mean((predict(lm.hdw, Auto.test, type="response") - Auto.test[,"mpg"])^2)

# Visualización de estadísticos
anova(lm.hdw)

# Visualización del patrón de residuos
plot(predict(lm.hdw), residuals(lm.hdw), pch=20, col="steelblue",
     xlab="Predicciones del modelo", ylab="Residuos")

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Ajustamos el modelo con los datos de entrenamiento
lm.hd <- lm(mpg ~ horsepower*displacement, data=Auto.train)
lm.hw <- lm(mpg ~ horsepower*weight, data=Auto.train)
lm.dw <- lm(mpg ~ displacement*weight, data=Auto.train)

# Cálculo de errores
lm.hd.err <- mean((predict(lm.hd, Auto.test, type="response") - Auto.test[,"mpg"])^2)
lm.hw.err <- mean((predict(lm.hw, Auto.test, type="response") - Auto.test[,"mpg"])^2)
lm.dw.err <- mean((predict(lm.dw, Auto.test, type="response") - Auto.test[,"mpg"])^2)

# Definimos una rejilla 1x3 para los plots
prev_par <- par(no.readonly = T)

# Ajustamos los márgenes
par(mfrow=c(1, 3), mar=c(0.1, 1.1, 2.1, 0.1), oma=2.5*c(1,1,1,1))

# Visualización del patrón de residuos
plot(predict(lm.hd), residuals(lm.hdw), pch=20, col="steelblue",
     main="H*D", ylab="Residuos")
plot(predict(lm.hw), residuals(lm.hdw), pch=20, col="steelblue",
     main="H*W", ylab="", yaxt="n")
plot(predict(lm.dw), residuals(lm.hdw), pch=20, col="steelblue",
     main="D*W", ylab="", yaxt="n")

# Ponemos título al gráfico conjunto
mtext("Patrón de los residuos", outer=TRUE, line=0.5)

par(prev_par)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de horsepower~displacement: ", lm.hd.err)
print(mensaje)
mensaje <- paste("Error de horsepower~weight: ", lm.hw.err)
print(mensaje)
mensaje <- paste("Error de displacement~weight: ", lm.dw.err)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Ajustamos el modelo con los datos de entrenamiento
lm.hd.w <- lm(mpg ~ horsepower*displacement+weight, data=Auto.train)

# Cálculo de errores
lm.hd.w.err <- mean((predict(lm.hd.w, Auto.test, type="response") - Auto.test[,"mpg"])^2)

# Visualización de estadísticos
anova(lm.hd.w)

# Visualización del patrón de residuos
plot(predict(lm.hd.w), residuals(lm.hd.w), pch=20, col="steelblue",
     xlab="Predicciones del modelo", ylab="Residuos")

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de horsepower*displacement ~ weight: ", lm.hd.w.err)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Cargamos la librería necesaria para usar la base de datos Boston
# Para usarla, hay que instalar con la orden
# install.packages('MASS')
library(MASS)

# Usamos Boston por defecto, evitando así poner el prefijo Boston$
# siempre que queramos acceder a una característica de esa base de datos
attach(Boston)

## ----eval=FALSE----------------------------------------------------------
class(Boston)
dim(Boston)
colnames(Boston)

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Visualizamos la relación entre todos los pares de variables
pairs(Boston, pch=20, cex=0.2, col="steelblue")

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Tomamos los valores absolutos de la correlación entre crim y todas las demás variables,
# sin incluirse a sí misma
corr <- abs(cor(Boston))["crim",-1]

# Visualizamos el grado de correlación en un gráfico de barras.
# Creamos el gráfico.
bp <- barplot(corr, axes = FALSE, axisnames = FALSE, col = "steelblue",
              main="Correlación entre crim y las demás variables")

# Añadimos el texto, girado 45 grados.
text(bp, par("usr")[3]-0.02, labels = colnames(Boston)[-1],
     srt = 45, adj = 1, xpd = TRUE)

# Dibujamos los ejes.
axis(2)

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Vector de índices para la muestra de entrenamiento (80%)
trainIdx  <- sample(nrow(Boston), size=0.8*nrow(Boston))

# Vector de índices para la muestra de test
testIdx   <- setdiff(1:nrow(Boston), trainIdx)

# Obtenemos las muestras de entrenamiento y de test
Boston.train <- as.matrix(Boston[trainIdx, ])
Boston.test  <- as.matrix(Boston[testIdx, ])

## ------------------------------------------------------------------------
# Cargamos la librería glmnet, que debe ser instalada con la orden
# install.packages("glmnet")
library(glmnet)

# Hacemos validación cruzada para encontrar el mínimo lambda (el parámetro de regularización
# que minimiza el error medio de validación cruzada). Tomamos alpha = 1 para realizar un método
# LASSO
lasso.cv <- cv.glmnet(Boston.train[, -1], Boston.train[,"crim"], alpha = 1)
lasso.lambda <- lasso.cv$lambda.min

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Parámetro lambda elegido por LASSO: ", lasso.lambda)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Calculamos ahora los coeficientes solicitados:
lasso.coefs <- predict(lasso.cv, Boston.test[,-1], s = lasso.lambda, alpha = 1,
                      type = "coefficients")

# Eliminamos la primera fila, que contiene el valor de Intercept; en este momento no nos interesa
lasso.coefs <- lasso.coefs[-1,]

## ------------------------------------------------------------------------
# Función para calcular el error residual estándar (raíz del error cuadrático medio)
# entre las variables predecidas y las reales
RSE <- function(pred, real){
  return(sqrt(mean((pred - real)^2)))
}

# Hacemos la selección de variables dependiente de un umbral pasado como parámetro
lasso.seleccionar <- function(lasso.umbral){
  # Extraemos los nombres de las variables cuyos coeficientes (en valor absoluto)
  # superan el umbral prefijado
  lasso.selec <- attributes(lasso.coefs[abs(lasso.coefs) > lasso.umbral])$names

  # Por último, hacemos las predicciones con las variables seleccionadas
  lasso.glm <- glmnet(as.matrix(Boston.train[,lasso.selec]),
                      Boston.train[,"crim"],alpha = 1, lambda = lasso.lambda)
  lasso.pred <- predict(lasso.glm, Boston.test[,lasso.selec],
                        s=lasso.lambda, alpha=1)

  # Devolvemos el error residual estándar (raíz del error cuadrático medio)
  #con las variables seleccionadas
  return(c(lasso.umbral, RSE(lasso.pred, Boston.test[,"crim"])))
}

## ------------------------------------------------------------------------
# Calculamos los errores para todos los umbrales de 0.1 a 0.7, con saltos de 0.05
lasso.errores <- t(sapply(seq(0.0,0.7,0.05), lasso.seleccionar))

# Recuperamos el umbral para el que el error calculado es mínimo
lasso.umbral <- lasso.errores[which.min(lasso.errores[,2]),1]

# Nos quedamos con las mejores variales según el umbral devuelto
lasso.selec <- attributes(lasso.coefs[abs(lasso.coefs) > lasso.umbral])$names

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Variables seleccionadas por LASSO: ", lasso.selec)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Función dependiente del parámetro lambda que nos permitirá después
# hacer un estudio del underfitting
weight.decay.rse <- function(param.lambda){
  # Hacemos la regresión, apuntando que es weight decay con el parámetro alpha = 0
  ridge <- glmnet(Boston.train[, lasso.selec], Boston.train[,"crim"],
                  alpha = 0, lambda = param.lambda)

  # Calculamos las predicciones del modelo ajustado con el lambda calculado anteriormente
  ridge.pred <- predict(ridge, Boston.test[,lasso.selec], s=param.lambda, alpha=0)

  # Calculamos el RSE de estas predicciones
  ridge.rse <- RSE(ridge.pred, Boston.test[,"crim"])

  return(ridge.rse)
}

weight.rse <- weight.decay.rse(lasso.lambda)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de weight-decay con lambda de LASSO: ", weight.rse)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Estudio de underfitting
ridge.errors <- sapply(lasso.cv$lambda, weight.decay.rse)

# Generación de la gráfica
plot(lasso.cv$lambda, ridge.errors, pch=20, type="l", cex=0.5,
     lwd=2, col="steelblue", xlab=expression(Valores~de~lambda),
     ylab="Error residual", main="Estudio de underfitting")
points(x=lasso.lambda, y=weight.decay.rse(lasso.lambda), pch=20,
       col="red")
legend("bottomright", c("RSE general", expression(RSE~para~lambda[lasso])),
       col=c("steelblue","red"), pch=c(NA,20), lwd=c(2,NA))

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Creamos una nueva variable booleana, crim1, en función de la mediana,
# y la añadimos a la base de datos
crim1 <- ifelse(crim > median(crim), 1, -1)
Boston <- data.frame(crim1, Boston)

# Obtenemos las muestras de entrenamiento y de test
Boston.train <- Boston[trainIdx,]
Boston.test  <- Boston[testIdx,]

## ------------------------------------------------------------------------
# Devuelve el porcentaje de muestras mal clasificadas que produce
# un modelo SVM con el kernel del tipo kernel.name.
svm.error <- function(kernel.name){
  # Vector de índices para la muestra de entrenamiento (80%)
  trainIdx  <- sample(nrow(Boston), size=0.8*nrow(Boston))
  # Vector de índices para la muestra de test
  testIdx   <- setdiff(1:nrow(Boston), trainIdx)

  # Obtenemos las muestras de entrenamiento y de test
  Boston.train <- Boston[trainIdx,]
  Boston.test  <- Boston[testIdx,]

  # Ajustamos el modelo
  svm <- svm(crim1~., data = Boston.train, kernel = kernel.name)

  # Hacemos la predicción
  svm.pred <- predict(svm, Boston.test, type = "response")

  # Calculamos la variable según las predicciones
  pred <- ifelse(svm.pred > 0, 1, -1)

  # Devolvemos el porcentaje de muestras mal clasificadas
  return(mean(pred != Boston.test[,"crim1"]))
}

# Ajustamos un modelo de SVM con un núcleo lineal
svm.linear <- mean(replicate(100,svm.error("linear")))

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de SVM lineal: ", 100*svm.linear, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Ajustamos SVM con los núcleos disponibles
svm.polynomial <- mean(replicate(100,svm.error("polynomial")))
svm.radial <- mean(replicate(100,svm.error("radial")))
svm.sigmoid <- mean(replicate(100,svm.error("sigmoid")))

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de SVM polinómico: ", 100*svm.polynomial, "%")
print(mensaje)
mensaje <- paste("Error de SVM radial: ", 100*svm.radial, "%")
print(mensaje)
mensaje <- paste("Error de SVM sigmoidal: ", 100*svm.sigmoid, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Hacemos la regresión, apuntando que es weight decay con el parámetro alpha = 0
ridge <- cv.glmnet(as.matrix(Boston.train[, lasso.selec]), Boston.train[,"crim"],
                   alpha = 0, nfolds=10)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Mejor error de modelo con weight-decay y cv.glmnet: ", min(ridge$cvm))
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Calculamos las predicciones del modelo ajustado con el lambda calculado anteriormente
ridge.cv.pred <- predict(ridge$glmnet.fit, as.matrix(Boston.test[,lasso.selec]))

# Calculamos el RSE de estas predicciones
ridge.cv.rse <- RSE(ridge.cv.pred, Boston.test[,"crim"])

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de test con weight-decay y cv.glmnet: ", ridge.cv.rse)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Ajustamos con validación cruzada el SVM con núcleo radial. Encontraremos así
# los parámetros que optimizan este modelo.
svm.cv = tune(svm, crim1 ~ ., data = Boston.train, kernel ="radial",
              ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                            gamma = c(0.5, 1, 2, 3, 4)))

# Observamos un resumen del comportamiento de la validación cruzada
summary(svm.cv)

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Predecimos con el nuevo modelo
svm.train.cv.pred <- predict(svm.cv$best.model, Boston.train, type = "response")
svm.test.cv.pred <- predict(svm.cv$best.model, Boston.test, type = "response")

# Calculamos la variable según las predicciones
svm.train.cv.pred <- ifelse(svm.train.cv.pred > 0, 1, -1)
svm.test.cv.pred <- ifelse(svm.test.cv.pred > 0, 1, -1)

# Devolvemos el porcentaje de muestras mal clasificadas
svm.train.cv.err <- mean(svm.train.cv.pred != Boston.train[,"crim1"])
svm.test.cv.err <- mean(svm.test.cv.pred != Boston.test[,"crim1"])

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de entrenamiento con SVM radial y validación cruzada: ", 100*svm.train.cv.err, "%")
print(mensaje)
mensaje <- paste("Error de test con SVM radial y validación cruzada: ", 100*svm.test.cv.err, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Cargamos las librerías randomForest y gbm, que se pueden instalar con las órdenes
# install.packages(c("randomForest","gbm"))
library(randomForest)
library(gbm)

# Devolvemos la base de datos a su estado original
Boston <- Boston[, !(names(Boston) %in% "crim1")]

# Vector de índices para la muestra de entrenamiento (80%)
trainIdx  <- sample(nrow(Boston), size=0.8*nrow(Boston))

# Vector de índices para la muestra de test
testIdx   <- setdiff(1:nrow(Boston), trainIdx)

# Obtenemos las muestras de entrenamiento y de test
Boston.train <- Boston[trainIdx, ]
Boston.test  <- Boston[testIdx, ]

## ------------------------------------------------------------------------
# Ajustamos bagging
bag <- randomForest(medv ~ ., data = Boston, subset = trainIdx,
                    mtry = ncol(Boston)-1, importance = T)

## ------------------------------------------------------------------------
# Predecimos la variable medv con el modelo ajustado
bag.pred <- predict(bag, Boston.test)

# Calculamos el error cuadrático medio
bag.error <- mean((bag.pred - Boston.test[,"medv"])^2)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de bagging: ", bag.error)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Ajustamos randomForest con el valor por defecto de número árboles: 500
forest.500 <- randomForest(medv ~ ., data = Boston, subset = trainIdx,
                           ntrees=500, importance = T)

# Generamos el gráfico del error producido con cada valor de ntrees de 1a 500
plot(forest.500, col="steelblue", lwd=2)

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Tomamos el valor de ntrees en el que se minimiza el error
forest.best.N <- which(forest.500$mse == min(forest.500$mse))

# Reajustamos el modelo con este valor como ntrees
forest.best <- randomForest(medv ~ ., data = Boston, subset = trainIdx,
                            ntrees=forest.best.N, importance = T)

## ------------------------------------------------------------------------
# Calculamos las predicciones con ambos modelos
forest.500.pred <- predict(forest.500, Boston.test)
forest.best.pred <- predict(forest.best, Boston.test)

# Calculamos el error cuadrático medio con respecto a los valores reales
forest.500.err <- mean((forest.500.pred - Boston.test[,"medv"])^2)
forest.best.err <- mean((forest.best.pred - Boston.test[,"medv"])^2)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de random forest con 500 árboles: ", forest.500.err)
print(mensaje)
mensaje <- paste("Error de random forest con número óptimo de árboles: ", forest.best.err)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Calcula el error cuadrático medio de randomForest, con el parámetro
# ntrees especificado, con 10 particiones aleatorias
errorNtree <- function(param.ntrees){

  # Calcula el error cuadrático medio en una partición 80-20 aleatoria
  forest.fold <- function(){
    # Vector de índices para la muestra de entrenamiento (80%)
    trainIdx  <- sample(nrow(Boston), size=0.8*nrow(Boston))

    # Vector de índices para la muestra de test
    testIdx   <- setdiff(1:nrow(Boston), trainIdx)

    # Obtenemos las muestras de entrenamiento y de test
    Boston.train <- Boston[trainIdx, ]
    Boston.test  <- Boston[testIdx, ]

    forest <- randomForest(medv ~ ., data = Boston, subset = trainIdx,
                           ntrees=param.ntrees, importance = T)

    pred <- predict(forest, Boston.test)

    return(mean((pred - Boston.test[,"medv"])^2))
  }

  # Calcula el error medio en 10 particiones aleatorias
  error <- mean(replicate(10, forest.fold()))

  return(c(param.ntrees,error))
}

## ------------------------------------------------------------------------
# Calculamos los errores para ntrees igual a 30, 50, 70, 90, ..., 350
errores <- t(sapply(seq(30, 350, 20), errorNtree))

# Obtenemos el ntree tal que minimiza los errores obtenidos
best.ntree <- errores[which(errores[,2] == min(errores[,2])), 1]

## ------------------------------------------------------------------------
# Reajustamos, recalculamos predicciones y vemos el error cuadrático
forest.cv <- randomForest(medv ~ ., data = Boston, subset = trainIdx,
                          ntrees=best.ntree, importance = T)
forest.cv.pred <- predict(forest.cv, Boston.test)
forest.cv.err <- mean((forest.cv.pred - Boston.test[,"medv"])^2)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de random forest con número de árboles = ", best.ntree, " elegido por validación cruzada: ", forest.cv.err)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
boost <- gbm(medv ~ ., data = as.data.frame(Boston.train),
             distribution = "gaussian" , n.trees=5000, interaction.depth=4)

# Resumimos la información devuelta por el modelo y
# generamos un gráfico de la influencia de cada variable
summary(boost)

## ------------------------------------------------------------------------
boost.pred <- predict(boost, Boston.test, n.trees=5000)
boost.err <- mean((boost.pred - Boston.test[,"medv"])^2)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de boosting: ", boost.err)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Reajustamos con 5cv
boost.cv <- gbm(medv ~ ., data = as.data.frame(Boston.train), cv.folds=5,
                distribution = "gaussian" , n.trees=5000, interaction.depth=4)

# Buscamos el valor de n.trees óptimo con validación cruzada
boost.cv.n <- gbm.perf(boost.cv,method="cv")

# Hacemos las predicciones con este valor
boost.cv.pred <- predict(boost.cv, Boston.test, n.trees=boost.cv.n)
boost.cv.err <- mean((boost.cv.pred - Boston.test[,"medv"])^2)

## ---- eval=FALSE, include=FALSE------------------------------------------
mensaje <- paste("Error de boosting con validación cruzada: ", boost.cv.err)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ---- include=FALSE------------------------------------------------------
set.seed(19921201)

## ------------------------------------------------------------------------
# Vector de índices para la muestra de entrenamiento (80%)
trainIdx  <- sample(nrow(OJ), size=800)

# Vector de índices para la muestra de test
testIdx   <- setdiff(1:nrow(OJ), trainIdx)

# Obtenemos las muestras de entrenamiento y de test
OJ.train <- OJ[trainIdx, ]
OJ.test  <- OJ[testIdx, ]

attach(OJ)

# Incluimos la librería tree, que se puede instalar con la orden
# install.packages("tree")
library(tree)

## ------------------------------------------------------------------------
arbol <- tree(Purchase ~ ., OJ.train)

## ------------------------------------------------------------------------
# Generamos un resumen del modelo obtenido
summary(arbol)

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Generación del gráfico del árbol
plot(arbol)

#Le añadimos los nombres de las variables y los umbrales
text(arbol, pretty=0)

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Cargamos la librería caret para poder generar la matriz de confusión.
# Se puede instalar con la orden
# install.packages("caret")
library(caret)

# Predecimos y calculamos la matriz de confusión
arbol.pred <- predict(arbol, OJ.test, type="class")
arbol.conf <- confusionMatrix(arbol.pred, OJ.test[,"Purchase"])

# Visualizamos la matriz de confusión
arbol.conf

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Hacemos validación cruzada
arbol.cv <- cv.tree(arbol, K = 5, FUN=prune.misclass)

# Visualizamos la salida
arbol.cv

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Estudiamos el número de muestras mal clasificadas por cada tamaño de árbol considerado
# y por cada parámetro k
par(mfrow = c(1,2))
plot(arbol.cv$size, arbol.cv$dev, type ="b", pch=20, col="steelblue", lwd=2,
     xlab="Tamaño del árbol", ylab="Muestras mal clasificadas")
plot(arbol.cv$k, arbol.cv$dev, type ="b", pch=20, col="steelblue", lwd=2,
     xlab="Parámetro k", ylab="")
par(mfrow=c(1,1))

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Tomamos el mejor valor de tamaño
arbol.n <- min(arbol.cv$size[which(arbol.cv$dev == min(arbol.cv$dev))])

# Podamos el árbol
arbol.podado = prune.misclass(arbol, best=arbol.n)

# Dibujamos de nuevo el árbol
plot(arbol.podado)
text(arbol.podado, pretty=0)

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
# Predecimos y calculamos matriz de confusión
arbol.podado.pred <- predict(arbol.podado, OJ.test, type="class")
arbol.podado.conf <- confusionMatrix(arbol.podado.pred, OJ.test[,"Purchase"])

# Estudiamos matriz de confusión
arbol.podado.conf

## ---- eval=FALSE, include=FALSE------------------------------------------
# Pausa antes de proseguir
pausa()
