## ----setup, include=FALSE------------------------------------------------
# Definimos la pantalla como salida principal de los gráficos
X11()

# Función kable
library(knitr)

# Funciones grad y hess
library(numDeriv)

# Necesario para llamar a la función ginv, que calcula la pseudo-inversa
library(MASS)

set.seed(19921201)

# Definimos una función para hacer las pausas entre los ejercicios
# Tomada de http://stackoverflow.com/a/15272920/3248221
pausa <- function(){
    print("Presiona [Enter] para continuar...")
    line <- readLines(con = "stdin", n=1)
}

## ----Definición de la función gradiente de E(u,v)------------------------
# Devuelve el valor de E en (u,v) = `punto`
E <- function(punto){
  # Tomamos las componentes del punto
  u <- punto[1]
  v <- punto[2]

  # Devolvemos el valor de E
  return((u*exp(v) - 2*v*exp(-u))^2)
}


# Devuelve el valor del gradiente de E en (u,v) = `punto`
E.gradiente <- function(punto){
  # Tomamos las componentes del punto
  u <- punto[1]
  v <- punto[2]

  # Calculamos el coeficiente común
  coeff <- 2*(u*exp(v) - 2*v*exp(-u))

  # Devolvemos el valor del gradiente de E
  return(coeff * c(exp(v) + 2*v*exp(-u), u*exp(v) - 2*exp(-u)))
}

## ----Gradiente descendente-----------------------------------------------
# Ejecuta el método del gradiente descendente para encontrar el mínimo
# de la función f.
gradienteDescendente <- function(f, f.gradiente, w_0 = c(1,1),
                                 eta = 0.1, tol = 1e-14, maxIter = 50){
  # Inicializamos solución y vectores de puntos/valores intermedios
  w <- w_0
  variables.x <- w[1]
  variables.y <- w[2]
  valores <- f(w)

  # Criterio de parada: el valor de f es menor que la tolerancia
  # o se ha alcanzado el máximo de iteraciones
  while(f(w) >= tol && length(valores) < maxIter){
    # Calculamos la dirección de menor descenso
    direccion <- -f.gradiente(w)

    # Avanzamos en esa dirección
    w <- w + eta*direccion

    # Actualizamos el valor de los vectores de puntos/valores intermedios
    variables.x <- c(variables.x, w[1])
    variables.y <- c(variables.y, w[2])
    valores <- c(valores, f(w))
  }

  # Devolvemos el punto encontrado, el número de iteraciones
  # empleadas en encontrarlo y un data frame con los puntos intermedios
  return(list("Pesos"=w, "Iter"=length(valores)-1,
              "Res"=data.frame(X = variables.x, Y = variables.y, Value = valores)))
}

## ----Ejecución de gradiente descendente----------------------------------
# Ejecución del gradiente descendente
resultado.E <- gradienteDescendente(E, E.gradiente)

## ----1.a.2 - Salida, eval=FALSE, include=FALSE---------------------------
mensaje <- paste("Gradiente descendente - Nº de iteraciones empleadas para conseguir un valor menor de 1e-14:", resultado.E$Iter)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ----1.a.3 - Salida, eval=FALSE, include=FALSE---------------------------
mensaje <- paste("Gradiente descendente - El punto donde se encontró el mínimo es: (", resultado.E$Pesos[1], ",", resultado.E$Pesos[2], ")")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ----Definición de la función f y su gradiente---------------------------
# Devuelve el valor de f en (x,y) = `punto`
f <- function(punto){
  # Tomamos las componentes del punto
  x <- punto[1]
  y <- punto[2]

  # Devolvemos el valor de f
  return( x*x + 2*y*y + 2*sin(2*pi*x)*sin(2*pi*y) )
}

# Devuelve el valor del gradiente de f en (x,y) = `punto`
f.gradiente <- function(punto){
  # Tomamos las componentes del punto
  x <- punto[1]
  y <- punto[2]

  # Calculamos las componentes del gradiente de f
  f.grad.x <- 2*x + 4*pi*sin(2*pi*y)*cos(2*pi*x)
  f.grad.y <- 4*y + 4*pi*sin(2*pi*x)*cos(2*pi*y)

  # Devolvemos el valor del gradiente de f
  return(c(f.grad.x, f.grad.y))
}

## ----Gráfico gradiente descendente---------------------------------------
# Ejecutamos el experimento con ambos parámetros
resultado.f.0.01 <- gradienteDescendente(f, f.gradiente, eta = 0.01,
                                         w_0 = c(1,1), maxIter = 50, tol=-Inf)
resultado.f.0.1 <- gradienteDescendente(f, f.gradiente, eta = 0.1,
                                        w_0 = c(1,1), maxIter = 50, tol=-Inf)

# Generamos las gráficas de ambos descensos
plot(resultado.f.0.1$Res$Value, col = 'red', pch = 20, asp=1, type='o',
     main=expression(paste("Gradiente descendente con ",
                           f(x,y) == x^2 + 2*y^2 + 2*plain(sin)(2*pi*x)*plain(sin)(2*pi*y))),
     xlab="Número de iteraciones", ylab="Valor de f")
points(resultado.f.0.01$Res$Value, col = 'blue', pch = 20, asp=1, type='o')

# Añadimos leyenda de los datos dibujados.
legend("bottomright", c(expression(eta == 0.1), expression(eta == 0.01)),
       bg="white", col=c("red", "blue"), lty=1, pch=20, lwd=1.75)

## ----1.b.1 - Salida, eval=FALSE, include=FALSE---------------------------
# Pausa antes de proseguir
pausa()

## ----Generación de datos con log2----------------------------------------
# Definimos una función que, dado p, devuelve el valor mínimo del gradiente descendente
# con punto inicial = (p,p) y dónde se alcanza este:
minimoGradiente <- function(p){
  valores <- gradienteDescendente(f, f.gradiente, c(p, p), tol = -Inf, maxIter = 50)$Res
  unlist(valores[which.min(valores$Value),])
}

# Definimos los puntos iniciales sobre los que iterar con el apply siguiente
puntosIniciales <- c(0.1, 1, -0.5, -1)

# Usamos la funcion minimoGradiente sobre cada uno de los puntos anteriores
tablaMinimos <- lapply(puntosIniciales, minimoGradiente)

# Convertimos la lista devuelta en un data frame y ponemos nombres a columnas y filas
tablaMinimos <- data.frame(matrix(unlist(tablaMinimos), nrow=4, byrow=T))
colnames(tablaMinimos) <- c("X", "Y", "Valor")
rownames(tablaMinimos) <- c("Pto. inicial = (0.1, 0.1)", "Pto. inicial = (1, 1)",
                            "Pto. inicial = (-0.5, 0.5)", "Pto. inicial = (-1, -1)")

## ----1.b.2 - Premensaje, eval=FALSE, include=FALSE-----------------------
mensaje <- paste("Gradiente descendente - La siguiente tabla indica, para cada punto inicial, el punto (x,y) donde se alcanza el mínimo y el valor del mismo:")
print(mensaje)

## ----Tabla minimos-------------------------------------------------------
# Generamos la tabla solicitada
kable(tablaMinimos, digits=5)

## ----1.b.2 - Salida, eval=FALSE, include=FALSE---------------------------
# Pausa antes de proseguir
pausa()

## ----Coordenada descendente - Definición---------------------------------
# Ejecuta el método de la coordenada descendente para encontrar el mínimo
# de la función f.
coordenadaDescendente <- function(f, f.grad, w_0 = c(1,1), eta = 0.1,
                                  tol = 1e-14, maxIter = 50){
  # Inicializamos solución y número de iteraciones
  w <- w_0
  iteraciones <- 0

  # Criterio de parada: el valor de f es menor que la tolerancia
  # o se ha alcanzado el máximo de iteraciones
  while(f(w) >= tol && iteraciones < maxIter){
    # Calculamos la dirección de menor descenso en la 1a coordenada
    direccion.x <- c(-f.grad(w)[1], 0)
    # Avanzamos en esa dirección
    w <- w + eta*direccion.x

    # Calculamos la dirección de menor descenso en la 2a coordenada
    direccion.y <- c(0, -f.grad(w)[2])
    # Avanzamos en esa dirección
    w <- w + eta*direccion.y

    # Aumentamos el contador de iteraciones
    iteraciones <- iteraciones + 1
  }

  # Devuelve la solución encontrada y el número de iteraciones
  return(list("Pesos"=w, "Iter"=iteraciones))
}

## ----Coordenada descendente - Ejecución----------------------------------
# Ejecutamos coordenada descendente
res.coordDesc <- coordenadaDescendente(E, E.gradiente, maxIter = 15)

## ----2.a - Salida, eval=FALSE, include=FALSE-----------------------------
mensaje <- paste("Coordenada descendente - Tras 15 iteraciones el valor de E encontrado es:",
                 E(res.coordDesc$Pesos))
print(mensaje)

# Pausa antes de proseguir
pausa()

## ----Comparación GradienteCoordenada-------------------------------------
# Tomamos los resultados para f
comp.f.grad <- gradienteDescendente(f, f.gradiente, maxIter = 20, tol=-Inf)
comp.f.coor <- coordenadaDescendente(f, f.gradiente, maxIter = 20, tol=-Inf)

# Tomamos los resultados para E
comp.E.grad <- gradienteDescendente(E, E.gradiente, maxIter = 20, tol=-Inf)
comp.E.coor <- coordenadaDescendente(E, E.gradiente, maxIter = 20, tol=-Inf)

# Obtenemos el valor de f en el mínimo devuelto por ambos algoritmos
min.f.grad <- f(comp.f.grad$Pesos)
min.f.coor <- f(comp.f.coor$Pesos)

# Obtenemos el valor de E en el mínimo devuelto por ambos algoritmos
min.E.grad <- E(comp.E.grad$Pesos)
min.E.coor <- E(comp.E.coor$Pesos)

## ----Dataframe de comparación--------------------------------------------
# Generamos un data frame con los datos obtenidos
tablaComp <- data.frame(c(min.f.coor, min.E.coor),
                        c(min.f.grad, min.E.grad))

colnames(tablaComp) <- c("Valor de f", "Valor de E")
rownames(tablaComp) <- c("Coordenada descendente", "Gradiente descendente")

## ----2.b - Salida, eval=FALSE, include=FALSE-----------------------------
mensaje <- paste("Coordenada descendente - Tabla comparativa tras 20 iteraciones:")
print(mensaje)

## ------------------------------------------------------------------------
# Generamos la tabla a partir del data frame
kable(tablaComp, digits=15)

## ----2.b.bis - Salida, eval=FALSE, include=FALSE-------------------------
# Pausa antes de proseguir
pausa()

## ----Comparación GradienteCoordenada 2-----------------------------------
# Tomamos los resultados para f
comp.f.grad <- gradienteDescendente(f, f.gradiente, maxIter = 20, tol=-Inf)
comp.f.coor <- coordenadaDescendente(f, f.gradiente, maxIter = 10, tol=-Inf)

# Tomamos los resultados para E
comp.E.grad <- gradienteDescendente(E, E.gradiente, maxIter = 20, tol=-Inf)
comp.E.coor <- coordenadaDescendente(E, E.gradiente, maxIter = 10, tol=-Inf)

# Obtenemos el valor de f en el mínimo devuelto por ambos algoritmos
min.f.grad <- f(comp.f.grad$Pesos)
min.f.coor <- f(comp.f.coor$Pesos)

# Obtenemos el valor de E en el mínimo devuelto por ambos algoritmos
min.E.grad <- E(comp.E.grad$Pesos)
min.E.coor <- E(comp.E.coor$Pesos)

## ----Dataframe de comparación 2------------------------------------------
# Generamos un data frame con los datos obtenidos
tablaComp <- data.frame(c(min.f.coor, min.E.coor),
                        c(min.f.grad, min.E.grad))

colnames(tablaComp) <- c("Valor de f", "Valor de E")
rownames(tablaComp) <- c("Coordenada descendente", "Gradiente descendente")

## ----2.b - Mensaje, eval=FALSE, include=FALSE----------------------------
mensaje <- paste("Coordenada descendente - Tabla comparativa tras 20 iteraciones reales:")
print(mensaje)

## ------------------------------------------------------------------------
# Generamos la tabla a partir del data frame
kable(tablaComp, digits=15)

## ----2.b.bis - Pausa, eval=FALSE, include=FALSE--------------------------
# Pausa antes de proseguir
pausa()

## ----Definición del método de Newton-------------------------------------
# Ejecuta el método del gradiente descendente para encontrar el mínimo
# de la función f.
metodoNewton <- function(f, w_0 = c(1,1), eta = 0.1, tol = -Inf, maxIter = 50){
  # Inicializamos solución y vectores de puntos/valores intermedios
  w <- w_0
  variables.x <- w[1]
  variables.y <- w[2]
  valores <- f(w)

  # Criterio de parada: el valor de f es menor que la tolerancia
  # o se ha alcanzado el máximo de iteraciones
  while(f(w) >= tol && length(valores) < maxIter){
    # Calculamos la dirección con la hessiana
    direccion <- -solve(hessian(f, w)) %*% grad(f, w)

    # Avanzamos en esa dirección
    w <- w + eta*direccion

    # Actualizamos el valor de los vectores de puntos/valores intermedios
    variables.x <- c(variables.x, w[1])
    variables.y <- c(variables.y, w[2])
    valores <- c(valores, f(w))
  }

  # Devolvemos el punto encontrado, el número de iteraciones
  # empleadas en encontrarlo y un data frame con los puntos intermedios
  return(list("Pesos"=w, "Iter"=length(valores)-1,
              "Res"=data.frame(X = variables.x, Y = variables.y, Value = valores)))
}

## ----Generación de datos con Newton--------------------------------------
# Definimos una función que, dado p, devuelve el valor mínimo del método de Newton
# con punto inicial = (p,p) y dónde se alcanza este:
minimoGradiente <- function(p){
  valores <- metodoNewton(f, c(p, p))$Res
  unlist(valores[which.min(valores$Value),])
}

# Definimos los puntos iniciales
puntosIniciales <- c(0.1, 1, -0.5, -1)

# Usamos la funcion minimoGradiente sobre cada uno de los puntos anteriores
tablaMinimos.N <- lapply(puntosIniciales, minimoGradiente)

# Convertimos la lista devuelta en un data frame y ponemos nombres a columnas y filas
tablaMinimos.N <- data.frame(matrix(unlist(tablaMinimos.N), nrow=4, byrow=T))
colnames(tablaMinimos.N) <- c("X", "Y", "Valor")
rownames(tablaMinimos.N) <- c("(0.1, 0.1)", "(1, 1)", "(-0.5, 0.5)", "(-1, -1)")

## ----3.1 - Mensaje, eval=FALSE, include=FALSE----------------------------
mensaje <- paste("Método de Newton - La siguiente tabla indica, para cada punto inicial, el punto (x,y) donde se alcanza el mínimo y el valor del mismo:")
print(mensaje)

## ------------------------------------------------------------------------
# Generamos la tabla a partir del data frame
kable(tablaMinimos.N, digits=15)

## ----3.1 - Salida, eval=FALSE, include=FALSE-----------------------------
# Pausa antes de proseguir
pausa()

## ----Gráfico método de Newton--------------------------------------------
# Ejecutamos el experimento con valores de eta = 0.1, 0.01
newton01 <- unlist(metodoNewton(f, eta = 0.1)$Res$Value)
newton001 <- unlist(metodoNewton(f, eta = 0.01)$Res$Value)

# Generamos los dos plots
plot(newton01, col = 'red', pch = 20, type='o',
     main=expression(paste("Método de Newton con ",
                           f(x,y) == x^2 + 2*y^2 + 2*plain(sin)(2*pi*x)*plain(sin)(2*pi*y))),
     xlab="Número de iteraciones", ylab="Valor de f")
points(newton001, col = 'blue', pch = 20, type='o')

# Añadimos leyenda de los datos dibujados.
legend("topright", c(expression(eta == 0.1), expression(eta == 0.01)),
       bg="white", col=c("red", "blue"), lty=1, pch=20, lwd=1.75)

## ----Comparación gradiente - Newton--------------------------------------
# Dibuja una gráfica comparando gradiente descendente y método de Newton sobre la función f
# con tasa de aprendizaje `tasa`
dibujarComparacion <- function(p, foo = f, tasa = 0.01, ...){
  # Generamos los resultados de ambos métodos
  res.grad <- unlist(gradienteDescendente(f, f.gradiente, w_0 = c(p,p),
                                          eta = tasa, tol = -Inf)$Res$Value)
  res.newt <- unlist(metodoNewton(f, w_0 = c(p,p),
                                  eta = tasa, tol = -Inf)$Res$Value)

  # Generamos el gráfico de ambos descensos
  plot(res.grad, pch = 20, type='o', col="red",
       main=bquote(paste(w[0],"= (",.(p),", ",.(p), ")")), ...)
  points(res.newt, pch = 20, type='o', col="blue", ...)
}

# Definimos una rejilla 2x2 para los plots
prev_par <- par(no.readonly = T)

# Ajustamos los márgenes
par(mfrow=c(2, 2), mar=c(0.1, 1.1, 2.1, 0.1), oma=2.5*c(1,1,1,1))

# Generamos los cuatro gráficos
dibujarComparacion(0.1, xaxt="n")
dibujarComparacion(1, xaxt="n", yaxt="n")
dibujarComparacion(-0.5)
dibujarComparacion(-1, yaxt="n")

# Ponemos título al gráfico conjunto
mtext("Gradiente descendente y método de Newton", outer=TRUE, line=0.5)

# Añadimos leyenda
legend("right", c("Gradiente descendente", "Método de Newton"),
       bg="white", col=c("red", "blue"), lty=1, pch=20, lwd=1.75)

# Dejamos los parámetros como estaban anteriormente
par(prev_par)

## ----3.1.bis - Salida, eval=FALSE, include=FALSE-------------------------
# Pausa antes de proseguir
pausa()

## ----Reusando código-----------------------------------------------------
# Devuelve una lista de N vectores de dimensión dim cuyos valores están cogidos de
# una distribución uniforme en el intervalo `rango`
simula_unif <- function(N, dim, rango){
  lapply(rep(dim, N), runif, min = rango[1], max = rango[2])
}

# Devuelve los parámetros de una recta aleatoria que cruza el cuadrado
# intervalo x intervalo
simula_recta <- function(intervalo){
  # Simulamos dos puntos dentro del cuadrado intervalo x intervalo
  punto1 <- runif(2, min=intervalo[1], max=intervalo[2])
  punto2 <- runif(2, min=intervalo[1], max=intervalo[2])

  # Generamos los parámetros que definen la recta
  a <- (punto2[2] - punto1[2]) / (punto2[1] - punto1[1])
  b <- -a * punto1[1] + punto1[2]

  # Devolvemos un vector concatenando ambos parámetros
  c(a,b)
}

# Devuelve una función etiquetadora basada en f
generador_etiquetados <- function(f){
  function(x,y){
    sign(f(x,y))
  }
}

## ----Definición de f y D-------------------------------------------------
# Generamos la frontera que define f
regLog.frontera <- simula_recta(c(-1,1))

# Generamos los 100 datos aleatorios
datos <- simula_unif(100, 2, c(-1,1))
regLog.datos <- matrix(unlist(datos), ncol = 2, byrow = T)

# Generamos la función f objetivo
regLog.f <- function(x,y){
  y - regLog.frontera[1]*x - regLog.frontera[2]
}

## ----Vistazo a los datos-------------------------------------------------
# Generamos la función etiquetadora y generamos las etiquetas
regLog.etiquetado <- generador_etiquetados(regLog.f)
regLog.etiquetas <- regLog.etiquetado(regLog.datos[,1], regLog.datos[,2])

# Generamos un vector de colores basado en las etiquetas
colores <- ifelse(regLog.etiquetas == 1, "green", "red")

# Generamos la gráfica
plot(regLog.datos, asp = 1, col = colores, pch = 20,
     main="Muestra uniforme etiquetada", xlab="", ylab="")
abline(rev(regLog.frontera), col="blue", lwd=1.75)

## ----4 - Salida, eval=FALSE, include=FALSE-------------------------------
# Pausa antes de proseguir
pausa()

## ----RL-SGD--------------------------------------------------------------
# Devuelve la distancia euclídea entre los puntos x e y
distance <- function(x,y){
  return(sqrt(sum((x-y)^2)))
}

# Ejecuta regresión logística con gradiente descendente estocástica sobre los datos
# (x_n, y_n) = (datos, etiquetas), cno punto inicial w_0, tasa de aprendizaje
# eta y criterio de parada basado en mejoras mayores de 0.01
RL.SGD <- function(datos, etiquetas, w_0 = c(0,0), eta = 0.01, tol= 0.01){
  # Inicializamos contador de iteraciones
  iteraciones <- 0

  # Definimos la solución actual como el vector inicial. Si tiene
  # una posición más que el número de datos, lo dejamos como está;
  # si no, añadimos un 0.
  w.curr <- ifelse(length(w_0) == ncol(datos) + 1, w_0, c(w_0, 0))

  # Añadimos una columna de unos a los datos para manejar el término independiente
  datos <- cbind(datos, 1)

  # Criterio de parada: que la distancia entre la solución actual y la anterior
  # sea menor que la tolerancia. Hacemos el OR lógico con iteraciones == 0 al
  # principio para entrar la primera vez
  while(iteraciones == 0 || distance(w.prev, w.curr) > tol){
    # Aumentamos el contador de iteraciones
    iteraciones <- iteraciones+1

    # La solución anterior es la actual del último bucle
    w.prev <- w.curr

    # Comportamiento estocástico: tomamos una permutación aleatoria de los datos
    indices <- sample(length(etiquetas))

    # Bucle sobre toda la muestra
    for(index in indices){
      # Tomamos el dato y la etiqueta
      dato <- datos[index,]
      etiq <- etiquetas[index]

      # Calculamos la dirección de descenso con la función logística
      g_t <- -(etiq*dato)/(1+exp(etiq*w.curr%*%dato))

      # Nos movemos en esa dirección
      w.curr <- w.curr - eta*g_t
    }
  }

  # Definimos la recta obtenida
  recta <- -c(w.curr[1], w.curr[3]) / w.curr[2]

  # Definimos la función estimada a partir del resultado obtenido
  g <- function(x, y){
    return(y - recta[1]*x - recta[2])
  }


  # Devolvemos la solución (los pesos), la recta generada, la función estimada
  # y el número de iteraciones empleadas
  return(list("Pesos"=w.curr, "Recta" = recta,
              "g"=g, "Iter"=iteraciones))
}

## ----Grafico RL----------------------------------------------------------
# Hacemos regresión logística sobre los datos generados anteriormente
regLog.res <- RL.SGD(regLog.datos, regLog.etiquetas)

# Dibujamos la muestra etiquetada
plot(regLog.datos, asp = 1, col = colores, pch = 20,
     main="Regresión logística", xlab="", ylab="")

# Añadimos al gráfico la función objetivo
abline(rev(regLog.frontera), col="blue", lwd=1.75)
# Añadimos al gráfico la función estimada
abline(rev(regLog.res$Recta), col="coral", lwd=1.75)

# Añadimos leyenda
legend("bottomright", c("Función objetivo", "Función estimada"),
       bg="white", col=c("blue", "coral"), lty=1, lwd=1.75)

## ----4.b - Pausa, eval=FALSE, include=FALSE------------------------------
# Pausa antes de proseguir
pausa()

## ----Medir Eout RL-------------------------------------------------------
# Definimos una función para generar muestras de tamaño tam en el
# cuadrado intervalo X intervalo
generar_muestra <- function(tam = 100, intervalo = c(-1,1)){
  # Generamos la muestra uniforme
  datos.muestra <- simula_unif(N = tam, dim = 2, rango = intervalo)

  # Devolvemos una matriz con la coordenada x en la primera columna
  # y la y en la segunda
  return(matrix(unlist(datos.muestra), ncol = 2, byrow = T))
}

# Definimos función etiquetadora basada en la función objetivo
regLog.g.etiquetado <- generador_etiquetados(regLog.res$g)

# Definimos una función para medir el error en una muestra de test
medirEtest <- function(etiquetadoObjetivo, N = 100){
  # Generamos una nueva muestra de test de tamaño 100
  regLog.test <- generar_muestra(100)

  # Generamos etiquetado con f de la nueva muestra
  regLog.test.etiq <- etiquetadoObjetivo(regLog.test[, 1], regLog.test[, 2])

  # Generamos etiquetado con g de la nueva muestra
  regLog.g.etiquetas <- regLog.g.etiquetado(regLog.test[, 1], regLog.test[, 2])

  # Devolvemos el porcentaje de muestras mal etiquetadas
  return(sum(regLog.g.etiquetas != regLog.test.etiq) / nrow(regLog.test))
}

# Realizamos el conteo sobre 100 muestras de test y tomamos la media
E_out <- mean(replicate(100, medirEtest(regLog.etiquetado)))

## ----4.c-eps - Salida, eval=FALSE, include=FALSE-------------------------
mensaje <- paste("Regresión logística - Estimación de E_out con muestra de test de tamaño 1000:", E_out*100, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ------------------------------------------------------------------------
experimentoEout <- function(N = 100){
  ################# GENERACIÓN DE DATOS #################
  #######################################################

  # Generamos la frontera que define f
  frontera <- simula_recta(c(-1,1))

  # Generamos los N datos aleatorios
  datos <- simula_unif(N, 2, c(-1,1))
  datos <- matrix(unlist(datos), ncol = 2, byrow = T)

  # Generamos la función f objetivo
  f <- function(x,y){
    y - frontera[1]*x - frontera[2]
  }

  # Generamos la función etiquetadora y generamos las etiquetas
  etiquetado <- generador_etiquetados(f)
  etiquetas <- regLog.etiquetado(datos[,1], datos[,2])

  ################### REGRESIÓN LINEAL ##################
  #######################################################

  # Hacemos regresión logística sobre los datos generados anteriormente
  res <- RL.SGD(datos, etiquetas)

  ################### MEDICIÓN DEL ERROR ################
  #######################################################

  # Realizamos el conteo sobre 100 muestras de test y tomamos la media
  E_out <- mean(replicate(100, medirEtest(etiquetado)))

  return(list("Error" = E_out, "Iteraciones" = res$Iter))
}

# Repetimos el experimento del error fuera de la muestra 100 veces
resExp <- replicate(100, experimentoEout())

# Tomamos los valores de E_out medios y el número medio de iteraciones
E_out <- mean(unlist(resExp["Error",]))
epoch <- mean(unlist(resExp["Iteraciones",]))

## ----5-eps - Salida, eval=FALSE, include=FALSE---------------------------
mensaje <- paste("Regresión logística - Estimación de E_out tras 100 experimentos con funciones objetivo distintas:", E_out*100, "%")
print(mensaje)

mensaje <- paste("Regresión logística - Número de épocas medio tras 100 experimentos con funciones objetivo distintas:", epoch)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ----Lectura de dígitos--------------------------------------------------
# Leemos el fichero de entrenamiento
digitos.train <- read.table("datos/zip.train")

# Leemos el fichero de test
digitos.test <- read.table("datos/zip.test")

# Nos quedamos únicamente con los números 1 y 5
digitos.train <- digitos.train[ is.element(digitos.train[,1], c(1,5)), ]
digitos.test <- digitos.test[ is.element(digitos.test[,1], c(1,5)), ]

## ----Análisis dígitos----------------------------------------------------
# Análisis de la intensidad y la simetría de los dígitos.
analisis_digito <- function(digito){
  # Nos aseguramos que el tipo de `digito` es el correcto
  digito <- as.numeric(digito)

  # Quitamos el primer valor (la clase) y ponemos los restantes
  # en una matriz de 16x16
  numero <- matrix(digito[-1], nrow = 16)

  # Tomamos también la matriz con las columnas invertidas
  numero.inv <- numero[, ncol(numero):1]

  # Calculamos intensidad y simetria
  intensidad <- mean(digito)
  simetria <- -sum( abs(numero.inv - numero) )

  # Devolvemos un vector con: la clase, la intensidad y la simetría
  c(digito[1], intensidad, simetria)
}

## ----Plot de simetria-intensidad de dígitos------------------------------
# Analizamos las filas de la matriz dígitos
digitos.analisis.train <- t(apply(digitos.train, 1, analisis_digito))
digitos.analisis.test <- t(apply(digitos.test, 1, analisis_digito))

# Generamos un vector de colores basado en las etiquetas
colores.train <- ifelse(digitos.analisis.train[, 1] == 1, "green", "red")
colores.test <- ifelse(digitos.analisis.test[, 1] == 1, "green", "red")

# Tomamos el rango máximo de ambos datos (train y test) para que las gráficas
# tengan la misma escala
limX <- range(c(digitos.analisis.train[,2], digitos.analisis.test[,2]))
limY <- range(c(digitos.analisis.train[,3], digitos.analisis.test[,3]))

# Definimos una rejilla 1x2 para los plots
prev_par <- par(no.readonly = T)

# Ajustamos los márgenes
par(mfrow=c(1, 2), mar=c(5.0, 1.0, 2.1, 0.1), oma=2.5*c(1,1,1,1))

# Generamos las gráfica de los datos de entrenamiento
plot(digitos.analisis.train[, 2:3], col = colores.train, pch = 20, cex=0.75,
     main="Entrenamiento", xlab="Intensidad", yaxt="n", xlim = limX, ylim = limY)

# Añadimos nombre al eje Y
title(ylab="Simetría", mgp=c(1,1,10), line=0, cex.lab=1.2)

# Generamos las gráfica de los datos de test
plot(digitos.analisis.test[, 2:3], col = colores.test, pch = 20, cex=0.75,
     main="Test", xlab="Intensidad", ylab="", yaxt="n", xlim = limX, ylim = limY)

# Llevamos la escala del eje Y a la derecha
axis(4)
mtext("", side=4, line=3)

# Asignamos título a la gráfica global
mtext("Intensidad-simetría", outer=TRUE, line=0.5, cex=1.5)

# Añadimos leyenda
legend("right", c("Clase: 1", "Clase: 5"), col=c("green", "red"), pch=20)

# Dejamos los parámetros como estaban anteriormente
par(prev_par)

## ----5 - Salida, eval=FALSE, include=FALSE-------------------------------
# Pausa antes de proseguir
pausa()

## ----Regresión lineal y PLA Pocket---------------------------------------
# Calcula regresión lineal de los datos (x_n, y_n) = (datos, label)
regresLin <- function(datos, label){
  # Añadimos una columna de 1 a los datos
  X <- cbind(datos, 1)
  Y <- label

  # Calculamos la descomposición de X
  SVD <- svd(X)
  D <- diag(SVD$d)
  U <- SVD$u
  V <- SVD$v

  # Calculamos la pseudo-inversa de X
  D[abs(D)>0] <- 1/D[abs(D)>0]
  X_pseudoinv <- V %*% D**2 %*% t(V)

  # Devolvemos la solución
  return(X_pseudoinv %*% t(X) %*% Y)
}

# Implementa el algoritmo Perceptron. Devuelve una lista con tres valores:
# Coefs: Pesos
# Sol: Recta solución
# Iter: Número de iteraciones que han sido necesarias
PLA <- function(datos, label, vini, max_iter = 100){
  # Definimos w como el vector inicial. Si tiene una posición más que
  # el número de datos, lo dejamos como está; si no, añadimos un 0.
  w <- ifelse(length(vini) == ncol(datos) + 1, vini, c(vini, 0))

  # Variables usadas en el bucle
  changing <- T
  iteraciones <- 0

  # Añadimos una columna de unos a los datos
  datos <- cbind(datos, 1)

  # Inicializamos el número de errores al total de datos recibidos
  # y la mejor solución al vector de pesos inicial
  mejor_error <- length(datos[,1])
  mejor_solucion <- w

  # Bucle principal, del que salimos si no ha habido cambios tras una
  # pasada completa a los datos (solución encontrada) o si se ha llegado
  # al máximo de iteraciones permitidas (solución no encontrada)
  while(changing && iteraciones < max_iter){
    iteraciones <- iteraciones+1

    changing <- F

    # Bucle sobre toda la muestra
    for(index in seq(label)){
      dato <- datos[index,]
      etiq <- label[index]

      # Comportamiento principal: si la muestra está mal etiquetada,
      # recalcular el hiperplano para etiquetarla bien.
      if(sign(sum(w * dato)) != etiq) {
        w <- w + etiq*dato
        changing <- T
      }
    }

    # Definimos la recta generada por los pesos
    recta <- -c(w[1], w[3]) / w[2]

    # Etiquetamos la muestra con la nueva recta
    nuevo_etiquetado <- generador_etiquetados(function(x, y){y - recta[1]*x - recta[2] })
    nuevas_etiquetas <- nuevo_etiquetado(datos[,1], datos[,2])

    # Calculamos el número de muestras mal etiquetadas con la recta actual
    error_actual <- sum(nuevas_etiquetas != label)

    # Si el error actual es mejor que el mejor encontrado hasta ahora, guardamos
    # los pesos actuales y actualizamos el mejor error
    if(error_actual < mejor_error){
      mejor_error <- error_actual
      mejor_solucion <- w
    }
  }

  # Actualizamos w con la mejor solución
  w <- mejor_solucion

  # Definimos la función estimada
  g <- function(x, y){
    return(y - recta[1]*x - recta[2])
  }

  # Devolvemos los pesos, el vector (a,b), coeficientes que determinan la
  # recta y = ax + b y el número de iteraciones.
  return(list("Coefs" = w, "Recta" = recta, "Iter" = iteraciones, "g" = g))
}

## ----Regresión lineal para clasificación---------------------------------
# Preparamos los datos
digitos.train.data <- digitos.analisis.train[,2:3]
digitos.train.etiq <- ifelse(digitos.analisis.train[,1] == 1, 1, -1)

digitos.test.data <- digitos.analisis.test[,2:3]
digitos.test.etiq <- ifelse(digitos.analisis.test[,1] == 1, 1, -1)

# Hacemos regresión lineal
digitos.reg <- regresLin(digitos.train.data, digitos.train.etiq)

# Usamos el resultado de la regresión como valor inicial para el PLA Pocket
digitos.pla <- PLA(digitos.train.data, digitos.train.etiq, vini = digitos.reg)

## ----Datos con función estimada------------------------------------------
# Definimos una rejilla 1x2 para los plots
prev_par <- par(no.readonly = T)

# Ajustamos los márgenes
par(mfrow=c(1, 2), mar=c(5.0, 1.1, 2.1, 0.1), oma=2.5*c(1,1,1,1))

# Generamos las gráficas
plot(digitos.train.data, col = colores.train, pch = 20, cex=0.75,
     main="Entrenamiento", xlab="Intensidad", yaxt="n", xlim=limX, ylim=limY)
abline(rev(digitos.pla$Recta), col="blue", lwd=1.5)

# Añadimos etiqueta del eje Y a la izquierda
title(ylab="Simetría", mgp=c(1,1,10), line=0, cex.lab=1.2)

plot(digitos.test.data, col = colores.test, pch = 20, cex=0.75,
     main="Test", xlab="Intensidad", ylab="", yaxt="n", xlim=limX, ylim=limY)
abline(rev(digitos.pla$Recta), col="blue", lwd=1.5)

# Añadimos escala del eje Y a la derecha
axis(4)
mtext("", side=4, line=3)

# Añadimos título global
mtext("Intensidad-simetría", outer=TRUE, line=0.5, cex=1.5)

# Añadimos leyenda
legend("bottomleft", c("1", "5", "g"),
       col=c("green", "red", "blue"), pch=c(20, 20, NA), lwd=c(NA, NA, 1.5))

# Dejamos los parámetros como estaban anteriormente
par(prev_par)

## ----5.a - Salida, eval=FALSE, include=FALSE-----------------------------
# Pausa antes de proseguir
pausa()

## ----Cálculo Ein y Etest-------------------------------------------------
# Definimos función etiquetadora
digitos.g.etiquetado <- generador_etiquetados(digitos.pla$g)

# Etiquetamos los datos de entrenamiento y de test
digitos.g.train.etiq <- digitos.g.etiquetado(digitos.train.data[,1], digitos.train.data[,2])
digitos.g.test.etiq <- digitos.g.etiquetado(digitos.test.data[,1], digitos.test.data[,2])

# Calculamos el error dentro de la muestra y en la muestra de test
E_in <- sum(digitos.g.train.etiq != digitos.train.etiq) / length(digitos.train.etiq)
E_test <- sum(digitos.g.test.etiq != digitos.test.etiq) / length(digitos.test.etiq)

## ----5.b - Salida, eval=FALSE, include=FALSE-----------------------------
mensaje <- paste("Clasificación binaria - Error dentro de la muestra: E_in =", E_in)
print(mensaje)

mensaje <- paste("Clasificación binaria - Error en la muestra de test: E_test =", E_test)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ---- include=FALSE------------------------------------------------------
N <- nrow(digitos.train.data)
delta <- 0.05
dVC <- 3
E_out.in <- E_in + sqrt((8/N) * log((4*(2*N)^dVC+4)/delta))

N <- nrow(digitos.test.data)
delta <- 0.05
epsilon = sqrt(-log(delta/2)/(2*nrow(digitos.test.data)))
E_out.test <- E_test + epsilon

## ----Tranformación polinómica de los datos-------------------------------
# Definimos la transformación polinómica de tercer orden
phi3 <- function(dato){
  x <- dato[1]
  y <- dato[2]

  return(c(x, y, x^2, y^2, x*y, x^3, y^3, x*y^2, x^2*y))
}

# Aplicamos la transformación a cada fila de la matriz de datos
digitos.train.dataTrans <- t(apply(digitos.train.data, 1, phi3))

## ----Regresión sobre datos transformados---------------------------------
# Hacemos regresión lineal
digitos.reg <- regresLin(digitos.train.dataTrans, digitos.train.etiq)

## ----Definimos g en base a la transformación-----------------------------
# Función g estimada de la regresión. Devuelve <phi(x,y), coefs de regresión>
digitos.g.trans <- function(x,y){
  coef <- digitos.reg
  res <-  x*coef[1] + y*coef[2] +
          x^2*coef[3] + y^2*coef[4] + x*y*coef[5] +
          x^3*coef[6] + y^3*coef[7] + x*y^2*coef[8] + x^2*y*coef[9] +
          coef[10]
  return(res)
}

## ----Dibujo de la transformación-----------------------------------------
# Definimos una rejilla 1x2 para los plots
prev_par <- par(no.readonly = T)

# Ajustamos los márgenes
par(mfrow=c(1, 2), mar=c(5.0, 1.1, 2.1, 0.1), oma=2.5*c(1,1,1,1))

# Cálculo de la gráfica de la función
g.x <- seq(limX[1]-1, limX[2]+1, length=100)
g.y <- seq(limY[1]-1, limY[2]+1, length=100)
g.z <- outer(g.x, g.y, digitos.g.trans)

# Generamos las gráficas
plot(digitos.train.data, col = colores.train, pch = 20, cex=0.75,
     main="Entrenamiento", xlab="Intensidad", yaxt="n", xlim=limX, ylim=limY)

# Dibujamos la función estimada
contour(g.x, g.y, g.z, levels=0, col = "blue", add=T, drawlabels=F)

# Añadimos etiqueta del eje Y a la izquierda
title(ylab="Simetría", mgp=c(1,1,10), line=0, cex.lab=1.2)

plot(digitos.test.data, col = colores.test, pch = 20, cex=0.75,
     main="Test", xlab="Intensidad", ylab="", yaxt="n", xlim=limX, ylim=limY)

# Dibujamos la función estimada
contour(g.x, g.y, g.z, levels=0, col = "blue", add=T,  drawlabels=F)

# Añadimos escala del eje Y a la derecha
axis(4)
mtext("", side=4, line=3)

# Añadimos título global
mtext(expression(paste("Transformación polinómica ", phi1[3])),
      outer=TRUE, line=0.5, cex=1.5)

# Añadimos leyenda
legend("bottomleft", c("1", "5", "g"),
       col=c("green", "red", "blue"), pch=c(20, 20, NA), lwd=c(NA, NA, 1.5))

# Dejamos los parámetros como estaban anteriormente
par(prev_par)

## ----5.d - Salida, eval=FALSE, include=FALSE-----------------------------
# Pausa antes de proseguir
pausa()

## ----Errores transformación polinómica-----------------------------------
# Definimos función etiquetadora
digitos.g.etiquetado <- generador_etiquetados(digitos.g.trans)

# Etiquetamos los datos de entrenamiento y de test
digitos.g.train.etiq <- digitos.g.etiquetado(digitos.train.data[,1], digitos.train.data[,2])
digitos.g.test.etiq <- digitos.g.etiquetado(digitos.test.data[,1], digitos.test.data[,2])

# Calculamos el error dentro de la muestra y en la muestra de test
E_in.trans <- sum(digitos.g.train.etiq != digitos.train.etiq) / length(digitos.train.etiq)
E_test.trans <- sum(digitos.g.test.etiq != digitos.test.etiq) / length(digitos.test.etiq)

## ----5.e - Salida, eval=FALSE, include=FALSE-----------------------------
mensaje <- paste("Clasificación binaria - Error dentro de la muestra tras transformación polinómica: E_in =", E_in.trans)
print(mensaje)

mensaje <- paste("Clasificación binaria - Error en la muestra de test tras transformación polinómica: E_test =", E_test.trans)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ----Definición de polinomios de Legendre--------------------------------
# Definición recursiva del polinomio de Legendre de orden k en el punto x
legendre <- function(k, x){
  # Caso base
  if(k == 0 || k == 1){
    return(x^k)
  }
  # Recursividad
  else{
    L_k1 <- legendre(k-1,x)
    L_k2 <- legendre(k-2, x)
    L_k <- ((2*k - 1) / k) * x * L_k1 - ((k - 1) / k) * L_k2
    return(L_k)
  }
}

# Definición iterativa del polinomio de Legendre de orden k en el punto x
legendre <- function(k,x){
  # Caso base
  res = c(1,x)

  # Iterador del bucle
  d = 3

  # Bucle para calcular todos los órdenes
  while(d <= k+1){
    res[d] = ((2*(d-1) - 1) / (d-1)) * x * res[d-1] - (((d-1) - 1) / (d-1)) * res[d-2]
    d <- d+1
  }

  return(res[k+1])
}

## ----Gráfico bonito------------------------------------------------------
# Definimos una función sobrel al que poder ejecutar el apply
f <- function(x, k){
  legendre(k,x)
}

# Generamos los datos del primer polinomio
x <- seq(-1,1,by=0.001)
y <- sapply(x, f, 0)

# Generamos la gráfica del primer polinomio
plot(x,y, type='l', lwd=1.5, xlim=c(-1,1), ylim=c(-1,1),
     main="10 primeros polinomios de Legendre")

# Para los diez primeros polinomios
for (k in seq(10)) {
  # Tomamos un color aleatorio
  color <- rgb(runif(1,0,1),runif(1,0,1),runif(1,0,1))

  # Generamos la gráfica
  y <- sapply(x, f, k)
  points(x,y, type='l', col=color, lwd=1.5, xlim=c(-1,1), ylim=c(-1,1))
}

## ----1.1 - Salida, eval=FALSE, include=FALSE-----------------------------
# Pausa antes de proseguir
pausa()

## ----Parámetros----------------------------------------------------------
Qf <- 20
N <- 50
sigma <- 1

## ----Generación de coeficientes------------------------------------------
# Generamos los coeficientes normalizados
a_q <- rnorm(Qf+1) / sqrt(sum(sapply(seq(0,Qf), function(q){1 / (2*q + 1)})))

# Definimos la función objetivo f
legendre.f <- function(x){
  res <- 0
  for (q in seq(0,Qf)) {
    res <- res + a_q[q+1] * legendre(q,x)
  }

  return(res)
}

# Definimos el ruido y generamos los datos y las etiquetas ruidosas
legendre.eps = rnorm(N)
legendre.X = runif(N, -1, 1)
legendre.Y = sapply(legendre.X, legendre.f) + sigma*legendre.eps

## ----Regresión lineal para ajustar a H2 y H10----------------------------
# Aplicamos las transformaciones a los datos para ajustarlos
# a polinomios de grado 2 y 10
legendre.g2.dat = t(sapply(legendre.X,
                           function(x){ sapply(seq(1,2),
                                               function(n){x^n}) }))
legendre.g10.dat = t(sapply(legendre.X,
                            function(x){ sapply(seq(1,10),
                                                function(n){x^n}) }))

# Ajustamos con regresión lineal
legendre.g2.pesos <- regresLin(legendre.g2.dat, legendre.Y)
legendre.g10.pesos <- regresLin(legendre.g10.dat, legendre.Y)

## ------------------------------------------------------------------------
# Definimos la función objetivo de grado 2 a partir de
# los pesos obtenidos de la regresión
legendre.g2 <- function(x){
  l <- legendre.g2.pesos
  return(l[3] + l[1]*x + l[2]*x^2)
}

# Definimos la función objetivo de grado 10 a partir de
# los pesos obtenidos de la regresión
legendre.g10 <- function(x){
  l <- legendre.g10.pesos
  return(l[11] + l[1]*x + l[2]*x^2 + l[3]*x^3 + l[4]*x^4 + l[5]*x^5
         + l[6]*x^6 + l[7]*x^7 + l[8]*x^8 + l[9]*x^9 + l[10]*x^10)
}

# Generamos la gráfica de la función objtetivo y las estimadas, así como de la muestra
plot(legendre.X, legendre.Y, pch=20, col="coral", xlab="", ylab="", main="Ajuste")
points(x, sapply(x,legendre.f), col="darkolivegreen", type='l', lwd=1.5)
plot(legendre.g2, -1, 1, col="red", lwd=1.5, add=T)
plot(legendre.g10, -1, 1, col="blue", lwd=1.5, add=T)

# Añadimos leyenda al gráficos
legend("bottomright", c(expression(f), expression(g[2]), expression(g[10])),
       col=c("darkolivegreen", "red", "blue"), lwd=1.5)

## ----1.1.a - Salida, eval=FALSE, include=FALSE---------------------------
# Pausa antes de proseguir
pausa()

## ----Test sobreajuste----------------------------------------------------
# Generamos una muestra de test
test.X = runif(100, -1, 1)
leg.test.Y = sapply(test.X, legendre.f)

# Calculamos los valores de las funciones estimadas
g2.test.Y = sapply(test.X, legendre.g2)
g10.test.Y = sapply(test.X, legendre.g10)

# Devolvemos una estimación del error cuadrático medio fuera de la muestra
Etest.g2 <- sum((leg.test.Y - g2.test.Y)^2)/100
Etest.g10 <- sum((leg.test.Y - g10.test.Y)^2)/100

## ----1.1.a - Mensaje, eval=FALSE, include=FALSE--------------------------
mensaje <- paste("Polinomios de Legendre - E_test(g2) =", Etest.g2)
print(mensaje)

mensaje <- paste("Polinomios de Legendre - E_test(g10) =", Etest.g10)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ----Error fuera de la muestra polinomios--------------------------------
# m : Tamaño de la muestra de test
# g : Función estimada
medirEout <- function(m, g){
  # Generamos una nueva muestra de tamaño m
  test.X = runif(m, -1, 1)

  # Generamos los datos reales
  test.f.Y = sapply(test.X, legendre.f)

  # Generamos los datos estimados
  test.g.Y = sapply(test.X, g)

  # Medimos el error cuadrático medio
  Eout <- sum((test.f.Y - test.g.Y)^2)/m

  return(Eout)
}

## ----Experimento para medir error----------------------------------------
# TODO: Poner 1 a 100 y 5 a 50
Eout.g2 <- mean(replicate(100,medirEout(50, legendre.g2)))
Eout.g10 <- mean(replicate(100,medirEout(50, legendre.g10)))

## ----1.1.a - Mensaje1, eval=FALSE, include=FALSE-------------------------
mensaje <- paste("Polinomios de Legendre - Estimación de E_out(g2) =", Eout.g2)
print(mensaje)

mensaje <- paste("Polinomios de Legendre - Estimación de E_out(g10) =", Eout.g10)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ----Generación de datos-------------------------------------------------
# Devuelve una lista de N vectores de dimensión dim con una muestra
# gaussiana de media cero y desviación sigma
simula_gauss <- function(N, dim, sd_){
    t(sapply(rep(dim, N), rnorm, mean = 0, sd = sd_))
}

# Fijamos las constantes del experimento
N <- 100
d <- 3
sigma <- 0.5

# Generamos la muestra
reg.X <- simula_gauss(N, d, sd = 1)

# Generamos los pesos
reg.W <- simula_gauss(1, d+1, sd = 1)

# Definimos las etiquetas
reg.Y <- sapply(1:N, function(i){ reg.W %*% c(reg.X[i,],1) }) + sigma * rnorm(N)

# Definimos el parametro de regularizacion
reg.lambda <- 0.05 / N

## ----Weight decay--------------------------------------------------------
# Devuelve el vector de pesos obtenido con regresión
# lineal con weight decay
weightDecay <- function(X,Y, lambda){
  # Calculamos Z, su traspuesta y la diagonal lambda*I
  Z <- cbind(X,1)
  Zt <- t(Z)
  lambdaI <- diag(lambda, ncol(Z))

  # Calculamos la solución
  w <- solve(Zt %*% Z + lambdaI) %*% Zt %*% Y

  return(w)
}

# Calculamos el estimador de w_f: w_reg
reg.Wreg <- weightDecay(reg.X, reg.Y, reg.lambda)

## ----Leave-one-out-------------------------------------------------------
# Definimos los valores de N indicados en el enunciado
valoresN <- seq(d+15, d+115, by = 10)

# Devuelve e_{loo}; es decir, el error usando
# leave-one-out con el conjunto de test = {x_{loo}}
errorLOO <- function(loo, N, X, Y, lambda){
  # Todos los índices menos el actual
  indices <- setdiff(1:N, loo)

  # Entrenamos el modelo sin los datos (x_{loo}, y_{loo})
  Wreg <- weightDecay(X[indices,], Y[indices], lambda)

  # Devolvemos el error cuadrático medio e_{loo}
  return((Y[loo] - (t(Wreg) %*% c(X[loo,],1)))^2)
}


# Devuelve un vector con los errores e_i, con i \in {1, ..., N}
medirErrores <- function(N, numLambda = 0.05){
  # Generamos la muestra
  X <- simula_gauss(N, d, sd = 1)

  # Generamos los pesos
  W <- simula_gauss(1, d+1, sd = 1)

  # Definimos las etiquetas
  Y <- sapply(1:N, function(i){ W %*% c(X[i,],1) }) + sigma * rnorm(N)

  # Definimos el parametro de regularización
  lambda <- numLambda / N

  # Devolvemos todos los e_i
  return(sapply(1:N, errorLOO, N, X, Y, lambda))
}

## ------------------------------------------------------------------------
# Medimos los errores e_i y calculamos su media E_{cv}
errores <- sapply(valoresN, medirErrores)
erroresMedios <- sapply(errores, mean)

# Generamos gráfico de barras
barplot(erroresMedios,col = "darkolivegreen", names.arg=valoresN,
        xlab = "Valor de N", ylab = expression(E[cv]),
        main=expression(Error~medio~E[cv]))

## ----3.1.a - Mensaje, eval=FALSE, include=FALSE--------------------------
# Pausa antes de proseguir
pausa()

## ----Repetición del experimento 1000 veces-------------------------------
# Función para ejecutar el experimento sobre los N
experimento <- function(N, numLambda = 0.05){
  # Medimos los errores e_i y calculamos su media E_{cv}
  e_i <- replicate(1000, medirErrores(N, numLambda))
  E_cv <- apply(e_i, 2, mean)

  # Tomamos e_1 y e_2
  e1 <- e_i[1,]
  e2 <- e_i[2,]

  # Devolvemos un vector con las medias de e_1, e_2 y E_cv y sus varianzas
  return(c(mean(e1), mean(e2), mean(E_cv), var(e1), var(e2), var(E_cv)))
}

# Ejecutamos el experimento sobre cada uno de los N
exp.res <- t(sapply(valoresN, experimento))

# Asignamos nombres a las columnas y filas de la matriz resultante
colnames(exp.res)  <- c("MediaE_1", "MediaE_2", "MediaE_cv", "VarE_1", "VarE_2", "VarE_cv")
rownames(exp.res) <- c(valoresN)

## ----3.1.b.2 - Premensaje, eval=FALSE, include=FALSE---------------------
mensaje <- paste("Regularización - La siguiente tabla indica, para cada valor de N, las medias de e_1, e_2 y E_cv y sus varianzas (con lambda = 0.05/N):")
print(mensaje)

## ----Tabla experimento, echo=FALSE---------------------------------------
kable(exp.res)

## ----3.1.b.2 - Salida, eval=FALSE, include=FALSE-------------------------
# Pausa antes de proseguir
pausa()

## ----Gráfico de las medias-----------------------------------------------
# Definimos una rejilla 1x3 para los plots
prev_par <- par(no.readonly = T)

# Ajustamos márgenes
par(mfrow=c(1, 3), mar=c(0.1, 0.1, 2.1, 0.1), oma=2.5*c(1,1,1,1))

# Calculamos el rango total para que las rtes gráficas
# compartan la misma escala
yrange = range(exp.res)

# Dibujamos media y varianza de e_1
plot(exp.res[,"MediaE_1"], type='l', col="red", lwd=1.75,
     main=expression(E[1]), ylim = yrange)
lines(exp.res[,"VarE_1"], type='l', col="blue", lwd=1.75)

# Dibujamos media y varianza de e_2
plot(exp.res[,"MediaE_2"], type='l', col="red", lwd=1.75,
     main=expression(E[2]), ylim = yrange, yaxt="n")
lines(exp.res[,"VarE_2"], type='l', col="blue", lwd=1.75)

# Dibujamos media y varianza de E_cv
plot(exp.res[,"MediaE_cv"], type='l', col="red", lwd=1.75,
     main=expression(E[cv]), ylim = yrange, yaxt="n")
lines(exp.res[,"VarE_cv"], type='l', col="blue", lwd=1.75)

# Asignamos título global
mtext("Errores de la validación cruzada", outer=TRUE, line=0.5)

# Añadimos leyenda
legend("right", c("Media", "Varianza"),
       bg="white", col=c("red", "blue"), lty=1, lwd=1.75)

# Dejamos los parámetros como estaban anteriormente
par(prev_par)

## ----3.1.b.2 - Salida1, eval=FALSE, include=FALSE------------------------
# Pausa antes de proseguir
pausa()

## ----Gráfica Neff--------------------------------------------------------
# Definimos una función que calcula Neff y dibuja una gráfica
# del porcentaje Neff / N para cada N
calcularNeff <- function(datos){
  # Calculamos Neff con el estimador estudiado
  Neff <- datos[,"VarE_1"] / datos[,"VarE_cv"]

  # Generamos la gráfica
  plot(valoresN, 100*Neff/valoresN, ylim=c(0,115),
       pch=20, cex=1.5, col="darkolivegreen",
       xlab="Valores de N", ylab = "Porcentaje")
  abline(c(mean(100*Neff/valoresN),0), col = "red")

  # Añadimos leyenda
  legend('bottomright',c('Media', expression(N[eff]/N)),
         lty=c(1,NA), pch=c(NA,20),
         col=c("red", "darkolivegreen"))

  # Añadimos el porcentaje de Neff con respecto a N
  return(mean(100*Neff/valoresN))
}

exp.res.Neff <- calcularNeff(exp.res)

## ----3.1.f - Mensaje, eval=FALSE, include=FALSE--------------------------
mensaje <- paste("Regularización - Media del tanto por ciento del número efectivo de nuevos ejemplos N_eff con respecto a N (lambda = 0.05 / N):", exp.res.Neff, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()

## ----Repetición del experimento 1000 veces con nuevo lambda--------------
# Repetimos el experimento con lambda = 2.5/N
exp.res.2.5 <- t(sapply(valoresN, experimento, 2.5))

# Asignamos nombres a las filas y columnas de la tabla
colnames(exp.res.2.5)  <- c("MediaE_1", "MediaE_2", "MediaE_cv",
                            "VarE_1", "VarE_2", "VarE_cv")
rownames(exp.res.2.5) <- c(valoresN)
exp.res.2.5.Neff <- calcularNeff(exp.res.2.5)

## ----3.1.g - Mensaje, eval=FALSE, include=FALSE--------------------------
mensaje <- paste("Regularización - Media del tanto por ciento del número efectivo de nuevos ejemplos N_eff con respecto a N (lambda = 2.5 / N):", exp.res.2.5.Neff, "%")
print(mensaje)

# Pausa antes de proseguir
pausa()
