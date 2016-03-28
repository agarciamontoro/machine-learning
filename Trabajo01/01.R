## ----Configuración------------------------------------------------
# Definimos la pantalla como salida principal de los gráficos
X11()

# Semilla para la generación aleatoria
set.seed(19921201)

# Definimos una función para hacer las pausas entre los ejercicios
# Tomada de http://stackoverflow.com/a/15272920/3248221
pausa <- function(){
    print("Presione [Enter] para continuar...")
    . <- readLines(con = "stdin", n=1)
}

## ----1.1-----------------------------------------------------------------
# Devuelve una lista de N vectores de dimensión dim con una muestra
# uniforme en el intervalo [rango[1], rango[2]]
simula_unif <- function(N, dim, rango){
    lapply(rep(dim, N), runif, min = rango[1], max = rango[2])
}

## ----1.2-----------------------------------------------------------------
# Devuelve una lista de N vectores de dimensión dim con una muestra
# gaussiana de media cero y desviación sigma
simula_gauss <- function(N, dim, sigma){
    lapply(rep(dim, N), rnorm, mean = 0, sd = sigma)
}

## ----1.3 - Datos---------------------------------------------------------
# Muestra uniforme de 50 puntos en el cuadrado [-50,50] x [-50,50]
datos_unif <- simula_unif(50, 2, c(-50,50))

# Guardamos las coordenadas en variables diferentes
unif.x <- unlist(lapply(datos_unif, '[[', 1))
unif.y <- unlist(lapply(datos_unif, '[[', 2))

## ----1.3 - Plot----------------------------------------------------------
plot(unif.x, unif.y, col = 'red', pch = 20, asp=1,
     main="Muestra uniforme", xlab="", ylab="")

## ----1.3 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----1.4 - Datos---------------------------------------------------------
# Muestra gaussiana de 50 puntos con media 0, desviación 5 en la
# abscisa y desviación 7 en la ordenada

# Guardamos las coordenadas en variables diferentes
datos_norm <- simula_gauss(50, 2, c(5,7))
norm.x <- unlist(lapply(datos_norm, '[[', 1))
norm.y <- unlist(lapply(datos_norm, '[[', 2))

## ----1.4 - Plot----------------------------------------------------------
plot(norm.x, norm.y, col = 'blue', pch = 20, asp = 1,
     main="Muestra gaussiana", xlab="Abscisas (desviación = 5)", ylab="Ordenadas (desviación = 7)")

## ----1.4 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----1.5-----------------------------------------------------------------
# Genera los parámetros (a,b) de una recta aleatoria y = a*x + b que corta
# al cuadrado [intervalo[1], intervalo[2]] x [intervalo[1], intervalo[2]]
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

## ----1.6 - Generador etiquetados-----------------------------------------
# Devuelve una función etiquetadora basada en el signo que
# toma el parámetro f
generador_etiquetados <- function(f){
    function(x,y){
        sign(f(x,y))
    }
}

## ----1.6 - Simulacion recta----------------------------------------------
# Generamos recta aleatoria
recta <- simula_recta(c(-50,50))

# Definimos la función cuyo signo etiquetará los datos
f1 <- function(x,y){
    y - recta[1]*x - recta[2]
}

# Generamos función etiquetadora
etiquetado1 <- generador_etiquetados(f1)

## ----1.6 - Simulacion datos----------------------------------------------
# Generamos muestra uniforme aleatoria
datos_unif <- simula_unif(50, 2, c(-50,50))

# Guardamos las coordenadas en variables separadas
unif.x <- unlist(lapply(datos_unif, '[[', 1))
unif.y <- unlist(lapply(datos_unif, '[[', 2))

# Encapsulamos los datos y las etiquetas en un data frame
datos <- data.frame(X = unif.x, Y = unif.y, Etiqueta = etiquetado1(unif.x, unif.y))

## ----1.6 - Plot----------------------------------------------------------
# Definimos un vector de colores basado en las etiqueta
colores <- ifelse(datos$Etiqueta == 1, "green", "red")

# Generamos la gráfica
plot(datos$X, datos$Y, asp = 1, col = colores, pch = 20,
     main="Muestra uniforme etiquetada", xlab="", ylab="")

# Dibujamos la recta clasificadora
abline(rev(recta))

## ----1.6 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----1.7 - Funciones-----------------------------------------------------
# Generación de funciones para etiquetar
f2 <- function(x,y){
    (x-10)**2 + (y-20)**2 - 400
}

f3 <- function(x,y){
    0.5*(x+10)**2 + (y-20)**2 - 400
}

f4 <- function(x,y){
    0.5*(x-10)**2 - (y+20)**2 - 400
}

f5 <- function(x,y){
    y - 20*x**2 - 5*x + 3
}

## ----1.7 - Genera_grafico------------------------------------------------
# Etiqueta la muestra (dat.x, dat.y) con la función f y genera el
# correspondiente gráfico
genera_grafico <- function(dat.x, dat.y, f, ...){
    # Etiqueta la muestra
    etiquetado <- generador_etiquetados(f)
    etiquetas <- etiquetado(dat.x, dat.y)

    # Genera vector de colores basado en las etiquetas
    colores <- ifelse(etiquetas == 1, "green", "red")

    # Dibujo de la muestra (pasamos los argumentos ... a plot)
    plot(dat.x, dat.y, asp = 1, col = colores, pch = 20, ...)

    # Dibujo de la gráfica de la función
    f.x <- seq(-50, 50, length=1000)
    f.y <- seq(-50, 50, length=1000)
    f.z <- outer(f.x, f.y, f)
    contour(f.x, f.y, f.z, levels=0, col = "blue", add=T, drawlabels=F)

    # Devolvemos las etiquetas, que las necesitaremos más adelante:
    return(etiquetas)
}

## ----1.7 - Plot----------------------------------------------------------
# Definimos una rejilla 2x2 para los plots
prev_par <- par(no.readonly = T)

par(mfrow=c(2, 2), mar=c(0.1, 1.1, 2.1, 0.1), oma=2.5*c(1,1,1,1))

# Generamos los cuatro plots
etiquetas_2 <- genera_grafico(unif.x, unif.y, f2, cex=0.5, cex.main=0.9, xlab="", ylab="", xaxt="n",
                              main=expression(f[2](x,y) == (x-10)^2 + (y-20)^2 - 400))

etiquetas_3 <- genera_grafico(unif.x, unif.y, f3, cex=0.5, cex.main=0.9, xlab="", ylab="", xaxt="n", yaxt="n",
                              main=expression(f[3](x,y) == 0.5(x+10)^2 + (y-20)^2 - 400))

etiquetas_4 <- genera_grafico(unif.x, unif.y, f4, cex=0.5, cex.main=0.9, xlab="", ylab="",
                              main=expression(f[4](x,y) == 0.5(x-10)^2 - (y+20)^2 - 400))

etiquetas_5 <- genera_grafico(unif.x, unif.y, f5, cex=0.5, cex.main=0.9, xlab="", ylab="", yaxt="n",
                              main=expression(f[5](x,y) == y - 20*x^2 - 5*x + 3))

mtext("Etiquetado de una muestra con diversas funciones", outer=TRUE, line=0.5)

# Dejamos los parámetros como estaban anteriormente
par(prev_par)

## ----1.7 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----1.8 - Etiquetas-----------------------------------------------------
# Tomamos los indices de las etiquetas positivas y los de las etiquetas negativas.
indices_pos <- which(datos$Etiqueta %in% 1)
indices_neg <- which(datos$Etiqueta %in% -1)

# Tomamos una muestra del 10% de esos índices
cambiar_pos <- sample(indices_pos, round(0.1*length(indices_pos)))
cambiar_neg <- sample(indices_neg, round(0.1*length(indices_neg)))

# Los índices positivos los ponemos a -1 y los negativos a 1
datos$Etiqueta[cambiar_pos] <- -1
datos$Etiqueta[cambiar_neg] <- 1

## ----1.8 - Plot----------------------------------------------------------
# Generamos vector de colores con las nuevas etiquetas
colores <- ifelse(datos$Etiqueta == 1, "green", "red")

# Dibujamos la muestra con los colores recién calculados y la recta del apartado 6
plot(datos$X, datos$Y, asp = 1, col = colores, pch = 20,
     main="Muestra uniforme con ruido", xlab="", ylab="")
abline(rev(recta), col="blue")

## ----1.8 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----1.8 - Genera_grafico------------------------------------------------
# Etiqueta la muestra (dat.x, dat.y) con la función f y genera el
# correspondiente gráfico
genera_grafico <- function(dat.x, dat.y, f, etiquetas, generarRuido = F, ...){
    # Si el parámetro etiquetas no se ha pasado, se generan con la f
    if(missing(etiquetas)){
        etiquetado <- generador_etiquetados(f)
        etiquetas <- etiquetado(dat.x, dat.y)
    }

    # Se genera ruido en el 10% de etiquetas positivas y negativas
    if(generarRuido){
        # Tomamos los indices de las etiquetas positivas y los de las etiquetas negativas.
        indices_pos <- which(etiquetas %in% 1)
        indices_neg <- which(etiquetas %in% -1)

        # Tomamos una muestra del 10% de esos índices
        cambiar_pos <- sample(indices_pos, round(0.1*length(indices_pos)))
        cambiar_neg <- sample(indices_neg, round(0.1*length(indices_neg)))

        # Los índices positivos los ponemos a -1 y los negativos a 1
        etiquetas[cambiar_pos] <- -1
        etiquetas[cambiar_neg] <- 1
    }

    # Genera vector de colores basado en las etiquetas
    colores <- ifelse(etiquetas == 1, "green", "red")

    # Dibujo de la muestra (pasamos los argumentos ... a plot)
    plot(dat.x, dat.y, asp = 1, col = colores, pch = 20, ...)

    # Dibujo de la gráfica de la función
    f.x <- seq(-50, 50, length=1000)
    f.y <- seq(-50, 50, length=1000)
    f.z <- outer(f.x, f.y, f)
    contour(f.x, f.y, f.z, levels=0, col = "blue", add=T, drawlabels=F)

    # Devolvemos las etiquetas, que las necesitaremos más adelante:
    return(etiquetas)
}

## ----1.8 - Plot fi-------------------------------------------------------
prev_par <- par(no.readonly = T)

par(mfrow=c(2, 2), mar=c(0.1, 1.1, 2.1, 0.1), oma=2.5*c(1,1,1,1))

# Generamos los cuatro plots
etiquetas_ruido_2 <- genera_grafico(datos$X, datos$Y, f2, generarRuido = T, cex=0.5, cex.main=0.9,
                                    xlab="", ylab="", xaxt="n",
                                    main=expression(f[2](x,y) == (x-10)^2 + (y-20)^2 - 400))

etiquetas_ruido_3 <- genera_grafico(datos$X, datos$Y, f3, generarRuido = T, cex=0.5, cex.main=0.9,
                                    xlab="", ylab="", xaxt="n", yaxt="n",
                                    main=expression(f[3](x,y) == 0.5(x+10)^2 + (y-20)^2 - 400))

etiquetas_ruido_4 <- genera_grafico(datos$X, datos$Y, f4, generarRuido = T, cex=0.5, cex.main=0.9,
                                    xlab="", ylab="",
                                    main=expression(f[4](x,y) == 0.5(x-10)^2 - (y+20)^2 - 400))

etiquetas_ruido_5 <- genera_grafico(datos$X, datos$Y, f5, generarRuido = T, cex=0.5, cex.main=0.9,
                                    xlab="", ylab="", yaxt="n",
                                    main=expression(f[5](x,y) == y - 20*x^2 - 5*x + 3))

mtext("Etiquetado ruidoso de una muestra con diversas funciones", outer=TRUE, line=0.5)

# Dejamos los parámetros como estaban anteriormente
par(prev_par)

## ----1.8 - Final------------------------------
# Pausa antes de proseguir
pausa()

## ----2.1-----------------------------------------------------------------
# Implementa el algoritmo Perceptron. Devuelve una lista con tres valores:
# Coefs: Pesos
# Recta: Recta solución
# Iter: Número de iteraciones que han sido necesarias
ajusta_PLA <- function(datos, label, max_iter, vini){

    # Definimos w como el vector inicial. Si tiene una posición más que
    # el número de datos, lo dejamos como está; si no, añadimos un 0.
    w <- ifelse(length(vini) == ncol(datos) + 1, vini, c(vini, 0))

    # Variables usadas en el bucle
    changing <- T
    iteraciones <- 0

    # Añadimos una columna de unos a los datos
    datos <- cbind(datos, 1)

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
    }

    # Devolvemos los pesos, el vector (a,b), coeficientes que determinan la
    # recta y = ax + b y el número de iteraciones.
    return(list(Coefs = w, Recta = -c(w[1], w[3]) / w[2], Iter = iteraciones))
}

## ----2.2 - Datos---------------------------------------------------------
# Damos forma de matriz a los datos y generamos el vector de etiquetas
datos.matriz <- matrix(c(unif.x, unif.y), ncol = 2)
datos.etiqueta <- etiquetado1(unif.x, unif.y)

## ----2.2 - Prueba PLA----------------------------------------------------
# Ejecución del algoritmo
out_PLA  <- ajusta_PLA(datos.matriz, datos.etiqueta, 10000000, c(0,0))

# Guardamos la solución encontrada y el número de iteraciones:
solucion <- out_PLA$Recta
iteraciones <- out_PLA$Iter

## ----2.2 - Salida-----------------------------
mensaje <- paste("EJERCICIO 2.2: Número de iteraciones necesarias para encontrar la solución óptima:", iteraciones)
print(mensaje)

# Pausa antes de proseguir
pausa()

## ----2.2 - Plot----------------------------------------------------------
# Generamos el vector de colores basado en las etiquetas
colores <- ifelse(datos.etiqueta == 1, "green", "red")

# Generamos el gráfico de la muestra
plot(datos.matriz, asp = 1, col = colores, pch = 20,
     main="PLA sobre muestra uniforme", xlab="", ylab="")
# Añadimos la recta devuelta
abline(rev(solucion), col="blue")

## ----2.2 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----2.2 - PLA random----------------------------------------------------
# Encapsula ajusta_PLA para llamar a lapply
wrapper <- function(vector){
    datos <- ajusta_PLA(datos.matriz, datos.etiqueta, 10000000, vector)
    return(datos$Iter)
}

## ----2.2 - Analisis------------------------------------------------------
# Ejecutamos PLA 10 veces con vectores aleatorios en [0,1]
inicio_aleatorio <- lapply(simula_unif(10, 3, c(0,1)), wrapper)

# Calculamos media de iteraciones
media <- mean(unlist(inicio_aleatorio))

## ----2.2 - Salida y pausa---------------------
mensaje <- paste("EJERCICIO 2.2: Número medio de iteraciones necesarias para encontrar la solución óptima con vector inicial aleatorio:", media)
print(mensaje)
# Pausa antes de proseguir
pausa()

## ----2.3 - Analisis PLA--------------------------------------------------
# Analiza el algoritmo PLA ante un número fijo de iteraciones:
analisis_PLA <- function(max_iter, dat.x, dat.y, dat.etiquetas){

    # Damos forma a los datos para pasárselos a ajusta_PLA
    datos.matriz <- matrix(c(dat.x, dat.y), ncol = 2)

    # Llamamos a la función y guardamos la recta solución
    out_PLA <- ajusta_PLA(datos.matriz, dat.etiquetas, max_iter, c(0,0))
    recta <- out_PLA$Recta

    # Clasificamos con la función etiquetadora determinada por la recta
    nuevo_etiquetado <- generador_etiquetados(function(x, y){ y - recta[1]*x - recta[2] })
    nuevas_etiquetas <- nuevo_etiquetado(dat.x, dat.y)

    # Devolvemos el número de muestras mal etiquetadas:
    return(sum(nuevas_etiquetas != dat.etiquetas))
}

## ----2.3 - Ejecutar analisis---------------------------------------------
# Llamamos a la función con 10, 100 y 1000 iteraciones
iteraciones <- c(10, 100, 1000)
analisis <- lapply(iteraciones, analisis_PLA, datos$X, datos$Y, datos$Etiqueta)

## ----2.3 - Salida-----------------------------
# Imprimimos los resultados
for(i in seq(analisis)){
    iter <- iteraciones[i]
    errores <- analisis[[i]]
    mensaje <- paste("EJERCICIO 2.3: Número de etiquetas diferentes con", iter, "iteraciones:", errores)
    print(mensaje)
}

## ----2.3 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----2.4 - Analisis------------------------------------------------------
# Llamamos a la función con 10, 100 y 1000 iteraciones
iteraciones <- c(10, 100, 1000)
analisis <- lapply(iteraciones, analisis_PLA, unif.x, unif.y, etiquetas_2)

## ----2.4 - Salida-----------------------------
# Imprimimos los resultados
for(i in seq(analisis)){
    iter <- iteraciones[i]
    errores <- analisis[[i]]
    mensaje <- paste("EJERCICIO 2.4: Número de etiquetas diferentes con", iter, "iteraciones:", errores)
    print(mensaje)
}

## ----2.4 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----2.5 - PLA_anim------------------------------------------------------
# Implementa el algoritmo Perceptron. Devuelve una lista con tres valores:
# Coefs: Pesos
# Sol: Recta solución
# Iter: Número de iteraciones que han sido necesarias
ajusta_PLA_anim <- function(datos, label, max_iter, vini){

    # Definimos w como el vector inicial. Si tiene una posición más que
    # el número de datos, lo dejamos como está; si no, añadimos un 0.
    w <- ifelse(length(vini) == ncol(datos) + 1, vini, c(vini, 0))

    # Variables usadas en el bucle
    changing <- T
    iteraciones <- 0

    # Añadimos una columna de unos a los datos
    datos <- cbind(datos, 1)

    # Generamos el vector de colores basado en las etiquetas
    colores <- ifelse(label == 1, "green", "red")

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

        # Generamos la gráfica con la muestra coloreada
        plot(tail(datos,-1), asp = 1, col = colores, pch = 20,
             main=paste("Ajustando PLA con",max_iter,"iteraciones"), xlab="", ylab="")
        # Añadimos la recta actual
        abline(-c(w[3], w[1]) / w[2], col="blue")
        # Pausamos 0.1 segundos
        Sys.sleep(0.2)
    }

    # Redibujamos la recta con más grosor
    abline(-c(w[3], w[1]) / w[2], col="blue", lwd=2)
    # Pausamos 1 segundo
    Sys.sleep(1)

    # Devolvemos los pesos, el vector (a,b), coeficientes que determinan la
    # recta y = ax + b y el número de iteraciones.
    return(list(Coefs = w, Recta = -c(w[1], w[3]) / w[2], Iter = iteraciones))
}

## ----2.5 - Animacion, eval=FALSE-----------------------------------------
# Damos forma a los datos
datos.matriz <- matrix(c(datos$X, datos$Y), ncol = 2)

# Llamamos a la función con 10, 100 y 1000 iteraciones
for(max_iter in c(10,100,1000)){
    ajusta_PLA_anim(datos.matriz, datos$Etiqueta, max_iter, c(0,0))
}

## ----2.5 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----2.6 - PLA_MOD-------------------------------------------------------
# Implementa el algoritmo Perceptron. Devuelve una lista con tres valores:
# Coefs: Pesos
# Sol: Recta solución
# Iter: Número de iteraciones que han sido necesarias
ajusta_PLA_MOD <- function(datos, label, max_iter, vini){

    # Definimos w como el vector inicial. Si tiene una posición más que
    # el número de datos, lo dejamos como está; si no, añadimos un 0.
    w <- ifelse(length(vini) == ncol(datos) + 1, vini, c(vini, 0))

    # Variables usadas en el bucle
    changing <- T
    iteraciones <- 0

    # Añadimos una columna de unos a los datos
    datos <- cbind(datos, 1)

    # Inicializamos el número de errores al total de datos recibidos
    mejor_error <- length(datos[,1])

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
        nuevo_etiquetado <- generador_etiquetados(function(x, y){ y - recta[1]*x - recta[2] })
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

    # Devolvemos los pesos, el vector (a,b), coeficientes que determinan la
    # recta y = ax + b y el número de iteraciones.
    return(list(Coefs = w, Recta = -c(w[1], w[3]) / w[2], Iter = iteraciones))
}

## ----2.6 - Analisis MOD--------------------------------------------------
# Analiza el algoritmo PLA ante un número fijo de iteraciones:
analisis_PLA_MOD <- function(dat.etiquetas, ajuste, dat.x, dat.y){

    # Damos forma a los datos para pasárselos a ajusta_PLA
    datos.matriz <- matrix(c(dat.x, dat.y), ncol = 2)

    # Llamamos a la función y guardamos la recta solución
    out_PLA <- ajuste(datos.matriz, dat.etiquetas, 1000, c(0,0))
    recta <- out_PLA$Recta

    # Clasificamos con la función etiquetadora determinada por la recta
    nuevo_etiquetado <- generador_etiquetados(function(x, y){ y - recta[1]*x - recta[2] })
    nuevas_etiquetas <- nuevo_etiquetado(dat.x, dat.y)

    # Devolvemos el número de muestras mal etiquetadas:
    return(sum(nuevas_etiquetas != dat.etiquetas))
}

## ----2.6 - Ejecutar analisis---------------------------------------------
# Lista de etiquetas del ejercicio 7 de la sección anterior
lista_etiquetas <- list(etiquetas_2, etiquetas_3, etiquetas_4, etiquetas_5)

# Generamos un vector con el número de muestras mal etiquetadas con ajusta_PLA
errores <- unlist(lapply(lista_etiquetas, analisis_PLA_MOD, ajusta_PLA, unif.x, unif.y))

# Generamos un vector con el número de muestras mal etiquetadas con ajusta_PLA_MOD
errores_MOD <- unlist(lapply(lista_etiquetas, analisis_PLA_MOD, ajusta_PLA_MOD, unif.x, unif.y))

## ----2.6 - Salida-----------------------------
funciones <- c("f_2", "f_3", "f_4", "f_5")
for(index in seq(errores)){
    mensaje <- paste("EJERCICIO 2.6: El algoritmo pocket tiene", errores[index] - errores_MOD[index],
                  "muestras erróneas menos que el algoritmo PLA original con la función", funciones[index])
    print(mensaje)
}

## ----2.6 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----3.2 - Lectura-------------------------------------------------------
# Leemos el fichero de entrenamiento
digitos <- read.table("datos/zip.train")

# Nos quedamos únicamente con los números 1 y 5
digitos <- digitos[ is.element(digitos[,1], c(1,5)), ]

# Guardamos en variables separadas los unos y los cincos
digitos.unos <- digitos[ digitos[,1] == 1, ]
digitos.cincos <- digitos[ digitos[,1] == 5, ]

## ----3.2 - Imagenes------------------------------------------------------
# Guardamos los ajustes de par
prev_par <- par(no.readonly = T)

# Generamos una rejilla 4x4 y ajustamos márgenes
par(mfrow = c(4, 4), mar = 0.2*c(1,1,1,1), oma = 3.5*c(1,1,1,1))

# Dibujamos los primeros 16 dígitos y desechamos la salida
. <- apply(digitos[1:16,], 1, function(digito) {
    # Matriz que define el número
    numero <- matrix(tail(digito, -1), nrow = 16)

    # Están tumbados, así que les damos la vuelta
    numero <- numero[, ncol(numero):1]

    # Generamos la imagen y ajustamos el grosor de la caja que los contiene
    image(numero, col = gray(seq(1,0,length=256)), xaxt="n", yaxt="n", pty="s", asp=1)
    box(lwd=3)
})

# Añadimos un título al gráfico
mtext("16 primeras instancias de los dígitos", outer=TRUE, line=1, cex=1.3)

# Reiniciamos los ajustes de par a su estado original
par(prev_par)

## ----3.2 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----3.3 - Analisis------------------------------------------------------
# A
analisis_digito <- function(digito){
    numero <- matrix(tail(digito, -1), nrow = 16)
    numero.inv <- numero[, ncol(numero):1]

    intensidad <- mean(digito)
    simetria <- -sum( abs(numero.inv - numero) )

    c(head(digito, 1), intensidad, simetria)
}

## ----3.4 - Analisis y plot-----------------------------------------------
# Analizamos las filas de la matriz dígitos
digitos.analisis <- t(apply(digitos, 1, analisis_digito))

# Generamos un vector de colores basado en las etiquetas
colores <- ifelse(digitos.analisis[, 1] == 1, "green", "red")

# Generamos la gráfica solicitada:
plot(digitos.analisis[, 2:3], col = colores, pch = 20,
     main="Intensidad-simetría", xlab="Intensidad", ylab="Simetría")

## ----3.4 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----3.5 - Regresion-----------------------------------------------------
# Necesario para llamar a la función ginv, que calcula la pseudo-inversa
library(MASS)

# Calcula regresión lineal
Regress_Lin <- function(datos, label){
    # Añadimos una columna de 1 a los datos
    X <- cbind(datos, 1)
    Y <- label

    # Calculamos la descomposición de X
    SVD <- svd(X)
    D <- diag(SVD$d)
    U <- SVD$u
    V <- SVD$v

    # Calculamos la inversa de X^tX
    XtX_inv <- V %*% ginv(D ** 2) %*% t(V)
    X_pseudoinv <- XtX_inv %*% t(X)

    # Devolvemos la solución
    return(X_pseudoinv %*% Y)
}

## ----3.6 - Prueba regresion y plot---------------------------------------
# Regresión lineal de la nube de puntos
digitos.regresion <- Regress_Lin(digitos.analisis[, 2], digitos.analisis[, 3])

# Generamos vector de colores basado en las etiquetas
colores <- ifelse(digitos.analisis[,1] == 1, "green", "red")

# Generamos el gráfico con los puntos y la recta encontrada
plot(digitos.analisis[,2:3], col = colores, pch=20,
     main="Regresión lineal de la nube de puntos", xlab="Intensidad", ylab="Simetría")
abline(rev(digitos.regresion), col="blue")

## ----3.6 - Pausa------------------------------
# Pausa antes de proseguir
pausa()

## ----3.6 - Prueba clasificacion y plot-----------------------------------
# Clasificación de la nube de puntos usando regresión lineal
digitos.etiquetas_reg <- ifelse(digitos.analisis[, 1] == 1, 1, -1)
digitos.clasificacion <- Regress_Lin(digitos.analisis[, 2:3], digitos.etiquetas_reg)

# Generamos el gráfico con los puntos
plot(digitos.analisis[,2:3], col = colores, pch=20,
     main="Clasificación usando regresión lineal", xlab="Intensidad", ylab="Simetría")

# Calculamos la recta definida por los pesos encontrados en la regresión y la dibujamos:
d <- digitos.clasificacion
digitos.recta_clas <- -c(d[3], d[1]) / d[2]
abline(digitos.recta_clas, col="blue")

## ----3.6 - Final------------------------------
# Pausa antes de proseguir
pausa()

## ----3.7 - Funciones-----------------------------------------------------
# Genera una muestra uniforme 2D de tamaño tam sobre el cuadrado [-10,10]x[-10,10].
# Devuelve una matriz con las x en la primera columna y las y en la segunda
generar_muestra <- function(tam = 100){

    # Generamos la muestra uniforme
    datos.muestra <- simula_unif(N = tam, dim = 2, rango = c(-10, 10))

    # Tomamos las x y las y por separado
    datos.x <- unlist(lapply(datos.muestra, '[[', 1))
    datos.y <- unlist(lapply(datos.muestra, '[[', 2))

    # Devolvemos una matriz con las x en la primera columna y las y en la segunda
    return(matrix(c(datos.x, datos.y), ncol = 2))
}

# Genera un etiquetado aleatorio sobre una muestra en el cuadrado [-10,10]x[-10,10]
# Devuelve la función generada, la recta, las etiquetas y el etiquetador
generar_etiquetado <- function(muestra){

    # Generamos una recta aleatoria
    recta <- simula_recta(intervalo = c(-10, 10))

    # Definimos la función etiquetadora en base a esa reca
    f <- function(x,y){
        y - recta[1]*x - recta[2]
    }

    # Generamos el etiquetado
    etiquetador <- generador_etiquetados(f)
    etiquetas <- etiquetador(muestra[, 1], muestra[, 2])

    # Devolvemos la función generada, la recta, las etiquetas y el etiquetador
    return(list(Foo = f, Recta = recta, Etiquetas = etiquetas, Etiquetador = etiquetador))
}

## ----3.7.a - Medir Ein---------------------------------------------------
# Mide el error dentro de la muestra con un etiquetado aleatorio
medir_Ein <- function(muestra){
    # Genera un etiquetado aleatorio de la muestra
    f.etiquetado <- generar_etiquetado(muestra)

    # Estima la recta que mejor aproxima la clasificación usando regresión
    regresion <- Regress_Lin(muestra, f.etiquetado$Etiquetas)
    g.recta <- -c(regresion[1], regresion[3]) / regresion[2]

    # Define la función clasificadora estimada
    g <- function(x,y){
        y - g.recta[1]*x - g.recta[2]
    }

    # Etiqueta la muestra con la función estimada
    g.etiquetador <- generador_etiquetados(g)
    g.etiquetas <- g.etiquetador(muestra[, 1], muestra[, 2])

    # Calcula el número de muestras mal etiquetadas
    E_in <- sum(g.etiquetas != f.etiquetado$Etiquetas)

    # Devuelve el porcentaje de error dentro de la muestra
    return(E_in / nrow(muestra))
}

## ----3.7.a - Analisis----------------------------------------------------
# Generamos una muestra aleatoria de tamaño 100
reg.muestra <- generar_muestra(100)

# Definimos el número de repiticiones
num_rep <- 1000

# Repetimos el experimento num_rep veces y tomamos la media de los valores devueltos
E_in <- sum(replicate(num_rep, medir_Ein(reg.muestra))) / num_rep

## ----3.7.a - Salida---------------------------
mensaje <- paste("EJERCICIO 3.7.a: Error medio dentro de la muestra tras", num_rep, "repeticiones:", E_in * 100, "%")
print(mensaje)

## ----3.7.a - Pausa----------------------------
# Pausa antes de proseguir
pausa()

## ----3.7.b - Medir Eout--------------------------------------------------
# Mide el error fuera de la muestra con una muestra de test aleatoria
medir_Eout <- function(muestra){
    # Genera un etiquetado aleatorio de la muestra
    f.etiquetado <- generar_etiquetado(muestra)

    # Estima la recta que mejor aproxima la clasificación usando regresión
    regresion <- Regress_Lin(muestra, f.etiquetado$Etiquetas)
    g.recta <- -c(regresion[1], regresion[3]) / regresion[2]

    # Define la función clasificadora estimada
    g <- function(x,y){
        y - g.recta[1]*x - g.recta[2]
    }

    # Generamos una nueva muestra
    test.muestra <- generar_muestra(1000)

    # Generamos etiquetado con f de la nueva muestra
    test.etiquetas <- f.etiquetado$Etiquetador(test.muestra[, 1], test.muestra[, 2])

    # Generamos etiquetado con g de la nueva muestra
    g.etiquetador <- generador_etiquetados(g)
    g.etiquetas <- g.etiquetador(test.muestra[, 1], test.muestra[, 2])

    # Calcula el número de muestras mal etiquetadas
    E_out <- sum(g.etiquetas != test.etiquetas)

    # Devuelve el porcentaje de error fuera de la muestra
    return(E_out / nrow(test.muestra))
}

## ----3.7.b - Analisis----------------------------------------------------
# Generamos una muestra aleatoria de tamaño 100
reg.muestra <- generar_muestra(100)

# Definimos el número de repiticiones
num_rep <- 1000

# Repetimos el experimento num_rep veces y tomamos la media de los valores devueltos
E_out <- sum(replicate(num_rep, medir_Eout(reg.muestra))) / num_rep

## ----3.7.b - Salida---------------------------
mensaje <- paste("EJERCICIO 3.7.b: Error medio fuera de la muestra tras", num_rep, "repeticiones:", E_out * 100, "%")
print(mensaje)

## ----3.7.b - Pausa----------------------------
# Pausa antes de proseguir
pausa()

## ----3.7.c - Medir iteraciones-------------------------------------------
medir_iter_PLA <- function(){
    # Generamos etiquetado con f
    muestra <- generar_muestra(10)
    f.etiquetado <- generar_etiquetado(muestra)

    # Estimamos g
    regresion <- Regress_Lin(muestra, f.etiquetado$Etiquetas)

    PLA_aleat <- ajusta_PLA(datos = muestra, label = f.etiquetado$Etiquetas,
                            max_iter = 1000, vini = regresion)
    PLA_ceros <- ajusta_PLA(datos = muestra, label = f.etiquetado$Etiquetas,
                            max_iter = 1000, vini = c(0,0,0))

    return(c(PLA_ceros$Iter, PLA_aleat$Iter))
}

## ----3.7.c - Analisis----------------------------------------------------
# Definimos el número de repiticiones
num_rep <- 1000

# Repetimos el experimento num_rep veces y tomamos la media de los valores devueltos
num_iter <- replicate(num_rep, medir_iter_PLA())
num_iter.media <- apply(num_iter, 1, mean)

## ----3.7.c - Salida---------------------------
mensaje <- paste("EJERCICIO 3.7.c: Número medio de iteraciones tras", num_rep, "repeticiones:",
              num_iter.media[1], "con vector inicial = (0, 0, 0) y",
              num_iter.media[2], "con vector inicial el resultado de la regresión.")
print(mensaje)

## ----3.7.c - Pausa----------------------------
# Pausa antes de proseguir
pausa()

## ----3.8 - Etiquetado----------------------------------------------------
# Definimos la función f
f <- function(x1, x2){
    x1 ** 2 + x2 ** 2 - 25
}

# Definimos la función etiquetadora
f.etiquetado <- generador_etiquetados(f)

## ----3.8 - Ruido---------------------------------------------------------
# Generación de la muestra
muestra <- generar_muestra(1000)

# Generamos las etiquetas de la muestra con f
f.etiquetas <- f.etiquetado(muestra[, 1], muestra[, 2])

# Añadimos un 10% de ruido a las etiquetas
ruido.indices <- sample(1000, 100)
f.etiquetas[ruido.indices] <- -f.etiquetas[ruido.indices]

## ----3.8 - Plot----------------------------------------------------------
# Generamos gráfico y desechamos su salida, que es un vector igual a f.etiquetas
. <- genera_grafico(muestra[, 1], muestra[, 2], f, f.etiquetas,
                    main="Clasificación de la muestra", xlab="", ylab="")

## ----3.8.a - Medir Ein---------------------------------------------------
# Mide el error dentro de la muestra
medir_Ein <- function(etiquetado){
    # Genera una muestra de 1000 puntos
    muestra <- generar_muestra(1000)

    # Etiqueta con la función recibida
    etiquetas <- etiquetado(muestra[, 1], muestra[, 2])

    # Generamos ruido en 100 puntos de la muestra
    ruido.indices <- sample(1000, 100)
    etiquetas[ruido.indices] <- -etiquetas[ruido.indices]

    # Calculamos la recta determinada por la regresión lineal
    regresion <- Regress_Lin(muestra, etiquetas)
    g.recta <- -c(regresion[1], regresion[3]) / regresion[2]

    # Definimos la función etiquetadora de la regresión lineal
    g <- function(x,y){
        y - g.recta[1]*x - g.recta[2]
    }

    # Etiquetamos con la regresión
    g.etiquetador <- generador_etiquetados(g)
    g.etiquetas <- g.etiquetador(muestra[, 1], muestra[, 2])

    # Calculamos el número de muestras mal etiquetadas
    E_in <- sum(g.etiquetas != etiquetas)

    # Devolvemos el porcentaje de error
    return(E_in / nrow(muestra))
}

## ----3.8.a - Analisis----------------------------------------------------
# Definimos el número de repiticiones
num_rep <- 1000

# Repetimos el experimento num_rep veces y tomamos la media de los valores devueltos
E_in <- sum(replicate(num_rep, medir_Ein(f.etiquetado))) / num_rep

## ----3.8.a - Salida---------------------------
mensaje <- paste("EJERCICIO 3.8.a: Error medio dentro de la muestra tras", num_rep, "repeticiones:", E_in * 100, "%")
print(mensaje)

## ----3.8.a - Pausa----------------------------
# Pausa antes de proseguir
pausa()

## ----3.8.b - Transformacion----------------------------------------------
# Hacemos la transformación de los datos
m.x <- muestra[, 1]
m.y <- muestra[, 2]
muestra.trans <- cbind(m.x, m.y, m.x * m.y, m.x ** 2, m.y ** 2)

# Calculamos la regresión lineal con los datos transformados y las etiquetas ruidosas
w <- Regress_Lin(muestra.trans, f.etiquetas)

## ----3.8.b - Plot--------------------------------------------------------
# Definimos la función según los pesos devueltos por la regresión
g <- function(x, y){
    w[1]*x + w[2]*y + w[3]*x*y + w[4]*x*x + w[5]*y*y + w[6]
}

# Generamos vector de colores basado en las etiquetas
colores <- ifelse(f.etiquetas == 1, "green", "red")

# Dibujamos la muestra original
plot(muestra, col = colores, pch=20, asp=1, cex=0.8,
     main="Estimación con datos transformados", xlab="", ylab="")

# Añadimos la gráfica de la función objetivo y la estimada
fg.x <- seq(-50, 50, length=1000)
fg.y <- seq(-50, 50, length=1000)
f.z <- outer(fg.x, fg.y, f)
g.z <- outer(fg.x, fg.y, g)

contour(fg.x, fg.y, f.z, levels=0, col = "blue",
        add=T, drawlabels=F, lwd=1.75)
contour(fg.x, fg.y, g.z, levels=0, col = "blue",
        add=T, drawlabels=F, lty=2, lwd=1.75)

# Añadimos leyenda de las funciones dibujadas.
legend("bottomright", c("Función objetivo", "Función estimada"),
       bg="white", col="blue", lty=c(1,2), lwd=1.75)

## ----3.8.b - Pausa----------------------------
# Pausa antes de proseguir
pausa()

## ----3.8.b - Medir Ein---------------------------------------------------
# Mide el error dentro de la muestra
medir_Ein <- function(etiquetado){
    # Genera una muestra de 1000 puntos
    muestra <- generar_muestra(1000)

    # Etiquetas con la función recibida
    etiquetas <- etiquetado(muestra[, 1], muestra[, 2])

    # Transformamos los datos de la muestra generada
    m.x <- muestra[, 1]
    m.y <- muestra[, 2]
    muestra.trans <- cbind(m.x, m.y, m.x * m.y, m.x ** 2, m.y ** 2)

    # Generamos ruido en 100 puntos de la muestra
    ruido.indices <- sample(1000, 100)
    etiquetas[ruido.indices] <- -etiquetas[ruido.indices]

    # Calculamos los pesos con regresión lineal
    w <- Regress_Lin(muestra.trans, etiquetas)

    # Definimos la función etiquetadora con los pesos anteriores
    g <- function(x, y){
        w[1]*x + w[2]*y + w[3]*x*y + w[4]*x*x + w[5]*y*y + w[6]
    }

    # Etiquetamos con la regresión
    g.etiquetador <- generador_etiquetados(g)
    g.etiquetas <- g.etiquetador(muestra[, 1], muestra[, 2])

    # Calculamos el número de muestras mal etiquetadas
    E_in <- sum(g.etiquetas != etiquetas)

    # Devolvemos el porcentaje de error
    return(E_in / nrow(muestra))
}

## ----3.8.b - Analisis----------------------------------------------------
g.etiquetado <- generador_etiquetados(g)

# Definimos el número de repiticiones
num_rep <- 1000

# Repetimos el experimento num_rep veces y tomamos la media de los valores devueltos
E_in <- sum(replicate(num_rep, medir_Ein(f.etiquetado))) / num_rep

## ----3.8.b - Salida---------------------------
mensaje <- paste("EJERCICIO 3.8.b: Error medio dentro de la muestra tras", num_rep, "repeticiones:", E_in * 100, "%")
print(mensaje)

## ----3.8.b - Final----------------------------
# Pausa antes de proseguir
pausa()

## ----3.8.c - Medir Eout--------------------------------------------------
# Medimos el error fuera de la muestra
medir_Eout <- function(g.etiquetado, f.etiquetado){
    # Generamos una nueva muestra
    test.muestra <- generar_muestra(1000)

    # Generamos etiquetado con f de la nueva muestra
    test.etiquetas <- f.etiquetado(test.muestra[, 1], test.muestra[, 2])

    # Generamos etiquetado con g de la nueva muestra
    g.etiquetas <- g.etiquetado(test.muestra[, 1], test.muestra[, 2])

    # Calculamos el número de muestras mal etiquetadas
    E_out <- sum(g.etiquetas != test.etiquetas)

    # Devolvemos el porcentaje de error fuera de la muestra
    return(E_out / nrow(test.muestra))
}

## ----3.8.c - Analisis----------------------------------------------------
# Definimos el número de repiticiones
num_rep <- 1000

# Repetimos el experimento num_rep veces y tomamos la media de los valores devueltos
E_out <- sum(replicate(num_rep, medir_Eout(g.etiquetado, f.etiquetado))) / num_rep

## ----3.8.c - Salida---------------------------
mensaje <- paste("EJERCICIO 3.8.c: Error medio fuera de la muestra tras", num_rep, "repeticiones:", E_out * 100, "%")
print(mensaje)

## ----3.8.c - Pausa----------------------------
# Pausa antes de proseguir
pausa()
