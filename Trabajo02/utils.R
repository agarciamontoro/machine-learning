## ------------------------------------------------------------------------
setwd("~/AA/2016/datos")

## ------------------------------------------------------------------------
set.seed(3)

## ------------------------------------------------------------------------
simula_unifM = function (N=2,dims=2, rango = c(0,1)){
 m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
 nrow = N, ncol=dims, byrow=T)
}

## ------------------------------------------------------------------------
simula_recta = function (intervalo = c(-1,1),visible=F, ptos = NULL){
   if(is.null(ptos)) m = simula_unifM(2,2,intervalo)
   a = (m[1,2] - m[2,2]) / (m[1,1]-m[2,1]) # calculo de la pendiente
   b = m[1,2]-a*m[1,1]
   if (visible) {
       if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
           plot(ejes, type="n", xlim=intervalo, ylim=intervalo)
       abline(b,a)
       points(m,col=3) #pinta en verde los puntos
   }
   return(c(a,b))
}


## ------------------------------------------------------------------------
pausa = function(){
  cat("Pulsa cualquier caracter")
  ch= scan()
}


## ------------------------------------------------------------------------
pinta_puntos = function(m, rangox = NULL, rangoy = NULL ,etiqueta=NULL){
 nptos=nrow(m)
 long = ncol=m
 
 if(is.null(rangox) && is.null(rangoy)){
   rangox = range(m[,1])
   rangoy = range(m[,1])
 }
 else if(is.null(rangoy)) 
   rangoy = rangox
 
 if(is.null(etiqueta)) 
   etiqueta = 1
 else etiqueta = etiqueta+2

 plot(m,xlab=paste("Pinta ",nptos," Puntos"), ylab="", 
 xlim=rangox, ylim=rangoy,col=etiqueta,pch=19)
}


## ------------------------------------------------------------------------
# Solucion del apartado 1 ejercicio 6 

datos = simula_unifM (N,2,rango)
param_ab = simula_recta(rango,visible=F) 
f6 = function(pto,param_recta) {  #funcion de la recta
  pto[2]-pto[1]*param_recta[1]-param_recta[2]
}
z0 = apply(datos,1,f6,param_ab) #obtiene los valores de la funcion para datos
etiqueta = sign(z0)             # apartir de ellos crea las etiquetas
pinta_puntos(datos,rangox=rango,etiqueta=etiqueta) # utiliza las etiquetas 
abline(param_ab[2],param_ab[1]) # pinta la recta
pausa()

## ------------------------------------------------------------------------
# Para pintar las funciones
#
f71b = function(x,y) {(x-10)^2 + (y-20)^2-400} # se define la función


## ------------------------------------------------------------------------
# funcion generica para pasarle una funcion como parametro f72 ..

pintar_grafica = function(f) {
  x=y=seq(-50,50,by=0.1)
  z = outer(x,y,FUN=f)
  contour(x,y,z, level=1, drawlabels = FALSE,add = TRUE)  # añade el gráfico
}
pintar_grafica(f71b)
pausa()

## ------------------------------------------------------------------------
digit.train <- read.table("~/Docencia/AAprendizaje/2016/datos/zipDigit.train",
                          quote="\"", comment.char="", stringsAsFactors=FALSE)
digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,]
digitos = digitos15.train[,1]
ndigitos = nrow(digitos15.train)
grises = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos,16,16))
rm(digit.train)
rm(digitos15.train)


## ------------------------------------------------------------------------
par(mfrow=c(2,2))
for(i in 1:4){
  imagen = t(grises[i,,])
  image(z=imagen)
}

## ------------------------------------------------------------------------
fsimetria1 <- function(A){
#  A = matrix(v,nrow=16,ncol=16,byrow=T)
  A = abs(A-A[,ncol(A):1])
  -sum(A)
}


## ------------------------------------------------------------------------
fsimetria2 <- function(A){
#  A = matrix(v,nrow=16,ncol=16,byrow=T)
  A = abs(A-A[nrow(A):1,])
  -sum(A)
}


## ------------------------------------------------------------------------
intensidad = apply(grises[1:ndigitos,,],1, mean)
simetria1 = apply(grises[1:ndigitos,,],1,fsimetria1)
datos1 = as.matrix(cbind(intensidad,simetria1))
dev.off()
plot(datos1,xlab="Intensidad Promedio",ylab="Simetria",col=digitos+1,pch=digitos+19)

## ------------------------------------------------------------------------
intensidad = apply(grises[1:ndigitos,,],1, mean)
simetria2 = apply(grises[1:ndigitos,,],1,fsimetria2)
datos2 = as.matrix(cbind(intensidad,simetria2))
dev.off()
plot(datos2,xlab="Intensidad Promedio",ylab="Simetria", col=digitos+1,pch=digitos+19)

 
