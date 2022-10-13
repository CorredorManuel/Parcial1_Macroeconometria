#INSTALANDO PAQUETES NECESARIOS PARA GRAFICAR

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("plotly")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)

#IMPORTACIñN DE DATOS
set.seed(123)
a <- rnorm(n=511)
N <- length(a)
phi1 <- -0.2
phi2 <- -0.01
m <- 50/(1-phi1-phi2)
Z <- matrix(data = NA,nrow = N,ncol = 1)
Z[1] <- 0
Z[2] <- 1
for(i in 3:N){
  Z[i] <- phi1*(Z[i-1]-m)+phi2*(Z[i-2]-m) +a[i]#SE ELIMINA CONTSTANTE
}
#BOX JENKINS

#1. ESTABILIZACIñN DE VARIANZA

serie <- as.matrix(Z)
serie

#NñMERO DE DATOS

T <- length(serie)
T

#NñMERO DE PERIODOS EN UN AñO

R <- 15
R

#ASIGNACIñN DEL VALOR H

H <- trunc(T/R)
H

#ELIMINACIñN DE DATOS SOBRANTES

n <- T-H*R
n

#ESTABLECIENDO AñO Y MES DE INICIO

#año <- 1998
año <- 1980
mes <- 1

#ORGANIZANDO LA SERIE

seriedetiempo <- ts(serie[1:(T-n)],freq=R,start=c(año,mes))
seriedetiempo

#CALCULANDO LAS MEDIAS DE CADA GRUPO

datosmatriz <- matrix(data=serie[1:(T-n)], ncol=H, nrow=R)
datosmatriz

medias <- matrix(data=NA, nrow=1, ncol=H)

for (h in 1:H) {
  medias[h] <-mean(datosmatriz[,h])
}

medias

#CALCULANDO LAS DESVIACIONES ESTñNDAR DE CADA GRUPO

desvest <- matrix(data=NA, nrow=1, ncol=H)

for(h in 1:H){
  desvest[h] <- sqrt(sum((datosmatriz[,h]-medias[h])^2)/(R-1))
}

desvest

#CREANDO LAMBDAS

lambdas <- c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
lambdas

L <- length(lambdas)
L

#CALCULANDO LAS MEDIAS DE "CV-POTENCIA" PARA CADA LAMBDA

mlambda <- matrix(data=NA, nrow=1, ncol=L)

for(l in 1:L){
  cvpotencia <- matrix(data=NA, nrow=1, ncol=H)
  for(h in 1:H){
    cvpotencia[h] <- desvest[h]/(medias[h]^(1-lambdas[l]))
  }
  mlambda[l] <- sum(cvpotencia)/H
}

mlambda
colnames(mlambda) <- c("-2","-1.5","-1","-0.5","0","0.5","1","1.5","2")
mlambda

#CALCULANDO LAS DEVIACIONES ESTñNDAR DE "CV-POTENCIA" PARA CADA LAMBDA

desvestlambda <- matrix(data=NA, nrow=1,ncol=L)

for(l in 1:L){
  cvpotencia <- matrix(data=NA, nrow=1, ncol=H)
  for(h in 1:H){
    cvpotencia[h] <- desvest[h]/(medias[h]^(1-lambdas[l]))
  }
  numeradorsd <- sum((cvpotencia-mlambda[l])^2)
  denominadorsd <- H-1
  desvestlambda[l] <- sqrt(numeradorsd/denominadorsd)
  
}

desvestlambda
colnames(desvestlambda) <- c("-2","-1.5","-1","-0.5","0","0.5","1","1.5","2")
desvestlambda

#SELECCIñN DE LAMBDA QUE MINIMICE EL "CV"

cv <- desvestlambda/mlambda
cv

#ELECCIñN DE MEJOR LAMBDA

cvmin <- min(cv)
cvmin

lambdamin <- match(cvmin, cv)
lambdamin

lambdacv <- lambdas[lambdamin]
lambdacv

#APLICACIñN TRANSFORMACIñN SEGñN GUERRERO

datosguerrero <-serie^(lambdacv)
datosguerrero

#APLICACIñN TRANSFORMACIñN SEGñN BOX-COX

datosboxcox <- (serie^(lambdacv)-1)/(lambdacv)
datosboxcox

#APLICACIñN TRANSFORMACIñN PARA LAMDBA=0

datoslog <- log1p(serie)
datoslog

#GRAFICANDO LA SERIE SIN TRANFORMAR VS LA SERIE TRANSFORMADA

#TRAYENDO LA SERIE ORIGINAL

datosmatriz <- ts(serie[1:(T-n)], freq=R, start=c(año,mes))
datosmatriz

#PONIENDO LA SERIE TRANSFORMADA

datosguerreroplot <- ts(datosguerrero[1:(T-n)], freq=R, start=c(año,mes))
datosguerreroplot

#GRAFICANDO INDIVIDUALMENTE

#SERIE SIN TRANFORMAR

plot.ts(datosmatriz, main="Serie sin transformar", xlab="Años", ylab="Valores",col="6")

#SERIE TRANSFORMADA

plot.ts(datosguerreroplot, main="Serie transformada", xlab="Años", ylab="Valores",col="3")

#2. ESTABILIZACIñN DE NIVEL (ESTACIONARIEDAD)

#DIFERENCIANDO LA SERIE

y0 <- datosguerrero
y0
length(y0)

y1 <- diff(y0)
y1
length(y1)

y2 <- diff(y1)
y2
length(y2)

y3 <- diff(y2)
y3
length(y3)

ydiff <- cbind(y0[4:T],y1[3:T],y2[2:T],y3[1:T])
ydiff <- ydiff[1:(T-3),]
ydiff

##### Creando una matriz con columnas de cada serie diferenciada

ydiff <- cbind(y0[4:T],y1[3:T],y2[2:T],y3[1:T])
ydiff <- ydiff[1:(T-3),]
ydiff

##### Valores de diferencias

j <- c(0,1,2,3)
j

J <- length(j)
J

##### Menor desviacion estandar

desvestdiff <- matrix(data=NA, nrow=1, ncol=J)

for(i in 0:J-1){
  desvestdiff[i+1] <- sqrt((1/(T-i-1))*sum((ydiff[,i+1]-sum(ydiff[,i+1]/(T-i)))^2))
}

desvestdiff
colnames(desvestdiff) <- c("0","1","2","3")
desvestdiff

##### Seleccion de la mejor diferenciacion de la serie

desvestdiffmin <- min(desvestdiff)
desvestdiffmin

jmin <- match(desvestdiffmin, desvestdiff)
jmin

ydef <- ydiff[,jmin]
ydef

############################################3 aca ahago cambios ojo
eliminar <- as.matrix(ydef)
length(ydef)
ydef <- y0  ###############este es el cambio
d <- 1

#GRAFICANDO LAS DISTINTAS ACF PAARA CADA DIFERENCIA

acf(y0, ylim=c(-1,1))
acf(ydef, ylim=c(-1,1))
acf(y2, ylim=c(-1,1))
acf(y3, ylim=c(-1,1))

#3. ACF

K <- T/4
K

media <- mean(ydef)
media

Tydeff <- length(ydef)
Tydeff

#CON EL MñTODO VISTO EN CLASE

ACF <- matrix(data=NA, nrow=K, ncol=1)

for(k in 1:K){
  datitosk <- ydef[(k+1):Tydeff]
  datitosklag <- ydef[1:(Tydeff-k)]
  numerador <- sum((datitosk-media)*(datitosklag-media))
  denominador <- sum((ydef-media)^2)
  ACF[k] <- numerador/denominador
}

ACF

plot(ACF, type="h", ylim=c(-1,1), main="ACF", xlab="k", ylab="Rho(k)",col="6")

#CON FUNCIñN DE MATRICES

autocorrelacionk <- function(ydef,k){
  T = length(ydef)
  media <- mean(ydef)
  a <- t(c(ydef[1:(Tydeff-k)])-media)
  b <- c(ydef[(k+1):Tydeff])-media
  c <- c(ydef[1:length(ydef)])-media
  RHO <- (a%*%b)/(t(c)%*%c)}

RHO <- autocorrelacionk(ydef,1)
RHO

#COMPROBACIñN CON EL COMANDO

autocorrelaciones <- acf(ydef, type ="correlation", plot = FALSE)
autocorrelaciones

acf(ydef, ylim=c(-1,1), lag.max = K)

#4. PACF

#CON EL MñTODO DE CLASE

PACF <- matrix(data=ydef, ncol=1, nrow=k)

for(k in 1:K){
  abajo <- matrix(data=NA, ncol=k, nrow=k)
  
  for(i in 1:k){
    for(j in 1:k){
      if(i==j){
        abajo[i,j] <- 1
      }else{
        abajo[i,j] <-ACF[abs(i-j)] 
      }
    }
  }
  
  arriba <- abajo
  arriba[,k] <- ACF[1:k]
  
  PACF[k] <- det(arriba)/det(abajo)
  PACF[k]
}

PACF

plot(PACF, type="h", ylim=c(-1,1), main="PACF", xlab="k", ylab="PHI(kk)",col="6")

#COMPROBACIñN CON EL COMANDO

autocorrelacionesparciales <- pacf(ydef, plot = FALSE, lag.max=K)
autocorrelacionesparciales

pacf(ydef, ylim=c(-1,1), lag.max = K)

#DEFINIENDO EL MODELO ARIMA A PARTIR DEL ANñLISIS GRñFICO

#DEFINICIñN DEL MA(Q)

#CALCULANDO LAS DESVIACIONES DE RHO PARA CADA Q

desvestrho <- matrix(data=NA, nrow=1, ncol=K)

for(j in 1:49){
  desvestrho[j] <-2* sqrt((1/(T-d))*(1+2*((sum(ACF[1:j]))^2)))#AÑADO EL MULTIPLICADOR DE 2
}

desvestrho


#VERIFICANDO CUñL Q SE DEBE ESCOGER

revisar <- abs(ACF)>desvestrho[1]

abs(ACF)>desvestrho[2]

abs(ACF)>desvestrho[3]

abs(ACF)>desvestrho[4]

abs(ACF)>desvestrho[5]

q <- 0
q

#ESCOGIENDO p,d y q

p <- 2
p

d <- 0
d

q <- 0
q


#5. ESTIMACIñN MODELO ARIMA

estimacion <- arima(x=ydef,order=c(p,d,q),method="ML")
estimacion

#6. VERIFICACIñN DE SUPUESTOS

#6.1. MEDIA CERO

residuales <- estimacion$residuals
residuales

TR <- length(residuales)
TR

tprima <- d+p+1
tprima

#MEDIA ESTIMADA DE A

mediaa <- (sum(residuales[(tprima):TR]))/(TR-d-p)
mediaa

#DESVIACIñN ESTñNDAR ESTIMADA DE A

desvesta <- sqrt((t(residuales-mediaa)%*%(residuales-mediaa))/(T-d-p-q))
desvesta

#PRUEBA HIPñTESIS

if(abs((sqrt(TR-d-p)*mediaa)/desvesta) < 2){print("No se rechaza H0")} else {"Se rechaza H0"}

#6.2. VARIANZA CONSTANTE

plot(residuales, main="Inspecciñn de residuales", xlab="Tiempo", ylab="Residuales",col="3")

#6.3. MUTUAMENTE INDEPENDIENTES

#6.3.1. SIGNIFICANCIA INDIVIDUAL

rho <- c()

conf <- c()

for(i in 1:K){
  a1 <- residuales[tprima:(TR-i)]
  a2 <- residuales[(tprima+i):TR]
  a3 <- residuales[tprima:TR]
  rho[i]=(t(a1)%*%a2)/(t(a3)%*%a3)
}
 
rho

ifelse((abs(rho))<(1/sqrt(TR-d-p)), "No es significativo", "Es significativo")

#6.3.2. SIGNIFICANCIA GLOBAL

#6.4. NORMALIDAD

mediaresiduales <- mean(residuales)
mediaresiduales

desvestresiduales <- sd(residuales)
desvestresiduales

limitesuperior <- mediaresiduales+2*desvestresiduales
limitesuperior

limiteinferior <- mediaresiduales-2*desvestresiduales
limiteinferior

INTERVALONOR <- c()

for(i in 1:TR){
  if(residuales[i]<limitesuperior & residuales[i]>limiteinferior){
    INTERVALONOR[i]=NA
  } else {INTERVALONOR[i]=residuales[i]}
}

INTERVALONOR <- na.omit(INTERVALONOR)
INTERVALONOR

100*length(INTERVALONOR)/TR

#6.5. OBSERVACIONES ABERRANTES

obsaberr <- function(ydef){
  
  T <- length(ydef)
  
  mediaydef <- mean(ydef)
  
  desvestydef <- sd(ydef)
  
  limitesuperior <- mediaydef+3*desvestydef
  
  limiteinferior <- mediaydef-3*desvestydef
  
  INTERVALOABERR <- c()
  
  for(i in 1:T){
    if(ydef[i]<limitesuperior & ydef[i]>limiteinferior){
      INTERVALOABERR[i]=NA
    } else {INTERVALOABERR[i]=ydef[i]}
  }
  
  INTERVALOABERR <- na.omit(INTERVALOABERR)
  
  print(length(INTERVALOABERR))
}

obsaberr(residuales)

#6.6. PARSIMONIA

Parsim <- function(coef,var){
  desv <- sqrt(var)
  vec <- coef
  for(i in 1:length(coef)){
    limsup <- coef[i]+2*desv[i]
    liminf <- coef[i]-2*desv[i]
    if(limsup>0 & liminf>0 || limsup<0 & liminf<0){vec[i]="Parñmetro necesario"
    } else {vec[i]="Parñmetro innecesario"}
  }
  print("Test de parsimonia")
  print(vec)
}

coefficientes <- estimacion$coef
varianza <- diag(estimacion$var.coef)
Parsim(coef=coefficientes,var=varianza)

#De acuerdo con la prueba todos los parñmetros son necesarios, no es un modelo parsimonioso

#7. SIMULACIñN
#VALORES INICIALES

ys1 <- 38.63
ys2 <- 39.09

#RUIDO BLANCO

e <- rnorm(n=(T+1),mean=0,sd=58)

#7.1 CASO AR(2)
#PARAMETROS AR

phi1 <- -0.1786
phi2 <- 0.34

#PRUEBA DE ESTACIONARIEDAD

a=-0.34
b=0.1786
c=1

raiz1 = (-b + sqrt((b^2 - 4*a*c)))/(2*a)
raiz2 = (-b - sqrt((b^2 - 4*a*c)))/(2*a)

raiz1
raiz2

Factor1 <- 1/raiz1
Factor1
Factor2 <- 1/raiz2
Factor2

#El proceso es estacionario, dado que el valor absoluto de sus rañces se encuentra fuera del circulo unitario y el valor absouto de sus factores se encuentra dentro del circulo unitario

#EL PROCESO AR(2)

y <- c(ys1,ys2)
for(t in 3:T){
  y[t]=phi1*y[t-1]+phi2*y[t-2]+e[t+1]
}
y

yserie<- ts(y[1:(T-n)], freq=R, start=c(1998,1))

#GRAFICANDO

plot.ts(yserie, main="Simulaciñn AR(2)", xlab="Años", ylab="Valores",col="6")

#7.2 CASO MA(1)
#PARAMETROS MA
theta1 <- -0.235

#Dado que el valor absoluto de theta es menor a 1, las innovaciones del pasado lejano pesan menos que las innovaciones del pasado reciente, el proceso es invertible

#EL PROCESO MA(1)
z <- c(ys1,ys2)
for(t in 3:T){
  z[t]=e[t+1]-theta1*e[t]
}
z

zserie<- ts(z[1:(T-n)], freq=R, start=c(1998,1))

#GRAFICANDO

plot.ts(zserie, main="Simulaciñn MA(1)", xlab="Años", ylab="Valores",col="4")

#7.3 CASO ARMA (2,1)
#EL PROCESO ARMA(2,1)
zt <- c(ys1,ys2)
for(t in 3:T){
  zt[t]=phi1*y[t-1]+phi2*y[t-2]+e[t+1]-theta1*e[t]
}
zt

ztserie<- ts(zt[1:(T-n)], freq=R, start=c(1998,1))

#GRAFICANDO

plot.ts(ztserie, main="Simulaciñn ARMA(2,1)", xlab="Años", ylab="Valores",col="15")

