
############################################################################### 
#
#         ESTE PROGRAMA REALIZA EL DIAGNOSTICO DE PARA UN MODELO ARMA
#
###############################################################################

## DARIO HERNANDEZ 
## PhD in Statistics


#Elimina todos los datos en memoria en la sesion de R
rm(list=ls(all=TRUE))

library(forecast)
library(TSA)
library(readxl) 
library(tseries)


## poner la ruta donde se encuentran los datos
## Nota: cambiar "\" por "/"
#define directorio donde se van a ubicar los datos:
n.dir <-c("/Users/dariohernandez/Documents/Cursos Dictados/Universidad_Nacional 24/24_2_Time_Series/Classes/Lesson04") 
setwd(n.dir)
#datos <- read.xlsx("Programa_2_datos.xls", sheetName="Datos",colNames = TRUE) 
datos = read_excel("Programa_2_datos.xls",sheet = "Datos")

#--------------------------------
#  Identificación: Preparación de datos
#--------------------------------


yt <-  ts(datos[,2], start=c(1998, 12), frequency=12)
par(mfrow=c(1,1))
plot(yt,type="l", main= "Serie ",xlab="Tiempo")

#Transformando para estabilizar varianza
ln.yt <- log(yt)
plot(ln.yt,type="l", main= "Serie en log",xlab="Tiempo",ylim=c(3.5,5))
par(mfrow=c(1,2))
acf(ln.yt) 

# Ho: Raiz unitaria (No estacionaria). vs Ha: Estacionariedad
adf.test(ln.yt) # p.valor>0.05 entonces NO se rechaza Ho

# Primera diferencia regular para obtener estacionariedad
ytd <- diff(ln.yt,1)
mean(ytd)
plot(ytd,type="l", main= "Serie en diferencia",xlab="Tiempo",ylim=c(-0.08,0.08))
adf.test(ytd) # p.valor<0.05 entonces se rechaza Ho.

#--------------------------------
#  Identificación: Selección del modelo ARMA(p,q)
#--------------------------------

par(mfrow=c(1,2))
acf(ytd)
pacf(ytd)

#Modelo sugerido 1: ARMA(3,0)=AR(3)
eacf(ytd)

#Modelo sugerido 2: ARIMA(3,0,0)=ARMA(3,0)=AR(3)
auto.arima(ytd, trace=T)


#--------------------------------
#Estimando dos modelos:
#--------------------------------

mod1 <- Arima(ytd, order=c(3,0,0),include.mean = F)
mod1
summary(mod1)  #arroja medidas de ajuste de bondad de ajuste dentro de muestra

#ARIMA(2,0,1)=ARMA(2,1)
mod2 <- Arima(ytd, order=c(2,0,1),include.mean = F)
mod2
summary(mod2)  #arroja medidas de ajuste de bondad de ajuste dentro de muestra

# error = yt-hat(yt)
#ME:   Mean error
#RMSE: Root-Mean-Square Deviation
#MAE : Mean absolute error
#MPE:  Mean Percentage Error
#MAPE: Mean absolute percentage error
#MASE: Mean Absolute Scaled Error

#------------------------------------
# Diagnóstico del modelo 1
#------------------------------------
### Una vez estimado el modelo ARIMA(p,d=0,q)=ARMA(p,q)
### Se realiza el diagn?stico

#Tomando el modelo 1

at_est <- residuals(mod1)

#qt(p=0.05/2, df=1000-4-1,lower.tail = TRUE, log.p = FALSE)
#-0.1117/0.1774
par(mfrow=c(1,1))
plot(at_est, ylim=c(-0.1,0.1),main="Residuales del modelo 1")
abline(h=0)

#Gr?fica de los residules estadarizados
par(mfrow=c(3,1))
plot(rstandard(mod1),ylab ='Standardized Residuals', type='o'); 
abline(h=0)
abline(h=2)
abline(h=-2)
acf(at_est,lag.max=36)
pacf(at_est,lag.max=36)


###  prueba Ljung_Box para un rezago
i=1
x <- at_est
Ljung_Box1 <- Box.test(x, lag = i, type="Ljung");Ljung_Box1

###  prueba Ljung_Box para 18 rezagos
Ljung_Box <- NULL
for(i in 1:18){
 Ljung_Box1 <- Box.test(x, lag = i, type="Ljung")$p.value
 Ljung_Box <- rbind(Ljung_Box, cbind(i,Ljung_Box1))
}
colnames(Ljung_Box) <- c("Rezago","p-valor");
cat(" \n \n")
cat("Prueba Ljung_Box Hip?tesis nula independencia \n \n")
Ljung_Box

par(mfrow=c(1,1))
plot(Ljung_Box, main="Prueba Ljung and Box \n H0: Independencia",ylim=c(0,1))
abline(h=0.05,col="red")


# Este comando hace lo mismo de las anteriores lineas
tsdiag(mod1) 

### prueba Ho: mu = 0
#Tarea: Buscar un prueba para la hipotesis nula Ho:mu_resi=0
mean(at_est)


### pruebas de hip?tesis de normalidad

library(car)
library(nortest)
# Si p.valor>0.05 entonces NO se rechaza Ho
ad.test(at_est) #test de normalidad (nortest)
shapiro.test(at_est) #test de normalidad (stats)
hist(at_est)
# Conclusion: Los residuales del modelo 1 son normales

qqnorm(at_est); qqline(at_est,col=2) #Buen comportamiento de los residuales mod1



#----------------------------------------------------------------------------
########              CUSUM Y CUSUMQ            #############################
#----------------------------------------------------------------------------

#############################################################################
#### Elaborado por: WILMER OSVALDO MART?NEZ RIVERA     ######################
####                MSC Estad?stica
#############################################################################


#############################################################################
### La siguiente funcion construye los diagramas CUSUM y la CUSUMQ      #####
### Los argumentos de la funcion son x la serie de residuales y         #####  
### el valor CO, que corresponde al valor critico de la CUSUMQ el cual  #####
### depende del tamano de la serie y del nivel de confianza.            #####
#############################################################################


#La prueba CUSUM es un indicador de la instabilidad de la media condicional 
#del modelo, no permite identificar las fechas de cambio

# La prueba CUSUMSQ esta asociado a cambios en la varianza de los errores, 
# por lo tanto se sugiere aplicar previamente las pruebas de heteroscedasticidad

cucuq <- function(x,nivel){ 
    
    #C0 <- 0.09506
    #x <- t(std.resids)[,n]
    
    A <- 0.948 # CONSTANTE PARA RECTAS DEL 95% EN LA PRUEBA CUSUM 
    # (independiente del tama?o de muestra ?)
    N <- length(x)
    K <- 0
    T <- 1:N
    
    
    cu <- cumsum(x)/sd(x)
    LS <- A*sqrt(N-K-1)+2*A*(T-K-1)/sqrt(N-K-1)
    LI <- -LS
    
    cu2 <- cumsum(x*x)/sum(x*x)
    LS2 <- nivel+(T-K-1)/(N-K-1)
    LI2 <- -nivel+(T-K-1)/(N-K-1)
    
    y <- cbind(cu,LS,LI)
    y2 <- cbind(cu2,LS2,LI2)
    
    par(mfrow=c(1,2))
    par(las=1)
    matplot(x=(1:N),y, xlab="tiempo", ylab= "CUSUM", type ="l" , col=c(1,2,2),lwd = 1,lend=2, lty=c(1,2,2))
    
    par(las=1)
    matplot(x=(1:N),y2, xlab="tiempo", ylab= "CUSUMQ", type ="l" , col=c(1,2,2),lwd = 1,lend=2, lty=c(1,2,2))
    
    #durbin.watson(x)
    #shapiro.test(x)
    #levene.test(x)
}

# at_est son los residuos univariados

#--------------------------------------------------------------------
# Para las bandas de la CUSUMQ se necesita establecer el valor de CO
#--------------------------------------------------------------------
# La idea es tomar n=1/2*N
#C0 = 0.11496  valor cr?tico CUSUMQ para tama?o de muestra 100 al 5%
#ALgunos valores para C0 son al 5%: 
#C0 = 0.12522 n = 61
#C0 = 0.12170 n = 65    
#C0 = 0.11848 n = 69    
#C0 = 0.11409 n = 75    
#C0 = 0.11017 n = 81    
#C0 = 0.10777 n = 85    
#C0 = 0.10446 n = 91    
#C0 = 0.10241 n = 95    
#C0 = 0.09506 n = 150    
#C0 = 0.08293 n = 200  
#C0 = 0.05960 n = 300  
#C0 = 0.05190 n = 400  
#----------------------------------------------------------------


cucuq(at_est,0.11848)

#Conclusiones:
#CUSUM: No se evidencia cambio estructural, 
#       la suma acumulada esta dentro de los límites
# CUSUMQ: Posiblemente un problema de heterocedasticidad no tan fuerte.
#         Sugerencia: Realizar prueba de efectos ARCH

#---------------------------------------------------------------
#evalua calidad predicitva de los modelos dentro de muestra.

summary(mod1)
summary(mod2)

#Conclusion: Similar desempeno de los dos modelos dentro de muestra

#---------------------------------------------------------------
#evalua calidad predicitva de los modelos fuera de muestra

#datos.evalua <- read.xlsx("Programa_2_datos.xls", sheetName="Evaluar",colNames = TRUE) 
datos.evalua = read_excel("Programa_2_datos.xls",sheet = "Evaluar")


yt.eval <-  ts(datos.evalua[,2], start=c(2010, 12), frequency=12)


# Apply fitted model to later data
mod1.evalua <- Arima(yt.eval,model=mod1)
mod2.evalua <- Arima(yt.eval,model=mod2)

# out-of-sample one-step forecasts.
accuracy(mod1.evalua)
accuracy(mod2.evalua)

#Conclusion: Se observa un leve mejor desempeno predictivo del modelo 2.
#            Es necesario entrar a evaluar los residuales del modelo 2 (Tarea).

#---------------------------------------------------------------
# Aplicaciones
#             Pronosticos con la libreria ggplot2
#---------------------------------------------------------------
#pronosticos de la serie en diferencias
library(ggplot2)
ytd%>%
 Arima(order=c(3,0,0),include.mean = F) %>%
 forecast(h=20) %>%
 autoplot
# Intervalos de confianza del 80% y 95%

#pronosticos de la serie en niveles

ln.yt%>%
 Arima(order=c(3,1,0),include.mean = F) %>%
 forecast(h=20) %>%
 autoplot


# ##########################################################################################
# ###########               FIN PROGRAMA                   ################################
# ##########################################################################################

