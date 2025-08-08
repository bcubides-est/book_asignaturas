#######################################################################
##  Ejercicio de ajuste modelo SAIMA                               ####
#######################################################################
## 

## Autor: Sergio Calderon
## Modificado por: DARIO HERNANDEZ (PhD in Statistics)
# Ajuste de la serie de pasajeros basada en "Time Series Analysis - With Applications in R, 2nd Ed"


library(urca)
library(forecast)
library(tseries)
library(lmtest)
library(uroot)
library(fUnitRoots)
library(aTSA)

#*******************************************************************************
# Parte 1
# Raiz Unitaria
Tlength=200

arimaej_raiz_unit=arima.sim(list(order = c(1,1,1), ar = 0.7,ma=0.6), n = Tlength)
layout(matrix(c(1,1,2, 1,1,3), nc=2))

par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
plot(arimaej_raiz_unit)
acf(arimaej_raiz_unit)
pacf(arimaej_raiz_unit)

# Ho: Raiz unitaria (No estacionaria). vs Ha: Estacionariedad
tseries::adf.test(arimaej_raiz_unit) # p.valor>0.05 entonces NO se rechaza Ho

#Other tests to check stationary
#---------------------------------------------------------
# D. Kwiatkowski, P. C. B. Phillips, P. Schmidt, and Y. 
# Shin (1992): Testing the Null Hypothesis of Stationarity 
# against the Alternative of a Unit Root. 
# Journal of Econometrics 54, 159–178.
#---------------------------------------------------------
# P. Perron (1988): Trends and Random Walks in Macroeconomic 
# Time Series. Journal of Economic Dynamics and Control 12, 
# 297–332.
# Computes the Phillips-Perron test for the null hypothesis 
# that x has a unit root.

#Kwiatkowski et al. (1992)°Øs stationarity test
# Ho: Estacionariedad vs Ha: Raiz unitaria (No estacionaria)
tseries::kpss.test(arimaej_raiz_unit)    #pval<0.05 reject times stationary

# Ho: Raiz unitaria (No estacionaria). vs Ha: Estacionariedad
tseries::pp.test(arimaej_raiz_unit)      # p.valor>0.05 entonces NO se rechaza Ho


#*******************************************************************************
# Parte 2
# Vamos a Analizar la Serie de Pasajeros.
# monthly totals of international airline passengers, 
# 1949 to 1960, taken from Box & Jenkins (1970).


#---------------------------------------------
# Estabilizar varianza
#---------------------------------------------

# Iniciamos con las gráficas y transformación de Box-Cox

data("AirPassengers")
par(mfrow=c(1,1))
plot(AirPassengers)
# Note that AirPassengers is the original series, which shows trend plus increasing variance

#Nos orientamos con esta de guerrero:
forecast::BoxCox.lambda(AirPassengers,method="guerrero",lower=0)
forecast::BoxCox.lambda(AirPassengers,method="loglik",lower=0)

forecast::BoxCox(AirPassengers,lambda="auto")
# De guerrero se tiene que lambda=0, entonces aplicamos logaritmo natural
logAirP=log(AirPassengers)

par(mfrow=c(2,1))
plot(AirPassengers)
plot(logAirP)
# Se soluciona el problema de varianza en la serie.

#---------------------------------------------
# Raices unitarias regulares y/o estacionales
#---------------------------------------------

# Ahora avanzamos en el sentido de verificar si la serie muestra 
# la presencia de una o varias raíces unitarias
par(mfrow=c(2,1))
acf(logAirP,lag.max = 60)
pacf(logAirP)

# A partir del ACF muestral: Presencia de raiz unitaria regular y patron estacional

# Ho: Raiz unitaria (No estacionaria). vs Ha: Estacionariedad
# prueba con tendencia y drift
tseries::adf.test(logAirP) # p.valor>=0.01 entonces NO se rechaza Ho. No concluyente

#Kwiatkowski et al. (1992)°Øs stationarity test
# Ho: Estacionariedad vs Ha: Raiz unitaria (No estacionaria)
tseries::kpss.test(logAirP)    #pval<0.05 reject times stationary

# Ho: Raiz unitaria (No estacionaria). vs Ha: Estacionariedad
tseries::pp.test(logAirP)      # p.valor>=0.01 entonces NO se rechaza Ho

     #---------------------------------------
     # Generalizacion de prueba de D-F
     #---------------------------------------
     #The Augmented Dickey-Fuller test incorporates three types of linear regression models. 
     
     # (a) The first type (type1) is a linear model with no drift and linear trend with respect to time:
     # 
     #     dx[t] = ρ*x[t-1] + β[1]*dx[t-1] + ... + β[nlag - 1]*dx[t - nlag + 1] +e[t],
     #where d is an operator of first order difference, i.e., dx[t] = x[t] - x[t-1], and e[t] is an error term.
     #
     # (b) The second type (type2) is a linear model with drift but no linear trend:
     # 
     #     dx[t] = μ + ρ*x[t-1] + β[1]*dx[t-1] + ... + β[nlag - 1]*dx[t - nlag + 1] +e[t].
     #
     # (c) The third type (type3) is a linear model with both drift and linear trend:
      
     #     dx[t] = μ + β*t + ρ*x[t-1] + β[1]*dx[t-1] + ... + β[nlag - 1]*dx[t - nlag + 1] +e[t].
aTSA::adf.test(logAirP,nlag = 8)

# Conclusion: Se tiene raiz unitaria regular.

# Para corrobar las diferencias regulares y estacionales
ndiffs(logAirP)    ####Decreta cuantas diferencias regulares son requeridas
nsdiffs(logAirP)   ####Decreta cuantas diferencias estacional son requeridas

# Se necesitan una difrencia regular (d=1) y una estacional (D=1).

#---------------------------------------------
# Diferencias regulares y/o estacionales
#---------------------------------------------
# Diferencia regular para eliminar la tendencia
dlogAirPass=diff(logAirP)

par(mfrow=c(3,1))
plot(AirPassengers)
plot(logAirP)
plot(dlogAirPass)

par(mfrow=c(2,1))
acf(dlogAirPass,lag.max = 48)
pacf(dlogAirPass)

aTSA::adf.test(dlogAirPass,nlag = 15)

# Ya no se tiene raiz unitaria regular, se elimino la tendencia.
# Todavia queda el patron estacional por remover. 
# It is clear the there is still persistence in the seasons. Posible raiz estacional

# Diferencia estacional para eliminar la persistencia de estacionalidad
d_dlogAirPass = diff(dlogAirPass, 12)

#comparando todas las series para ver si las transformaciones han servido
par(mfrow=c(4,1))
plot(AirPassengers)
plot(logAirP)
plot(dlogAirPass)
plot(d_dlogAirPass)

# La serie d_dlogAirPass con diferencia regular y estacional parece ser estacionaria.
# Verifiquemos calculando el ACF y PACF muestrales:

par(mfrow=c(2,1))
acf(d_dlogAirPass,lag.max = 50)
pacf(d_dlogAirPass,lag.max = 50)

aTSA::adf.test(dlogAirPass,nlag = 15)

# Ya se elimino tanto la tendencia como la persistencia de la estacionalidad
# Se corrobora con D-F que la serie no tiene raiz unitaria regular.

#--------------
# Conclusiones:

#Componente estacional: Parece que en las estaciones, el ACF está reduciendo 
#                        su valor en un rezago de 1s (s=12), mientras que 
#                       la PACF está reduciendo su valor en rezagos de 1s, 2s, 3s, 
#.                      4s, ... . Estos resultados implican una SMA(1), P=0, Q=1, 
#                       en la estación s=12.

#Componente no estacional: si se examinan los valores de ACF y PACF de la muestra
#                          en los rezagos inferiores, parece que ambos están disminuyendo.
#                          Esto sugiere un ARMA(1,1) dentro de las estaciones, p=q=1.
#                          Aunque NO es del todo claro el comportamiento


#---------------------------------------------
# Ajuste de posibles modelos
#---------------------------------------------


# Por lo tanto, primero probamos un ARIMA(1,1,1)x(0,1,1)[12] en los datos registrados:
# Aca en el comando Arima ya incluyo la transformacion de Box-Cox con el lambda=0
# Esto se hace para que al pronosticar me de resultdos en niveles de la serie orginal

Mod1=forecast::Arima(AirPassengers,order = c(1,1,1),seasonal = c(0, 1, 1),lambda = 0, method = c("ML"))


#Sin embargo, el parámetro AR no es significativo, por lo que deberíamos 
#intentar eliminar un parámetro de la parte dentro de las estaciones. 
#En este caso, probamos tanto un modelo ARIMA(0,1,1)x(0,1,1)_{12} como un 
#modelo ARIMA(1,1,0)x(0,1,1)_{12}:

Mod2=forecast::Arima(AirPassengers,order = c(0,1,1),seasonal = c(0, 1, 1),lambda = 0, method = c("ML"))
Mod3=forecast::Arima(AirPassengers,order = c(1,1,0),seasonal = c(0, 1, 1),lambda = 0, method = c("ML"))

Mod2
Mod3

# Conclusion:
# Todos los criterios de información prefieren el modelo ARIMA(0,1,1)x(0,1,1)_{12}, 
# es decir el modelo "Mod2. 

#---------------------------------------------
# Diagnostico Mod2: ARIMA(0,1,1)x(0,1,1)_{12}
#---------------------------------------------
#Los diagnósticos residuales se muestran a continuacion y,
# a excepción de uno o dos valores atípicos, el modelo parece ajustarse bien.

at_est <- residuals(Mod2)

tsdiag(Mod2) 

### pruebas de hip?tesis de normalidad

library(car)
library(nortest)
# Si p.valor>0.05 entonces NO se rechaza Ho
ad.test(at_est) #test de normalidad (nortest)
shapiro.test(at_est) #test de normalidad (stats)
hist(at_est, freq=F)
lines(density(at_est), col="red", lwd=2)
# Conclusion: Los residuales del modelo 2 son normales

qqnorm(at_est); qqline(at_est,col=2) #Buen comportamiento de los residuales mod2


#----------------------------------------------------------------------------
########              CUSUM Y CUSUMQ            #############################
#----------------------------------------------------------------------------
cum=cumsum(at_est)/sd(at_est)
N=length(at_est)
cumq=cumsum(at_est^2)/sum(at_est^2)
Af=0.948 ###Cuantil del 95% para la estad?stica cusum
co=0.14013####Valor del cuantil aproximado para cusumsq para n/2
####Para el caso de la serie de pasajeros es aprox (144-12)/2=66
LS=Af*sqrt(N)+2*Af*c(1:length(at_est))/sqrt(N)
LI=-LS
LQS=co+(1:length(at_est))/N
LQI=-co+(1:length(at_est))/N
plot(cum,type="l",ylim=c(min(LI),max(LS)),xlab="t",ylab="",main="CUSUM")
lines(LS,type="S",col="red")
lines(LI,type="S",col="red")
#CUSUMSQ
plot(cumq,type="l",xlab="t",ylab="",main="CUSUMSQ")                      
lines(LQS,type="S",col="red")                                                                           
lines(LQI,type="S",col="red")


#Conclusiones:
#CUSUM: No se evidencia cambio estructural, 
#       la suma acumulada esta dentro de los límites
# CUSUMQ: Posiblemente un problema de heterocedasticidad no tan fuerte.
#         Sugerencia: Realizar prueba de efectos ARCH

#---------------------------------------------------------------



#---------------------------------------------------------------
# Aplicaciones
#             Pronosticos con la libreria ggplot2
#---------------------------------------------------------------

par(mfrow=c(1,1))
pronosticos12=forecast::forecast(Mod2,h=12,level=0.95)
plot(pronosticos12)

#Pronosticos con la libreria ggplot2
AirPassengers%>%
 forecast::Arima(order = c(0,1,1),seasonal = c(0, 1, 1),lambda = 0, method = c("ML")) %>%
 forecast::forecast(h=12) %>%
 autoplot




# ##########################################################################################
# ###########               FIN PROGRAMA                   ################################
# ##########################################################################################






