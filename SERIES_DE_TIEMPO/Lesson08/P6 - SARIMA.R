#######################################################################
##  Ejercicio de Simulación SARIMA                                 ####
#######################################################################
## 

## Autor: Sergio Calderon
## Modificado por: DARIO HERNANDEZ (PhD in Statistics)

#Vamos a hacer un ejercicio de simulación para ver como se identifica la
#componente estacional.


library(urca)
library(forecast)
library(tseries)
library(lmtest)
library(uroot)
library(fUnitRoots)
library(sarima)
library(aTSA)
require("PolynomF")

#----------------------------------------------------------------------
###Simulación diapositiva SAR(1)=ARIMA(0,0,0)(1,0,0)[12] 
  
  set.seed(666)
  phi = c(rep (0,11),.9)
  sAR = arima.sim(list(order=c(12,0,0), ar=phi), n=37)
  sAR = ts(sAR, freq=12)
  layout(matrix(c(1,1,2, 1,1,3), nc=2))
  
  par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
  plot(sAR, axes=FALSE, main='seasonal AR(1)', xlab="year", type='c')
  Months = c("J","F","M","A","M","J","J", "A", "S", "0", "N", "D")
  points(sAR, pch=Months, cex=1.25, font=4, col=1:4)
  axis(1, 1:4); abline(v=1:4, lty=2, col=gray(.7))
  axis(2); box()
  ACF = ARMAacf(ar=phi, ma=0, 100)             #Compute Theoretical ACF for an ARMA Process
  PACF = ARMAacf(ar=phi, ma=0, 100, pacf=TRUE) #Compute Theoretical PACF for an ARMA Process
  plot(ACF,type="h", xlab="LAG", ylim=c(-.1,1)); abline(h=0)
  plot(PACF, type="h", xlab="LAG", ylim=c(-.1,1)); abline(h=0)


#----------------------------------------------------------------------
###Simulación diapositiva SMA(1)=ARIMA(0,0,0)(0,0,1)[12] 

  set.seed(123)
  theta = c(rep (0,11),.7)
  sMA = arima.sim(list(order=c(0,0,12), ma=theta), n=37)
  sMA = ts(sMA, freq=12)
  layout(matrix(c(1,1,2, 1,1,3), nc=2))

  par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
  plot(sMA, axes=FALSE, main='seasonal MA(1)', xlab="year", type='l')
  axis(1, 1:4); abline(v=1:4, lty=2, col=gray(.7))
  axis(2); box()
  ACF = ARMAacf(ma=theta, ar=0, 100)             #Compute Theoretical ACF for an ARMA Process
  PACF = ARMAacf(ma=theta, ar=0, 100, pacf=TRUE) #Compute Theoretical PACF for an ARMA Process
  plot(ACF,type="h", xlab="LAG", ylim=c(-.1,1)); abline(h=0)
  plot(PACF, type="h", xlab="LAG", ylim=c(-.1,1)); abline(h=0)


#----------------------------------------------------------------------
###Simulación de un proceso con raíz unitaria estacional
  set.seed(123)
  x <- ts(sarima::sim_sarima(n=144, model = list(iorder=0, siorder=1, nseasons=12, sigma2 = 1),n.start=24),frequency = 12)
  par(mfrow=c(2,1))
  plot(x)
  acf(x,lag.max = 36)
  monthplot(x)
  nsdiffs(x)####Decreta cuantas diferencias estacional son requeridas

###diferencia estacional
  Dx=diff(x,lag=12,differences = 1)###lag:periodo s.
  par(mfrow=c(2,1))
  plot(Dx)
  acf(Dx,lag.max = 36)
  monthplot(Dx)
  nsdiffs(Dx)

#----------------------------------------------------------------------
####Simulación de un SAR(1), 12  seasons
  set.seed(123)
  x1 <- ts(sim_sarima(n=144, model = list(ar=c(rep(0,11),0.8)),n.start=24),frequency=12)
  layout(matrix(c(1,1,2, 1,1,3), nc=2))
  
  par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
  plot(x1)
  acf(x1,lag.max = 36)
  pacf(x1,lag.max = 36)
  
  par(mfrow=c(1,1))
  monthplot(x1)
  nsdiffs(x1) ####Decreta cuantas diferencias estacional son requeridas
  ndiffs(x1) ####Decreta cuantas diferencias regulares son requeridas
  p <- polynom(c(1,c(rep(0,11),-0.8)))
  solve(p)
  abs(solve(p))
###Note lo cerca que están la raíces de la no estacionariedad del proceso, por eso
####aunque si bien el proceso es estacionario, notamos hay una cercanía a 
####e tener una componente estacional.
  
  
####El anterior modelo puede escribirse como: SAR(1)
  set.seed(123)
  x2 <- ts(sim_sarima(n=144, model=list(sar=0.8, iorder=0, siorder=0, nseasons=12),n.start=24),frequency = 12)
  layout(matrix(c(1,1,2, 1,1,3), nc=2))
  
  par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
  plot(x2)
  acf(x2, lag.max=48)
  pacf(x2, lag.max=48)
  
  par(mfrow=c(1,1))
  nsdiffs(x2)
  
  
#----------------------------------------------------------------------
####Simulación de un SARIMA(0,0,1)(1,1,0)[12]
  set.seed(2025)
  x3=ts(sim_sarima(n=144, model=list(sar=0.8, ma=-0.5, iorder=0, siorder=1, nseasons=12),n.start=24),frequency = 12)
  
  layout(matrix(c(1,1,2, 1,1,3), nc=2))
  
  par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
  plot(x3)
  acf(x3, lag.max=48)
  pacf(x3, lag.max=48)
  
  par(mfrow=c(1,1))
  monthplot(x3)
  nsdiffs(x3) ####Decreta cuantas diferencias estacional son requeridas

###diferencia estacional
  Dx3=diff(x3,lag=12,differences = 1)###lag:periodo s.
  
  layout(matrix(c(1,1,2, 1,1,3), nc=2))
  
  par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
  plot(Dx3)
  acf(Dx3,lag.max = 36)
  pacf(Dx3,lag.max = 36)
  monthplot(Dx3)
  nsdiffs(Dx3)
  
  #Comparando los modelos estimados a la serie original y a la serie con prime dif estacional
  auto.arima(Dx3)
  auto.arima(x3)

  
#----------------------------------------------------------------------
####Simulación de un SARIMA(0,0,1)(0,1,1)[12]
  
  set.seed(2025)  
  x4=ts(sim_sarima(n=144, model=list(ma=0.6, sma=0.5, iorder=0, siorder=1, nseasons=12),n.start=36),frequency = 12)
  layout(matrix(c(1,1,2, 1,1,3), nc=2))
  
  par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
  plot(x4)
  acf(x4, lag.max=48)
  pacf(x4, lag.max=48)
  
  nsdiffs(x4)
  
  Dx4=diff(x4,lag=12,differences = 1)###lag:periodo s.
  layout(matrix(c(1,1,2, 1,1,3), nc=2))
  
  par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
  
  plot(Dx4)
  acf(Dx4,lag.max = 36)
  pacf(Dx4, lag.max=48)
  
  nsdiffs(Dx4)
  
  #Comparando los modelos estimados a la serie original y a la serie con prime dif estacional
  auto.arima(Dx4)
  auto.arima(x4)
