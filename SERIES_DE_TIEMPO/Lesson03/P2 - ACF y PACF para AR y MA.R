#######################################################################
##  ACF MA, AR models                                              ####
#######################################################################

## DARIO HERNANDEZ 
## PhD in Statistics

#Elimina todos los datos en memoria en la sesion de R
rm(list=ls(all=TRUE))
set.seed(123)
#************************
# ACF AR model
#AR(1)
xt <- arima.sim(list(ar = c(0.79)), n=1000 )
ts.plot(xt)
acf(xt) # Decae de forma exponencial

### modelo simulado AR(2)
xt2 <- arima.sim(list(order=c(2,0,0), ar = c(0.8,-0.55)), n=1000 )
ts.plot(xt2)
acf(xt2)

### modelo simulado AR(2) Diapositiva 52 leccion 3.
xt3 <- arima.sim(list(order=c(2,0,0), ar = c(1.5,-0.75)), n=1000 )
ts.plot(xt3)
acf(xt3) # Decae de forma sinusoidal

#************************
# ACF MA model

#MA(1)
yt1 <- arima.sim(list(ma = c(0.8)), n=1000 )
ts.plot(yt1)
acf(yt1)

#MA(1)
yt2 <- arima.sim(list(ma = c(-0.8)), n=1000 )
ts.plot(yt2)
acf(yt2)

#MA(2)
yt3 <- arima.sim(list(ma = c(0.9,0.8)), n=1000 )
ts.plot(yt3)
acf(yt3)

#************************
# PACF AR model

#AR(1)
xt <- arima.sim(list(ar = c(0.79)), n=1000 )
ts.plot(xt)
par(mfrow=c(1,2))
acf(xt)
pacf(xt)


### modelo simulado AR(2)
xt2 <- arima.sim(list(order=c(2,0,0), ar = c(0.8,-0.55)), n=1000 )
ts.plot(xt2)
par(mfrow=c(1,2))
acf(xt2)
pacf(xt2)




### modelo simulado AR(2) Diapositiva 52 leccion 3.
xt3 <- arima.sim(list(order=c(2,0,0), ar = c(1.5,-0.75)), n=1000 )
ts.plot(xt3)
par(mfrow=c(1,2))
acf(xt3)
pacf(xt3)

#************************
# PACF MA model
#MA(1)
yt1 <- arima.sim(list(ma = c(0.88)), n=1000 )
ts.plot(yt1)
par(mfrow=c(1,2))
acf(yt1)
pacf(yt1)

#MA(1)
yt2 <- arima.sim(list(ma = c(-0.8)), n=1000 )
ts.plot(yt2)
par(mfrow=c(1,2))
acf(yt2)
pacf(yt2)

#MA(2)
yt3 <- arima.sim(list(ma = c(0.9,0.8)), n=1000 )
ts.plot(yt3)
par(mfrow=c(1,2))
acf(yt3)
pacf(yt3)




xt2 <- arima.sim(list(order=c(2,0,0), ar = c(0.2,0.55)),sd=1.5, n=1000 )

yt3 <- arima.sim(list(ma = c(0.9,-0.8,-0.8)), sd=3,n=1000 )
par(mfrow=c(1,2))
acf(yt3)
pacf(yt3)
