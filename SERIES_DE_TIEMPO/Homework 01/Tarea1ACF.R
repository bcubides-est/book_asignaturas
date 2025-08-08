# Simulación y cálculo de ACF para n = 500 y n = 50
rm(list=ls(all=TRUE))
set.seed(1)

# Serie con n = 500
v1 <- filter(rnorm(500 + 2), filter = rep(1/3, 3), sides = 2)
v1 <- v1[2:(500 + 1)]  # sin las observaciones 1 y 502

# Serie con n = 50
v2 <- filter(rnorm(50 + 2), filter = rep(1/3, 3), sides = 2)
v2 <- v2[2:(50 + 1)]

# Graficar ambas series
par(mfrow = c(1,2))
plot.ts(v1, main = "Serie MA(3) simulada con n = 500")
plot.ts(v2, main = "Serie MA(3) simulada con n = 50")

# Comparación de ACF muestral
par(mfrow = c(1,2))
acf(v1, lag.max = 20, main = "ACF muestral - MA(3), n = 500")
acf(v2, lag.max = 20, main = "ACF muestral - MA(3), n = 50")

# ACF teórica
rho_teo <- c(1, 2/3, 1/3, rep(0, 18))

# Tabla Comparativa:
acf_v1 <- acf(v1, lag.max = 20, plot = FALSE)$acf
acf_v2 <- acf(v2, lag.max = 20, plot = FALSE)$acf

# Construir la tabla comparativa
Autocorrelaciones <- data.frame( Rezago = 0:20,
  ACF500_Estimada = round(acf_v1, 4),
  ACF50_Estimada = round(acf_v2, 4),
  ACF_Teorica = round(rho_teo, 4)
)
head(Autocorrelaciones, 8)