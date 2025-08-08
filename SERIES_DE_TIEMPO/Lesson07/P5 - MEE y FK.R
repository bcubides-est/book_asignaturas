#######################################################################
##  MEE y FK para el modelo AR(1)                                   ####
#######################################################################

## DARIO HERNANDEZ 
## PhD in Statistics

# Descripción del Código
#
# 1. Instalación y carga del paquete dlm.
# 2. Definición del modelo AR(1) utilizando la función dlmModARMA.
# 3. Simulación de una serie temporal a partir del modelo AR(1) utilizando arima.sim.
# 4. Definición de la función de construcción del modelo para el ajuste (máxima verosimilitud).
# 5. Estimación de los parámetros por máxima verosimilitud utilizando dlmMLE.
# 6. Extracción de las estimaciones del modelo ajustado.
# 7. Estimación de los estados del modelo utilizando el filtro de Kalman (dlmFilter y dlmSmooth).
# 8. Visualización de los datos simulados y los estados estimados.

#######################################################################


# Instalar el paquete si no lo tienes ya
install.packages("dlm")

#By using the dlm package in R, you can easily model an AR(1) process in a 
#state space framework. The key steps are to define the transition and 
#observation matrices, set up the noise variances, and then fit the model 
#to your data using the Kalman filter. This approach is flexible and allows you 
#to easily extend the model to more complex state space formulations, 
#such as higher-order AR models or even models with exogenous variables.

# Cargar el paquete
library(dlm)

# Definir el modelo AR(1) en términos de matrices de espacio de estado
phi_inicial <- 0.5
sigma2_inicial <- 1

modAR1 <- dlmModARMA(ar = phi_inicial, ma = NULL, sigma2 = sigma2_inicial)

# Simular una serie temporal a partir de un modelo AR(1)
set.seed(123)
sim_data <- arima.sim(model = list(ar = phi_inicial), n = 100, sd = sqrt(sigma2_inicial))


acf(sim_data)
pacf(sim_data)


# Definir la función de construcción del modelo para el ajuste (Maximum Likelihood Estimation)
build_mod <- function(par) {
 dlmModARMA(ar = par[1], ma = NULL, sigma2 = par[2])
}

# Realizar la estimación por máxima verosimilitud
fit <- dlmMLE(sim_data, parm = c(phi_inicial, sigma2_inicial), build = build_mod)

# Extraer las estimaciones del modelo ajustado
mod_est <- build_mod(fit$par)

# Ver la estructura del modelo ajustado
print(mod_est)

# Estimar los estados del modelo utilizando el filtro de Kalman
filt <- dlmFilter(sim_data, mod_est)
# Obtener los estados filtrados
states <- dlmSmooth(filt)

# Visualizar los datos simulados y los estados estimados a_tt
plot(sim_data, type = "l", col = "blue", main = "Serie Temporal Simulada y Estados Estimados")
lines(filt$m, col = "red", lty = 2)
legend("topright", legend = c("Datos Simulados", "Estados filtrados Estimados"), col = c("blue", "red"), lty = 1:2)

# Visualizar los datos simulados y los estados estimados suavizados atT
plot(sim_data, type = "l", col = "blue", main = "Serie Temporal Simulada y Estados Estimados")
lines(states$s, col = "red", lty = 2)
legend("topright", legend = c("Datos Simulados", "Estados suavizados Estimados"), col = c("blue", "red"), lty = 1:2)

