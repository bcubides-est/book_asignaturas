#------------------------------------
# Causalidad modelos ARMA
#------------------------------------


## DARIO HERNANDEZ 
## PhD in Statistics
  


#--------------------------------------
# Ejemplo 1: AR(2) proceso Causal y estacionario
#--------------------------------------

# Coeficientes del polinomio
coef <- c(1,-0.2,-0.48)  # Nótese que los coeficientes están en orden ascendente de grado

coef <- c(1,1.8,0.81)
# Calcular las raíces
raices <- polyroot(coef)

# Imprimir las raíces
print(raices)


# Calcular el módulo de las raíces
modulos_raices <- Mod(raices)

# Verificar si ambas raíces tienen módulo mayor a 1
if (all(modulos_raices > 1)) {
 print("El proceso AR(2) es causal.")
} else {
 print("El proceso AR(2) no es causal.")
}


# Si ambas raíces tienen un módulo mayor que 1, significa que los valores 
# futuros de la serie dependen solo de los valores pasados y presentes, 
# y no de valores futuros, lo que cumple con la condición de causalidad.

par(mfrow=c(1,1))
plot(0, 0, type = "n", xlim = c(-2, 2), ylim = c(-2, 2),
     xlab = "Parte real", ylab = "Parte imaginaria",
     main = "Diagrama de Argand para las raíces")

# Un diagrama de Argand (o plano complejo) es una representación gráfica 
# de los números complejos. Cada número complejo se representa como un punto 
# en el plano, donde la parte real corresponde a la coordenada en el eje 
# horizontal y la parte imaginaria a la coordenada en el eje vertical.

# Agregar las raíces al gráfico
points(Re(raices), Im(raices), col = "blue", pch = 19)

# Agregar el círculo unitario (opcional)
theta <- seq(0, 2*pi, length.out = 100)
lines(cos(theta), sin(theta), col = "gray")


#--------------------------------------
# Ejemplo 2:  AR(2) proceso estacionario, pero NO causal
#--------------------------------------

# Coeficientes del polinomio
coef <- c(1,1.5,-0.75)  # Nótese que los coeficientes están en orden ascendente de grado

# Calcular las raíces
raices <- polyroot(coef)

# Imprimir las raíces
print(raices)


# Calcular el módulo de las raíces
modulos_raices <- Mod(raices)

# Verificar si ambas raíces tienen módulo mayor a 1
if (all(modulos_raices > 1)) {
 print("El proceso AR(2) es causal.")
} else {
 print("El proceso AR(2) no es causal.")
}

#

#--------------------------------------
# Ejemplo 3: AR(1) causal y estacionario
#--------------------------------------
# Coeficientes del polinomio
coef <- c(1,-0.8)  # Nótese que los coeficientes están en orden ascendente de grado

# Calcular las raíces
raices <- polyroot(coef)

# Imprimir las raíces
print(raices)


# Calcular el módulo de las raíces
modulos_raices <- Mod(raices)

# Verificar si ambas raíces tienen módulo mayor a 1
if (all(modulos_raices > 1)) {
 print("El proceso AR(2) es causal.")
} else {
 print("El proceso AR(2) no es causal.")
}





