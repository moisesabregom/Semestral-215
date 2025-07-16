# Problema 1 - Equilibrio químico
#Nombre: Moises Abrego
# Función para concentraciones en función de x
concentraciones <- function(x) {
  # Concentraciones iniciales (en mol/L, volumen = 1L)
  H2_ini <- 0.5
  I2_ini <- 0.5
  HI_ini <- 0
  
  # Concentraciones en función de x
  H2 <- H2_ini - x
  I2 <- I2_ini - x
  HI <- 2*x
  
  return(data.frame(x = x, H2 = H2, I2 = I2, HI = HI))
}

# Cálculo de Q y determinación del equilibrio
Kc <- 50.2
x_values <- seq(0, 0.5, by = 0.001)  # Valores de x desde 0 hasta 0.5

# Calcular concentraciones y Q para cada x
resultados <- concentraciones(x_values)
resultados$Q <- (resultados$HI^2)/(resultados$H2 * resultados$I2)
resultados$dif_cuad <- (resultados$Q - Kc)^2

# Encontrar x en el equilibrio (donde Q = Kc)
x_eq <- resultados$x[which.min(resultados$dif_cuad)]
concent_eq <- concentraciones(x_eq)

# Mostrar resultados
print(paste("El valor de x en el equilibrio es:", round(x_eq, 4)))
print("Concentraciones en el equilibrio:")
print(concent_eq)
