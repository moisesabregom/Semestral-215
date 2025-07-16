# Parámetros dados
X0 <- 0.100  # Concentración inicial (mol/L)
k <- 0.25    # Constante de velocidad (min⁻¹)

# Generar vector de tiempos de 0 a 20 minutos
tiempo <- seq(0, 20, by = 1)

# Calcular concentración teórica usando la ecuación de primer orden
X_teorico <- X0 * exp(-k * tiempo)

# Crear dataframe con los datos teóricos
datos_cinetica <- data.frame(Tiempo = tiempo, Concentracion = X_teorico)

set.seed(123)  # Para reproducibilidad

# Agregar ruido aleatorio con distribución normal (media=0, sd=0.005)
datos_cinetica$Concentracion_medida <- datos_cinetica$Concentracion + 
  rnorm(n = length(tiempo), mean = 0, sd = 0.005)

# Asegurar que no haya valores negativos
datos_cinetica$Concentracion_medida <- pmax(datos_cinetica$Concentracion_medida, 0)
plot(datos_cinetica$Tiempo, datos_cinetica$Concentracion_medida,
     pch = 19, col = "blue",
     main = "Descomposición de Primer Orden del Compuesto X",
     xlab = "Tiempo (min)",
     ylab = "Concentración [X] (mol/L)",
     ylim = c(0, X0 * 1.1))

# Añadir curva teórica
lines(datos_cinetica$Tiempo, datos_cinetica$Concentracion, 
      col = "red", lwd = 2)

# Añadir leyenda
legend("topright",
       legend = c("Datos simulados", "Curva teórica"),
       col = c("blue", "red"),
       pch = c(19, NA),
       lty = c(NA, 1))
# Transformación logarítmica para regresión lineal
datos_cinetica$ln_X <- log(datos_cinetica$Concentracion_medida)

# Ajustar modelo lineal a los datos transformados
modelo_cinetica <- lm(ln_X ~ Tiempo, data = datos_cinetica)

# Extraer la constante de velocidad estimada
k_estimado <- -coef(modelo_cinetica)[2]

# Mostrar resultados
cat("Constante de velocidad teórica:", k, "min⁻¹\n")
cat("Constante de velocidad estimada:", round(k_estimado, 4), "min⁻¹\n")
cat("Diferencia porcentual:", round(abs(k - k_estimado)/k * 100, 2), "%\n")

# Gráfico de la transformación lineal
plot(datos_cinetica$Tiempo, datos_cinetica$ln_X,
     pch = 19, col = "darkgreen",
     main = "Transformación Lineal para Cinética de Primer Orden",
     xlab = "Tiempo (min)",
     ylab = "ln([X])")

# Añadir línea de regresión
abline(modelo_cinetica, col = "orange", lwd = 2)

# Añadir ecuación
text(5, -1, 
     paste("ln[X] =", round(coef(modelo_cinetica)[1], 3), 
           round(-coef(modelo_cinetica)[2], 3), "t"),
     pos = 4, col = "orange")
# Resumen del modelo
summary(modelo_cinetica)

# Análisis de residuales
plot(modelo_cinetica$fitted.values, modelo_cinetica$residuals,
     pch = 19, col = "blue",
     main = "Residuales del Modelo Cinético",
     xlab = "Valores Ajustados",
     ylab = "Residuales")
abline(h = 0, col = "red", lty = 2)

