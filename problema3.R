# Datos
concentracion <- c(0.00, 0.02, 0.04, 0.06, 0.08, 0.10)
absorbancia <- c(0.00, 0.18, 0.38, 0.59, 0.81, 1.01)

# Gráfico
plot(concentracion, absorbancia, 
     xlab = "Concentración (mol/L)", 
     ylab = "Absorbancia",
     main = "calibración")

# Ajustar el modelo lineal
modelo <- lm(Absorbancia ~ Concentracion, data = datos_espectro)

# Mostrar resumen del modelo
summary(modelo)

# Añadir la línea de regresión al gráfico
abline(modelo, col = "red", lwd = 2)

# Mostrar ecuación en el gráfico
ecuacion <- paste("Abs =", round(coef(modelo)[1], 3), "+", round(coef(modelo)[2], 3), "* C")
text(0.02, 0.9, ecuacion, pos = 4, col = "red")

nueva_abs <- 0.75
concentracion_estimada <- (nueva_abs - coef(modelo)[1]) / coef(modelo)[2]

cat("La concentración estimada para una absorbancia de 0.75 es:", 
    round(concentracion_estimada, 4), "mol/L\n")

# Coeficiente de determinación R²
r_cuadrado <- summary(modelo)$r.squared
cat("El R² del modelo es:", round(r_cuadrado, 4), "\n")

if(r_cuadrado > 0.98) {
  cat("El modelo se ajusta excelentemente a los datos.")
} else if(r_cuadrado > 0.90) {
  cat("El modelo tiene un buen ajuste.")
} else {
  cat("El modelo no se ajusta adecuadamente a los datos.")
}

# Gráfico de residuales para validación
plot(modelo$fitted.values, modelo$residuals,
     pch = 19, col = "blue",
     main = "Análisis de Residuales",
     xlab = "Valores Ajustados",
     ylab = "Residuales")
abline(h = 0, col = "red", lty = 2)
