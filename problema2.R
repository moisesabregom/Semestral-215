# Cargar el archivo desde GitHub
url <- "https://raw.githubusercontent.com/moisesabregom/Semestral-215/main/datco2.csv"

# Intentar leer el archivo con diferentes opciones
  if(nrow(datos_co2) > 0) {
    
    plot(datos_co2$T, datos_co2$Cp,
         type = "o",
         pch = 19,
         col = "dodgerblue3",
         main = "Capacidad Calorífica del CO₂",
         xlab = "Temperatura (K)",
         ylab = expression(C[p]~"(J/mol·K)"),
         cex.main = 1.2,
         font.lab = 2)
    
    #línea de tendencia
    tryCatch({
      lines(lowess(datos_co2$T, datos_co2$Cp, f = 0.2), 
            col = "red3", lwd = 2)
    }, error = function(e) {
      message("No se pudo calcular la línea de tendencia: ", e$message)
    })
    
    grid()

    