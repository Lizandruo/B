#columna Price
data <- read.csv(file.choose())
precio <- data$price
anio <- data$year
kilometraje <- data$mileage
lote <- data$lot

rango1 <- function(vector){
rangoValores <-vector[1:10]
}

rango2 <- function(vector){
  rangoValores2 <-vector[10:20]
}

rango3 <- function(vector){
  rangoValores3 <-vector[30:40]
}



#tabla de frecuencias
tablaFrecuencia <- function(vector){
  listax <- hist(vector,plot=FALSE)
  tf <- table.freq(listax) 
}

#histograma
histograma <- function (vector){
  hist(vector, 
       main = "Histograma",xlab = "Valores",ylab = "Frecuencia",  col = "lightblue",border = "black")
}

#poligono de frecuencia
poligonoFrecuencia <- function(vector){
  polygon(vector, col = "blue", border = "purple")
}

#Gráfico Circular
graficoPastel <- function(vector){
  pie(vector)
}

#Media, mediana, moda
#Media
media <- function(vector){
  return(mean(vector))
}

#Mediana
mediana <- function(vector){
  x <- sort(vector)
  return(median(x))
}
#Moda
moda <- function(vector){
  return(as.numeric(names(which.max(table(vector)))))
}

#Varianza, Desviacion estandar, Coeficiente de variacion
#varianza
varianza <- function(vector){
  return(var(vector, na.rm = FALSE))
}

#desviacion estandar
desviacionEstandar <- function(vector){
  return(sd(vector)) 
}

#coeficiente de variacion
coeficienteVariacion <- function(vector){
  return(cv(vector)) 
} 

#Cuartiles, Deciles, Percentiles
#cuartiles
cuartiles <- function(vector){
  q <- quantile(vector, probs = c(0.25, 0.5, 0.75))
  return(q)
}

#deciles
deciles <- function(vector){
  d <- quantile(vector, probs = seq(0.1, 0.9, by=0.1))
  return(d)
}


#percentiles
percentiles <- function(vector){
  p <- quantile(vector, probs = seq(0.01, 0.99, by=0.01))
  return(p)
}



#Diagrama de caja
diagramaCaja <- function (vector){
  boxplot(vector, col ="lightpink", border = "black", horizontal = TRUE, main = "Diagrama de Caja",
          xlab="Valores")
}

#Diagrama de pareto

diagramaPareto <- function(vector){
paste("El tamaño de la muestra es: ",length(vector))
names(vector) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
pareto.chart(vector[1:10], ylab = "Frecuencias", col = heat.colors(length(vector)), cumperc = seq(0, 100, by = 20),
             ylab2 = "Porcentaje acumulado", main = "Diagrama de Pareto")
}

#Resultados

# Análisis de resultados:

# Tabla de frecuencia:
print("Análisis de la tabla de frecuencia de precios:")
print("l histograma nos permite visualizar la distribución de los precios de manera gráfica. Podemos identificar la forma de la distribución y la frecuencia de los diferentes intervalos de precios. Además, nos ayuda a identificar posibles sesgos o patrones en los datos")
print(tablaFrecuencia(precio))

# Histograma:
print("Análisis del histograma de precios:")
print("Podemos concluir que en el rango aproximado de 10000 a 20000 fueron los más resaltantes")
histograma(precio)

# Polígono de frecuencia:
print("Análisis del polígono de frecuencia de precios:")
print("El polígono de frecuencia es una representación gráfica de la tabla de frecuencia. Conecta los puntos medios de cada intervalo de clase y nos proporciona una representación suave de la distribución de los precios. Nos ayuda a visualizar la tendencia central y la dispersión de los datos.")
poligonoFrecuencia(precio)

# Gráfico de pastel:
print("Análisis del gráfico de pastel de precios:")
print("El gráfico de pastel muestra la proporción de cada categoría en relación con el total. Es útil para identificar la contribución relativa de cada intervalo de precios al conjunto de datos. Nos permite identificar rápidamente las categorías dominantes y su influencia en la distribución total de los precios.")
graficoPastel(rango3(precio))

# Medidas de tendencia central:
print("Análisis de las medidas de tendencia central:")
print("La media, mediana y moda son medidas de tendencia central que nos proporcionan información sobre el valor típico o central de los datos de precios. La media es la suma de todos los precios dividida por el número de observaciones, la mediana es el valor central cuando los datos están ordenados, y la moda es el valor que aparece con mayor frecuencia.")
print(paste("La media de los precios es: ", media(precio)))
print(paste("La mediana de los precios es: ", mediana(precio)))
print(paste("La moda de los precios es: ", moda(precio)))

# Medidas de dispersión:
print("Análisis de las medidas de dispersión:")
print("La varianza, la desviación estándar y el coeficiente de variación son medidas de dispersión que nos proporcionan información sobre la variabilidad o dispersión de los datos de precios. La varianza es la media de las diferencias al cuadrado entre cada precio y la media, la desviación estándar es la raíz cuadrada de la varianza, y el coeficiente de variación es la desviación estándar dividida por la media, expresada como un porcentaje.")
print(paste("La varianza de los precios es: ", varianza(precio)))
print(paste("La desviación estándar de los precios es: ", desviacionEstandar(precio)))
print(paste("El coeficiente de variación de los precios es: ", coeficienteVariacion(precio)))

# Diagrama de caja de precios:
print("Análisis del diagrama de caja de precios:")
print("El diagrama de caja nos proporciona una representación visual de la distribución de los precios, incluyendo la mediana, los cuartiles y los valores atípicos. Nos ayuda a identificar la dispersión y simetría de los datos, así como posibles valores extremos que podrían influir en nuestro análisis.")
diagramaCaja(rango1(precio))
diagramaCaja(rango2(precio))
diagramaCaja(rango3(precio))

histograma
poligonoFrecuencia(rango1(precio))
graficoPastel(rango3(precio))
media(precio)
mediana(precio)
moda(precio)
paste("La varianza es: ", varianza(precio))
paste("La Desviacion Estandar es: ", desviacionEstandar(precio))
paste("El Coeficiente de Variacion es: ", coeficienteVariacion(precio))
diagramaCaja(rango1(precio))
diagramaCaja(rango2(precio))
diagramaCaja(rango3(precio))
cuartiles(precio)
deciles(precio)
table(percentiles(precio))
diagramaPareto(precio)
diagramaCaja(precio)
