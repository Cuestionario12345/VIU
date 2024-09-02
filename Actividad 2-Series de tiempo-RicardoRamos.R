library(dplyr)
library(ggplot2)
library(tidyr)
library(tseries)
library(stats)
library(forecast)
library(corrplot)

#1. Importar el dataset y ver cantidad de datos
dataset2 <- read.csv("https://raw.githubusercontent.com/ricardoramos12/VIU/main/raw_sales.csv", header = TRUE)

cat("La cantidad de columnas es", ncol(dataset2), "y sus nombres son:", paste(names(dataset2), collapse = ", "), "\n");
cat("Número de filas:", nrow(dataset2), "\n");

#Analizamos los datos

# Fecha mínima y fecha máxima
min_date <- min(dataset2$datesold, na.rm = TRUE)
max_date <- max(dataset2$datesold, na.rm = TRUE)

#se transforma la variable datesold de "character" a "Date"
dataset2$datesold <- as.Date(dataset2$datesold)

#2. Se divide el dataset en datos de entrenamiento y prueba, así evitar tocar datos no vistos (datos prueba)
set.seed(123);
dato_entrenamiento <- dataset2[sample(nrow(dataset2), 0.8 * nrow(dataset2)), ]
dato_prueba <- dataset2[-seq_len(nrow(dato_entrenamiento)), ]

#ordenar por fecha
dato_entrenamiento <- dato_entrenamiento[order(dato_entrenamiento$datesold), ]
dato_prueba <- dato_prueba[order(dato_prueba$datesold), ]
  
#3 Verifica si hay datos nulos en el dataset original (solo verificar)
cantidad_nulos <- sum(is.na(dataset2))
cat("La cantidad de datos nulos es:", cantidad_nulos, "\n")


#4. Verifica duplicados en el dataset original (solo verificar)
duplicados <- dataset2[duplicated(dataset2), ]
cat("La cantidad de duplicados es:", nrow(duplicados), "\n")

###################################Se comienza el análisis de train##################

#5. Calcula estadísticas descriptivas
descriptive_stats <- summary(dato_entrenamiento)
print(descriptive_stats)

#6. boxplot para identificar outliers
generar_boxplot <- function(data, variable) {
  data %>%
    select(any_of(variable)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
    ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
    geom_boxplot() +
    labs(title = paste("Boxplot de", variable),
         x = "Variable", y = "Valor") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.8),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

generar_boxplot(dato_entrenamiento, "price")
generar_boxplot(dato_entrenamiento, "bedrooms")


outliers_count <- sum(dato_entrenamiento$price > 1000000)
# Se trata los outliers de price en un rango determinado según criterio
dato_entrenamiento <- dato_entrenamiento[dato_entrenamiento$price <= 1000000, ]
dato_prueba <- dato_prueba[dato_prueba$price <= 1000000, ]


#7. Se encodea la variable  propertyType

dato_entrenamiento$propertyType <- as.factor(dato_entrenamiento$propertyType)
dato_entrenamiento$propertyType <- ifelse(dato_entrenamiento$propertyType == 'house', 1, 0)

dato_prueba$propertyType <- as.factor(dato_prueba$propertyType)
dato_prueba$propertyType <- ifelse(dato_prueba$propertyType == 'house', 1, 0)

#8. Verificar la normalidad mediante prueba de Kolmogorov-Smirnov complementado con Regla de Scott para definir bindwidth
binwidth_scott <- function(x) {
  3.5 * sd(x) / length(x)^(1/3)
}

#Definir la función para crear histogramas
crear_histograma <- function(data, variable) {
  ggplot(data, aes(x = !!sym(variable))) +
    geom_histogram(binwidth = binwidth_scott(data[[variable]]),
                   fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histograma de", variable),
         x = "Valor", y = "Frecuencia") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

crear_histograma(dato_entrenamiento, "postcode")
crear_histograma(dato_entrenamiento, "price")
crear_histograma(dato_entrenamiento, "propertyType")
crear_histograma(dato_entrenamiento, "bedrooms")


#Definir la función para la prueba de normalidad

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

ks_test <- function(data, variable) {
  standardized_data <- standardize(data[[variable]])
  ks.test(standardized_data, "pnorm")
}

ks_test(dato_entrenamiento, "postcode")
ks_test(dato_entrenamiento, "price")
ks_test(dato_entrenamiento, "propertyType")
ks_test(dato_entrenamiento, "bedrooms")


#9. Se analiza la matriz de correlación

numeric_data <- dato_entrenamiento[sapply(dato_entrenamiento, is.numeric)]
matriz_correlacion <- cor(numeric_data)

# Configurar los márgenes de la gráfica
par(mar = c(1, 1, 1, 1))

corrplot(matriz_correlacion, 
         method = "color", 
         type = "full", 
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black")


#7. Descomposicion de la serie temporal

ts_data <- ts(dato_entrenamiento$price, start = min(dato_entrenamiento$datesold), end = max(dato_entrenamiento$datesold), frequency = 365)
decomposition <- stl(ts_data, s.window = "periodic")

trend_component <- decomposition$time.series[, "trend"] 
seasonal_component <- decomposition$time.series[, "seasonal"]
residual_component <- decomposition$time.series[, "remainder"]
plot(decomposition)
#por recursos en algún momento mostró solo data y seasonal (luego todo con normalidad), por si pasa al ejecutar lo menciono profe

print("Componente de Tendencia:")
#print(trend_component)

print("Componente Estacional:")
#print(seasonal_component)

print("Componente de Residuos:")
#print(residual_component)


#9. Análisis de autocorrelación 

#Coeficientes de Autocorrelación (ACF) 
resultado_acf <- acf(ts_data, plot = TRUE)
print(resultado_acf$acf)
acf(ts_data, lag.max = 50, main = "Función de Autocorrelación (ACF)")

# Análisis de autocorrelación parcial (PACF)
resultado_pacf <- pacf(ts_data, lag.max = 50, plot = FALSE)
print(resultado_pacf$acf)
pacf(ts_data, lag.max = 50, main = "Función de Autocorrelación Parcial (PACF)")


# Prueba de Ljung-Box 
lag_max <- 50
residuos_componente_error <- residual_component
resultado_prueba <- Box.test(residuos_componente_error, lag = lag_max, type = "Ljung-Box")
resultado_prueba

#corroboramos con Prueba de Dickey-Fuller aumentada (ADF) para ver si es estacionaria
adf_result <- adf.test(ts_data)
print(adf_result)


#10. Creación del modelo adecuado, este proceso puede demorar unos 10 minutos

modelo_arima <- auto.arima(ts_data)
print(summary(modelo_arima))

# Diagnóstico de residuos del modelo ARIMA
checkresiduals(modelo_arima)

# Realizar previsiones con el modelo ARIMA
forecast_result <- forecast(modelo_arima, h = 30) 
plot(forecast_result, main = "Pronóstico ARIMA")
















