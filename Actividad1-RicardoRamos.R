#Si no se cuenta con alguna libreria toca instalarla con el siguiente comando como guía
#install.packages("magrittr")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("corrplot")
#install.packages("lmtest")
#install.packages("glmnet")
#install.packages("caret")
#install.packages("yardstick")
install.packages("e1071")


library(magrittr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(lmtest)
library(glmnet)
library(caret)
library(yardstick)
library(e1071)



#1. Cargar el dataset y ver cantidad de datos
dataset <- read.csv("https://raw.githubusercontent.com/ricardoramos12/VIU/main/housing_price_dataset.csv", header = TRUE)
cat("La cantidad de columnas es", ncol(dataset), "y sus nombres son:", paste(names(dataset), collapse = ", "), "\n");
cat("Número de filas:", nrow(dataset), "\n");

#2. Se divide el dataset en datos para entrenamiento y prueba, y evitar tocar datos no vistos (datos prueba)
set.seed(123);
dataset_entrenamiento <- dataset[sample(nrow(dataset), 0.8 * nrow(dataset)), ]
dataset_prueba <- dataset[-seq_len(nrow(dataset_entrenamiento)), ]


#3 Verificarr si hay datos nulos en el dataset original antes de comprobar si toca imputar 
cantidad_nulos <- sum(is.na(dataset))
cat("La cantidad de datos nulos es:", cantidad_nulos, "\n")

#4. Verificar duplicados en el dataset original
duplicados <- dataset[duplicated(dataset), ]
cat("La cantidad de duplicados es:", nrow(duplicados), "\n")


#5. Calcular estadísticas descriptivas
descriptive_stats <- summary(dataset_entrenamiento)
print(descriptive_stats)



#6. Realizar diagrama de caja para identificar outliers
crear_boxplot <- function(dataset, variables) {
  dataset %>%
    select(any_of(variables)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
    ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
    geom_boxplot() +
    labs(title = "Boxplot de Variables Numéricas",
         x = "Variable", y = "Valor") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.8),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

crear_boxplot(dataset_entrenamiento, c("SquareFeet"))
crear_boxplot(dataset_entrenamiento, c("Bedrooms", "Bathrooms"))
crear_boxplot(dataset_entrenamiento, c("Price"))


# Tratar la variable 'Price' según criterio propio, replicamos en datos de prueba para mantener consistencia

cantidad_filas_eliminadas <- nrow(dataset_entrenamiento) - nrow(subset(dataset_entrenamiento, Price >= 60000 & Price <= 400000))
print(paste("Cantidad de filas eliminadas:", cantidad_filas_eliminadas))

dataset_entrenamiento <- subset(dataset_entrenamiento, Price >= 60000 & Price <= 400000)
dataset_prueba <- subset(dataset_prueba, Price >= 60000 & Price <= 400000)


#7. Encodear la variable Neighborhood y creamos variable 'HouseAge'

dataset_entrenamiento <- cbind(dataset_entrenamiento, model.matrix(~ Neighborhood - 1, data = dataset_entrenamiento))
dataset_entrenamiento$Neighborhood <- NULL
dataset_prueba <- cbind(dataset_prueba, model.matrix(~ Neighborhood - 1, data = dataset_prueba))
dataset_prueba$Neighborhood <- NULL


current_year <- as.numeric(format(Sys.Date(), "%Y"))
dataset_entrenamiento$HouseAge <- current_year - dataset_entrenamiento$YearBuilt
dataset_entrenamiento$YearBuilt <- NULL
dataset_prueba$HouseAge <- current_year - dataset_prueba$YearBuilt
dataset_prueba$YearBuilt <- NULL


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


crear_histograma(dataset_entrenamiento, "SquareFeet")
crear_histograma(dataset_entrenamiento, "Bedrooms")
crear_histograma(dataset_entrenamiento, "Bathrooms")
crear_histograma(dataset_entrenamiento, "Price")
crear_histograma(dataset_entrenamiento, "HouseAge")

#Definir la función para la prueba de normalidad

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

ks_test <- function(data, variable) {
  standardized_data <- standardize(data[[variable]])
  ks.test(standardized_data, "pnorm")
}

ks_test(dataset_entrenamiento, "SquareFeet")
ks_test(dataset_entrenamiento, "Bedrooms")
ks_test(dataset_entrenamiento, "Bathrooms")
ks_test(dataset_entrenamiento, "Price")
ks_test(dataset_entrenamiento, "HouseAge")


# Calcular la asimetría y curtosis
asimetria <- skewness(dataset_entrenamiento$Price)
curtosis <- kurtosis(dataset_entrenamiento$Price)
cat("Asimetría:", asimetria, "\n")
cat("Curtosis:", curtosis, "\n")



#9. Diagrama de dispersión

crear_diagrama_dispersión <- function(dataset, x_variable, y_variable) {
  ggplot(dataset, aes_string(x = x_variable, y = y_variable)) +
    geom_point() +
    labs(title = paste("Diagrama de Dispersión:", x_variable, "vs", y_variable),
         x = x_variable, y = y_variable) +
    theme_minimal()
}

crear_diagrama_dispersión(dataset_entrenamiento, "SquareFeet", "Price")
crear_diagrama_dispersión(dataset_entrenamiento, "Bedrooms", "Price")
crear_diagrama_dispersión(dataset_entrenamiento, "Bathrooms", "Price")
crear_diagrama_dispersión(dataset_entrenamiento, "HouseAge", "Price")



#10. Matriz de correlación

matriz_correlacion <- cor(dataset_entrenamiento)

# Configurar los márgenes de la gráfica
par(mar = c(1, 1, 1, 1))

# Visualizar la matriz de correlación
corrplot(matriz_correlacion, 
         method = "color", 
         type = "full", 
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black")



###################################MODELO DE REGRESION LINEAL##################

#10.5. Algoritmo de Regresión lineal


# Ajustar un modelo de regresión lineal simple
modelo_lineal <- lm(Price ~ SquareFeet, data = dataset_entrenamiento)
predicciones <- predict(modelo_lineal, newdata = dataset_prueba)
# Mostrar un resumen del modelo 
summary(modelo_lineal)


# Obtener los residuos del modelo
residuos <- residuals(modelo_lineal)
# Graficar los residuos vs valores ajustados
plot(fitted(modelo_lineal), residuos, 
     xlab = "Valores Ajustados", 
     ylab = "Residuos",
     main = "Gráfico de Residuos vs Valores Ajustados")

MAE <- mean(abs(dataset_prueba$Price - predicciones))
print(MAE)



###################################MODELO DE REGRESION MULTILINEAL##################

#11. Algoritmo de Regresión multilineal

modelo_regresion_multilineal <- lm(Price ~ SquareFeet + Bedrooms + Bathrooms + NeighborhoodRural + NeighborhoodSuburb + NeighborhoodUrban + HouseAge, data = dataset_entrenamiento)
summary(modelo_regresion_multilineal)
predicciones <- predict(modelo_regresion_multilineal, newdata = dataset_prueba)
head(predicciones)

mae <- mean(abs(predicciones - dataset_prueba$Price))
print(mae)


###################################MODELO DE REGRESION LOGISTICA##################

#12. Preparación y creación del algoritmo Regresión Logística

dataset_entrenamiento_logistica <- dataset_entrenamiento
dataset_prueba_logistica <- dataset_prueba

# Convertir la variable 'Price' en categórica para la clasificación posterior
convert_price_to_category <- function(data) {
  data$Price <- cut(
    data$Price,
    breaks = c(-Inf, 230001, Inf),
    labels = c("Promedio", "Elevado"),
    right = FALSE
  )
  return(data)
}

dataset_entrenamiento_logistica <- convert_price_to_category(dataset_entrenamiento_logistica)
dataset_prueba_logistica <- convert_price_to_category(dataset_prueba_logistica)

# Verificar el balanceo de datos
  table(dataset_entrenamiento_logistica$Price)


 #Se crea el modelo de regresión logística
  dataset_entrenamiento_logistica$Price <- factor(dataset_entrenamiento_logistica$Price, levels = c("Promedio", "Elevado"))
  modelo_regresion_logistica <- glm(
    Price ~ SquareFeet + Bedrooms + Bathrooms + NeighborhoodRural + NeighborhoodSuburb + NeighborhoodUrban + HouseAge,
    data = dataset_entrenamiento_logistica,
    family = binomial()  
  )

  summary(modelo_regresion_logistica)
  
  dataset_prueba_logistica$Price <- factor(dataset_prueba_logistica$Price, levels = c("Promedio", "Elevado"))
  predicciones_prob <- predict(modelo_regresion_logistica, newdata = dataset_prueba_logistica, type = "response")
  predicciones_cat <- ifelse(predicciones_prob > 0.5, "Elevado", "Promedio")

  
  
  # Crear la matriz de confusión
  matriz_confusion <- confusionMatrix(factor(predicciones_cat, levels = c("Promedio", "Elevado")), 
                                      dataset_prueba_logistica$Price)
  matriz_confusion_df <- as.data.frame(matriz_confusion$table)
  colnames(matriz_confusion_df) <- c("Predicción", "Real", "Frecuencia")
  ggplot(matriz_confusion_df, aes(x = Predicción, y = Real, fill = Frecuencia)) +
    geom_tile() +
    geom_text(aes(label = Frecuencia), vjust = 1) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = "Matriz de Confusión",
         x = "Predicción",
         y = "Real") +
    theme_minimal()
  
  
  
  # Imprimir métricas de evaluación de clasificación
  cat("Accuracy:", matriz_confusion$overall["Accuracy"], "\n")
 
  