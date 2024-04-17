
# Probando Librerias
install.packages('GGally')

# Import library
library(plyr)
library(GGally)
library(ggplot2)
library(caTools)

###
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
# Set working directory
setwd("C:/Users/HUAWEI MATEBOOK/Desktop/UC/PAC")

# Importing the dataset
dataset <- read_excel('calibres.xlsx', sheet = "Reporte")
str(dataset)

# Agrupar los datos 
cal_sem <- dataset %>% 
  group_by(Semana) %>%
  summarise(sum_semana = sum(KgCalibre, na.rm = TRUE))
# Mostrar el resultado
print(cal_sem)

ca_sem <- dataset %>% 
  group_by(Calibre) %>%
  summarise(sum_semana = sum(KgCalibre, na.rm = TRUE))
# Mostrar el resultado
print(ca_sem)
# Crear un gráfico de barras con ggplot2 usando los datos resumidos
ggplot(ca_sem %>% drop_na(Calibre, sum_semana), aes(x = Calibre, y = sum_semana, fill = Calibre)) +
  geom_col() +  # geom_col crea un gráfico de barras
  geom_text(aes(label = sprintf("%g", sum_semana)),  # Añade texto en cada barra
            vjust = -0.3,  # Ajusta la posición vertical del texto
            color = "black",  # Color del texto
            size = 3.5) +  # Tamaño del texto
  labs(title = "Suma semanal por Calibre",
       x = "Calibre",
       y = "Suma Semanal de KgCalibre",
       fill = "Calibre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotar etiquetas del eje x para mejor visualización
##################

# Crear el gráfico de líneas
ggplot(dataset, aes(x = Semana, y = KgCalibre, group = Calibre, color = Calibre)) +
  geom_line(na.rm = TRUE) +  # Añadir líneas
  geom_point(na.rm = TRUE) +  # Añadir puntos
  labs(title = "Producción de Frutas por Semana",
       x = "Semana",
       y = "Producción en Kg",
       color = "Fruta") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:max(dataset$Semana, na.rm = TRUE))  # Ajustar los breaks del eje X

# Error con los NA
# Asumiendo que tus datos ya están limpios y listos para ser graficados
ggplot(dataset, aes(x = KgCalibre)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") +  # Ajusta 'bins' según necesidad
  facet_wrap(~ Semana) +  # Crea un histograma separado por cada 'Semana'
  labs(title = "Histograma de Producción por Semana", x = "Kg de Producción", y = "Frecuencia") +
  theme_minimal()
