# ejercicio
library(dplyr)
library(readr)
library(tidyverse)
# setear directorio de trabajo (ajusta a tu ruta)
setwd("C:\\Users\\Juli ❤️\\Documents\\analitica")

# Leer el archivo CSV

concerts <- read_csv("concerts.csv")

summary_stats <- concerts %>%
  summarise(
    variable = "Ventas netas (net_sales)",
    media = mean(net_sales, na.rm = TRUE),
    mediana = median(net_sales, na.rm = TRUE),
    moda = paste(unique(net_sales)[tabulate(match(net_sales, unique(net_sales))) == max(tabulate(match(net_sales, unique(net_sales))))], collapse = ", "),
    varianza = var(net_sales, na.rm = TRUE),
    desviacion = sd(net_sales, na.rm = TRUE),
    rango = max(net_sales, na.rm = TRUE) - min(net_sales, na.rm = TRUE),
    coef_variacion = sd(net_sales, na.rm = TRUE) / mean(net_sales, na.rm = TRUE)
  )
print(summary_stats)

# matriz de correlacion
concerts <- concerts %>%
  mutate(spend = cost_Facebook_Prospecting +
                 cost_Facebook_Retargeting +
                 cost_Google_Pmax +
                 cost_Google_Search +
                 cost_TikTok_Prospecting +
                 cost_TikTok_Retargeting)

# Calcular matriz de correlación entre spend y net_sales
cor_matrix <- concerts %>%
  select(spend, net_sales) %>%
  cor(use = "complete.obs")

print(cor_matrix)

<<<<<<< Updated upstream
summary_stats
=======
>>>>>>> Stashed changes
