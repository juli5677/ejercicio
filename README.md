# ejercicio
library(dplyr)
library(readr)
library(tidyverse)
# setear directorio de trabajo (ajusta a tu ruta)
setwd("C:/Users/claud/OneDrive/Documentos")
getwd()
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

ggplot(concerts, aes(x = unemployment, y = net_sales)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relación entre net_sales y unemployment",
       x = "unemployment", y = "net_sales") +
  theme_minimal()
  #La gráfica sugiere que existe una relación negativa entre el desempleo y las ventas netas: cuando el desempleo aumenta, las ventas tienden a disminuir. Sin embargo, la dispersión de los puntos indica que el desempleo por sí solo probablemente no explique gran parte de la variación en las ventas, por lo que, aunque podría ser un predictor estadísticamente significativo, su poder explicativo sería limitado y sería recomendable considerar otras variables adicionales en el análisis.
