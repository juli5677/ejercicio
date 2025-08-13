# ejercicio
library(dplyr)
library(readr)
library(tidyverse)

summary_stats <- concerts %>%
  summarise(
    variable = "Ventas netas (net_sales)",
    media = mean(net_sales, na.rm = TRUE),
    mediana = median(net_sales, na.rm = TRUE),
    moda = paste(unique(net_sales)[tabulate(match(net_sales, unique(net_sales))) == max(tabulate(match(net_sales, unique(net_sales))))], collapse = ", "),
    varianza = var(net_sales, na.rm = TRUE),
    desviacion = sd(net_sales, na.rm = TRUE),
    rango = max(net_sales, na.rm = TRUE) - min(net_sales, na.rm = TRUE)
  )

summary_stats
