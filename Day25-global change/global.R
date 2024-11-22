library(tidyverse)
library(ggplot2)
library(dplyr)

url <- "https://github.com/Rodrigojaure92/30daychartchallenge/raw/refs/heads/main/Day25-global%20change/data_ozono.csv"
data_ozono <- read_csv(url)

# Visualización
ggplot(data_ozono, aes(x = Año, y = Capa_Ozono)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Cambio en el Grosor de la Capa de Ozono (1995-2025)",
       x = "Año",
       y = "Capa de Ozono (Unidades Dobson)") +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", linetype = "dashed") # Línea de tendencia lineal
