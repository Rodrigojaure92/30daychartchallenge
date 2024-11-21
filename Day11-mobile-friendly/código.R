# Instalar y cargar las bibliotecas necesarias
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
library(dplyr)

url2 <- "https://github.com/Rodrigojaure92/30daychartchallenge/raw/refs/heads/main/Day11-mobile-friendly/dataset.csv"
data <- read.csv(url2,sep = ",")
data <- data %>% filter(tipo_app != "Mensajería")

# Resumir la cantidad total de sesiones por hora y tipo de aplicación
data_summary <- data %>%
  group_by(hora, tipo_app) %>%
  summarise(total_sesiones = sum(sesiones), .groups = "drop")
ggplot(data_summary, aes(x = hora, y = total_sesiones, color = tipo_app)) +
  geom_line(size = 1.2) +
  labs(
    title = "Distribución de manejo de Apps Móviles a lo Largo del Día",
    x = "Hora",
    y = "Número Total de Sesiones",
    color = "Tipo de Aplicación"
  ) +
  theme_minimal()


