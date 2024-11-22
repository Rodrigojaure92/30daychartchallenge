# Cargar librerías necesarias
library(ggplot2)
library(readr)

url <- "https://github.com/Rodrigojaure92/30daychartchallenge/raw/refs/heads/main/Day20-Correlation/data.csv"
data <- read_csv(url)


# Calcular la correlación
correlacion <- round(cor(data$Gastos_Publicidad, data$Ventas), 2)

# Crear el gráfico con el título centrado y el valor de la correlación
ggplot(data, aes(x = Fecha)) +
  geom_line(aes(y = Gastos_Publicidad, color = "Gastos en Publicidad"), size = 1) +
  geom_line(aes(y = Ventas, color = "Ventas"), size = 1) +
  labs(
    title = paste("Series de Tiempo: Gastos en Publicidad y Ventas\nCorrelación:", correlacion),
    x = "Fecha",
    y = "Valores",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))
