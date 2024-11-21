# Cargar librerías necesarias
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
library(readr)

# Cargar datos desde la URL
url <- "https://github.com/Rodrigojaure92/30daychartchallenge/raw/refs/heads/main/Day10-Physical/data.csv"
data <- read_csv(url, show_col_types = FALSE)

# Gráfico de densidad de peso por deporte
ggplot(data, aes(x = Weight, fill = Sport)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribución de Peso de Atletas en Diferentes Deportes",
       x = "Peso", y = "Densidad") +
  scale_fill_manual(values = c("green", "red", "orange")) +
  theme_minimal()

