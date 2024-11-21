# Cargar las bibliotecas necesarias
library(ggplot2)
library(dplyr)

# Leer el archivo CSV desde el enlace
url <- "https://github.com/Rodrigojaure92/30daychartchallenge/raw/refs/heads/main/Day05-diverging/sales_data.csv"
sales_data <- read.csv(url)

# Asegurarnos de que las columnas sean numéricas y la fecha sea reconocida
sales_data$Month <- as.Date(sales_data$Month, format = "%Y-%m-%d")
sales_data$Proyectadas <- as.numeric(sales_data$Proyectadas)
sales_data$Realizadas <- as.numeric(sales_data$Realizadas)

# Calcular la divergencia entre ventas realizadas y proyectadas
sales_data <- sales_data %>%
  mutate(Divergencia = Realizadas - Proyectadas)

# Graficar las ventas proyectadas vs realizadas con áreas de divergencia
ggplot(sales_data, aes(x = Month)) +
  geom_line(aes(y = Proyectadas, color = "Proyectadas"), size = 1.2) +
  geom_line(aes(y = Realizadas, color = "Realizadas"), size = 1.2) +
  geom_ribbon(aes(ymin = pmin(Proyectadas, Realizadas), 
                  ymax = pmax(Proyectadas, Realizadas), 
                  fill = ifelse(Divergencia > 0, "Exceso", "Déficit")), 
              alpha = 0.3) +
  scale_color_manual(values = c("Proyectadas" = "blue", "Realizadas" = "green")) +
  scale_fill_manual(values = c("Exceso" = "green", "Déficit" = "red")) +
  labs(title = "Comparación de Ventas Realizadas vs Proyectadas Maria's store",
       x = "Mes",
       y = "Ventas",
       color = "Línea",
       fill = "Divergencia") +
  theme_minimal() +
  theme(legend.position = "top")
