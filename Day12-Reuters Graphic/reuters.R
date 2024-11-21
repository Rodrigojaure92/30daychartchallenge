library(readxl)
library(ggplot2)


# URL del archivo
url <- "https://raw.githubusercontent.com/Rodrigojaure92/30daychartchallenge/main/Day09-major-minor/stock_retail_enero2024.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
download.file(url, temp_file, mode = "wb")
data <- read_excel(temp_file)
unlink(temp_file)


# Asignar colores HEX a cada categoría
colores <- c(
  "Electrónica" = "#569ed3",       # Azul claro
  "Electrodomésticos" = "#74c476", # Verde claro
  "Ropa" = "#f68e26",              # Naranja claro
  "Hogar" = "#9582c1"              # Púrpura claro
)

# Crear gráfico de barras con valores encima
grafico <- ggplot(data, aes(x = Categoria, y = Stock_Final, fill = Categoria)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = Stock_Final), vjust = -0.5, size = 4, color = "black") + # Etiquetas de valores
  scale_fill_manual(values = colores) +
  theme_minimal() +
  labs(
    title = "Distribución de stock por categorías - Enero 2024",
    x = "Categoría",
    y = "Stock Final"
  ) +
  theme(
    text = element_text(color = "#333333"), # Texto en negro ligero
    panel.grid.major = element_line(color = "#cccccc"), # Líneas de cuadrícula en gris claro
    panel.grid.minor = element_blank(), # Sin líneas de cuadrícula menores
    plot.background = element_rect(fill = "#f9f9f9"), # Fondo blanco ligero
    panel.background = element_rect(fill = "#f9f9f9"), # Fondo panel blanco ligero
    legend.position = "none" # Sin leyenda
  )

print(grafico)

