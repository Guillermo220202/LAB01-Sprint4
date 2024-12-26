# LAB01-Sprint4
# Crear vectores de datos
energia <- rep(c("renovable", "no renovable"), each = 10)
consumo <- c(
  # Consumo energía renovable (con algunos NA)
  100, 120, NA, 95, 115, 105, NA, 125, 110, 130,
  # Consumo energía no renovable (con algunos NA)
  150, NA, 165, 145, 170, NA, 155, 160, 175, 180
)
costo_kwh <- c(
  rep(0.12, 10),  # Costo energía renovable
  rep(0.15, 10)   # Costo energía no renovable
)

# Paso 2: Limpieza de datos
# Reemplazar NA con la mediana por tipo de energía
consumo_limpio <- numeric(length(consumo))
for(tipo in unique(energia)) {
  indices <- which(energia == tipo)
  mediana <- median(consumo[indices], na.rm = TRUE)
  consumo_limpio[indices] <- ifelse(is.na(consumo[indices]), mediana, consumo[indices])
}

# Paso 3: Crear dataframe
df_consumo <- data.frame(
  tipo_energia = energia,
  consumo = consumo_limpio,
  costo_kwh = costo_kwh
)

# Paso 4: Cálculos
# Agregar columna de costo total
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Agregar columna de ganancia (10% más)
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Calcular totales por tipo de energía
totales_por_tipo <- aggregate(consumo ~ tipo_energia, data = df_consumo, sum)
costos_por_tipo <- aggregate(costo_total ~ tipo_energia, data = df_consumo, sum)
medias_por_tipo <- aggregate(consumo ~ tipo_energia, data = df_consumo, mean)

# Ordenar dataframe por costo total
df_ordenado <- df_consumo[order(df_consumo$costo_total, decreasing = TRUE), ]

# Obtener top 3 costos
top_3_costos <- head(df_ordenado, 3)

# Paso 5: Crear lista resumen
resumen_energia <- list(
  dataframe_ordenado = df_ordenado,
  consumo_total_por_tipo = totales_por_tipo,
  costo_total_por_tipo = costos_por_tipo,
  consumo_promedio_por_tipo = medias_por_tipo,
  top_3_costos = top_3_costos
)

# Mostrar resultados
print("Resumen de análisis energético:")
print(resumen_energia)
