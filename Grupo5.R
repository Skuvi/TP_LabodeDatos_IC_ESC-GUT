
# =============================================================================
# TRABAJO PRÁCTICO 1 - LABORATORIO DE DATOS - Tobías Escobar & Alejo Gutierrez 
# Instituto de Cálculo - Comisión 2
# Fecha límite: martes 26/5/2026 - 22:00 h
# =============================================================================
#
# IMPORTANTE: leer todo el enunciado antes de empezar a resolverlo.
#
# Tema: análisis del desempeño de estudiantes en las pruebas Aprender 2024
# (lengua y matemática) a nivel nacional.
#
# =============================================================================
# DATASETS
# =============================================================================
#
# -----------------------------------------------------------------------------
# base_aprender_secu_2024.csv
# -----------------------------------------------------------------------------
# Información relevada por Aprender 2024 a nivel sujeto:
#   jurisdiccion       : provincia a la que pertenece la escuela
#   sector             : sector de la escuela (privado / estatal)
#   ambito             : ámbito de la escuela (rural / urbano)
#   ID_colegio         : ID anónimo de la escuela
#   ID_seccion         : ID anónimo del curso (grado)
#   ID_alumno          : ID anónimo del alumno
#   lpuntaje           : puntaje en la prueba de lengua
#   mpuntaje           : puntaje en la prueba de matemática
#   ldesemp            : desempeño categorizado en lengua
#   mdesemp            : desempeño categorizado en matemática
#   Nivel_Ed_MadreX    : nivel educativo de la madre
#
# -----------------------------------------------------------------------------
# presupuesto_educacion_2024.csv
# -----------------------------------------------------------------------------
# Información a nivel provincia/año sobre gasto educativo:
#   Año                              : año del dato
#   jurisdiccion                     : provincia
#   Total                            : gasto total ($)
#   Personal                         : gasto en personal ($)
#   Bienes_y_servicios_no_personales : gasto en bienes y servicios ($)
#   Gasto_x_alumno_estatal           : gasto total por alumno (sector estatal)
#
# =============================================================================
#===========================LIBRERIAS A USAR===================================

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("ggrepel")
library(ggrepel)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(knitr)
library(kableExtra)

# =============================================================================
# DATASETS
# =============================================================================

base <- read.csv("base_aprender_secu_2024.csv")

presupuesto <- read.csv("presupuesto_educacion_2024.csv")

# =============================================================================
# 1.A) VARIABLES NUMÉRICAS (Estadísticos Resumen)
# =============================================================================
#Tratamiento de nulos aca:
na_tabla <- base %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to= "Variable", values_to= "Cant_NA") %>%
  mutate(Prop_NA = round(Cant_NA / nrow(base)*100, 2)) %>%
  arrange(desc(Cant_NA))
cant_na <- sum(na_tabla$Cant_NA)
sprintf("La cantidad total de valores nulos fue %d  ", sum(cant_na))

# Base Aprender: Lengua
resumen_lpuntaje <- base %>%
  summarise(
    Variable = "lpuntaje (Lengua)",
    Media   = mean(lpuntaje, na.rm = TRUE),
    Mediana = median(lpuntaje, na.rm = TRUE),
    Desvio  = sd(lpuntaje, na.rm = TRUE),
    Minimo  = min(lpuntaje, na.rm = TRUE),
    Maximo  = max(lpuntaje, na.rm = TRUE)
  )
print(resumen_lpuntaje)

# Base Aprender: Matemática
resumen_mpuntaje <- base %>%
  summarise(
    Variable = "mpuntaje (Matemática)",
    Media   = mean(mpuntaje, na.rm = TRUE),
    Mediana = median(mpuntaje, na.rm = TRUE),
    Desvio  = sd(mpuntaje, na.rm = TRUE),
    Minimo  = min(mpuntaje, na.rm = TRUE),
    Maximo  = max(mpuntaje, na.rm = TRUE)
  )
print(resumen_mpuntaje)

# Base Presupuesto: Gasto Total
resumen_gasto_total <- presupuesto %>%
  summarise(
    Variable = "Gasto Total",
    Media   = mean(Total, na.rm = TRUE),
    Mediana = median(Total, na.rm = TRUE),
    Desvio  = sd(Total, na.rm = TRUE),
    Minimo  = min(Total, na.rm = TRUE),
    Maximo  = max(Total, na.rm = TRUE)
  )
print(resumen_gasto_total)

# Base Presupuesto: Gasto en Personal
resumen_gasto_personal <- presupuesto %>%
  summarise(
    Variable = "Gasto en Personal",
    Media   = mean(Personal, na.rm = TRUE),
    Mediana = median(Personal, na.rm = TRUE),
    Desvio  = sd(Personal, na.rm = TRUE),
    Minimo  = min(Personal, na.rm = TRUE),
    Maximo  = max(Personal, na.rm = TRUE)
  )
print(resumen_gasto_personal)

# Base Presupuesto: Bienes y Servicios No Personales
resumen_gasto_bienes <- presupuesto %>%
  summarise(
    Variable = "Bienes y Servicios No Personales",
    Media   = mean(Bienes_y_servicios_no_personales, na.rm = TRUE),
    Mediana = median(Bienes_y_servicios_no_personales, na.rm = TRUE),
    Desvio  = sd(Bienes_y_servicios_no_personales, na.rm = TRUE),
    Minimo  = min(Bienes_y_servicios_no_personales, na.rm = TRUE),
    Maximo  = max(Bienes_y_servicios_no_personales, na.rm = TRUE)
  )
print(resumen_gasto_bienes)

# Base Presupuesto: Gasto por Alumno Estatal
resumen_gasto_alumno <- presupuesto %>%
  summarise(
    Variable = "Gasto por Alumno Estatal",
    Media   = mean(Gasto_x_alumno_estatal, na.rm = TRUE),
    Mediana = median(Gasto_x_alumno_estatal, na.rm = TRUE),
    Desvio  = sd(Gasto_x_alumno_estatal, na.rm = TRUE),
    Minimo  = min(Gasto_x_alumno_estatal, na.rm = TRUE),
    Maximo  = max(Gasto_x_alumno_estatal, na.rm = TRUE)
  )
print(resumen_gasto_alumno)


# =============================================================================
# 1.B) VARIABLES CATEGÓRICAS (Frecuencias Absolutas y Relativas)
# =============================================================================

# Variable: Sector
tabla_sector <- base %>%
  count(sector, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Sector")
print(tabla_sector)

# Variable: Ámbito
tabla_ambito <- base %>%
  count(ambito, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Ámbito")
print(tabla_ambito)

# Variable: Jurisdicción
tabla_jurisdiccion <- base %>%
  count(jurisdiccion, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Jurisdicción")
print(tabla_jurisdiccion)

# Variable: Desempeño Lengua
tabla_ldesemp <- base %>%
  count(ldesemp, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Desempeño Lengua")
print(tabla_ldesemp)

# Variable: Desempeño Matemática
tabla_mdesemp <- base %>%
  count(mdesemp, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Desempeño Matemática")
print(tabla_mdesemp)

# Variable: Nivel Educativo de la Madre
tabla_madre <- base %>%
  count(Nivel_Ed_MadreX, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Nivel Educativo Madre")
print(tabla_madre)



# =============================================================================
# CONSIGNA 2 - COBERTURA Y ESTRUCTURA DEL OPERATIVO (PARTE 1)
# =============================================================================

# -----------------------------------------------------------------------------
# 1. ¿En cuántas escuelas se realizó la evaluación?
# -----------------------------------------------------------------------------

# Contamos cuántos IDs de colegio únicos existen en la base antes de filtrar

total_escuelas_antes <- n_distinct(base$ID_colegio)

cat("La evaluación se realizó en un total de:", total_escuelas_antes, "escuelas.\n\n")


# -----------------------------------------------------------------------------
# 2. Estructura interna: Secciones y Alumnos por escuela
# -----------------------------------------------------------------------------
# Primero agrupamos por escuela para saber la cantidad exacta que tiene cada una
estructura_por_escuela <- base %>%
  group_by(ID_colegio) %>%
  summarise( # Para cada colegio, cuenta cuántas secciones distintas (ID_seccion) 
             # y cuántos alumnos únicos (ID_alumno) registra.
    cant_secciones = n_distinct(ID_seccion),
    cant_alumnos   = n_distinct(ID_alumno)
  )


# -----------------------------------------------------------------------------
# 3. Calcular y reportar: media, mediana, desvío estándar y rango
# -----------------------------------------------------------------------------

# Métricas para las Secciones por Escuela
resumen_secciones <- estructura_por_escuela %>%
  summarise(
    Indicador = "Secciones por escuela",
    Media   = mean(cant_secciones),
    Mediana = median(cant_secciones),
    Desvio_Estandar = sd(cant_secciones),
    Minimo  = min(cant_secciones),
    Maximo  = max(cant_secciones)
  )

print("Tabla Resumen: Secciones por Escuela")
print(resumen_secciones)

# Métricas para los Alumnos por Escuela
resumen_alumnos <- estructura_por_escuela %>%
  summarise(
    Indicador = "Alumnos por escuela",
    Media   = mean(cant_alumnos),
    Mediana = median(cant_alumnos),
    Desvio_Estandar = sd(cant_alumnos),
    Minimo  = min(cant_alumnos),
    Maximo  = max(cant_alumnos)
  )

print("Tabla Resumen: Alumnos por Escuela")
print(resumen_alumnos)


# =============================================================================
# CONSIGNA 2 - COBERTURA Y ESTRUCTURA DEL OPERATIVO (PARTE 2: FILTRADO)
# =============================================================================

# Guardamos el total de alumnos iniciales en la base antes de filtrar
total_alumnos_antes <- n_distinct(base$ID_alumno)

# Identificamos cuáles son los IDs de las escuelas que cumplen el requisito (>= 10 alumnos)
# Usamos la tabla 'estructura_por_escuela' que creamos en el paso anterior
escuelas_validas <- estructura_por_escuela %>%
  filter(cant_alumnos >= 10) %>%
  pull(ID_colegio)

# Creamos el nuevo dataframe filtrado
# Este dataframe es el que usaremos para el resto del TP
df_filtrado <- base %>%
  filter(ID_colegio %in% escuelas_validas)

# Calculamos cuántos elementos quedaron excluidos del análisis
escuelas_fuera <- total_escuelas_antes - n_distinct(df_filtrado$ID_colegio)
alumnos_fuera   <- total_alumnos_antes - n_distinct(df_filtrado$ID_alumno)

# Reportamos los resultados por consola
print("RESULTADO DEL FILTRADO (EXCLUSIÓN)")
cat("Cantidad de ESCUELAS que quedan fuera:", escuelas_fuera, "\n")
cat("Cantidad de ESTUDIANTES que quedan fuera:", alumnos_fuera, "\n")



# =============================================================================
# CONSIGNA 3 - DISTRIBUCIÓN DEL DESEMPEÑO INDIVIDUAL (ANÁLISIS GRÁFICO)
# =============================================================================

# -----------------------------------------------------------------------------
# 3.1) Distribución del puntaje en Lengua (Histograma)
# -----------------------------------------------------------------------------
grafico_hist_lengua <- ggplot(df_filtrado, aes(x = lpuntaje)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 30) +
  labs(
    title = "Distribución de los Puntajes en Lengua",
    subtitle = "Operativo Aprender 2024 (Escuelas con >= 10 alumnos)",
    x = "Puntaje obtenido",
    y = "Cantidad de estudiantes"
  ) +
  theme_minimal()

print(grafico_hist_lengua)


# -----------------------------------------------------------------------------
# 3.2) Distribución del puntaje en Matemática (Histograma)
# -----------------------------------------------------------------------------
grafico_hist_mate <- ggplot(df_filtrado, aes(x = mpuntaje)) +
  geom_histogram(fill = "salmon", color = "white", bins = 30) +
  labs(
    title = "Distribución de los Puntajes en Matemática",
    subtitle = "Operativo Aprender 2024 (Escuelas con >= 10 alumnos)",
    x = "Puntaje obtenido",
    y = "Cantidad de estudiantes"
  ) +
  theme_minimal()

print(grafico_hist_mate)


# -----------------------------------------------------------------------------
# 3.3) Distribución de los niveles de desempeño en ambas áreas (Gráfico de Barras)
# -----------------------------------------------------------------------------
# Como queremos comparar ambas áreas en un mismo gráfico, nos conviene 
# reestructurar los niveles a formato largo para facilitar el diseño.

# df_niveles_largo <- df_filtrado %>%
#   select(ID_alumno, ldesemp, mdesemp) %>%
#   pivot_longer(cols = c(ldesemp, mdesemp), names_to = "Area", values_to = "Nivel") %>%
#   filter(!is.na(Nivel), Nivel != "") %>%   # ← reemplaza drop_na por esto
#   mutate(Area = if_else(Area == "ldesemp", "Lengua", "Matemática"))

df_niveles_largo <- df_filtrado %>%
  select(ID_alumno, ldesemp, mdesemp) %>%
  pivot_longer(cols = c(ldesemp, mdesemp), names_to = "Area", values_to = "Nivel") %>%
  # Limpiamos posibles valores NA para que el gráfico quede prolijo (aca conviene usar !is.na, ya que el otro no funciona bien)
  filter(!is.na(Nivel), Nivel != "") %>%
  mutate(Area = if_else(Area == "ldesemp", "Lengua", "Matemática"))

grafico_barras_desempenio <- ggplot(df_niveles_largo, aes(x = Nivel, fill = Area)) +
  geom_bar(position = "dodge") + # 'dodge' coloca las barras de cada área una al lado de la otra
  labs(
    title = "Comparación de Niveles de Desempeño",
    subtitle = "Distribución individual por área evaluada",
    x = "Nivel de desempeño categorizado",
    y = "Cantidad de estudiantes",
    fill = "Área"
  ) +
  # Mantenemos coherencia de colores con los histogramas previos
  scale_fill_manual(values = c("Lengua" = "skyblue", "Matemática" = "salmon")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rota las etiquetas

print(grafico_barras_desempenio)



# =============================================================================
# CONSIGNA 4 - RENDIMIENTO POR ESCUELA SEGÚN CARACTERÍSTICAS INSTITUCIONALES
# =============================================================================

# -----------------------------------------------------------------------------
# 4.0) Agregación de datos a nivel Escuela
# -----------------------------------------------------------------------------
# Transformamos la base de alumnos a una base donde cada fila es una escuela única
df_escuelas <- df_filtrado %>%
  group_by(ID_colegio) %>%
  summarise(
    lpuntaje_prom = mean(lpuntaje, na.rm = TRUE),
    mpuntaje_prom = mean(mpuntaje, na.rm = TRUE),
    # Usamos first para conservar las características de la escuela, 
    # ya que todos los alumnos de una misma escuela comparten la misma provincia, sector y ámbito
    jurisdiccion  = first(jurisdiccion),
    sector        = first(sector),
    ambito        = first(ambito)
  )

# -----------------------------------------------------------------------------
# 4.a) Análisis según JURISDICCIÓN (Provincias)
# -----------------------------------------------------------------------------
# Tabla Resumen por Provincia
tabla_rend_jurisdiccion <- df_escuelas %>%
  group_by(jurisdiccion) %>%
  summarise(
    Cant_Escuelas = n(),
    Promedio_Lengua = mean(lpuntaje_prom, na.rm = TRUE),
    Promedio_Mate   = mean(mpuntaje_prom, na.rm = TRUE)
  ) %>%
  arrange(desc(Promedio_Mate)) # Las ordenamos de mayor a menor rendimiento en Matemática

print("Rendimiento Promedio de Escuelas por Jurisdicción")
print(tabla_rend_jurisdiccion)

# Gráfico Comparativo por Jurisdicción (Boxplot de Matemática)
grafico_box_jurisdiccion <- ggplot(df_escuelas, aes(x = reorder(jurisdiccion, mpuntaje_prom, FUN = median), y = mpuntaje_prom)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  coord_flip() + # Rotamos el gráfico de costado para poder leer los nombres de las provincias
  labs(
    title = "Rendimiento en Matemática de las Escuelas por Jurisdicción",
    subtitle = "Ordenado por la mediana de cada provincia",
    x = "Jurisdicción (Provincia)",
    y = "Puntaje Promedio de la Escuela"
  ) +
  theme_minimal() + theme(axis.text.y = element_text(size = 7))

print(grafico_box_jurisdiccion)


# -----------------------------------------------------------------------------
# 4.b) Análisis según SECTOR DE GESTIÓN (Estatal / Privado)
# -----------------------------------------------------------------------------
# Tabla Resumen por Sector
tabla_rend_sector <- df_escuelas %>%
  group_by(sector) %>%
  summarise(
    Cant_Escuelas = n(),
    Promedio_Lengua = mean(lpuntaje_prom, na.rm = TRUE),
    Promedio_Mate   = mean(mpuntaje_prom, na.rm = TRUE)
  )

print("Rendimiento Promedio de Escuelas por Sector")
print(tabla_rend_sector)


# -----------------------------------------------------------------------------
# 4.c) Análisis según ÁMBITO (Urbano / Rural) y Cruce con Sector
# -----------------------------------------------------------------------------
# Tabla Resumen por Ámbito
tabla_rend_ambito <- df_escuelas %>%
  group_by(ambito) %>%
  summarise(
    Cant_Escuelas = n(),
    Promedio_Lengua = mean(lpuntaje_prom, na.rm = TRUE),
    Promedio_Mate   = mean(mpuntaje_prom, na.rm = TRUE)
  )

print("Rendimiento Promedio de Escuelas por Ámbito")
print(tabla_rend_ambito)

# Gráfico Comparativo Combinado: Sector vs Ámbito
grafico_box_institucional <- ggplot(df_escuelas, aes(x = sector, y = mpuntaje_prom, fill = ambito)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Rendimiento Escolar en Matemática según Sector y Ámbito",
    subtitle = "Comparativa cruzada del promedio institucional",
    x = "Sector de Gestión",
    y = "Puntaje Promedio de la Escuela",
    fill = "Ámbito"
  ) +
  theme_minimal()

print(grafico_box_institucional)

#aca esta el calculo hecho para sacar el promedio de escuelas que sacamos:
round(escuelas_fuera / total_escuelas_antes * 100, 1)

# =============================================================================
# CONSIGNA 5 - VINCULACIÓN ENTRE GASTO EDUCATIVO Y RENDIMIENTO ESCOLAR
# =============================================================================

# -----------------------------------------------------------------------------
# 5.1) Filtrado y Agregación de Rendimiento Estatal por Jurisdicción
# -----------------------------------------------------------------------------
# Nos quedamos solo con escuelas estatales y promediamos su rendimiento por provincia
# Para evitar un sesgo entre las escuelas que cuentan con menos alumnos que otras, usamos df_filtrado
# y luego no utilizamos directamente los promedios para no volver a promediarlos.
rendimiento_estatal_jurisdiccion <- df_filtrado %>%
  filter(sector == "Estatal") %>%
  group_by(jurisdiccion) %>%
  summarise(
    prom_mate_estatal = mean(mpuntaje, na.rm = TRUE),
    prom_lengua_estatal = mean(lpuntaje, na.rm = TRUE)
  )

# -----------------------------------------------------------------------------
# 5.2) Unión de Bases
# -----------------------------------------------------------------------------
# Vinculamos los datos de rendimiento con la base de presupuesto usando la provincia como llave
df_vinculado <- rendimiento_estatal_jurisdiccion %>%
  inner_join(presupuesto, by = "jurisdiccion")


# -----------------------------------------------------------------------------
# 5.3) Gráfico de Dispersión (Scatterplot) con Línea de Tendencia Descriptiva
# -----------------------------------------------------------------------------
grafico_dispersion <- ggplot(df_vinculado, aes(x = Gasto_x_alumno_estatal, y = prom_mate_estatal)) +
  # Dibujamos la nube de puntos (cada punto es una provincia)
  geom_point(color = "darkgreen", size = 3, alpha = 0.8) +
  # Agregamos una línea de tendencia suave para ver la dirección de los datos
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") + 
  # Le ponemos el nombre de la provincia a cada punto para poder identificarlas
  ggrepel::geom_text_repel(aes(label = jurisdiccion),size = 3, check_overlap = TRUE) + 
  labs(
    title = "Relación entre Gasto por Alumno Estatal y Rendimiento en Matemática",
    subtitle = "Análisis a nivel jurisdiccional (Solo escuelas de sector Estatal)",
    x = "Gasto por Alumno Estatal ($)",
    y = "Puntaje Promedio de la Jurisdicción (Matemática)"
  ) +
  theme_minimal()

print(grafico_dispersion)



# =============================================================================
# CONSIGNA 6 - ANÁLISIS LIBRE: DETECCIÓN DE ESCUELAS OUTLIERS EN PBA
# =============================================================================

# -----------------------------------------------------------------------------
# 6.1) Filtrado de datos para la Jurisdicción bajo estudio
# -----------------------------------------------------------------------------
# Nos quedamos únicamente con las escuelas de la Provincia de Buenos Aires
df_pba <- df_escuelas %>%
  filter(jurisdiccion == "Buenos Aires")

# -----------------------------------------------------------------------------
# 6.2) Metodología Estadística para Detectar Outliers
# -----------------------------------------------------------------------------
# Calculamos los cuartiles y el Interquartile Range (IQR) para Matemática en PBA
q1_pba  <- quantile(df_pba$mpuntaje_prom, 0.25, na.rm = TRUE)
q3_pba  <- quantile(df_pba$mpuntaje_prom, 0.75, na.rm = TRUE)
iqr_pba <- q3_pba - q1_pba

# Definimos los límites matemáticos para considerar a una escuela como "atípica"
limite_inferior <- q1_pba - 1.5 * iqr_pba
limite_superior <- q3_pba + 1.5 * iqr_pba

# -----------------------------------------------------------------------------
# 6.3) Identificación de las Escuelas Outliers
# -----------------------------------------------------------------------------
# Escuelas con rendimiento excepcionalmente ALTO
outliers_altos <- df_pba %>%
  filter(mpuntaje_prom > limite_superior) %>%
  select(ID_colegio, mpuntaje_prom, sector, ambito) %>%
  arrange(desc(mpuntaje_prom))

# Escuelas con rendimiento excepcionalmente BAJO
outliers_bajos <- df_pba %>%
  filter(mpuntaje_prom < limite_inferior) %>%
  select(ID_colegio, mpuntaje_prom, sector, ambito) %>%
  arrange(mpuntaje_prom)

print("Escuelas Outliers de ALTO Rendimiento en PBA")
print(head(outliers_altos, 10))

print("Escuelas Outliers de BAJO Rendimiento en PBA")
print(head(outliers_bajos, 10))

# -----------------------------------------------------------------------------
# 6.4) Visualización Gráfica del Análisis de Dispersión
# -----------------------------------------------------------------------------
grafico_outliers_pba <- ggplot(df_pba, aes(x = sector, y = mpuntaje_prom, fill = sector)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2.5, outlier.shape = 16) +
  # Agregamos los límites teóricos con líneas punteadas para que se entienda el criterio
  geom_hline(yintercept = limite_superior, linetype = "dashed", color = "darkgreen", size = 0.8) +
  geom_hline(yintercept = limite_inferior, linetype = "dashed", color = "darkred", size = 0.8) +
  labs(
    title = "Detección de Escuelas Outliers en la Provincia de Buenos Aires",
    subtitle = "Identificación de anomalías de rendimiento en Matemática (Criterio 1.5xIQR)",
    x = "Sector de Gestión",
    y = "Puntaje Promedio de la Escuela (Matemática)",
    fill = "Sector"
  ) +
  theme_minimal()

print(grafico_outliers_pba)



# =============================================================================
# PAUTAS DE ENTREGA
# =============================================================================
#
# - Formato: Rmd + reporte en PDF (o HTML).
# - Subir al campus (solapa "Trabajo Práctico N° 1") como "Grupo_X".
# - Entregar también versión impresa del reporte.
# - Fecha límite: martes 26/5/2026, 22:00 h.
#
# =============================================================================