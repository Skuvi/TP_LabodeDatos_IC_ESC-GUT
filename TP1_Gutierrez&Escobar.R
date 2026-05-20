
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

library(ggplot2)
library(dplyr)

# =============================================================================
# DATASETS
# =============================================================================

base <- read.csv("base_aprender_secu_2024.csv")

presupuesto <- read.csv("presupuesto_educacion_2024.csv")

# =============================================================================
# 1.A) VARIABLES NUMÉRICAS (Estadísticos Resumen)
# =============================================================================

# --- Base Aprender: Lengua ---
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

# --- Base Aprender: Matemática ---
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

# --- Base Presupuesto: Gasto Total ---
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

# --- Base Presupuesto: Gasto en Personal ---
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

# --- Base Presupuesto: Bienes y Servicios No Personales ---
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

# --- Base Presupuesto: Gasto por Alumno Estatal ---
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

# --- Variable: Sector ---
tabla_sector <- base %>%
  count(sector, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Sector")
print(tabla_sector)

# --- Variable: Ámbito ---
tabla_ambito <- base %>%
  count(ambito, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Ámbito")
print(tabla_ambito)

# --- Variable: Jurisdicción ---
tabla_jurisdiccion <- base %>%
  count(jurisdiccion, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Jurisdicción")
print(tabla_jurisdiccion)

# --- Variable: Desempeño Lengua ---
tabla_ldesemp <- base %>%
  count(ldesemp, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Desempeño Lengua")
print(tabla_ldesemp)

# --- Variable: Desempeño Matemática ---
tabla_mdesemp <- base %>%
  count(mdesemp, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Desempeño Matemática")
print(tabla_mdesemp)

# --- Variable: Nivel Educativo de la Madre ---
tabla_madre <- base %>%
  count(Nivel_Ed_MadreX, name = "Frec_Absoluta") %>%
  mutate(Frec_Relativa_Porc = (Frec_Absoluta / sum(Frec_Absoluta)) * 100)
print("Tabla de Frecuencias: Nivel Educativo Madre")
print(tabla_madre)


# ==============base# =============================================================================
# CONSIGNA 2 - COBERTURA Y ESTRUCTURA DEL OPERATIVO
# =============================================================================
#
# A partir de la base Aprender, responder:
#   - ¿En cuántas escuelas se realizó la evaluación?
#   - ¿Cuántas secciones participaron por escuela?
#   - ¿Cuántos alumnos participaron por escuela?
#
# Reportar media, mediana, desvío estándar y rango.
#
# FILTRO IMPORTANTE:
# A partir de este punto se trabaja SOLO con escuelas que tengan al menos
# 10 estudiantes evaluados.
#   -> ¿Cuántas escuelas quedan fuera del análisis?
#   -> ¿Cuántos estudiantes quedan fuera del análisis?
#
# =============================================================================



# =============================================================================
# CONSIGNA 3 - DISTRIBUCIÓN DEL DESEMPEÑO INDIVIDUAL
# =============================================================================
#
# Análisis gráfico de:
#   - Distribución del puntaje en Lengua.
#   - Distribución del puntaje en Matemática.
#   - Distribución de los niveles de desempeño en ambas áreas.
#
# =============================================================================



# =============================================================================
# CONSIGNA 4 - RENDIMIENTO POR ESCUELA SEGÚN CARACTERÍSTICAS INSTITUCIONALES
# =============================================================================
#
# Calcular indicadores agregados a nivel escuela (ej: promedio de puntaje
# en Lengua y/o Matemática) y analizar el rendimiento según:
#   a) Jurisdicción
#   b) Sector de gestión (estatal / privado)
#   c) Ámbito (urbano / rural)
#
# Incluir tablas resumen y gráficos comparativos.
#
# =============================================================================



# =============================================================================
# CONSIGNA 5 - RELACIÓN ENTRE DESEMPEÑO Y GASTO EDUCATIVO (SECTOR ESTATAL)
# =============================================================================
#
# SOLO escuelas de gestión estatal.
# Analizar la posible asociación entre:
#   - Puntaje promedio (Lengua y/o Matemática)
#   - Variables de gasto educativo
# Agrupando por jurisdicción.
#
# =============================================================================



# =============================================================================
# CONSIGNA 6 - ANÁLISIS LIBRE
# =============================================================================
#
# Elegir UN objetivo adicional no incluido antes. Opciones sugeridas:
#   - Desigualdad de desempeño según nivel educativo de la madre.
#   - Brechas urbano-rural dentro de una jurisdicción.
#   - Diferencia estatal vs. privada controlando por ámbito.
#   - Dispersión entre escuelas dentro de una misma jurisdicción.
#   - Detección de escuelas outliers (alto / bajo rendimiento).
#
# Mostrar metodología y principales hallazgos.
#
# =============================================================================



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