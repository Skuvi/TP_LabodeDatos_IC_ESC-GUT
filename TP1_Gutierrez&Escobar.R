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
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")

library(ggplot2)
library(dplyr)
library(tidyverse)

# =============================================================================
# DATASETS
# =============================================================================

base <- read.csv("/home/skuvi/Documentos/Facultad/Lab de Datos Calculo/TP1/base_aprender_secu_2024.csv")

presupuesto <- read.csv("/home/skuvi/Documentos/Facultad/Lab de Datos Calculo/TP1/presupuesto_educacion_2024.csv")

# =============================================================================
# VARIABLES NUMERICAS
# ---------------------------
# MEDIA
# ---------------------------

probase <- mean(base[,7], na.rm = TRUE)

# Calcula el promedio de lpuntaje
# na.rm = TRUE elimina valores NA

propre <- mean(presupuesto[,3], na.rm = TRUE)

# ---------------------------
# MEDIANA
# ---------------------------

medianaBase <- median(base[,7], na.rm = TRUE)

medianaPre <- median(presupuesto[,3], na.rm = TRUE)

# ---------------------------
# DESVIO ESTANDAR
# ---------------------------

# NO existe ds()
# La funcion correcta es sd()

dsBase <- sd(base[,7], na.rm = TRUE)

dsPre <- sd(presupuesto[,3], na.rm = TRUE)

# ---------------------------
# MINIMO
# ---------------------------

minBase <- min(base[,7], na.rm = TRUE)

minPre <- min(presupuesto[,3], na.rm = TRUE)

# ---------------------------
# MAXIMO
# ---------------------------

maxBase <- max(base[,7], na.rm = TRUE)

maxPre <- max(presupuesto[,3], na.rm = TRUE)

# =============================================================================
# MOSTRAR RESULTADOS
# =============================================================================

print(probase)
print(propre)

print(medianaBase)
print(medianaPre)

print(dsBase)
print(dsPre)

print(minBase)
print(minPre)

print(maxBase)
print(maxPre)

# =============================================================================
# VARIABLES CATEGORICAS
# =============================================================================
#
# Vamos a usar:
#   table()        -> frecuencia absoluta
#   prop.table()   -> frecuencia relativa
#
# =============================================================================

# ---------------------------
# SECTOR
# ---------------------------

freqSector <- table(base$sector)

# Cuenta cuantos hay de cada categoria

print(freqSector)

freqRelSector <- prop.table(freqSector)

# Convierte las frecuencias a porcentajes/proporciones

print(freqRelSector)

# ---------------------------
# AMBITO
# ---------------------------

freqAmbito <- table(base$ambito)

print(freqAmbito)

freqRelAmbito <- prop.table(freqAmbito)

print(freqRelAmbito)

# ---------------------------
# JURISDICCION
# ---------------------------

freqJur <- table(base$jurisdiccion)

print(freqJur)

freqRelJur <- prop.table(freqJur)

print(freqRelJur)
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