# Tarea T2 semana 5
# Autor: Francisco Inostroza
# Fecha: 22-09-2026
# Qué hace: Análisis del sector económico donde rinde más cada año de edad de los 
#           trabajadores.

library(dplyr)


# 1. PREGUNTA ECONÓMICA
# ¿En qué sector económico rinde más cada año de edad de los trabajadores?


# 2. CARGA Y EXPLORA
casen    <- read.csv("data/raw/casen_reducido.csv")
ingresos <- read.csv("data/raw/casen_ingresos.csv")

# Inspección de dimensiones, tipos y NA
dim(casen)                  # 60 filas y 6 columnas
str(casen)                  # Tipos de variables
summary(casen$ingreso)      # Se presentan 5 NA en ingreso
sum(is.na(casen$ingreso))   # NA = 5


# 2b. ELIGE COLUMNAS POR PATRÓN
names(select(ingresos, starts_with("ing")))  # 4 columnas
names(select(ingresos, where(is.numeric)))   # 7 columnas

# starts_with("ing") selecciona únicamente variables que comienzan con "ing".
# is.numeric evalúa el tipo de dato y suma columnas numéricas que no son 
# ingresos, como educ, edad y horas.


# 3. USA LOS CINCO VERBOS: MUTATE Y CASE_WHEN()
# Cálculo de experiencia potencial de Mincer e ingreso por año de edad
casen <- casen |>
  mutate(
    experiencia      = pmax(edad - educ - 6, 0),
    ingreso_por_edad = ingreso / edad
  )

# 3b. Creación de variables categóricas de 3 niveles con case_when
casen <- casen |>
  mutate(
    nivel_educ = case_when(
      educ <  12 ~ "Sin media completa",
      educ == 12 ~ "Media completa",
      TRUE       ~ "Superior"
    ),
    grupo_etario = case_when(
      edad < 30 ~ "Joven",
      edad < 50 ~ "Adulto",
      TRUE      ~ "Mayor"
    )
  )

# Verificación de ausencia de valores NA en las categorías
table(casen$nivel_educ, useNA = "ifany")    
table(casen$grupo_etario, useNA = "ifany")  


# 4. AGREGA CON GROUP_BY()

nrow(filter(casen, (region == "Ñuble" | region == "Biobío") & edad > 30))
nrow(filter(casen, region == "Ñuble" | region == "Biobío" & edad > 30))

# 4a. Agregación por UN grupo 
# Se utilizan los 5 verbos (filter, select, group_by, summarise, arrange)
resultado_un_grupo <- casen |>
  filter((region == "Ñuble" | region == "Biobío") & !is.na(ingreso)) |>
  select(sector, edad, ingreso, ingreso_por_edad) |>
  group_by(sector) |>
  summarise(
    n              = n(),
    ingreso_medio  = mean(ingreso, na.rm = TRUE),
    edad_media     = mean(edad, na.rm = TRUE),
    rendimiento    = mean(ingreso_por_edad, na.rm = TRUE)
  ) |>
  arrange(desc(rendimiento))

resultado_un_grupo

# 4a Agregación cruzada por DOS grupos (sector x grupo_etario)
resultado_dos_grupos <- casen |>
  filter(!is.na(ingreso)) |>
  group_by(sector, grupo_etario) |>
  summarise(
    n             = n(),
    ingreso_medio = mean(ingreso, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(sector, desc(ingreso_medio))

resultado_dos_grupos

# 4b. Comparación individual respecto al promedio del propio sector
casen_brechas <- casen |>
  filter(!is.na(ingreso)) |>
  group_by(sector) |>
  mutate(brecha = ingreso - mean(ingreso, na.rm = TRUE)) |>
  ungroup() |>
  select(sector, edad, ingreso, brecha) |>
  arrange(brecha)

casen_brechas 
head(casen_brechas, 5) #muestra los 5 primeros resultados

# summarise() agrupa las observaciones y entrega una sola fila de resumen por sector.
# group_by() con mutate() mantiene todas las filas de la tabla original: permite 
# comparar el valor de cada persona con el promedio de su propio grupo.


# 5. TRATA LOS NA EXPLÍCITAMENTE
mean(casen$ingreso)                # Retorna NA por existencia de datos faltantes
mean(casen$ingreso, na.rm = TRUE)  # Calcula la media sobre los 55 casos válidos

# Se eliminan 5 observaciones con datos faltantes en ingreso para calcular bien
# las medias salariales. Aunque la muestra pasa de 60 a 55 observaciones.


# 6. ENCADENA CON EL PIPE
# Se presenta en la pregunta 4

resultado_un_grupo


# 7. INTERPRETACIÓN ECONÓMICA
# El sector Servicios presenta el mayor rendimiento por cada año de edad de los 
# trabajadores, de $19.295 pesos por año (Con un ingreso medio de $794.000. 
# Esta cifra supera a sectores como el de educacion que a pesar de contar con un 
# ingreso medio mas alto ($905.250 pesos), Sin embargo cuenta  con un rendimiento 
# de $17.579 pesos,por cada año de edad, por lo que es menor que en servicios. 
# Es decir, en el sector educación los trabajadores tienen una edad 
# promedio bastante más alta, por lo que se requieren más años de vida para alcanzar
# sueldos elevados.
# LIMITACIÓN: Al segmentar la muestra, sectores como Comercio con un n=2 o Educación
# con un n=4, quedan con muy pocas observaciones, por lo que no son representativos y un 
# valor atípico alterar completamente el resultado. 
# Además, la edad no es el único factor que  determina el ingreso
# también influyen las horas trabajadas, el nivel educativo y el cargo, elementos
# que no fueron considerados en este caso.