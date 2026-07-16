## NOMBRE: ALEX UYAGUARI 
## . Librerias --------------
## Manejo de Datos
library(dplyr) # manipulacion de dataframes
library(readr) # importar en r
library(readxl) # importar desde excel
library(magrittr) # Pipe %>% 
library(stringr) # Manipulacion de texto
library(tidyr)

## Descriptivas y Graficos
library(ggplot2) # graficos
library(GGally) # graficos compuestos
library(skimr) # Descriptivas
## Modelamiento
library(tidymodels) # Modelamiento
library(parallel) # Paralelizacion
library(doParallel) # Paralelizacion 
library(vip) # Variables importantes
## Modelos
library(glmnet) # Regularizacion
library(earth) # MARS
library(openxlsx)   # cargar paquetes
library(readr)
library(tidyverse)
library(magrittr)
library(moments)  # Para calcular curtosis y asimetría
library(recipes)
library(lubridate)  # Para trabajar con fechas
library(factoextra)
library(ISLR2)
# balanceo
library(probably) # Optimizar umbral
library(themis)   # Over/Sub Sampling
library(openxlsx)
# Cargar la librería
library(readxl)
library(haven)

#para recetas
library(tidymodels)
#para X2
library(purrr)
library(tibble)
## intento ocn 2024 


## importacion de bases ----------------------------------------------------

datos_persona2024 <- read_delim(
  "C:/Users/alexu/OneDrive/Desktop/2_BDD_DATOS_ABIERTOS_ENEMDU_2024_CSV/BDDenemdu_personas_2024_anual.csv",
  delim = ";",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE,
  trim_ws = TRUE
)
#view(datos_persona2024)
#dim(datos_persona2024)
#names(datos_persona2024)

datos_hogar2024 <- read_delim(
  "C:/Users/alexu/OneDrive/Desktop/2_BDD_DATOS_ABIERTOS_ENEMDU_2024_CSV/BDDenemdu_vivienda_2024_anual.csv",
  delim = ";",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE,
  trim_ws = TRUE
)
#view(datos_hogar2024)
#dim(datos_hogar2024)
#names(datos_hogar2024)
# Union de bases ----------------------------------------
typeof(datos_hogar2024$vivienda) # ambas deben esta como caracter
typeof(datos_persona2024$vivienda)

#datos_persona2024$vivienda <- as.character(datos_persona2024$vivienda)
#datos_hogar2024$vivienda   <- as.character(datos_hogar2024$vivienda)
# Unión con dos identificadores
datos_persona_hogar24 <- inner_join(
  datos_persona2024,
  datos_hogar2024,
  by = c("id_vivienda", "id_hogar")
)
#dim(datos_persona_hogar24)
#summary(datos_persona_hogar24)

# respaldo de la base y grafico inicial  -------------------------------------------------------
dataPH2024 <- datos_persona_hogar24 # CREO UNA BASE COMO RESPALDO
dim(datos_persona_hogar24)
dim(dataPH2024)
names(dataPH2024)

datos_persona_hogar24 %>% count(area.x) %>% mutate(pct = n / sum(n) * 100)


# Filtracion de universo de estudio y N/As  ------------------------
# Limitar observaciones a la PEA. /No pasa nada porque ya se filtra dejando solo trabajadores
dataPH2024 <- dataPH2024 %>% 
  filter(p03 >= 15 & p03 <= 65)
dim(dataPH2024)

# filtro de secemp  ---------------------------------------------

# Trabajar solo con empleador formal e informal # 1 es Formal, 2 es Informal
dataPH2024 <- dataPH2024 %>%
  filter(secemp == 1 | secemp == 2)
dim(dataPH2024)
#names(dataPH2024)

# datos perdidos  ---------------------------------------------------------
# Porcentaje de datos perdidos
missing_percentage <- dataPH2024 %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Porcentaje_Perdido") %>%
  filter(Porcentaje_Perdido > 29)
print(missing_percentage, n = Inf)
# Vector con nombres de variables a eliminar # Se elimina primeramente estas variables sueprioes al 30% en perdidos
vars_a_eliminar <- missing_percentage$Variable
# Base limpia (sin variables con >30% NA)
dataPH2024 <- dataPH2024 %>%
  select(-all_of(vars_a_eliminar))


missing_percentage <- dataPH2024 %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Porcentaje_Perdido") %>%
  filter(Porcentaje_Perdido > 0) 
print(missing_percentage, n = Inf)
# NO sirven: p09, p40 y p41 (hay otras completas de lo mismo)
# NO sirven: p47b, p48, p49, (por endogeneidad, usadas para construir la variable dependiente)
# talvez sirvan: ingreso, pobreza para probar en otros modelos
# p10b, año aprobado # p27 deseo de trabajar mas horas


# Analisis de p10b año aprobado -------------------------------
 
dataPH2024 %>% 
  count(p10b) %>% 
  mutate(pct = n / sum(n) * 100)

# cruze de verificaicon años aprobados y nivel de instruccion
dataPH2024 %>%
  count(p10a, p10b) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print(n = Inf)

dataPH2024 %>%
  count(p61b1,secemp) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print(n = Inf)

# quienes son los NA? son personas mayores sin educacion
obs_p10b_NA <- dataPH2024 %>%
  filter(is.na(p10b))
obs_p10b_NA %>%
  select(
    id_persona,
    p03,        # edad
    p02,        # sexo
    dominio,    # área urbano / rural
    p45,        # años de experiencia laboral
    ingrl,      # ingreso laboral
    rama1,      # rama de actividad
    grupo1,     # grupo de ocupación
    p10a,       # nivel de instrucción
    p10b        # año aprobado (el 0)
  )
#View(obs_p10b_NA)
#glimpse(obs_p10b_NA)

# quien es ese unico cero en p10b 
obs_p10b_0 <- dataPH2024 %>%
  filter(p10b == 0) %>%
  transmute(
    id_persona,
    edad                = p03,
    sexo                = p02,
    area                = dominio,
    experiencia_laboral = p45,
    ingreso_laboral     = ingrl,
    rama_actividad      = rama1,
    grupo_ocupacion     = grupo1,
    nivel_instr         = p10a,
    anio_aprobado       = p10b
  )
#View(obs_p10b_0)
#glimpse(obs_p10b_0)
# imputacion de p10b cuando es cero, unico caso, se hizo por gripo comparable a esa obs

# mediana de años aprobados en educación básica
mediana_p10b_basica <- dataPH2024 %>%
  filter(p10a == 5, !is.na(p10b), p10b > 0) %>%
  summarise(mediana = median(p10b)) %>%
  pull(mediana)
mediana_p10b_basica # 9 años aprobados es la mediana

# Imputacion de caso unico de error 
dataPH2024 <- dataPH2024 %>%
  mutate(
    p10b = if_else(
      p10a == 5 & p10b == 0,
      mediana_p10b_basica,
      p10b
    )
  ) 
# verificaicon con tabla de frecuencaia, aumnto uno en categoria 9 
dataPH2024 %>%
  count(p10b) %>%
  mutate(pct = n / sum(n) * 100) 

# p10b=NA, NO SE IMPUTA, se recodifica
# Si la persona nunca tuvo instrucción, no puede tener años aprobados.
# entonces NA no es dato perdido sino un valor estructural
# asignamos los NA al valor de uno (sin educacion)

dataPH2024 <- dataPH2024 %>%
mutate(
  p10b = ifelse(p10a == 1 & (is.na(p10b)), 1, p10b)
)
dataPH2024 %>%
  count(p10b) %>%
  mutate(pct = n / sum(n) * 100) # P10B limpia y exitosa

# variable de escolaridad  ------------------------------------------------
dataPH2024 %>% count(p10a) %>% 
  mutate(pct = n / sum(n) * 100)

dataPH2024 <- dataPH2024 %>%
  mutate(
    nivel_instr = p10a,   # Nivel de instrucción
    aprobado = p10b,      # Año aprobado
    esc = case_when(
      nivel_instr == 1 ~ 0,                        # Ninguno
      nivel_instr %in% c(2, 3, 4) ~ aprobado,      # Alfabetización, jardín, primaria
      nivel_instr %in% c(5, 6, 7) ~ 7 + aprobado,  # Básica, secundaria, media
      nivel_instr %in% c(8, 9) ~ 13 + aprobado,    # Superior no universitario / universitario
      nivel_instr == 10 ~ 18 + aprobado,           # Postgrado
      TRUE ~ NA_real_                              # Otros casos
    )
  )
dataPH2024 %>% count(esc) %>% 
  mutate(pct = n / sum(n) * 100)
#class(dataPH2024$esc) # hemos configurado como numerica a la escolaridad

# analisis de p27 deseo de trabajar mas  -------------------------
# considerando que es una variable subjetiva, no fácilmente deducible
dataPH2024 %>%
  count(p27) %>%
  mutate(pct = n / sum(n) * 100) # 
# No se imputa por el momento
# se opta por crear una nueva categoria para los valores faltantes
dataPH2024 <- dataPH2024 %>%
  mutate(
    p27_clean = case_when(
      is.na(p27) ~ "faltante",   # ocupados con NA
      TRUE ~ as.character(p27)   # respuestas válidas 1–4
    )
  )
dataPH2024 <- dataPH2024 %>%
  mutate(
    p27_clean = case_when(
      p27 == 1 ~ "mas_horas_actual",
      p27 == 2 ~ "mas_horas_otro",
      p27 == 3 ~ "cambiar_trabajo",
      p27 == 4 ~ "no_desea",
      is.na(p27) ~ "faltante"
    )
  )
dataPH2024 %>%
  count(p27_clean) %>%
  mutate(pct = n / sum(n) * 100) # 

# recodificación informalidad ------------------------------------
# Re-codificar SECEMP a dummy binaria (informal = 1)///////
dataPH2024 <- dataPH2024 %>%
  mutate(
    informal = case_when(
      secemp == 2 ~ 1,  # Sector informal
      secemp == 1 ~ 0,  # Sector formal
      TRUE ~ NA_real_   # Otros (3,4 o NA)
    )
  )
dataPH2024 %>%
  count(informal) %>%
  mutate(pct = n / sum(n) * 100) # 0 es formal, 1 es informal 


# Variable tenencia de vivienda -------------------------------------------
dataPH2024 %>% count(vi14) %>% 
  mutate(pct = n / sum(n) * 100)

dataPH2024 <- dataPH2024 %>%
  mutate(
    vi14_grp = case_when(
      vi14 %in% c(3, 4) ~ "Propia",
      vi14 %in% c(1, 2) ~ "Arrendada",
      vi14 %in% c(5, 6, 7) ~ "Cedida/Otra",
      TRUE ~ NA_character_
    )
  )
dataPH2024 %>% count(vi14_grp) %>% 
  mutate(pct = n / sum(n) * 100)

# Creación Var hacinamiento de vivienda  ----------------------------------
#names(dataPH2024)
#Variable hacinamiento= num personas hogar / dormitorios/////
# todas las personas que comparten el mismo id_hogar viven en el mismo hogar.
# Al agrupar por id_hogar y contar filas te da el número de personas en ese hogar, 
dataPH2024 %>% count(id_hogar) %>% 
  mutate(pct = n / sum(n) * 100)
dataPH2024 %>% count(vi07) %>% 
  mutate(pct = n / sum(n) * 100)

dataPH2024 <- dataPH2024 %>% 
  group_by(id_hogar) %>%
  mutate(personas_hogar = n()) %>% 
  ungroup()

dataPH2024 <- dataPH2024 %>%
  mutate(
    hacinamiento = case_when(
      vi07 == 0 ~ personas_hogar,
      TRUE      ~ personas_hogar / vi07
    )
  )
dataPH2024 <- dataPH2024 %>%
  mutate(
    hacinamiento_cat = case_when(
      hacinamiento < 2.5 ~ "No hacinado",
      hacinamiento >= 2.5 ~ "Hacinamiento crítico",
      TRUE ~ NA_character_
    )
  )
dataPH2024 %>% count(hacinamiento_cat) %>% 
  mutate(pct = n / sum(n) * 100)
# tabla de frecuencia en hogares con vi07 = 0 en numero de personas
dataPH2024 %>%
  filter(vi07 == 0) %>%
  count(personas_hogar, hacinamiento_cat) %>%
  mutate(pct = n / sum(n) * 100)
names(dataPH2024)

# tratamiento variable ingresos  ------------------------------------------
# 99999 es  no informa # -1 es gastar mas de lo que gana  # hay algnas personas con ingresos altos validos
# Outliers de ingresos dataPH2024_clean ----------------------------------------------------
dataPH2024 %>%
  mutate(
    lower = quantile(ingrl, 0.25, na.rm = TRUE) - 1.5 * IQR(ingrl, na.rm = TRUE),
    upper = quantile(ingrl, 0.75, na.rm = TRUE) + 1.5 * IQR(ingrl, na.rm = TRUE),
    outlier = ingrl < lower | ingrl > upper
  ) %>%
  summarise(
    Outliers = sum(outlier, na.rm = TRUE),
    Total = sum(!is.na(ingrl)),
    Porcentaje = mean(outlier, na.rm = TRUE) * 100
  )
# ver grafico de cajas 
options(scipen = 999)
boxplot(dataPH2024$ingrl,
        main = "Boxplot de ingresos",
        ylab = "Ingreso",
        col = "lightblue")

#rm(dataPH2024_clean) #para borrar la data
dataPH2024_clean <- dataPH2024 %>%
  filter(ingrl <= 8000 | is.na(ingrl)) # salario max 8mil 

options(scipen = 999)
boxplot(dataPH2024_clean$ingrl,
        main = "Boxplot de ingresos",
        ylab = "Ingreso",
        col = "lightblue")
# Analisis de valores altos en ingresos, son correctos y no fallas 
dataPH2024_clean %>%
  filter(ingrl > 2000, informal == 1) %>%
  select(ingrl, rama1, informal) # existen informales que ganan bastante al mes 

# tratamiento para valores -1 +, creacion de ingrl_clean
dataPH2024_clean <- dataPH2024_clean %>%
  mutate(
    gasta_mas = if_else(ingrl == -1, 1, 0, missing = 0), #indicador gasta mas
    ingrl_clean = if_else(ingrl == -1, NA_real_, ingrl) # remplazar por NA los -1
  )

# VERIFICAR gasta mas coincide con -1
dataPH2024_clean %>%
  count(gasta_mas, ingrl == -1) # nota que existen NA, esos deben irr como 0,

# vamos a dejar los NA como cero en gasta_mas
dataPH2024_clean <- dataPH2024_clean %>%
  mutate(
    gasta_mas = if_else(ingrl == -1, 1, 0, missing = 0)
  )

dataPH2024_clean %>%
  count(gasta_mas) # ahora si esta correcto 
# Verificar que ingrl_clean reemplazó solo los -1 con NA
dataPH2024_clean %>%
  filter(ingrl == -1) %>%
  select(ingrl, ingrl_clean, gasta_mas) # %>% View()
# Verificar que no hay valores negativos
dataPH2024_clean %>%
  filter(ingrl_clean < 0) %>%
  count()

# Valores vacios NA Ingresos ----------------------------------------------
missing_percentage <- dataPH2024%>%
  summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Porcentaje_Perdido") %>%
  filter(Porcentaje_Perdido > 0) 
print(missing_percentage, n = Inf)

missing_percentage <- dataPH2024_clean%>%
  summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Porcentaje_Perdido") %>%
  filter(Porcentaje_Perdido > 0) 
print(missing_percentage, n = Inf)

dataPH2024_clean %>%
  summarise(
    NA_ingrl_clean = sum(is.na(ingrl_clean))
  )
dataPH2024 %>%
  summarise(
    NA_ingrl_clean = sum(is.na(ingrl))
  )
# ver que tipo de personas no registraron ingresos # son realidades diferentes que usa KNN para imputar
dataPH2024 %>%
  filter(is.na(ingrl)) %>%
  select(ingrl, p20,rama1,p03) # %>% View()

# forma eficiente de KNN para ingresos -------------------------------------------------
# 1. Preparación previa
dataPH2024_clean <- dataPH2024_clean %>%
  mutate(
    # Flag para identificar qué valores serán imputados
    flag_imputado = is.na(ingrl_clean),
    
    # Crear copia de la variable ingreso (esta será imputada)
    ingrl_clean_imp = ingrl_clean
  )

# 2. Recipe de imputación KNN
rec <- recipe(~ ., data = dataPH2024_clean) %>%
  # Imputar SOLO la copia (NO la original)
  step_impute_knn(
    ingrl_clean_imp,
    neighbors = 5,
    impute_with = imp_vars(
      p02, p03, p06, nnivins,
      p27_clean, rama1, grupo1,
      informal, gasta_mas
    )
  )
# Preparar y aplicar
rec_prep <- prep(rec)
dataPH2024_clean <- bake(rec_prep, new_data = NULL)

# 3. Verificación básica
dataPH2024_clean %>%
  summarise(
    NA_original = sum(is.na(ingrl_clean)),
    NA_imputado = sum(is.na(ingrl_clean_imp))
  )

# 4. Validación por grupos
dataPH2024_clean %>%
  group_by(gasta_mas, flag_imputado) %>%
  summarise(
    media = mean(ingrl_clean_imp, na.rm = TRUE),
    mediana = median(ingrl_clean_imp, na.rm = TRUE),
    n = n()
  )

# 5. Distribución (escala log)
# Crear etiqueta para gráfico
dataPH2024_clean <- dataPH2024_clean %>%
  mutate(tipo_dato = if_else(flag_imputado, "Imputado", "Observado"))

# Densidad en log (clave para ingresos)
ggplot(dataPH2024_clean, aes(x = log1p(ingrl_clean_imp), fill = tipo_dato)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribución de ingresos (log) - Observado vs Imputado")

# 6. Inspección puntual
# Ver casos imputados ordenados de mayor a menor
dataPH2024_clean %>%
  filter(flag_imputado) %>%
  select(ingrl_clean, ingrl_clean_imp, gasta_mas) %>%
  arrange(desc(ingrl_clean_imp)) %>%
  head(20)

# 7. Diagnóstico rápido
summary(dataPH2024_clean$ingrl_clean_imp)

options(scipen = 999)
boxplot(dataPH2024_clean$ingrl_clean_imp,
                main = "Boxplot de ingresos imputados",
                 ylab = "Ingreso",
                 col = "lightblue")
# faltantes en ingr
dataPH2024_clean %>%
  summarise(
    faltantes = sum(is.na(ingrl)),
    total = n(),
    porcentaje = mean(is.na(ingrl)) * 100
  )
# faltantes en ingrl_clean
dataPH2024_clean %>%
  summarise(
    faltantes = sum(is.na(ingrl_clean)),
    total = n(),
    porcentaje = mean(is.na(ingrl_clean)) * 100
  )
# faltantes en ingrl_clean
dataPH2024_clean %>%
  summarise(
    faltantes = sum(is.na(ingrl_clean_imp)),
    total = n(),
    porcentaje = mean(is.na(ingrl_clean_imp)) * 100
  )

# REVISION DE LO QUE SE HA IMPUTADO
ggplot(dataPH2024_clean, aes(x = log1p(ingrl_clean_imp))) +
  geom_density(fill = "blue", alpha = 0.3) +
  labs(title = "Distribución log de ingresos")
dataPH2024_clean <- dataPH2024_clean %>%
  mutate(tipo_dato = if_else(is.na(ingrl), "Imputado", "Observado"))
ggplot(dataPH2024_clean, aes(x = log1p(ingrl_clean_imp))) +
  geom_density(fill = "blue", alpha = 0.3) +
  labs(title = "Distribución log de ingresos")
ggplot(dataPH2024_clean, aes(x = ingrl_clean_imp)) +
  geom_histogram(bins = 50) +
  labs(title = "Histograma de ingresos imputados")

# atipicos de ingresos se usar logaritmo ----------------------------------------------------
dataPH2024_clean <- dataPH2024_clean %>%
  mutate(ingrl_imp_log = log1p(ingrl_clean_imp))

dataPH2024_clean %>%
  ggplot(aes(x = ingrl_imp_log)) +
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "white") +
  labs(
    title = "Histograma del ingreso imputado (log)",
    x = "Log(ingreso imputado + 1)",
    y = "Frecuencia"
  )
dataPH2024_clean %>%
  ggplot(aes(x = ingrl_imp_log)) +
  geom_density(fill = "orange", alpha = 0.4) +
  labs(
    title = "Densidad del ingreso imputado (log)",
    x = "Log(ingreso imputado + 1)",
    y = "Densidad"
  )

# tratamiento de outliers experiencia laboral -----------------------------
# 1. RENOMBRAR VARIABLE
dataPH2024_clean <- dataPH2024_clean %>% 
  rename(ExperienciaL = p45)


dataPH2024_clean %>% 
  count(ExperienciaL) %>% 
  mutate(pct = n / sum(n) * 100)

# 2. BOXPLOT INICIAL
# Vector de datos
x <- dataPH2024_clean$ExperienciaL

# Crear boxplot
boxplot(x,
        main = "Boxplot de Experiencia laboral",
        ylab = "Años de experiencia laboral",
        col = "lightblue")

# Calcular outliers y porcentaje
outliers <- boxplot.stats(x)$out
porcentaje_out <- length(outliers) / length(x) * 100

# Añadir texto dentro del gráfico (debajo del título)
mtext(paste0("Atípicos: ", round(porcentaje_out, 2), "%"),
      side = 3, line = 0.5, col = "red", cex = 0.9)

# 

# Vector de datos
y <- dataPH2024_clean$p51a

# Crear boxplot
boxplot(y,
        main = "Boxplot de horas trabajadas",
        ylab = "Horas",
        col = "lightblue")

# Calcular outliers y porcentaje
outliers <- boxplot.stats(y)$out
porcentaje_out <- length(outliers) / length(y) * 100

# Añadir texto en la parte superior del gráfico
mtext(paste0("Atípicos: ", round(porcentaje_out, 2), "%"),
      side = 3, line = 0.5, col = "red", cex = 0.9)


# Vector de datos
z <- dataPH2024_clean$esc

# Crear boxplot
boxplot(z,
        main = "Boxplot de años de escolaridad",
        ylab = "escolar",
        col = "lightblue")
outliers <- boxplot.stats(z)$out
porcentaje_out <- length(outliers) / length(z) * 100

# Añadir texto en la parte superior del gráfico
mtext(paste0("Atípicos: ", round(porcentaje_out, 2), "%"),
      side = 3, line = 0.5, col = "red", cex = 0.9)




# Vector de datos
a <- dataPH2024_clean$vi06

# Crear boxplot
boxplot(a,
        main = "Boxplot de número de cuartos",
        ylab = "ingresos",
        col = "lightblue")

# Calcular outliers y porcentaje
outliers <- boxplot.stats(a)$out
porcentaje_out <- length(outliers) / length(a) * 100

# Añadir texto en la parte superior del gráfico
mtext(paste0("Atípicos: ", round(porcentaje_out, 2), "%"),
      side = 3, line = 0.5, col = "red", cex = 0.9)






# 3. CÁLCULO DE OUTLIERS (IQR)
Q1 <- quantile(dataPH2024_clean$ExperienciaL, 0.25, na.rm = TRUE)
Q3 <- quantile(dataPH2024_clean$ExperienciaL, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

limite_superior <- Q3 + 1.5 * IQR

# 🔴 Corrección: convertir a entero
limite_superior_int <- floor(limite_superior)

# 4. PORCENTAJE DE OUTLIERS
n_outliers <- sum(dataPH2024_clean$ExperienciaL > limite_superior_int, na.rm = TRUE)
n_total <- sum(!is.na(dataPH2024_clean$ExperienciaL))

porcentaje_outliers <- (n_outliers / n_total) * 100
porcentaje_outliers

# 5. VALIDACIÓN LÓGICA
# Experiencia no puede ser mayor que la edad
inconsistencias <- dataPH2024_clean %>%
  filter(ExperienciaL > p03) %>%
  summarise(
    n_inconsistencias = n(),
    total = nrow(dataPH2024_clean),
    pct_inconsistencias = (n_inconsistencias / total) * 100
  )

inconsistencias

validacion_realista <- dataPH2024_clean %>%
  filter(ExperienciaL > (p03 - 10)) %>%
  summarise(n = n())

validacion_realista

# 6. REVISIÓN DE OUTLIERS
outliers_con_edad <- dataPH2024_clean %>%
  filter(ExperienciaL > limite_superior_int) %>%
  select(p03, ExperienciaL) %>% 
  arrange(desc(ExperienciaL))

outliers_con_edad

# 7. WINSORIZACIÓN CORREGIDA
dataPH2024_clean <- dataPH2024_clean %>%
  mutate(
    ExperienciaL_wins = ifelse(
      ExperienciaL > limite_superior_int,
      limite_superior_int,
      ExperienciaL
    )
  )

# 8. BOXPLOT FINAL
boxplot(dataPH2024_clean$ExperienciaL_wins,
        main = "Boxplot de Experiencia laboral (Winsorizada)",
        ylab = "Años de experiencia laboral",
        col = "lightgreen")

# 9. VERIFICACIÓN FINAL
# Confirmar que no hay decimales
any(dataPH2024_clean$ExperienciaL_wins %% 1 != 0, na.rm = TRUE)

# Adddnalsis descriptivo   ----------------------------------------------------
glimpse(dataPH2024)

dataPH2024 %>%
    count(area.x, area.y) %>%
    mutate(pct = n / sum(n) * 100)
dataPH2024 %>%
  count(nnivins) %>%
  mutate(pct = n / sum(n) * 100) 
# graficos de barras para variables categoricas
ggplot(dataPH2024, aes(x = p27_clean)) +
  geom_bar() +
  labs(
    title = "Distribución del deseo de trabajar más horas",
    x = "Categoría",
    y = "Frecuencia"
  ) +
  theme_minimal()


# tratamiento de la varible ciudad  ---------------------------------------
# se la va a volver categorica como deberia de serlo 
glimpse(dataPH2024_clean)
dataPH2024_clean <- dataPH2024_clean %>%
  mutate(ciudad = as.factor(ciudad.x)) # en realidad debe ser factor
dataPH2024_clean <- dataPH2024_clean %>%
  select(-ciudad.x)
dim(dataPH2024_clean)

# tratamiento variables pobreza -------------------------------------------
# podria usar imputacion con KNN para el 0.190 de perdidos 

# elimnacion de variables no relevantes para el estudio base -------------------
missing_percentage <- dataPH2024_clean%>%
  summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Porcentaje_Perdido") %>%
  filter(Porcentaje_Perdido > 0) 
print(missing_percentage, n = Inf)
# NO sirven: p09, p40 y p41 (hay otras completas de lo mismo)
# NO sirven: p47b, p48, p49, (por endogeneidad, usadas para construir la variable dependiente)
# talvez sirvan: ingreso, pobreza para probar en otros modelos
# p10b, año aprobado
# p27 deseo de trabajar mas horas

#nueba base de respaldo
base <- dataPH2024_clean %>%
  select(-c(p09, p27, p47b, p48,
            p49, ingrl, ingpc, vi14, area.y))

# pobreza se queda por si a caso
# p47b es personas en establecimiento,
# p48 es llevar contabilidadte
# p49 es llevar RUC
dataPH2024_clean %>%
    count(gasta_mas, ingrl == -1)
missing_percentage <- base%>%
  summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Porcentaje_Perdido") %>%
  filter(Porcentaje_Perdido > 0) 
print(missing_percentage, n = Inf)

# agrupamiento de categorias p46 sitio de trabajo -------------------------
base <- base %>%
  mutate(
    p46sitio_trabajo = case_when(
      p46 %in% c(1,6,7)  ~ "establecimiento",
      p46 == 2           ~ "construccion",
      p46 == 3           ~ "movil",
      p46 %in% c(4,5)    ~ "via_publica",
      p46 %in% c(8,9)    ~ "vivienda",
      p46 %in% c(10,11,12) ~ "agropecuario",
      TRUE ~ NA_character_
    )
  ) 
base %>%
  count(p46sitio_trabajo, informal) %>%
  mutate(pct = n / sum(n) * 100)

# Establcer formato correcto a categoricas como factor ---------------------------------------------
# variables categorical relevantes pasan a tipo factor
vars_factor <- c(
  # Personas # p05a (seguro no va), p10 bes anios aprobados como numero 1 al 10
  "p02", "p04", "p05b", "p06", "p07",
  "p10a","p15", "p15aa", "p20", "p27_clean","nnivins", "p75","nivel_instr",
  
  # Empleo # "p61b1" NO va, es el seguro (endogeneidad)
  "p40","p41","p42", "p46","p47a","p50",
  "condact",
  "grupo1", "rama1","p46sitio_trabajo",
  
  #mas contexto p77 recibir bono y bono de discapacidad
  "p75", "p77", "ciudad",
  
  # Contexto
  "pobreza", "epobreza", "informal",
  
  # Vivienda
  "vi01", "vi02", "vi03a", "vi03b", "vi04a", "vi04b",
  "vi05a", "vi05b", "vi07b",
  "vi08", "vi09", "vi10", "vi10a", "vi11", "vi12",
  "vi13", "vi14_grp",
  
  # Derivadas
  "hacinamiento_cat","gasta_mas", #gasta talvez no deberia ir
  
  # geograficas 
  "area.x","ciudad","prov","dominio"
)
vars_factor <- intersect(vars_factor, names(base))
base <- base %>%
  mutate(across(all_of(vars_factor), as.factor))
  
#glimpse(base)
#base %>%
 # count(nivel_instr, informal) %>%
  #mutate(pct = n / sum(n) * 100)
  
  
# variables numéricas en formato correcto baes1 nueva eliminado variables --------------------------------
glimpse(base)
dim(base)
# p01 es numero de persona en el hogar
# p04 es relacion de parentesco
# p05a y p05b opcion de seguro 1 y 2
# p07 asistir a clases, ya no relevante 
# p09 razon porque no asiste, no relevante
#p10a nivel de intruccion fue renonbrada
# cod_inf codigo informante
# p20 trabajó la semana pasada si o no, no relevante
# p61b1, a cual seguro aporta actualmente, DATA lEAKAGE conceptual
# empleo, sin variabilidad porque todos trabajan
# vi07a cuartos de negocio, baja variabilidad, poco util
# vi12 tipo de alumbrado, desbalance extremo 99%
# ExperienciaL ya no sirve porque cree ExperienciaL_wins que si esta
#p02 sexo
#p03 edad, debe ser int 
#p06 estadoCivil
#p10b anio aprobado
#p15 etnia
#p15aa donde nacio (como tipo migracion)
#p24 horas de trabajo la semana pasada, debe ser int
#p42 categ de ocupacion, DATA lEAKAGE conceptual

#p46 sitio de trabajo (desbalanceado), agrupada p46sitio_trabajo
#p47a tamaño de establecimiento, DATA lEAKAGE conceptual
#p47b num de personas que trab en establ, DaTA LEAKAGE conceptual
#p50 num de trabajos
#p51a horas de trabajo principal 
#p71a ingresos derivados del Kapital, NO util por desbalance extremo
#p72a ingresos por jubilacion si o no, No util 
#p73a ingresos por regalos donaciones, no util
#p74a ingreso del exterior si  o no, no util
#p75 recibir bono si o no, posiblemente util pero desbalanceada
#p77 recibir bono de discapacidad
#vi07a num de cuartos para negocio
#vi07b espacio exclusivo para cocinar, desbalanceada
#vi08 material con el que cocinan, desbalanceada
#vi09 tipo de servicoi higenico, balance moderado
#vi10 de donde optienen el agua, desbalanceada
#vi10a el agua es por tuberia ? algo desbalaceada
#vi11 servicio de ducha exclusivo ?, algo desbalanceada
#vi12 tipo de alumbrado, desbalance extremo
#vi13 como eliminan la basura, algo desbalanceada
# vi06, vi07, esc,nivel_instr, aprobado, ExperienciaL_wins deben ser int, no double
#hist(base$nivel_instr,
 #       main = "Gráfico de cajas para p71a",
  #      ylab = "Valores de p71a",
   #     col = "lightblue",
    #    border = "darkblue")
base %>%
  count(p10b, aprobado) %>%
  mutate(pct = n / sum(n) * 100)
base %>%
  count(base$nivel_instr) %>%
  mutate(pct = n / sum(n) * 100)

# eliminacion de las vairable no necesarias 
base1 <- base %>%
  select(-c(conglomerado.x, panelm.x, vivienda.x, hogar.x, estrato.x, fexp.x,
            secemp, upm.x, id_vivienda, id_hogar, id_persona, periodo.x, mes.x,
            conglomerado.y, panelm.y, vivienda.y, hogar.y, estrato.y,
            upm.y, periodo.y, mes.y, ciudad.y, 
            p01, p04, p05a, p05b, p07, p10a, p10b, cod_inf, ced01a,
            p20, p24, p47a, p61b1, empleo, vi12, ExperienciaL, hacinamiento, p71a, p72a, p73a,
            p74a, p75, p77, dominio, prov, flag_imputado, ingrl_clean,tipo_dato))
#glimpse(base1)
dim(base1)
#base$informal <- as.factor(dataPH2024$informal)

# analisis de Chi cuadrado X2 ---------------------------------------------
vars_cat <- base1 %>%
  select(where(is.factor)) %>%
  select(-informal) %>%
  names()

cramer_chi <- function(var, data, y = "informal") {
  
  df <- data %>%
    select(all_of(c(var, y))) %>%
    filter(!is.na(.data[[var]]), !is.na(.data[[y]]))
  
  tab <- table(df[[var]], df[[y]])
  
  if (nrow(tab) < 2 | ncol(tab) < 2) return(NULL)
  
  chi <- suppressWarnings(chisq.test(tab))
  
  n <- sum(tab)
  r <- nrow(tab)
  c <- ncol(tab)
  
  v <- sqrt(chi$statistic / (n * (min(r - 1, c - 1))))
  
  tibble(
    variable  = var,
    cramers_v = round(as.numeric(v), 5),          # 5 decimales
    chi_sq    = round(as.numeric(chi$statistic), 5), # χ² con 5 decimales
    p_value   = sprintf("%.5f", chi$p.value)            # p‑valor con 5 cifras significativas
  )
  
}
cat_results <- map_dfr(vars_cat, cramer_chi, data = base1) %>%
  arrange(desc(cramers_v))
print(cat_results, n = Inf)

# variables que dejamos para trabajar, pero primero ver su colinealidad
#grupo1, rama1, nnivins, vi04a (material piso), pobreza(pobreza),
#vi09 (tipo servicio higenico), area.x, p27_clean(deseo de trabajar mas)
#vi04b (estado del piso), vi05b(estado paredes), vi10a(tipo de agua que recibe)
#vi01 (via de acceso a vivienda), vi03b(estado del techo), vi10 (de donde obtiene el agua),
#prov(provincia), vi02(tipo de vivienda),vi13(como elimin basura), vi03a(material del techo),
#p15(etnia), vi05a(material de paredes), vi11(servicio de ducha privado?)
#epobreza (pobrezaExt), vi08(material con que cocinan), p06(estado civil), vi14_grp(forma tenencia vivienda),
#hacinamiento_cat (haciniamieto_cat), gasta_mas(gasta_mas), p50(num de trabajos)


base1 %>%
  count(p41) %>%  # p40 y 41 son altas son su alto numero de categorias
  mutate(porcentaje = round(n / sum(n) * 100, 2))


# colinealidad entre variables categoricas  -------------------------------
library(vcd)

base1 <- base1 %>% # eliminamos las de cramer menor a .10 y superior a .5 expecto rama (por evidencia empirica)
  select(-c(p46sitio_trabajo, hacinamiento_cat, vi07b, p50, p15aa ))

#glimpse(base1) 

# 2. Seleccionar todas las variables explicativas categóricas (factores)
vars_expl <- base1 %>%
  select(where(is.factor))

# 3. Función para calcular Cramer’s V entre dos variables
cramer_pair <- function(x, y, data){
  tab <- table(data[[x]], data[[y]])
  cv  <- assocstats(tab)$cramer
  tibble(var1 = x, var2 = y, cramers_v = cv)
}

# 4. Generar todas las combinaciones de pares de variables categóricas
pairs <- combn(names(vars_expl), 2, simplify = FALSE)

# 5. Calcular Cramer’s V para cada par
colin_results <- map_dfr(pairs, ~cramer_pair(.x[1], .x[2], vars_expl)) %>%
  arrange(desc(cramers_v))

# 6. Revisar resultados
print(colin_results, n = Inf)

# 7. Clasificación metodológica
colin_results %>%
  mutate(decision = case_when(
    cramers_v > 0.50 ~ "Posible colinealidad (considerar excluir)",
    cramers_v >= 0.30 & cramers_v <= 0.50 ~ "Moderada asociación (revisar teoría)",
    cramers_v < 0.30 ~ "Aceptable (mantener)"
  )) %>% print(n=Inf)



base1 <- base1 %>% # elimamos variables con colinealidad muy alta
  select(-c(nivel_instr, ciudad))
base1 %>%
  count(p46) %>% 
  mutate(porcentaje = round(n / sum(n) * 100, 2))
base1 %>%
  count(p42) %>% 
  mutate(porcentaje = round(n / sum(n) * 100, 2))
base1 %>%
  count(grupo1) %>% 
  mutate(porcentaje = round(n / sum(n) * 100, 2))

#glimpse(base1)

####### relacion de las numericas con informal base1 ---------------------------------

base1 <- base1 %>%
  mutate(aprobado = as.integer(aprobado))
glimpse(base1)

# 1. SPLIT TRAIN / TEST (CRÍTICO)
set.seed(123)
train_index <- sample(seq_len(nrow(base1)), size = 0.7 * nrow(base1))

train_data <- base1[train_index, ]
test_data  <- base1[-train_index, ]

# 2. SELECCIÓN DE VARIABLES NUMÉRICAS
vars_num <- train_data %>%
  select(-informal) %>%        # primero eliminas target
  select(where(is.numeric)) %>% 
  names()

# 3. FUNCIÓN AUC UNIVARIADO
auc_univar <- function(var, data, y = "informal") {
  
  df <- data %>%
    select(all_of(c(var, y))) %>%
    filter(!is.na(.data[[var]]), !is.na(.data[[y]]))
  
  # evitar variables constantes o sin variación
  if(length(unique(df[[var]])) < 2) return(NULL)
  
  # calcular ROC
  roc_obj <- pROC::roc(df[[y]], df[[var]], quiet = TRUE)
  
  tibble(
    variable = var,
    auc = round(as.numeric(pROC::auc(roc_obj)), 5)
  )
}

# 4. CALCULAR AUC PARA TODAS
auc_results <- map_dfr(vars_num, auc_univar, data = train_data) %>%
  arrange(desc(auc))

print(auc_results, n = Inf)

# 5. FILTRO INICIAL (AJUSTABLE)
vars_num_selected <- auc_results %>%
  filter(auc >= 0.4) %>%
  pull(variable)

# 6. MATRIZ DE CORRELACIÓN
data_num_selected <- train_data %>%
  select(all_of(vars_num_selected))

cor_matrix <- cor(data_num_selected, use = "complete.obs")

print(cor_matrix)

# 7. DETECCIÓN DE ALTA CORRELACIÓN
high_corr_pairs <- which(abs(cor_matrix) > 0.6 & abs(cor_matrix) < 1, arr.ind = TRUE)

high_corr_df <- data.frame(
  var1 = rownames(cor_matrix)[high_corr_pairs[,1]],
  var2 = colnames(cor_matrix)[high_corr_pairs[,2]],
  corr = cor_matrix[high_corr_pairs]
) %>%
  distinct()
print(high_corr_df)

#### CORRELACIONES ALTAS PARA 
#p24(hrs trabajo semana ante) y p51a (hrs trab princial), de 0.89
#vi07 (num dormitorios) vi06 (numero cuartos) 0.8922205
#experiencia laboral 60% de correlacion con edad, pero se deja por la evidencia empirica
# 


# 8. (OPCIONAL) ELIMINACIÓN AUTOMÁTICA
vars_to_remove <- c()
for(i in 1:nrow(high_corr_df)) {
  
  v1 <- high_corr_df$var1[i]
  v2 <- high_corr_df$var2[i]
  
  auc1 <- auc_results %>% filter(variable == v1) %>% pull(auc)
  auc2 <- auc_results %>% filter(variable == v2) %>% pull(auc)
  
  if(length(auc1) == 0 | length(auc2) == 0) next
  
  # eliminar la de menor AUC
  if(auc1 > auc2) {
    vars_to_remove <- c(vars_to_remove, v2)
  } else {
    vars_to_remove <- c(vars_to_remove, v1)
  }
}
vars_num_final <- setdiff(vars_num_selected, unique(vars_to_remove))
print(vars_num_final) # nose porque sale vi07a, si esa es factor

# nos quedamos con estas vairables
#ingrl_imp_log, esc, p51a(hrs trabajo principal), vi06 (num cuartos),
#vi07 (num dormitorios), experienciaL_wins, p03 (edad), personas hogar

#aprobado no porque se uso para crear esc, personas hogar
#persona hogar y vi07 en caso de que ayude mas que hacinamiento
  


base1 <- base1 %>% # se van correlacionadas y vi07b por estar debajo del umbral 0.5 de auc
  select(-c(vi07, aprobado, vi07a))



missing_percentage <- base1%>%
  summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Porcentaje_Perdido") %>%
  filter(Porcentaje_Perdido > 0) 
print(missing_percentage, n = Inf)
glimpse(base1)
names(base1)
# creacion de base final BASE ---------------------------------------------

# Variables categóricas relevantes (ajustadas a base1)
vars_cat_final <- c(
  "rama1", "nnivins", "vi04a", "pobreza",
  "vi09", "area.x", "p27_clean", "vi04b", "vi05b",
  "vi10a", "vi01", "vi03b", "vi10", "vi02",
  "vi13", "vi03a", "p15", "vi05a", "vi11", "epobreza",
  "vi08", "p06", "vi14_grp", "gasta_mas", "grupo1","p02","condact"
)

# Variables numéricas relevantes (ajustadas a base1)
vars_num_final <- c(
  "ingrl_clean_imp", "ingrl_imp_log", "esc", "p51a",
  "vi06", "ExperienciaL_wins", "p03","fexp.y"
)

BASE <- base1 %>%
  select(all_of(c("informal", vars_cat_final, vars_num_final)))

glimpse(BASE)

#formato correto int en las numericas que esta como dbl 
vars_int <- c(
  "esc", "p51a", "vi06", 
  "ExperienciaL_wins", "p03")

# evrificacion 
sapply(BASE[vars_int], function(x) any(x %% 1 != 0, na.rm = TRUE))

# conversion segura a intero
BASE <- BASE %>%
  mutate(across(all_of(vars_int), as.integer))

glimpse(BASE)


# grafico de caja por posibles outliers

vars_num_wins <- c(
  "esc", "p51a", "vi06",
  "p03", "ingrl_clean_imp", )


BASE_long <- BASE %>%
  select(all_of(vars_num_wins)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "valor")

#. Boxplots en lote (formato tidy)
# Calcular outliers y posición de anotación por variable
outliers_info <- BASE_long %>%
  group_by(variable) %>%
  summarise(
    Q1 = quantile(valor, 0.25, na.rm = TRUE),
    Q3 = quantile(valor, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    .groups = "drop"
  ) %>%
  left_join(BASE_long, by = "variable") %>%
  mutate(
    outlier = valor < (Q1 - 1.5 * IQR) | valor > (Q3 + 1.5 * IQR)
  ) %>%
  group_by(variable) %>%
  summarise(
    n_outliers = sum(outlier, na.rm = TRUE),
    pct_outliers = round(100 * sum(outlier, na.rm = TRUE) / n(), 1),
    y_pos = max(valor, na.rm = TRUE) * 1.05,   # posición un poco arriba del máximo
    .groups = "drop"
  )

# Gráfico con etiquetas ajustadas
ggplot(BASE_long, aes(x = variable, y = valor)) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots de variables numéricas con outliers",
       x = "", y = "Valor") +
  theme(axis.text.x = element_blank()) +
  geom_text(
    data = outliers_info,
    aes(x = variable, y = y_pos,
        label = paste0("Outliers: ", n_outliers, " (", pct_outliers, "%)")),
    inherit.aes = FALSE,
    color = "red"
  )


# Histograma en lote (formato tidy)
ggplot(BASE_long, aes(x = valor)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histogramas de variables numéricas",
       x = "Valor",
       y = "Frecuencia")

# Revisar si hay valores 99 en las variables de interés
BASE %>%
  select(p51a, vi06) %>%
  summarise(across(everything(),
                   ~ sum(. == 99, na.rm = TRUE)))



# correcion atipicos p51a y vi06 --------------------------------------------


# Función para winsorizar variables enteras
winsorizar_int <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower <- ceiling(Q1 - 1.5 * IQR_val)
  upper <- floor(Q3 + 1.5 * IQR_val)
  
  x_wins <- case_when(
    x < lower ~ lower,
    x > upper ~ upper,
    TRUE ~ x
  )
  
  as.integer(x_wins)
}

# Aplicar winsorización con tidyverse
BASE <- BASE %>%
  mutate(
    p51a = winsorizar_int(p51a),   # horas trabajadas
    vi06 = winsorizar_int(vi06)    # número de cuartos
  )

# Verificación: confirmar que son enteros
BASE %>%
  select(p51a, vi06) %>%
  summarise(across(everything(),
                   ~ any(. %% 1 != 0, na.rm = TRUE)))


# revision de las categorias de las variables factor  ---------------------------------------------

# Seleccionar solo las variables factor
vars_factor <- BASE %>%
  select(where(is.factor))

# Crear tablas de frecuencia por variable
freq_tables <- vars_factor %>%
  map(~ as.data.frame(table(.)))   # lista de dataframes

# Nombrar cada tabla con el nombre de la variable
names(freq_tables) <- names(vars_factor)

# Ejemplo: ver la tabla de frecuencias de la primera variable
freq_tables[[1]]

# Si quieres imprimir todas las tablas en orden:
for (var in names(freq_tables)) {
  cat("\nFrecuencias de:", var, "\n")
  print(freq_tables[[var]])
}

# Tablas de frecuencia con porcentajes para todas las variables factor
BASE %>%
  select(where(is.factor)) %>%
  map(~ janitor::tabyl(.) %>% janitor::adorn_pct_formatting(digits = 1))


# Conteo de NAs en variables factor
BASE %>%
  select(where(is.factor)) %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Porcentaje de NAs en variables factor
BASE %>%
  select(where(is.factor)) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>% print(n=Inf)




# renombrando las variables BASE1 ----------------------------------------------

#glimpse(BASE1)

BASE1 <- BASE %>%
  rename(
    # Variables categóricas
    mat_piso     = vi04a,
    est_piso     = vi04b,
    mat_pared    = vi05a,
    est_pared    = vi05b,
    mat_techo    = vi03a,
    est_techo    = vi03b,
    mat_cocina   = vi08,
    
    serv_sanit   = vi09,
    serv_ducha   = vi11,
    agua_fuente  = vi10,
    agua_tipo    = vi10a,
    basura_disp  = vi13,
    via_acceso   = vi01,
    tipo_viv     = vi02,
    tenencia_viv = vi14_grp,
    
    pobreza      = pobreza,
    pobreza_ext  = epobreza,
    etnia        = p15,
    estado_civil = p06,
    deseo_trab_mas = p27_clean,
    gasta_mas    = gasta_mas,
    
    # Variables numéricas
    hrs_trab       = p51a,
    num_cuartos    = vi06,
    edad           = p03,
    
    Área           = area.x,
    ingrl_log      = ingrl_imp_log,
    niv_instr      = nnivins,
    exp_lab        = ExperienciaL_wins,
    género         = p02
  )


#glimpse(BASE1)


dataPH2024_clean %>%
  count(vi14) %>%
  mutate(pct = n / sum(n) * 100) 


# codificacion correcta de genero p02 -------------------------------------

BASE1 %>%
  count(género) %>%
  mutate(pct = n / sum(n) * 100) 


# AGRUAPCION DE ETnia -----------------------------------------------------


BASE1 <- BASE1 %>%
  mutate(etnia = fct_collapse(as.character(etnia),
                              "Mestizo"         = "6",                    # Grupo mayoritario (85.7%)
                              "Indigena"        = "1",                    # Grupo con dinámicas estructurales propias (8.6%)
                              "Afroecuatoriano" = c("2", "3", "4"),       # Afro, Negro y Mulato (aprox. 3.6%)
                              "Otros_Minorias"  = c("5", "7", "8")        # Montubio, Blanco y Otros (aprox. 2.1%)
  )) %>%
  # Establecer 'Mestizo' como la categoría de referencia para el modelo
  mutate(etnia = fct_relevel(etnia, "Mestizo"))

# Verificación de la nueva estructura
BASE1 %>%
  count(etnia, informal, ingrl_clean_imp) %>%
  mutate(pct = n / sum(n) * 100) #1 indi, 2 blanc, 3 mesti, 4 afro


# agrupacion de estado civil  ---------------------------------------------
BASE1 <- BASE1 %>%
  mutate(
    estado_civil = case_when(
      estado_civil %in% c(1, 5) ~ "Pareja_Estable",     # Casado o unión libre
      estado_civil == 6         ~ "Soltero",            # Nunca casado
      estado_civil %in% c(2, 3, 4) ~ "Union_Disuelta",  # Separado, divorciado, viudo
      TRUE ~ "Otros"                                  
    )
  ) %>%
  mutate(estado_civil = as.factor(estado_civil))

BASE1 %>%
  count(estado_civil) %>%
  mutate(pct = n / sum(n) * 100)


# agrupacion material pis o -----------------------------------------------

BASE1 <- BASE1 %>%
  mutate(mat_piso = case_when(
    mat_piso %in% c(1, 3)    ~ "Alta_Calidad",
    mat_piso == 2            ~ "Estandar",
    mat_piso == 4            ~ "Basico_Cemento",
    mat_piso %in% c(5, 6, 7, 8) ~ "Precario_Inadecuado",
    TRUE ~ "Otros"
  )) %>%
  mutate(mat_piso = factor(mat_piso, 
                               levels = c("Estandar", "Alta_Calidad", "Basico_Cemento", "Precario_Inadecuado")))

BASE1 %>%
  count(mat_piso) %>%
  mutate(pct = n / sum(n) * 100)


# agrupacion mateiral techo -----------------------------------------------

BASE1 <- BASE1 %>%
  mutate(mat_techo = case_when(
    mat_techo == 1            ~ "Hormigon_Losa",
    mat_techo == 3            ~ "Zinc_Aluminio",
    mat_techo %in% c(2, 4)    ~ "Fibrocemento_Teja",
    mat_techo %in% c(5, 6)    ~ "Precario_Inadecuado",
    TRUE ~ "Otros"
  )) %>%
  mutate(mat_techo = factor(mat_techo, 
                                levels = c("Zinc_Aluminio", "Hormigon_Losa", "Fibrocemento_Teja", "Precario_Inadecuado")))


BASE1 %>%
  count(mat_techo) %>%
  mutate(pct = n / sum(n) * 100)



# agrupacion material paredes  --------------------------------------------

BASE1 <- BASE1 %>%
  mutate(mat_pared = case_when(
    mat_pared == 1            ~ "Hormigon_Ladrillo",
    mat_pared %in% c(3, 4)    ~ "Tradicional_Madera",
    mat_pared %in% c(2, 5, 6, 7) ~ "Precario_Ligero",
    TRUE ~ "Otros"
  )) %>%
  mutate(mat_pared = factor(mat_pared, 
                                levels = c("Hormigon_Ladrillo", "Tradicional_Madera", "Precario_Ligero")))

BASE1 %>%
  count(mat_pared) %>%
  mutate(pct = n / sum(n) * 100)


# rama actividad  ---------------------------------------------------------
BASE1 <- BASE1 %>%
  mutate(rama1 = case_when(
    rama1 %in% c(1, 2)                         ~ "Primario_Extractivo",
    rama1 %in% c(3, 4, 5)                      ~ "Industria_Suministros",
    rama1 == 6                                 ~ "Construccion",
    rama1 == 7                                 ~ "Comercio",
    rama1 %in% c(8, 9, 18, 19)                 ~ "Servicios_Consumo",
    rama1 %in% c(10, 11, 12, 13, 14)           ~ "Servicios_Profesionales",
    rama1 %in% c(15, 16, 17)                   ~ "Servicios_Sociales_Publicos",
    rama1 %in% c(20, 21, 22)                   ~ "Otros_Marginales",
    TRUE ~ "Otros"
  )) %>%
  mutate(rama1 = factor(rama1, 
                           levels = c("Comercio", "Primario_Extractivo", "Industria_Suministros", 
                                      "Construccion", "Servicios_Consumo", "Servicios_Profesionales", 
                                      "Servicios_Sociales_Publicos", "Otros_Marginales")))


BASE1 %>%
  count(rama1) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print(n = Inf)


# deseo de trabajar  ------------------------------------------------------
BASE1 <- BASE1 %>%
  mutate(deseo_trab_mas = case_when(
    deseo_trab_mas == "no_desea"         ~ "Satisfecho",
    deseo_trab_mas %in% c("mas_horas_actual", "mas_horas_otro", "cambiar_trabajo") ~ "Presion_Subempleo",
    deseo_trab_mas == "faltante"         ~ "No_Disponible",
    TRUE ~ "No_Disponible"
  )) %>%
  mutate(deseo_trab_mas = factor(deseo_trab_mas, 
                                     levels = c("Satisfecho_SinPresion", "Presion_Subempleo", "No_Disponible")))
BASE1 %>%
  count(deseo_trab_mas) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print(n = Inf)


library(forcats)
BASE1 <- BASE1 %>%
  mutate(deseo_trab_mas = fct_na_value_to_level(deseo_trab_mas, level = "Satisfecho")) %>%
  # Aprovechamos para poner a "Satisfecho" como el primer nivel (referencia para el modelo)
  mutate(deseo_trab_mas = fct_relevel(deseo_trab_mas, "Satisfecho"))
# Verificación final
BASE1 %>% count(deseo_trab_mas)



# agrupacion servicio sanit -----------------------------------------------
BASE1 <- BASE1 %>%
  mutate(serv_sanit = fct_collapse(serv_sanit,
                                       "Red_Publica"   = "1",
                                       "Pozo_Septico"  = "2",
                                       "Deficiente_Precario" = c("3", "4", "5")
  )) %>%
  # Establecer Red_Publica como referencia
  mutate(serv_sanit = fct_relevel(serv_sanit, "Red_Publica"))

# Verificación

BASE1 %>%
  count(serv_sanit) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print(n = Inf)


# agrupacion ducha -------------------------------------------------------------
BASE1 <- BASE1 %>%
  mutate(serv_ducha = fct_collapse(serv_ducha,
                                       "Privado_Exclusivo" = "1",
                                       "Deficiente_NoExclusivo" = c("2", "3")
  )) %>%
  # Establecer Privado_Exclusivo como referencia
  mutate(serv_ducha = fct_relevel(serv_ducha, "Privado_Exclusivo"))

# Verificación
BASE1 %>%
  count(serv_ducha) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print(n = Inf)


# agrupacion agua fuente -------------------------------------------------------------
BASE1 <- BASE1 %>%
  mutate(agua_fuente = fct_collapse(agua_fuente,
                                        "Red_Publica"      = "1",
                                        "Fuente_Alterna"   = c("3", "5"),
                                        "Precario_Excluido" = c("2", "4", "6", "7")
  )) %>%
  # Establecer Red_Publica como referencia
  mutate(agua_fuente = fct_relevel(agua_fuente, "Red_Publica"))

# Verificación final
BASE1 %>%
  count(agua_fuente) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print(n = Inf)


# agrupacion via_acceso ---------------------------------------------------
BASE1 <- BASE1 %>%
  mutate(via_acceso = fct_collapse(via_acceso,
                                   "Moderna_Pavimentada" = "1",
                                   "Permanente_NoPavimentada" = c("2", "3"),
                                   "Precaria_Inaccesible" = c("4", "5", "6")
  )) %>%
  # Establecer Moderna_Pavimentada como referencia para el modelo
  mutate(via_acceso = fct_relevel(via_acceso, "Moderna_Pavimentada"))

# Verificación
BASE1 %>%
  count(via_acceso) %>%
  mutate(pct = n / sum(n) * 100)


# agrupacion eliminacion basura  ------------------------------------------
BASE1 <- BASE1 %>%
  mutate(basura_disp = fct_collapse(basura_disp,
                                    "Recoleccion_Formal" = c("1", "2"),
                                    "Eliminacion_Inadecuada" = c("3", "4", "5")
  )) %>%
  # Establecer Recoleccion_Formal como nivel de referencia
  mutate(basura_disp = fct_relevel(basura_disp, "Recoleccion_Formal"))

# Verificación
BASE1 %>% count(basura_disp) %>%
  mutate(pct = n / sum(n) * 100)


# agrupacion mat_cocina ---------------------------------------------------
BASE1 <- BASE1 %>%
  mutate(mat_cocina = fct_collapse(mat_cocina,
                                   "Moderno_Convencional" = c("1", "3"),
                                   "Solido_Contaminante"    = c("2", "4")
  )) %>%
  # Establecer Moderno_Convencional como referencia para el modelo
  mutate(mat_cocina = fct_relevel(mat_cocina, "Moderno_Convencional"))

# Verificación final
BASE1 %>% count(mat_cocina) %>%
  mutate(pct = n / sum(n) * 100)



# agrupacion tipo_viv -----------------------------------------------------
BASE1 <- BASE1 %>%
  mutate(tipo_viv = fct_collapse(tipo_viv,
                                 "Consolidada_Independiente" = c("1", "2"),
                                 "Marginal_Subestandar"      = c("3", "4"),
                                 "Precaria_Vulnerable"       = c("5", "6", "7")
  )) %>%
  # Establecer Consolidada_Independiente como nivel de referencia para el modelo
  mutate(tipo_viv = fct_relevel(tipo_viv, "Consolidada_Independiente"))

# Verificación
BASE1 %>% count(tipo_viv) %>%
  mutate(pct = n / sum(n) * 100)


# agrupacion tenencia_viv -------------------------------------------------
BASE1 <- BASE1 %>%
  mutate(tenencia_viv = fct_collapse(as.character(vi14),
                                     "Propia"           = c("3", "4"),
                                     "Arrendada"        = c("1", "2"),
                                     "Cedida_Prestada"  = c("5", "6", "7")
  )) %>%
  # Establecer 'Propia' como categoría de referencia
  mutate(tenencia_viv = fct_relevel(tenencia_viv, "Propia"))

# Verificación final
BASE1 %>% count(tenencia_viv) %>% mutate(pct = n / sum(n) * 100)

#ya fue agrupada antes
BASE1 %>% count(tenencia_viv) %>%
  mutate(pct = n / sum(n) * 100)

# revision de gasta mas  --------------------------------------------------
BASE1 <- BASE1 %>%
  mutate(
    gasta_mas = factor(
      gasta_mas,
      levels = c(1, 0),                      # orden: primero 1, luego 0
      labels = c("Gasta_más", "No_gasta_más") # etiquetas descriptivas
    )
  )
BASE1 %>%
  count(gasta_mas) %>%
  mutate(pct = n / sum(n) * 100)
BASE1 %>% count(gasta_mas) %>%
  mutate(pct = n / sum(n) * 100)


#BASE1 <- BASE1 %>%
#  rename("Área" = area_X1)
names(BASE1)

# tablas de frecuencia normales  ------------------------------------------
library(gtsummary)
library(dplyr)

# Asegúrate de que tab_data esté bien cargado
tab_data <- BASE1 %>%
  mutate(num_cuartos = as.numeric(num_cuartos)) %>%
  select(
    informal, esc, exp_lab, edad, etnia, estado_civil, Área,
    rama1, ingrl_clean_imp, hrs_trab, deseo_trab_mas,
    mat_piso, mat_techo, mat_pared, serv_sanit, serv_ducha, 
    agua_fuente, via_acceso, basura_disp, mat_cocina, 
    tipo_viv, tenencia_viv, num_cuartos, gasta_mas
  ) %>%
  mutate(informal = factor(informal, levels = c(0, 1), labels = c("Sector Formal", "Sector Informal")))

# Generar la tabla de nuevo
tabla_final <- tab_data %>%
  tbl_summary(
    by = informal,
    missing = "no",
    type = list(num_cuartos ~ "continuous"), 
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    label = list(
      esc ~ "Escolaridad (años)", exp_lab ~ "Experiencia laboral", edad ~ "Edad",
      etnia ~ "Etnia", estado_civil ~ "Estado civil", Área ~ "Área",
      rama1 ~ "Rama de actividad", ingrl_clean_imp ~ "Ingresos laborales ($)",
      hrs_trab ~ "Horas de trabajo", deseo_trab_mas ~ "Deseo de trabajar (Presión)",
      mat_piso ~ "Material del piso", mat_techo ~ "Material del techo",
      mat_pared ~ "Material de paredes", serv_sanit ~ "Saneamiento",
      serv_ducha ~ "Servicio de higiene", agua_fuente ~ "Suministro de agua",
      via_acceso ~ "Acceso a la vivienda", basura_disp ~ "Eliminación de basura",
      mat_cocina ~ "Material de cocción", tipo_viv ~ "Tipo de vivienda",
      tenencia_viv ~ "Tenencia de vivienda", num_cuartos ~ "Número de cuartos",
      gasta_mas ~ "Gasta más"
    )
  ) %>%
  add_overall(last = FALSE, col_label = "**Total (N = {N})**") %>%
  add_p(
    # Simplifiqué esto para evitar errores de dependencias antiguas
    test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test")
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Sector de Ocupación**")

tabla_final


# cambiar directorio
#setwd("C:/Users/alexu/Desktop")
# Para exportar a Word (formato muy común en envío a revistas)
install.packages("flextable")
library(flextable)
tabla_descriptivos %>% as_flex_table() %>% flextable::save_as_docx(path = "Tabla_Descriptivos_Q1.docx")


install.packages("writexl")
library(writexl)
write_xlsx(tabla_descriptivos, path = "Tabla_Descriptivos_Q1.xlsx")

# Exportar directamente el data frame
write_xlsx(tabla_descriptivos, path = "Tabla_Descriptivos_Q1.xlsx")
getwd()



# REGRESION LOGISTICA ELASTIC NET sin p  ---------------------------------------------------------

vars_model <- c(
  #"ingrl_log",
  "rama1",              # corregido (antes rama1)

  # Variables educativas y sociodemográficas
  "etnia",                  
  "estado_civil",           
  "Área",
  "esc",                   # para comapara entre modelos
  "género",
  
  # Variables económicas y laborales
  #"gasta_mas",              
  "hrs_trab",               # 
  "exp_lab",                # 
  "edad",                  # se queda por contexto socioeconomico individual
  "deseo_trab_mas",         # 
  
  # Variables de vivienda
  "mat_piso",               
  "serv_sanit",             
  "via_acceso",             
  "agua_fuente",           #  se queda por mejor valor interpretativo socioeconomico           
  "tipo_viv",               
  "basura_disp",            
  "serv_ducha",             
  "mat_cocina",             
  "tenencia_viv",           
  "mat_techo",              # nueva
  "mat_pared",              # nueva
  
  # Variables de hogar
  "num_cuartos"             # vi06
)

BASE_MODEL <- BASE1 %>% select(all_of(c("informal", vars_model)))

glimpse(BASE_MODEL)
names(BASE_MODEL)

library(tidymodels)
set.seed(1234)

# split de datos 
split <- initial_split(BASE_MODEL, prop = 0.8, strata = informal)

train <- training(split)
test  <- testing(split)

train %>% 
  group_by(pesos) %>% count

# PROCESAMIENTO CORTO 
enet_rct <- recipe(informal ~ ., data = train) %>%
  
  # agrupar categorías raras (evita sobreajuste)
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  
  # one-hot encoding (OBLIGATORIO para glmnet)
  step_dummy(all_nominal_predictors()) %>%
  
  # normalización (IMPORTANTE para regularización)
  step_normalize(all_numeric_predictors()) %>%
  
  # eliminar variables sin varianza
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors())

enet_rct

#enet_rct %>% prep( train) %>% bake( new_data = NULL) # %>% View

# especificacion del modelo 

## Regresion logística con regularización
enet_sp <- logistic_reg(
  penalty = tune(),
  mixture = tune()   # Hiperparametros
) %>%     # Tipo de Modelo
  set_engine("glmnet", family = "binomial") %>% # Engine
  set_mode("classification") # Tipo de Problema

enet_sp
#enet_sp %>% translate()

# WORKFLOW  con pesos 
enet_wf <- 
  workflow() %>%
  add_recipe(enet_rct) %>% # Se agrega la receta sin preparar
  add_model(enet_sp) %>% # Especificación del modelo
  add_case_weights(pesos) # AQUI SE INCLUYEN LOS PESOS DEL INEC 
#enet_wf


# Entrenamiento y ajuste de Hiperparámetros
## .. Remuestreo
set.seed(1234)
cv <- vfold_cv(train, v = 5, strata = informal)
#cv
## .. Métricas 
metricas <- metric_set(
  yardstick::roc_auc, 
  yardstick::accuracy, 
  yardstick::sens, 
  yardstick::spec, 
  yardstick::bal_accuracy, 
  yardstick::f_meas
)
#metricas <- metric_set(roc_auc, accuracy, sens, spec, bal_accuracy,f_meas)
#metricas

## .. Paralelizacion
library(doParallel)
cl <- parallel::makePSOCKcluster(4)
doParallel::registerDoParallel(cl)
# parallel::stopCluster(cl) ## Esto se debe ejecutar al final 

## .. Afinamiento de hiperparametros

## ... Malla de Busqueda

enet_grid <- grid_regular(
  penalty(range = c(-4, 1)),
  mixture(range = c(0, 1)),
  levels = 10
)


## ... Entrenamiento de MALLA DE BuSQUEDA en la Crossvalidation
set.seed(123)
enet_tuned <- tune_grid(
  enet_wf,                # Modelo
  resamples = cv,         # Crosvalidacion
  grid = enet_grid,       # Malla de Busqueda
  metrics = metricas,     # Metricas
  control = control_grid(    allow_par = TRUE,    save_pred = TRUE)
)

# MEJORES RESUULTADOS 
#show_best(enet_tuned, metric = "roc_auc", n = 5)
#show_best(enet_tuned, metric = "bal_accuracy", n = 5)


# MODELO FINAL 
## Definir la mejor combinacion
enet_pars_fin <- select_best(enet_tuned, metric = "roc_auc")
## Finalizar (darle valores a parametros tuneables) el workflow
enet_wf_fin <- 
  finalize_workflow(enet_wf, enet_pars_fin)
## Entrenar el modelo final
enet_fitted <- fit(enet_wf_fin, train)

#enet_fitted


# TRAIN+
train %>%
  predict(enet_fitted, new_data = .) %>%
  bind_cols(train %>% select(informal)) %>%
  conf_mat(truth = informal, estimate = .pred_class) %>%
  summary()

# TEST
test %>%
  predict(enet_fitted, new_data = .) %>%
  bind_cols(test %>% select(informal)) %>%
  conf_mat(truth = informal, estimate = .pred_class) %>%
  summary()

# IMPORTANCIA DE VARIABLES 
library(vip)
enet_model_fin <- extract_fit_parsnip(enet_fitted)
enet_model_fin %>%
  vip(geom = "point", num_features = 27)


####
BASE1 %>%
   count(informal) %>%
   mutate(pct = n / sum(n) * 100)

# PROBABILIDADES ROC AUC (no clases)
train_pred <- predict(enet_fitted, new_data = train, type = "prob") %>%
  bind_cols(train %>% select(informal))
roc_auc(train_pred, truth = informal, .pred_1, event_level = "second")
test_pred <- predict(enet_fitted, new_data = test, type = "prob") %>%
  bind_cols(test %>% select(informal))
roc_auc(test_pred, truth = informal, .pred_1, event_level = "second")

# Analisis de la distribucion de los pesos  -------------------------------
# Construcción robusta con pesos
BASE_MODEL <- BASE1 %>%
  mutate(
    fexp.y = as.numeric(as.character(fexp.y)),   # 🔴 corrección crítica
    pesos = importance_weights(fexp.y)
  ) %>%
  filter(!is.na(pesos), fexp.y > 0) %>%          # 🔴 control básico
  select(informal, all_of(vars_model), pesos)

# analisis de la distribucion de los pesos
summary(as.numeric(BASE_MODEL$pesos))

quantile(as.numeric(BASE_MODEL$pesos),
         probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1))

library(ggplot2)
ggplot(BASE_MODEL, aes(x = as.numeric(pesos))) +
  geom_histogram(bins = 50) +
  scale_x_log10() +   # Escala log para visualizar mejor
  theme_minimal() +
  labs(title = "Distribución de pesos (escala log)",
       x = "Pesos (log10)",
       y = "Frecuencia")

BASE_MODEL %>% # quien domina el modelo
  arrange(desc(as.numeric(pesos))) %>%
  mutate(
    pesos_num   = as.numeric(pesos),
    cum_weight  = cumsum(pesos_num) / sum(pesos_num)
  ) %>%
  select(pesos_num, cum_weight) %>%
  head(20)


BASE_MODEL %>% # distribuicon por clase
  group_by(informal) %>%
  summarise(
    n             = n(),
    peso_total    = sum(as.numeric(pesos), na.rm = TRUE),
    peso_promedio = mean(as.numeric(pesos), na.rm = TRUE)
  )

# REGRESION LOGISTICA Con pesos  ----------------------------------------------------

vars_model <- c(
  #"rama1",              
  "etnia",                  
  "estado_civil",           
  "Área",
  "esc",                   
  "género",
  #"hrs_trab",               
  "exp_lab",                
  "edad",                  
  #"deseo_trab_mas",         
  "mat_piso",               
  "serv_sanit",             
  "via_acceso",             
  "agua_fuente",           
  "tipo_viv",               
  "basura_disp",            
  "serv_ducha",             
  "mat_cocina",             
  "tenencia_viv",           
  "mat_techo",              
  "mat_pared",              
  "num_cuartos"             
)

# Construcción robusta con pesos
BASE_MODEL <- BASE1 %>%
  mutate(
    fexp.y = as.numeric(as.character(fexp.y)),
    pesos = importance_weights(fexp.y)
  ) %>%
  filter(!is.na(fexp.y), fexp.y > 0) %>%
  select(informal, all_of(vars_model), pesos)

# REGRESION LOGISTICA
library(tidymodels)
set.seed(1234)

split <- initial_split(BASE_MODEL, prop = 0.8, strata = informal)
train <- training(split)
test  <- testing(split)

# 🔴 Asegurar factor
train <- train %>% mutate(informal = as.factor(informal))
test  <- test  %>% mutate(informal = as.factor(informal))

# PROCESAMIENTO CORTO 
enet_rct <- recipe(informal ~ ., data = train) %>%
  
  # ❌ NO tocar pesos aquí
  # ❌ NO update_role
  # ❌ NO step_rm
  
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors())

# MODELO
enet_sp <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%     
  set_engine("glmnet", family = "binomial") %>%
  set_mode("classification")

# WORKFLOW (✔ aquí van los pesos, no en recipe)
enet_wf <- 
  workflow() %>%
  add_recipe(enet_rct) %>%
  add_model(enet_sp) %>%
  add_case_weights(pesos)

# VALIDACIÓN
set.seed(1234)
cv <- vfold_cv(train, v = 5, strata = informal)


metricas <- metric_set(
  roc_auc,          # prob
  accuracy,         # class
  sens,             # class
  yardstick::spec,  # class (evita conflicto con readr::spec)
  bal_accuracy,     # class
  f_meas,           # class
  mcc               # class
)
# PARALLEL (⚠️ warning moderno, pero no crítico)
library(doParallel)
cl <- parallel::makePSOCKcluster(4)
registerDoParallel(cl)

# GRID
enet_grid <- grid_regular(
  penalty(range = c(-4, 1)),
  mixture(range = c(0, 1)),
  levels = 10
)

# TUNING (✔️ ya usa pesos correctamente)
set.seed(123)
enet_tuned <- tune_grid(
  enet_wf,
  resamples = cv,
  grid = enet_grid,
  metrics = metricas,
  control = control_grid(
    allow_par = TRUE,
    save_pred = TRUE
  )
)

# FINAL
enet_pars_fin <- select_best(enet_tuned, metric = "roc_auc")

enet_wf_fin <- finalize_workflow(enet_wf, enet_pars_fin)

enet_fitted <- fit(enet_wf_fin, train)

# TRAIN (ponderado)
train %>%
  predict(enet_fitted, new_data = .) %>%
  bind_cols(train %>% select(informal, pesos)) %>%
  conf_mat(truth = informal, estimate = .pred_class, case_weights = pesos) %>%
  summary()

# TEST (ponderado)
test %>%
  predict(enet_fitted, new_data = .) %>%
  bind_cols(test %>% select(informal, pesos)) %>%
  conf_mat(truth = informal, estimate = .pred_class, case_weights = pesos) %>%
  summary()

# ROC AUC ponderado
train_pred <- predict(enet_fitted, new_data = train, type = "prob") %>%
  bind_cols(train %>% select(informal, pesos))

roc_auc(train_pred, truth = informal, .pred_1,
        case_weights = pesos, event_level = "second")

test_pred <- predict(enet_fitted, new_data = test, type = "prob") %>%
  bind_cols(test %>% select(informal, pesos))

roc_auc(test_pred, truth = informal, .pred_1,
        case_weights = pesos, event_level = "second")

# IMPORTANCIA DE VARIABLES 
library(vip)
enet_model_fin <- extract_fit_parsnip(enet_fitted)

enet_model_fin %>%
  vip(geom = "point", num_features = 27)

.. tabla de importancias 
library(dplyr)
library(broom)
library(openxlsx)

# 🔴 Extraer modelo final glmnet
modelo_glmnet <- extract_fit_engine(enet_fitted)

# 🔴 Obtener mejor lambda
best_lambda <- enet_pars_fin$penalty

# 🔴 Extraer coeficientes SOLO para ese lambda
coefs <- coef(modelo_glmnet, s = best_lambda)

# 🔴 Convertir a dataframe limpio
tabla_importancia <- as.matrix(coefs) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  rename(coeficiente = s1) %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    importancia = abs(coeficiente)
  ) %>%
  arrange(desc(importancia))

# 🔴 Ver tabla
print(tabla_importancia)

# 🔴 Exportar a Excel
write.xlsx(tabla_importancia, "importancia_logit_limpia.xlsx")
#
.fL


library(dplyr)
library(tibble)
library(writexl)

# 1. Extraer modelo
enet_model_fin <- extract_fit_parsnip(enet_fitted)$fit

# 2. Lambda óptimo desde tuning
best_lambda <- enet_pars_fin$penalty

# 3. Extraer coeficientes (FORZANDO formato correcto)
coef_sparse <- coef(enet_model_fin, s = best_lambda)

coef_df <- data.frame(
  variable = rownames(coef_sparse),
  coeficiente = as.numeric(coef_sparse)
)

# 4. Quitar intercepto
coef_df <- coef_df %>%
  filter(variable != "(Intercept)")

# 5. Importancia
coef_df <- coef_df %>%
  mutate(
    importancia = abs(coeficiente)
  )

# 6. Normalización
coef_df <- coef_df %>%
  mutate(
    importancia_relativa = importancia / sum(importancia),
    importancia_pct = importancia_relativa * 100
  ) %>%
  arrange(desc(importancia))

# 7. Exportar
write_xlsx(coef_df, "importancia_logit_enet.xlsx")

# 8. Ver resultado
print(coef_df)


# RANDOM FOREST no pesos -----------------------------------------------------------------------
## 1. Preliminares
library(tidyverse)   # Manipulacion de dataframes y graficos
library(tidymodels)  # Ecosistema de modelamiento
library(doParallel)  # Paralelizacion
library(vip)         # Importancia de variables
library(ranger)      # Motor de Random Forest

## 2. Manejo de Objetos y Datos}
# Aseguramos factor para clasificacion
train <- train %>% mutate(informal = as.factor(informal))
test  <- test  %>% mutate(informal = as.factor(informal))
# ESTRATEGIA Q1: Submuestreo estratificado (20%) para eficiencia en Tuning
set.seed(123)
train_sub <- train %>%
  group_by(informal) %>%
  slice_sample(prop = 0.20) %>% 
  ungroup()
# Definición de validación cruzada y métricas
set.seed(123)
cv_sub <- vfold_cv(train_sub, v = 3, strata = informal)
metricas <- metric_set(roc_auc, sens, specificity, accuracy, bal_accuracy)

## 3. Paralelizacion
all_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(all_cores - 1)
doParallel::registerDoParallel(cl)
# parallel::stopCluster(cl) ## Se ejecuta al final

## 4. Preprocesamiento e Ingeniería de Variables (Recipe
rf_rct <- recipe(informal ~ ., data = train) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_nzv(all_predictors()) 
# Verificación (opcional)
# rf_rct %>% prep() %>% bake(new_data = NULL) %>% glimpse()

## 5. Especificación del Modelo
rf_sp <- 
  rand_forest(
    mtry = tune(), 
    trees = tune(), # Ahora tuneable segun el ejemplo SEE
    min_n = tune() 
  ) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

## 6. Workflow (Paso Crítico SEE
rf_wf <- 
  workflow() %>% 
  add_recipe(rf_rct) %>% 
  add_model(rf_sp)
# add_case_weights(weights_var) # Si aplicaras balanceo iria aqui

## 7. Entrenamiento y ajuste de Hiperparámetros

### 7.1. Malla de Busqueda (Grid LHS
set.seed(123)
rf_grid <- rf_sp %>%
  extract_parameter_set_dials() %>%
  update(
    # Mantenemos tus rangos solicitados
    min_n = min_n(range = c(50, 150)),
    mtry  = mtry(range = c(2, 7)),
    trees = trees(range = c(100, 300)) # Rango eficiente para tuning
  ) %>%
  grid_latin_hypercube(size = 8)

### 7.2. Ejecución del Tuning
tictoc::tic("Tuning RF")
rf_tuned <- tune_grid(
  rf_wf,            
  resamples = cv_sub,      
  grid = rf_grid,   
  metrics = metricas, 
  control = control_grid(allow_par = TRUE, parallel_over = "resamples")
)
tictoc::toc()

## 8. Modelo Final

# Definir la mejor combinación (Optimizando por AUC para Q1)
rf_pars_fin <- select_best(rf_tuned, metric = 'roc_auc')

# Finalizar el workflow (inyectar parametros)
rf_wf_fin <- 
  rf_wf %>% 
  finalize_workflow(rf_pars_fin)

# Entrenar el modelo final sobre el 100% de la data de entrenamiento
tictoc::tic("Ajuste Final Muestra Completa")
rf_fitted <- fit(rf_wf_fin, train)
tictoc::toc()

## 9. Evaluacion del Modelo

# Evaluación en TRAIN (Diagnóstico de sobreajuste)
print("Métricas en Entrenamiento:")
train %>% 
  predict(rf_fitted, new_data = . ) %>% 
  mutate(Real = train$informal) %>% 
  conf_mat(truth = Real, estimate = .pred_class ) %>% 
  summary()

# Evaluación en TEST (Capacidad de generalización)
print("Métricas en Test:")
test %>% 
  predict(rf_fitted, new_data = . ) %>% 
  mutate(Real = test$informal) %>% 
  conf_mat(truth = Real, estimate = .pred_class ) %>% 
  summary()

# ROC AUC Final en Test (Métrica Reina Q1)
test_auc <- predict(rf_fitted, test, type = "prob") %>%
  bind_cols(test %>% select(informal)) %>%
  roc_auc(truth = informal, .pred_1, event_level = "second")
print(test_auc)

train_auc <- predict(rf_fitted, train, type = "prob") %>%
  bind_cols(train %>% select(informal)) %>%
  roc_auc(truth = informal, .pred_1, event_level = "second")
print(train_auc)

# Importancia de variables
rf_fitted %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20, geom = "point") + theme_minimal()

## 10. Cierre de Procesos
parallel::stopCluster(cl)
registerDoSEQ()





# MODELO RF CON PESOS ponderado-----------------------------------------------------
# RANDOM FOREST CON PESOS (VERSIÓN CORRECTA Y ESTABLE)

library(tidymodels)
library(dplyr)
library(doParallel)
library(tictoc)
library(yardstick)
library(hardhat)
#

# 1. VARIABLES
vars_model <- c(
  "rama1","etnia","estado_civil","Área","esc","género",
  "hrs_trab","exp_lab","edad","deseo_trab_mas",
  "mat_piso","serv_sanit","via_acceso","agua_fuente",
  "tipo_viv","basura_disp","serv_ducha","mat_cocina",
  "tenencia_viv","mat_techo","mat_pared","num_cuartos"
)
# vector para pre mercado laboral 

vars_model <- c(
  #"rama1",              
  "etnia",                  
  "estado_civil",           
  "Área",
  "esc",                   
  "género",
  #"hrs_trab",               
  "exp_lab",                
  "edad",                  
  #"deseo_trab_mas",         
  "mat_piso",               
  "serv_sanit",             
  "via_acceso",             
  "agua_fuente",           
  "tipo_viv",               
  "basura_disp",            
  "serv_ducha",             
  "mat_cocina",             
  "tenencia_viv",           
  "mat_techo",              
  "mat_pared",              
  "num_cuartos"             
)



# 2. BASE
BASE_MODEL <- BASE1 %>%
  mutate(
    fexp.y = as.numeric(as.character(fexp.y))
  ) %>%
  filter(!is.na(fexp.y), fexp.y > 0) %>%
  select(informal, all_of(vars_model), fexp.y)

# 🔴 SPLIT
set.seed(1234)
split <- initial_split(BASE_MODEL, prop = 0.8, strata = informal)

train <- training(split)
test  <- testing(split)

# 🔴 RECONSTRUIR PESOS DESPUÉS DEL SPLIT (CLAVE)
train <- train %>%
  mutate(
    informal = as.factor(informal),
    pesos = importance_weights(fexp.y)
  )

test <- test %>%
  mutate(
    informal = as.factor(informal),
    pesos = importance_weights(fexp.y)
  )

# 3. CV
set.seed(123)
cv_folds <- vfold_cv(train, v = 5, strata = informal)

metricas <- metric_set(
  roc_auc, sens, yardstick::spec, accuracy, bal_accuracy
)

# 4. PARALEL
cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE) - 1)
registerDoParallel(cl)

# 5. RECIPE (🚫 SIN pesos)
rf_rct <- recipe(informal ~ ., data = train) %>%
  step_rm(fexp.y) %>%  # 🔴 quitar variable original
  step_other(all_nominal_predictors(), threshold = 0.05) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors())

# 6. MODELO
rf_sp <- rand_forest(
  mtry = tune(), 
  trees = tune(),
  min_n = tune()
) %>%
  set_engine(
    "ranger",
    importance = "permutation",   # 🔴 mejor interpretación
    max.depth = 10,
    sample.fraction = 0.7
  ) %>%
  set_mode("classification")

# 7. WORKFLOW (✔️ AQUÍ VAN LOS PESOS)
rf_wf_tune <- workflow() %>%
  add_recipe(rf_rct) %>%
  add_model(rf_sp) %>%
  add_case_weights(pesos)

# 8. GRID
set.seed(123)
rf_grid <- rf_sp %>%
  extract_parameter_set_dials() %>%
  update(
    min_n = min_n(range = c(20, 100)),
    mtry  = mtry(range = c(2, 20)),
    trees = trees(range = c(200, 500))
  ) %>%
  grid_latin_hypercube(size = 8)

# 9. TUNING (✔️ ESTABLE)
tictoc::tic("Tuning RF")
rf_tuned <- tune_grid(
  rf_wf_tune,            
  resamples = cv_folds,      
  grid = rf_grid,   
  metrics = metricas, 
  control = control_grid(
    allow_par = TRUE,
    parallel_over = "resamples"
  )
)
tictoc::toc()

# 10. MEJOR MODELO
rf_pars_fin <- select_best(rf_tuned, metric = "roc_auc")

rf_final_model <- finalize_model(rf_sp, rf_pars_fin)

rf_wf_fin <- workflow() %>%
  add_recipe(rf_rct) %>%
  add_model(rf_final_model) %>%
  add_case_weights(pesos)

# 11. FIT FINAL
tictoc::tic("Fit final")
rf_fitted <- fit(rf_wf_fin, train)
tictoc::toc()

# 12. IMPORTANCIA DE VARIABLES (A NIVEL DUMMY)

rf_model <- extract_fit_parsnip(rf_fitted)$fit

var_imp <- rf_model$variable.importance

var_imp_df <- data.frame(
  variable = names(var_imp),
  importance = as.numeric(var_imp)
) %>%
  arrange(desc(importance)) %>%
  mutate(
    importancia_relativa = importance / sum(importance),
    importancia_pct = 100 * importancia_relativa
  )

# ver
head(var_imp_df, 20)

# exportar
writexl::write_xlsx(var_imp_df, "importancia_variablesPre_RF.xlsx")

# 13. CIERRE

stopCluster(cl)
registerDoSEQ()

#.. tablas de importancia de vairables ...

## IMPORTANCIA RF (TABLA COMPLETA)
+
library(openxlsx)

rf_model <- extract_fit_parsnip(rf_fitted)$fit

# Extraer importancia
var_imp <- rf_model$variable.importance

var_imp_df <- data.frame(
  variable = names(var_imp),
  importance = as.numeric(var_imp)
) %>%
  arrange(desc(importance)) %>%
  mutate(
    importancia_relativa = importance / sum(importance),
    importancia_pct = importancia_relativa * 100
  )

# Ver top
head(var_imp_df, 20)

## EXPORTAR A EXCEL

write.xlsx(
  var_imp_df,
  file = "importancia_random_forestFINAL.xlsx",
  sheetName = "importancia",
  rowNames = FALSE
)

# Importancia de variables
rf_fitted %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20, geom = "point") + theme_minimal()

# evaluacion del los modelos 
## 16. Evaluación TRAIN
print("TRAIN")

train_pred <- predict(rf_fitted, train, type = "prob") %>%
  bind_cols(predict(rf_fitted, train)) %>%
  bind_cols(train %>% select(informal, pesos))

train_pred %>%
  conf_mat(truth = informal, estimate = .pred_class, case_weights = pesos) %>%
  summary()

train_auc <- train_pred %>%
  roc_auc(truth = informal, .pred_1, case_weights = pesos, event_level = "second")

print(train_auc)

## 17. Evaluación TEST
print("TEST")

test_pred <- predict(rf_fitted, test, type = "prob") %>%
  bind_cols(predict(rf_fitted, test)) %>%
  bind_cols(test %>% select(informal, pesos))

test_pred %>%
  conf_mat(truth = informal, estimate = .pred_class, case_weights = pesos) %>%
  summary()

test_auc <- test_pred %>%
  roc_auc(truth = informal, .pred_1, case_weights = pesos, event_level = "second")

print(test_auc)








# MODELO XGBOOST ----------------------------------------------------------

## 1. Preliminares
library(tidyverse)   # Manipulacion de dataframes
library(tidymodels)  # Ecosistema Tidymodels
library(doParallel)  # Paralelizacion
library(vip)         # Importancia de variables
library(xgboost)     # Motor de XGBoost

## 2. Manejo de Objetos y Datos

# Aseguramos factor para clasificación (Variable: informal)
train <- train %>% mutate(informal = as.factor(informal))
test  <- test  %>% mutate(informal = as.factor(informal))

# ESTRATEGIA Q1: Submuestreo estratificado (20%) para eficiencia en Tuning
set.seed(123)
train_sub <- train %>%
  group_by(informal) %>%
  slice_sample(prop = 0.20) %>% 
  ungroup()

# Definición de validación cruzada y métricas (Consistencia con RF)
set.seed(123)
cv_sub <- vfold_cv(train_sub, v = 3, strata = informal)

metricas <- metric_set(roc_auc, sens, specificity, accuracy, bal_accuracy)

## 3. Paralelizacion
all_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(all_cores - 1)
doParallel::registerDoParallel(cl)

## 4. Preprocesamiento e Ingeniería de Variables (Recipe
# XGBoost requiere estrictamente variables numéricas (One-Hot Encoding)
xgb_rct <- recipe(informal ~ ., data = train) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% # Requerido para XGBoost
  step_nzv(all_predictors()) 

## 5. Especificación del Modelo
# Tuneamos profundidad, tasa de aprendizaje y regularización (gamma)
xgb_sp <- 
  boost_tree(
    trees = 500,               # Fijamos 500 para comparabilidad con RF
    tree_depth = tune(),       # Profundidad del árbol
    learn_rate = tune(),       # Tasa de aprendizaje (Eta)
    loss_reduction = tune(),   # Reducción de pérdida (Gamma)
    stop_iter = 15             # Early stopping para evitar sobreajuste
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

## 6. Workflow (Paso Obligatorio SEE
xgb_wf <- 
  workflow() %>% 
  add_recipe(xgb_rct) %>% 
  add_model(xgb_sp)

## 7. Entrenamiento y ajuste de Hiperparámetros

### 7.1. Malla de Busqueda (Grid LHS
set.seed(123)
xgb_grid <- xgb_sp %>%
  extract_parameter_set_dials() %>%
  update(
    tree_depth = tree_depth(range = c(3, 10)),
    learn_rate = learn_rate(range = c(-3, -1), trans = log10_trans()),
    loss_reduction = loss_reduction(range = c(-10, 1.5), trans = log10_trans())
  ) %>%
  grid_latin_hypercube(size = 8)

### 7.2. Ejecución del Tuning (Cross-Validation
tictoc::tic("Tuning XGBoost Workflow")
xgb_tuned <- tune_grid(
  xgb_wf,            
  resamples = cv_sub,      
  grid = xgb_grid,   
  metrics = metricas, 
  control = control_grid(allow_par = TRUE, parallel_over = "resamples")
)
tictoc::toc()

## 8. Modelo Final

# Seleccionamos la mejor combinación basada en AUC-ROC
xgb_pars_fin <- select_best(xgb_tuned, metric = 'roc_auc')

# Finalizar el workflow (inyectar los mejores parámetros)
xgb_wf_fin <- 
  xgb_wf %>% 
  finalize_workflow(xgb_pars_fin)

# Entrenar el modelo final sobre el 100% de la data de entrenamiento (120k obs)
tictoc::tic("Ajuste Final XGBoost Muestra Completa")
xgb_fitted <- fit(xgb_wf_fin, train)
tictoc::toc()

## 9. Evaluacion del Modelo

# Evaluación en TRAIN (Diagnóstico de sobreajuste)
print("Métricas en Entrenamiento (XGBoost):")
train %>% 
  predict(xgb_fitted, new_data = . ) %>% 
  mutate(Real = train$informal) %>% 
  conf_mat(truth = Real, estimate = .pred_class ) %>% 
  summary()

# Evaluación en TEST (Generalización - 30k obs aprox)
print("Métricas en Test (XGBoost):")
test %>% 
  predict(xgb_fitted, new_data = . ) %>% 
  mutate(Real = test$informal) %>% 
  conf_mat(truth = Real, estimate = .pred_class ) %>% 
  summary()

# ROC AUC Final en Test
test_auc_xgb <- predict(xgb_fitted, test, type = "prob") %>%
  bind_cols(test %>% select(informal)) %>%
  roc_auc(truth = informal, .pred_1, event_level = "second")
print(paste("AUC Final XGBoost:", round(test_auc_xgb$.estimate, 4)))

train_auc_xgb <- predict(xgb_fitted, train, type = "prob") %>%
  bind_cols(train %>% select(informal)) %>%
  roc_auc(truth = informal, .pred_1, event_level = "second")
print(paste("AUC Final XGBoost:", round(train_auc_xgb$.estimate, 4)))


# Importancia de variables (VIP)
xgb_fitted %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20, geom = "point") + theme_minimal()



# XGBOOST con pesos  (PIPELINE ROBUSTO Y CONSISTENTE)------------------------------------------------------

## 1. Librerías ##
library(tidymodels)
library(dplyr)
library(doParallel)
library(tictoc)
library(yardstick)
library(xgboost)
library(vip)

## 2. Variables
vars_model <- c(
  "rama1","etnia","estado_civil","Área","esc","género",
  "hrs_trab","exp_lab","edad","deseo_trab_mas",
  "mat_piso","serv_sanit","via_acceso","agua_fuente",
  "tipo_viv","basura_disp","serv_ducha","mat_cocina",
  "tenencia_viv","mat_techo","mat_pared","num_cuartos"
)

## 3. Base
BASE_MODEL <- BASE1 %>%
  mutate(
    fexp.y = as.numeric(as.character(fexp.y)),
    pesos = importance_weights(fexp.y)
  ) %>%
  filter(!is.na(fexp.y), fexp.y > 0) %>%
  select(informal, all_of(vars_model), pesos)

## 4. Split
set.seed(1234)
split <- initial_split(BASE_MODEL, prop = 0.8, strata = informal)

train <- training(split) %>%
  mutate(informal = as.factor(informal))

test  <- testing(split) %>%
  mutate(informal = as.factor(informal))

## 5. CV (SIN SUBMUESTREO)
set.seed(123)
cv_folds <- vfold_cv(train, v = 5, strata = informal)

## 6. Paralelización
all_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(all_cores - 1)
registerDoParallel(cl)

## 7. Métricas
metricas <- metric_set(
  roc_auc, 
  sens, 
  spec, 
  accuracy, 
  bal_accuracy
)

## 8. Recipe (XGBoost necesita dummies)
xgb_rct <- recipe(informal ~ ., data = train) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors())

## 9. Modelo (más completo)
xgb_sp <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  min_n = tune(),
  sample_size = tune(),
  mtry = tune(),
  stop_iter = 20
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

## 10. Workflow con pesos
xgb_wf <- workflow() %>%
  add_recipe(xgb_rct) %>%
  add_model(xgb_sp) %>%
  add_case_weights(pesos)

## 11. Grid optimizado (no excesivo)
set.seed(123)
xgb_grid <- xgb_sp %>%
  extract_parameter_set_dials() %>%
  update(
    trees = trees(range = c(300, 800)),
    tree_depth = tree_depth(range = c(3, 8)),
    learn_rate = learn_rate(range = c(-3, -1), trans = log10_trans()),
    loss_reduction = loss_reduction(range = c(-6, 1), trans = log10_trans()),
    min_n = min_n(range = c(10, 50)),
    sample_size = sample_prop(range = c(0.6, 1)),
    mtry = mtry(range = c(5, 30))
  ) %>%
  grid_latin_hypercube(size = 10)

## 12. Tuning (consistente)
tictoc::tic("Tuning XGBoost")
xgb_tuned <- tune_grid(
  xgb_wf,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = metricas,
  control = control_grid(
    allow_par = TRUE,
    parallel_over = "everything"
  )
)
tictoc::toc()

## 13. Mejor modelo
xgb_pars_fin <- select_best(xgb_tuned, metric = "roc_auc")

## 14. Modelo final
xgb_wf_fin <- workflow() %>%
  add_recipe(xgb_rct) %>%
  add_model(finalize_model(xgb_sp, xgb_pars_fin)) %>%
  add_case_weights(pesos)

## 15. Entrenamiento final
tictoc::tic("Ajuste Final XGB")
xgb_fitted <- fit(xgb_wf_fin, train)
tictoc::toc()

## 16. Evaluación TRAIN
print("TRAIN")

train_pred <- predict(xgb_fitted, train, type = "prob") %>%
  bind_cols(predict(xgb_fitted, train)) %>%
  bind_cols(train %>% select(informal, pesos))

train_pred %>%
  conf_mat(truth = informal, estimate = .pred_class, case_weights = pesos) %>%
  summary()

train_auc <- train_pred %>%
  roc_auc(truth = informal, .pred_1, case_weights = pesos, event_level = "second")

print(train_auc)

## 17. Evaluación TEST
print("TEST")

test_pred <- predict(xgb_fitted, test, type = "prob") %>%
  bind_cols(predict(xgb_fitted, test)) %>%
  bind_cols(test %>% select(informal, pesos))

test_pred %>%
  conf_mat(truth = informal, estimate = .pred_class, case_weights = pesos) %>%
  summary()

test_auc <- test_pred %>%
  roc_auc(truth = informal, .pred_1, case_weights = pesos, event_level = "second")

print(test_auc)




## 18. Importancia (más robusta)
vip(xgb_fitted, num_features = 15) + theme_minimal()

## 19. Cierre
parallel::stopCluster(cl)
registerDoSEQ()


....GRAFICO SHAP.....
#install.packages("fastshap")
## =========================================
## SHAP NATIVO XGBOOST (EFICIENTE Y CORRECTO)
## =========================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)

## 1. Extraer booster
xgb_booster <- extract_fit_parsnip(xgb_fitted)$fit

## 2. Preparar datos (igual que el modelo)
rec_prep <- prep(xgb_rct)

train_processed <- bake(rec_prep, new_data = train)

X_train <- train_processed %>%
  select(-informal, -pesos) %>%
  as.matrix()

## 3. SHAP exacto (rápido)
shap_values <- predict(
  xgb_booster,
  X_train,
  predcontrib = TRUE
)



shap_values <- as.data.frame(shap_values)

# eliminar bias (última columna)
shap_values <- shap_values[, -ncol(shap_values)]

## 4. IMPORTANCIA GLOBAL (media |SHAP|)
shap_importance <- shap_values %>%
  summarise(across(everything(), ~ mean(abs(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "mean_abs_shap") %>%
  arrange(desc(mean_abs_shap)) %>%
  mutate(
    importancia_relativa = mean_abs_shap / sum(mean_abs_shap),
    importancia_pct = importancia_relativa * 100
  )

## 5. EXPORTAR TABLA
write.xlsx(shap_importance,
           "importancia_shap_xgboost.xlsx",
           rowNames = FALSE)

## 6. SAMPLE PARA GRÁFICO (CLAVE)
set.seed(123)
sample_idx <- sample(1:nrow(X_train), 2000)

shap_sample <- shap_values[sample_idx, ]
X_sample <- X_train[sample_idx, ]

## 7. FORMATO LARGO PARA BEESWARM
shap_long <- shap_sample %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "shap")

X_long <- as.data.frame(X_sample) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value")

plot_data <- left_join(shap_long, X_long, by = c("id", "variable"))

## 8. TOP VARIABLES (para no saturar gráfico)
top_vars <- shap_importance %>%
  slice(1:15) %>%
  pull(variable)

plot_data <- plot_data %>%
  filter(variable %in% top_vars) %>%
  mutate(variable = factor(variable, levels = rev(top_vars)))

## 9. SHAP SUMMARY PLOT
ggplot(plot_data, aes(x = shap, y = variable, color = value)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "SHAP Summary Plot - XGBoost",
    x = "Impacto en la predicción (SHAP value)",
    y = "Variables",
    color = "Valor"
  )

................................................................................
# GRÁFICO SHAP MEJORADO (COLORES ACADÉMICOS Y RENOMBRADO SEGURO)
library(dplyr)
library(ggplot2)

# 1. Renombrar y ordenar sin perder datos
# Usamos 'recode' dentro de mutate para cambiar el nombre técnico por el estético
# Manteniendo el orden que definiste en 'top_vars'
plot_data_final <- plot_data %>%
  mutate(
    variable = factor(variable, levels = rev(top_vars)),
    variable = case_when(
      variable == "rama1_Servicios_Sociales_Publicos" ~ "rama1_Servicios_Sociales_Públicos",
      variable == "hrs_trab"                         ~ "hrs_trab",
      variable == "rama1_Primario_Extractivo"        ~ "rama1_Primario_Extractivo",
      variable == "rama1_Construccion"               ~ "rama1_Construcción",
      variable == "esc"                              ~ "esc",
      variable == "etnia_Indigena"                   ~ "etnia_Indígena",
      variable == "mat_piso_Precario_Inadecuado"     ~ "mat_piso_Precario_Inadecuado",
      variable == "exp_lab"                          ~ "exp_lab",
      variable == "deseo_trab_mas_Presion_Subempleo" ~ "deseo_trab_mas_Presión_Subempleo",
      variable == "rama1_Servicios_Profesionales"    ~ "rama1_Servicios_Profesionales",
      variable == "mat_piso_Basico_Cemento"          ~ "mat_piso_Básico_Cemento",
      variable == "deseo_trab_mas_Satisfecho"        ~ "deseo_trab_mas_Satisfecho",
      variable == "edad"                             ~ "edad",
      variable == "estado_civil_Soltero"             ~ "estado_civil_Soltero",
      # Aquí corregimos el problema del Área:
      variable %in% c("Área Urbana", "Área_X1")      ~ "Área Urbana", 
      TRUE                                           ~ variable # Por si hay alguna otra
    ),
    
    # 3. Definimos el ORDEN EXACTO (De arriba hacia abajo según tu imagen izquierda)
    variable = factor(variable, levels = rev(c(
      "rama1_Servicios_Sociales_Públicos",
      "hrs_trab",
      "rama1_Primario_Extractivo",
      "rama1_Construcción",
      "esc",
      "etnia_Indígena",
      "mat_piso_Precario_Inadecuado",
      "exp_lab",
      "deseo_trab_mas_Presión_Subempleo",
      "rama1_Servicios_Profesionales",
      "mat_piso_Básico_Cemento",
      "deseo_trab_mas_Satisfecho",
      "edad",
      "estado_civil_Soltero",
      "Área Urbana"
    )))      )

# 2. Generar el gráfico con paleta profesional Q1
ggplot(plot_data_final, aes(x = shap, y = variable, color = value)) +
  # Aumentamos un poco el jitter para evitar solapamiento excesivo
  geom_jitter(height = 0.25, width = 0, alpha = 0.6, size = 1.2) +
  # Paleta de colores: Azul (Bajo) -> Gris (Medio) -> Rojo (Alto)
  scale_color_gradient2(
    low = "#2E5A88",   # Azul profesional desaturado
    mid = "#D5D8DC",   # Gris neutro para valores medios
    high = "#9B2226",  # Rojo vino / Borgoña
    midpoint = median(plot_data_final$value, na.rm = TRUE)
  ) +
  # Configuración estética y fuente Times New Roman (serif)
  theme_minimal(base_family = "serif") + 
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b=15)),
    axis.title = element_text(size = 18, face = "plain"),
    axis.text = element_text(size = 18, color = "black"),
    legend.title = element_text(size = 18),
    legend.position = "right",
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    #title = "Impacto de Variables en la Informalidad (Valores SHAP)",
    x = "Impacto en la predicción (SHAP value)",
    y = NULL, # Quitamos "Variables" para que se vea más limpio
    color = "Valor de la\nvariable"
  )


...............................................................................
# EXPORTACIÓN PROFESIONAL PARA LA REVISTA
p_shap <- ggplot(plot_data_final, aes(x = shap, y = variable, color = value)) +
  geom_jitter(height = 0.25, width = 0, alpha = 0.6, size = 1.2) +
  scale_color_gradient2(
    low = "#2E5A88",   
    mid = "#D5D8DC",   
    high = "#9B2226",  
    midpoint = median(plot_data_final$value, na.rm = TRUE)
  ) +
  # Ajustamos a 'sans' (que es Arial en la mayoría de sistemas) y tamaño 8 según directrices
  theme_minimal(base_family = "sans", base_size = 8) + 
  theme(
    plot.title = element_blank(), # Las directrices dicen que el título va en el texto del doc
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8),
    legend.position = "right",
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Impacto en la predicción (SHAP value)",
    y = NULL, 
    color = "Valor de la\nvariable"
  )

# 2. Exportar en PDF (Formato Vectorial - Recomendado por la revista)
ggsave("grafico_shap.pdf", plot = p_shap, width = 16, height = 10, units = "cm", device = cairo_pdf)

# 3. Exportar en SVG (Otro formato vectorial que piden)
# install.packages("svglite") # Si no lo tienes
ggsave("grafico_shap.svg", plot = p_shap, width = 16, height = 10, units = "cm")

# 4. Exportar en PNG de alta resolución (como respaldo a 300 dpi)
ggsave("grafico_shap.png", plot = p_shap, width = 16, height = 10, units = "cm", dpi = 300)

.............................



# tablas de importancia ---------------------------------------------------


..tabla de importancias....


# IMPORTANCIA XGBOOST + EXPORT

## 1. Extraer modelo interno
xgb_model <- extract_fit_parsnip(xgb_fitted)$fit

## 2. Importancia (GAIN)
library(xgboost)

imp_xgb <- xgb.importance(model = xgb_model)

## 3. Convertir a tabla limpia
library(dplyr)

imp_xgb_df <- imp_xgb %>%
  as_tibble() %>%
  rename(
    variable = Feature,
    gain = Gain,
    cover = Cover,
    frequency = Frequency
  ) %>%
  arrange(desc(gain)) %>%
  mutate(
    importancia_relativa = gain / sum(gain),
    importancia_pct = importancia_relativa * 100
  )

## 4. Limpieza de nombres (consistencia con otros modelos)
imp_xgb_df <- imp_xgb_df %>%
  mutate(variable = gsub("\\.", "_", variable))

## 5. Tabla completa
imp_xgb_df

## 6. Top variables (para paper)
top_xgb <- imp_xgb_df %>%
  slice_max(order_by = importancia_pct, n = 20)

top_xgb

## 7. Exportar a Excel
library(writexl)

write_xlsx(
  list(
    "completo" = imp_xgb_df,
    "top_20" = top_xgb
  ),
  path = "importancia_xgboost.xlsx"
)





# XGBOOST con INGRESOS y pesos  -------------------------------------------
names(BASE1)
# XGBOOST CON PESOS (PIPELINE ROBUSTO Y CONSISTENTE)

## 1. Librerías
library(tidymodels)
library(dplyr)
library(doParallel)
library(tictoc)
library(yardstick)
library(xgboost)
library(vip)

## 2. Variables
vars_model <- c(
  "ingrl_log","gasta_mas","rama1","etnia","estado_civil","Área","esc","género",
  "hrs_trab","exp_lab","edad","deseo_trab_mas",
  "mat_piso","serv_sanit","via_acceso","agua_fuente",
  "tipo_viv","basura_disp","serv_ducha","mat_cocina",
  "tenencia_viv","mat_techo","mat_pared","num_cuartos"
)

## 3. Base
BASE_MODEL <- BASE1 %>%
  mutate(
    fexp.y = as.numeric(as.character(fexp.y)),
    pesos = importance_weights(fexp.y)
  ) %>%
  filter(!is.na(fexp.y), fexp.y > 0) %>%
  select(informal, all_of(vars_model), pesos)

## 4. Split
set.seed(1234)
split <- initial_split(BASE_MODEL, prop = 0.8, strata = informal)

train <- training(split) %>%
  mutate(informal = as.factor(informal))

test  <- testing(split) %>%
  mutate(informal = as.factor(informal))

## 5. CV (SIN SUBMUESTREO)
set.seed(123)
cv_folds <- vfold_cv(train, v = 5, strata = informal)

## 6. Paralelización
all_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(all_cores - 1)
registerDoParallel(cl)

## 7. Métricas
metricas <- metric_set(
  roc_auc, 
  sens, 
  spec, 
  accuracy, 
  bal_accuracy
)

## 8. Recipe (XGBoost necesita dummies)
xgb_rct <- recipe(informal ~ ., data = train) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors())

## 9. Modelo (más completo)
xgb_sp <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  min_n = tune(),
  sample_size = tune(),
  mtry = tune(),
  stop_iter = 20
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

## 10. Workflow con pesos
xgb_wf <- workflow() %>%
  add_recipe(xgb_rct) %>%
  add_model(xgb_sp) %>%
  add_case_weights(pesos)

## 11. Grid optimizado (no excesivo)
set.seed(123)
xgb_grid <- xgb_sp %>%
  extract_parameter_set_dials() %>%
  update(
    trees = trees(range = c(300, 800)),
    tree_depth = tree_depth(range = c(3, 8)),
    learn_rate = learn_rate(range = c(-3, -1), trans = log10_trans()),
    loss_reduction = loss_reduction(range = c(-6, 1), trans = log10_trans()),
    min_n = min_n(range = c(10, 50)),
    sample_size = sample_prop(range = c(0.6, 1)),
    mtry = mtry(range = c(5, 30))
  ) %>%
  grid_latin_hypercube(size = 10)

## 12. Tuning (consistente)
tictoc::tic("Tuning XGBoost")
xgb_tuned <- tune_grid(
  xgb_wf,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = metricas,
  control = control_grid(
    allow_par = TRUE,
    parallel_over = "everything"
  )
)
tictoc::toc()

## 13. Mejor modelo
xgb_pars_fin <- select_best(xgb_tuned, metric = "roc_auc")

## 14. Modelo final
xgb_wf_fin <- workflow() %>%
  add_recipe(xgb_rct) %>%
  add_model(finalize_model(xgb_sp, xgb_pars_fin)) %>%
  add_case_weights(pesos)

## 15. Entrenamiento final
tictoc::tic("Ajuste Final XGB")
xgb_fitted <- fit(xgb_wf_fin, train)
tictoc::toc()

## 16. Evaluación TRAIN
print("TRAIN")

train_pred <- predict(xgb_fitted, train, type = "prob") %>%
  bind_cols(predict(xgb_fitted, train)) %>%
  bind_cols(train %>% select(informal, pesos))

train_pred %>%
  conf_mat(truth = informal, estimate = .pred_class, case_weights = pesos) %>%
  summary()

train_auc <- train_pred %>%
  roc_auc(truth = informal, .pred_1, case_weights = pesos, event_level = "second")

print(train_auc)

## 17. Evaluación TEST
print("TEST")

test_pred <- predict(xgb_fitted, test, type = "prob") %>%
  bind_cols(predict(xgb_fitted, test)) %>%
  bind_cols(test %>% select(informal, pesos))

test_pred %>%
  conf_mat(truth = informal, estimate = .pred_class, case_weights = pesos) %>%
  summary()

test_auc <- test_pred %>%
  roc_auc(truth = informal, .pred_1, case_weights = pesos, event_level = "second")

print(test_auc)

## 18. Importancia (más robusta)
vip(xgb_fitted, num_features = 15) + theme_minimal()

## 19. Cierre
parallel::stopCluster(cl)
registerDoSEQ()


..tablas.....


# tablas xgboost de INGRs  -----------------------------------------------

## 1. Extraer modelo interno
xgb_model <- extract_fit_parsnip(xgb_fitted)$fit

## 2. Importancia (GAIN)
library(xgboost)

imp_xgb <- xgb.importance(model = xgb_model)

## 3. Construir tabla completa
library(dplyr)

tabla_xgb <- imp_xgb %>%
  as_tibble() %>%
  rename(
    variable = Feature,
    gain = Gain,
    cover = Cover,
    frequency = Frequency
  ) %>%
  arrange(desc(gain)) %>%
  mutate(
    importancia_relativa = gain / sum(gain),
    importancia_pct = importancia_relativa * 100
  )

## 4. Limpieza nombres (consistencia)
tabla_xgb <- tabla_xgb %>%
  mutate(variable = gsub("\\.", "_", variable))

## 5. Ver tabla completa
tabla_xgb

## 6. Top 20 (para paper)
top_xgb <- tabla_xgb %>%
  slice_max(order_by = importancia_pct, n = 20)

top_xgb

## 7. Exportar a Excel
library(writexl)

write_xlsx(
  list(
    "completo" = tabla_xgb,
    "top_20" = top_xgb
  ),
  path = "importancia_xgboost_ingresos.xlsx"
)



# XGBOOST LATENTE CON PESOS (PIPELINE tuning con pesos) ---------------------

## 1. Preliminares
library(tidymodels)
library(doParallel)
library(vip)

## 2. Manejo de Datos

vars_model <- c(
  #"ingrl_log",
  #"rama1",              # corregido (antes rama1)
  
  # Variables educativas y sociodemográficas
  "etnia",                  
  "estado_civil",           
  "Área",
  "esc",                   # para comapara entre modelos
  "género",
  
  # Variables económicas y laborales
  #"gasta_mas",              
  #"hrs_trab",               # 
  "exp_lab",                # 
  "edad",                  # se queda por contexto socioeconomico individual
  #"deseo_trab_mas",         # 
  
  # Variables de vivienda
  "mat_piso",               
  "serv_sanit",             
  "via_acceso",             
  "agua_fuente",           #  se queda por mejor valor interpretativo socioeconomico           
  "tipo_viv",               
  "basura_disp",            
  "serv_ducha",             
  "mat_cocina",             
  "tenencia_viv",           
  "mat_techo",              # nueva
  "mat_pared",              # nueva
  
  # Variables de hogar
  "num_cuartos"             # vi06
)


BASE_MODEL <- BASE1 %>%
  mutate(
    fexp.y = as.numeric(as.character(fexp.y)),
    pesos = importance_weights(fexp.y)
  ) %>%
  filter(!is.na(pesos), fexp.y > 0) %>%
  select(informal, all_of(vars_model), pesos)
names(BASE_MODEL)
## 🔴 SPLIT
set.seed(1234)
split <- initial_split(BASE_MODEL, prop = 0.8, strata = informal)

train <- training(split)
test  <- testing(split)

train <- train %>% mutate(informal = as.factor(informal))
test  <- test  %>% mutate(informal = as.factor(informal))

## 🔴 SUBMUESTRA (mantiene pesos)
set.seed(123)
train_sub <- train %>%
  group_by(informal) %>%
  slice_sample(prop = 0.20) %>%
  ungroup()

## 🔴 CV con pesos (IMPORTANTE)
set.seed(123)
cv_sub <- vfold_cv(train_sub, v = 3, strata = informal)

## 🔴 MÉTRICAS PONDERADAS
library(yardstick)
# Definir métricas correctamente
metricas <- metric_set(
  roc_auc,          # prob
  sens,             # class
  yardstick::spec,  # class (evita conflicto con readr::spec)
  accuracy,         # class
  bal_accuracy,     # class
  f_meas,           # class
  mcc               # class
)

## 3. Paralelización
cl <- parallel::makePSOCKcluster(parallel::detectCores(logical = FALSE) - 1)
doParallel::registerDoParallel(cl)

## 4. Recipe
xgb_rct <- recipe(informal ~ ., data = train_sub) %>%
  step_other(all_nominal_predictors(), threshold = 0.05) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors())

## 5. Modelo
xgb_sp <- boost_tree(
  trees = 500,
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  stop_iter = 15
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

## 6. Workflow (CON PESOS
xgb_wf <- workflow() %>%
  add_recipe(xgb_rct) %>%
  add_model(xgb_sp) %>%
  add_case_weights(pesos)

## 7. Tuning

### Grid
set.seed(123)
xgb_grid <- xgb_sp %>%
  extract_parameter_set_dials() %>%
  update(
    tree_depth = tree_depth(range = c(3, 10)),
    learn_rate = learn_rate(range = c(-3, -1), trans = log10_trans()),
    loss_reduction = loss_reduction(range = c(-10, 1.5), trans = log10_trans())
  ) %>%
  grid_latin_hypercube(size = 10)

### 🔴 Tuning CON PESOS
tictoc::tic("Tuning XGBoost PONDERADO")
xgb_tuned <- tune_grid(
  xgb_wf,
  resamples = cv_sub,
  grid = xgb_grid,
  metrics = metricas,
  control = control_grid(
    allow_par = TRUE,
    save_pred = TRUE
  )
)
tictoc::toc()

## 8. Modelo Final
xgb_pars_fin <- select_best(xgb_tuned, metric = "roc_auc")

xgb_wf_fin <- workflow() %>%
  add_recipe(xgb_rct) %>%
  add_model(finalize_model(xgb_sp, xgb_pars_fin)) %>%
  add_case_weights(pesos)

tictoc::tic("Ajuste Final Ponderado")
xgb_fitted <- fit(xgb_wf_fin, train)
tictoc::toc()

## 9. Evaluación (PONDERADA
### TRAIN
print("TRAIN (ponderado):")
train %>%
  predict(xgb_fitted, new_data = .) %>%
  bind_cols(train %>% select(informal, pesos)) %>%
  conf_mat(truth = informal, estimate = .pred_class, case_weights = pesos) %>%
  summary()

### TEST
print("TEST (ponderado):")
test %>%
  predict(xgb_fitted, new_data = .) %>%
  bind_cols(test %>% select(informal, pesos)) %>%
  conf_mat(truth = informal, estimate = .pred_class, case_weights = pesos) %>%
  summary()

### ROC AUC ponderado
train_auc <- predict(xgb_fitted, train, type = "prob") %>%
  bind_cols(train %>% select(informal, pesos)) %>%
  roc_auc(truth = informal, .pred_1, case_weights = pesos, event_level = "second")
print(train_auc)

test_auc <- predict(xgb_fitted, test, type = "prob") %>%
  bind_cols(test %>% select(informal, pesos)) %>%
  roc_auc(truth = informal, .pred_1, case_weights = pesos, event_level = "second")
print(test_auc)



## 10. Importancia
xgb_fitted %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20, geom = "point") + theme_minimal()

## 11. Cierre
parallel::stopCluster(cl)
registerDoSEQ()


....tabla de importancias relativas....



## 1. Cargar librerías necesarias
library(vip)
library(writexl)
library(dplyr)

## 2. Extraer importancia de variables
# Usamos vi() para obtener un tibble (tabla) en lugar de un gráfico
tab_importancia <- xgb_fitted %>%
  extract_fit_parsnip() %>%
  vi(scale = TRUE)  # scale = TRUE para que la suma sea relativa

## 3. Procesar la tabla para tu análisis Q1
# Vamos a calcular el porcentaje de contribución y limpiar nombres
tab_exportar <- tab_importancia %>%
  mutate(
    # Calcular ganancia relativa (Gain pct)
    Importance_pct = (Importance / sum(Importance)) * 100,
    # Crear una columna de "Dimensión" basada en tus grupos (opcional)
    Dimension = case_when(
      Variable %in% c("esc", "exp_lab", "edad") ~ "Capital Humano",
      Variable %in% c("etnia", "estado_civil", "género", "área_urbana") ~ "Sociodemográfica",
      grepl("mat_|num_cuartos|tenencia", Variable) ~ "Estructura Habitacional",
      grepl("serv_|basura_|agua_|via_", Variable) ~ "Infraestructura de Servicios",
      TRUE ~ "Otros"
    )
  ) %>%
  rename(Variable_Dummy = Variable, Importancia_Original = Importance)

## 4. Agrupar por dimensiones (Para tu análisis de texto)
# Esto te servirá para citar que la "Dinámica laboral domina el X%"
tab_dimensiones <- tab_exportar %>%
  group_by(Dimension) %>%
  summarise(Contribucion_Total = sum(Importance_pct)) %>%
  arrange(desc(Contribucion_Total))

## 5. Exportar a Excel
# Creamos un archivo con dos pestañas: una por variable y otra por dimensiones
listas_hojas <- list(
  "Importancia_Variables" = tab_exportar,
  "Resumen_Dimensiones" = tab_dimensiones
)

write_xlsx(listas_hojas, "Tabla_Importancia_XGBoost_Latente.xlsx")

print("Tabla exportada con éxito como: Tabla_Importancia_XGBoost_Latente.xlsx")









# grafico SHAP ------------------------------------------------------------
# GENERACIÓN DE GRÁFICA SHAP PARA MODELO LATENTE (NIVEL Q1)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidymodels)

# 1. Extraer el objeto booster de tu modelo final
xgb_booster_latente <- extract_fit_parsnip(xgb_fitted)$fit

# 2. Procesar los datos a través de la receta (Recipe)
# Esto genera las variables dummy (X1, X2...) que el modelo entiende
rec_prep <- prep(xgb_rct)
train_processed <- bake(rec_prep, new_data = train)

X_train <- train_processed %>%
  select(-informal, -pesos) %>%
  as.matrix()

# 3. Calcular valores SHAP exactos
shap_values <- predict(xgb_booster_latente, X_train, predcontrib = TRUE)
shap_values <- as.data.frame(shap_values)
shap_values <- shap_values[, -ncol(shap_values)] # Eliminar bias

# 4. Calcular importancia global para el orden de la gráfica
shap_importance <- shap_values %>%
  summarise(across(everything(), ~ mean(abs(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "mean_abs") %>%
  arrange(desc(mean_abs))

# 5. Muestreo (2000 obs) para evitar saturación visual
set.seed(123)
sample_idx <- sample(1:nrow(X_train), 2000)
shap_sample <- shap_values[sample_idx, ]
X_sample <- X_train[sample_idx, ]

# 6. Preparar datos para ggplot
shap_long <- shap_sample %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "shap")

X_long <- as.data.frame(X_sample) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value")

plot_data <- left_join(shap_long, X_long, by = c("id", "variable"))

# 7. Selección de Top variables y RENOMBRADO ESTÉTICO
top_vars <- shap_importance %>% slice(1:15) %>% pull(variable)

plot_data_final <- plot_data %>%
  filter(variable %in% top_vars) %>%
  mutate(
    variable = factor(variable, levels = rev(top_vars)),
    # AQUÍ RENOMBRAMOS ÁREA_X1 Y OTROS SI ES NECESARIO
    variable = recode(variable, 
                      "Área_X1" = "Área Urbana",
                      "esc" = "Escolaridad",
                      "exp_lab" = "Experiencia Laboral",
                      "edad" = "Edad")
  )

# 8. GRÁFICA SHAP FINAL (Estilo Q1)
ggplot(plot_data_final, aes(x = shap, y = variable, color = value)) +
  geom_jitter(height = 0.25, width = 0, alpha = 0.6, size = 1.8) + 
  scale_color_gradient2(
    low = "#2E5A88",   # Azul profesional
    mid = "#D5D8DC",   # Gris
    high = "#9B2226",  # Rojo vino
    midpoint = median(plot_data_final$value, na.rm = TRUE)
  ) +
  theme_minimal(base_family = "serif") + # "serif" = Times New Roman
  theme(
    # Tamaño de letra 18 para máxima legibilidad en el artículo
    plot.title = element_text(size = 18, face = "bold", margin = margin(b=15)),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18, color = "black"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "Contribución Marginal a la Informalidad Latente (Valores SHAP)",
    x = "Impacto en la predicción (SHAP value)",
    y = NULL,
    color = "Valor de la\nvariable"
  )

# 9. GUARDAR EN ALTA RESOLUCIÓN
ggsave("Grafico_SHAP_Latente_Q1.png", width = 11, height = 8, dpi = 300)

























# tabla de importancia de variables lantete  ------------------------------
install.packages("writexl")
library(writexl)# TABLA DE IMPORTANCIA COMPLETA PARA EL PUNTO 10
library(dplyr)
library(vip)

# 1. Extraer importancia de TODAS las variables
tabla_importancia_completa <- xgb_fitted %>%
  extract_fit_parsnip() %>%
  vi() %>% # Esto extrae todas por defecto
  mutate(
    # 2. Asignar Dimensiones (basado en los nombres de las variables dummy)
    Dimension = case_when(
      grepl("^esc|^exp_lab|^edad", Variable) ~ "Capital Humano",
      grepl("^etnia|^estado_civil|^Área|^género", Variable) ~ "Sociodemográficas",
      grepl("^mat_piso|^mat_techo|^mat_pared", Variable) ~ "Estructura Habitacional",
      grepl("^serv_sanit|^via_acceso|^agua_fuente|^basura_disp|^serv_ducha|^mat_cocina", Variable) ~ "Infraestructura de Servicios",
      grepl("^tipo_viv|^tenencia_viv|^num_cuartos", Variable) ~ "Estabilidad Patrimonial",
      TRUE ~ "Otras"
    ),
    # 3. Calcular porcentaje para mejor interpretación
    Porcentaje = (Importance / sum(Importance)) * 100
  )

# 4. Ver la tabla completa en RStudio
View(tabla_importancia_completa)

# 5. (Opcional) Guardar para Excel si prefieres graficar allá
# write.csv(tabla_importancia_completa, "importancia_todas_las_variables.csv", row.names = FALSE)

# Mostrar las primeras filas en consola
print(head(tabla_importancia_completa, 20))


write_xlsx(tabla_importancia_completa, "Importancia_Variables_XGBoost.xlsx")
getwd()




# PREDICCION DE EMPLEO SECT INFORMAL PARA DESEMPLEADOS DE 2024 (dice data2025 pero es del 2024)  -----------
data2025 <- data2025 %>%
  rename("Área" = area)


#se utiliza la Experiencia Potencial. Es el estándar académico para papers de alto impacto.
data2025 <- data2025 %>%
  mutate(exp_lab = edad - esc - 6,
         # Evitamos valores negativos si alguien tiene mucha educación y es muy joven
         exp_lab = ifelse(exp_lab < 0, 0, exp_lab),
         exp_lab = as.integer(exp_lab))


# INFERENCIA PROSPECTIVA: VULNERABILIDAD LABORAL LATENTE (DESEMPLEADOS 2024)

# 1. PREPARACIÓN Y LIMPIEZA DE TIPOS DE DATOS
# El modelo XGBoost requiere consistencia exacta en los tipos de datos.
data_prospectiva <- data2025 %>%
  mutate(across(c(edad, esc, num_cuartos, exp_lab), as.integer))

# 2. GENERACIÓN DE PREDICCIONES (CLASE Y PROBABILIDAD)
# .pred_class: Clasificación binaria (Informal/No Informal)
# .pred_1: Score de riesgo (0 a 1) - Este es el valor de mayor peso analítico.
data_final <- data_prospectiva %>%
  bind_cols(
    predict(xgb_fitted, new_data = ., type = "class"),
    predict(xgb_fitted, new_data = ., type = "prob")
  )

# 3. CÁLCULO DEL ÍNDICE GLOBAL DE VULNERABILIDAD LATENTE (IVL)
# Este valor es el "Headline" de tu artículo.
ivl_nacional <- data_final %>%
  summarise(
    tasa_informalidad_latente = weighted.mean(.pred_class == "1", w = fexp.y) * 100,
    score_riesgo_promedio = weighted.mean(.pred_1, w = fexp.y)
  )

print("--- Índice Global de Vulnerabilidad Latente ---")
print(ivl_nacional)


# 4. TABLA DE BRECHAS ESTRUCTURALES (VALOR ECONÓMICO Q1)
# Esta tabla identifica dónde debe intervenir el Estado.
tabla_brechas <- data_final %>%
  group_by(Área, género) %>% # Interseccionalidad básica
  summarise(
    riesgo_informal = weighted.mean(.pred_class == "1", w = fexp.y) * 100,
    poblacion_representada = sum(fexp.y),
    .groups = "drop"
  ) %>%
  arrange(desc(riesgo_informal))
# 2 es mujer, 1 es hombre
print("--- Brechas de Vulnerabilidad por Área y Género ---")
print(tabla_brechas)


# --- TABLA DE VULNERABILIDAD EXPANDIDA (NIVEL Q1) ---

tabla_vulnerabilidad_completa <- data_final %>%
  mutate(segmento = case_when(
    .pred_1 < 0.30 ~ "1. Bajo (0-30%)",
    .pred_1 >= 0.30 & .pred_1 < 0.70 ~ "2. Moderado (30-70%)",
    .pred_1 >= 0.70 ~ "3. Crítico (IVL) (>70%)"
  )) %>%
  group_by(segmento) %>%
  summarise(
    # Magnitud poblacional
    `% Población` = (sum(fexp.y) / sum(data_final$fexp.y)) * 100,
    `Riesgo Promedio` = weighted.mean(.pred_1, w = fexp.y),
    
    # Capital Humano
    `Escolaridad Media` = weighted.mean(esc, w = fexp.y),
    `Experiencia Laboral` = weighted.mean(exp_lab, w = fexp.y),
    `Edad Media` = weighted.mean(edad, w = fexp.y),
    
    # Infraestructura y Servicios (Prevalencias %)
    # Nota: Asegúrate que los nombres "Urbana" y "Red_Publica" coincidan con tus datos
    `Área Urbana (%)` = weighted.mean(Área == "Urbana", w = fexp.y, na.rm = TRUE) * 100,
    `Acceso Red Pública (%)` = weighted.mean(serv_sanit == "Red_Publica", w = fexp.y, na.rm = TRUE) * 100,
    `Piso Precario (%)` = weighted.mean(mat_piso == "Precario_Inadecuado", w = fexp.y, na.rm = TRUE) * 100
  )

# Mostrar resultados
print(tabla_vulnerabilidad_completa)

# Exportar a Excel con formato profesional
library(writexl)
write_xlsx(tabla_vulnerabilidad_completa, "Perfil_Vulnerabilidad_Latente_Completo.xlsx")




# categorias de riesgos de informalidad -----------------------------------
# Crear categorías de riesgo basándonos en la probabilidad (score)
tabla_perfiles <- data_final_2025 %>%
  mutate(nivel_riesgo = case_when(
    .pred_1 < 0.30 ~ "Bajo (0-30%)",
    .pred_1 >= 0.30 & .pred_1 < 0.70 ~ "Medio (30-70%)",
    .pred_1 >= 0.70 ~ "Alto (70-100%)"
  )) %>%
  group_by(nivel_riesgo) %>%
  summarise(
    n_poblacional = sum(fexp.y),          # Cuántos ecuatorianos representan
    escolaridad_media = weighted.mean(esc, w = fexp.y),
    edad_media = weighted.mean(edad, w = fexp.y),
    # Para variables de vivienda usamos la "Moda" (el material más común)
    material_piso_predominante = names(which.max(table(mat_piso))),
    area_predominante = names(which.max(table(area)))
  ) %>%
  mutate(porcentaje_del_total = n_poblacional / sum(n_poblacional) * 100)

print(tabla_perfiles)













