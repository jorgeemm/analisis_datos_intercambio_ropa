# Librerías ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(stringi)
library(stringr)


# Intercambio noviembre 2023 ---------------------------------------------------

intercambio_23 <- read_xlsx("00_datos/originales/intercambio_23_11.xlsx") %>% 
  
  # Normalizar y limpiar los nombres de las variables
  clean_names() %>% 
  
  rename(conocimiento_intercambio = como_te_has_enterado_de_este_evento,
         uni = cual_es_tu_universidad,
         destacable_evento = que_es_lo_que_mas_te_llama_la_atencion_de_este_evento,
         prendas_entrada = no_prendas,
         prendas_salida = no_prendas_sale,
         hora_inscripcion = marca_temporal) %>% 
  
  # Variables nuevas, edición de categorías y corrección errores
  mutate(
    
    prendas_entrada = as.numeric(prendas_entrada),
    
    # Por qué conocen el intercambio:
    conocimiento_intercambio = case_when(
      conocimiento_intercambio == "Redes social o comunicación de la Universidad" ~
        "Comunicación UC3M",
      conocimiento_intercambio == "Mi amigx lo organiza" ~
        "Amigo organizador",
      conocimiento_intercambio %in% c("Me lo ha contado algún amigx", 
                                      "Me ha llegado por WhatsApp") ~ "Amigos",
      conocimiento_intercambio == "Redes sociales de MUCAM (instagram, tiktok)" ~
        "Redes MUCAM",
      str_detect(tolower(conocimiento_intercambio), "folleto|panfletos") ~
        "Material impreso",
      str_detect(tolower(conocimiento_intercambio), "mail|correo|email") ~
        "Comunicación UC3M",
      str_detect(conocimiento_intercambio, "Alfredo|profe|profesor") ~
        "Amigo organizador",
      T ~ "Otros"),
    
    # Normalizar los nombres y apellidos y juntarlos en una única variable:
    # 1. Extraer primer apellido compuesto
    primer_apellido = str_extract(
      apellido,
      regex("^(de_la|de_los|de_las|de_el|del|de|la|las|el|los|)\\s*\\w+",
            ignore_case = TRUE)) %>%
      str_replace_all("\\s+", "_") %>%
      str_to_lower(),
    # 2. Normalizar ambas versiones
    across(c(nombre, apellido, primer_apellido), 
           ~ str_to_lower(.) %>% 
             stri_trans_general("Latin-ASCII") %>% 
             str_replace_all("\\s+", "_"),
           .names = "{.col}_norm"),  # Crea variables normalizadas
    # 3. Combinar variables
    nombre_apellido = str_c(nombre_norm,
                            primer_apellido_norm,
                            sep = "_"),
    nombre_apellidos = str_c(nombre_norm,
                             apellido_norm,
                             sep = "_"),
    
    
    # Añadir una variable para indicar de qué intercambio se trata
    intercambio = "2023_11") %>% 
  
  # Eliminar filas sin datos
  filter(!is.na(nombre)) %>% 
  
  # Selección de variables
  select(intercambio, nombre, apellido, genero, uni, conocimiento_intercambio,
         prendas_entrada, prendas_salida, puntos_entrada, puntos_salida,
         hora_inscripcion,
         
         nombre_apellido, nombre_apellidos,
         destacable_evento)


# Intercambio abril 2024 -------------------------------------------------------

intercambio_24_4 <- read_xlsx("00_datos/originales/intercambio_24_4.xlsx") %>% 
  
  # Normalizar y limpiar los nombres de las variables
  clean_names() %>% 
  
  rename(conocimiento_intercambio = como_te_has_enterado_de_este_evento,
         uni = cual_es_tu_universidad,
         prendas_entrada = numero_de_prendas,
         prendas_salida = n_prendas_salida,
         hora_inscripcion = marca_temporal) %>% 
  
  # Variables nuevas, edición de categorías y corrección errores
  mutate(
    
    puntos_salida = as.numeric(puntos_salida),
    
    # Por qué conocen el intercambio:
    conocimiento_intercambio = case_when(
      str_detect(conocimiento_intercambio,
                 "RRSS o correo de la universidad") ~ 
        "Comunicación UC3M",
      str_detect(conocimiento_intercambio,
                 "Mi amigx lo organiza") ~ 
        "Amigo organizador",
      str_detect(conocimiento_intercambio,
                 "Me lo han contado por ahí") ~ 
        "Amigos",
      str_detect(conocimiento_intercambio,
                 "Os sigo en insta @mucam_uc3m|\\bMUCAM\\b") ~ 
        "Redes MUCAM",
      str_detect(conocimiento_intercambio,
                 "folleto|panfleto|material") ~ 
        "Material impreso",
      str_detect(conocimiento_intercambio,
                 "He venido a otros intercambios") ~
        "Otros intercambios",
      str_detect(conocimiento_intercambio,
                 "Whatsapp") ~ "Amigos",
      T~ "Otros"),
    
    # Normalizar los nombres y apellidos y juntarlos en una única variable:
    # 1. Extraer primer apellido compuesto
    primer_apellido = str_extract(
      apellido,
      regex("^(de_la|de_los|de_las|de_el|del|de|la|las|el|los|)\\s*\\w+",
            ignore_case = TRUE)) %>%
      str_replace_all("\\s+", "_") %>%
      str_to_lower(),
    # 2. Normalizar ambas versiones
    across(c(nombre, apellido, primer_apellido), 
           ~ str_to_lower(.) %>% 
             stri_trans_general("Latin-ASCII") %>% 
             str_replace_all("\\s+", "_"),
           .names = "{.col}_norm"),  # Crea variables normalizadas
    # 3. Combinar variables
    nombre_apellido = str_c(nombre_norm,
                            primer_apellido_norm,
                            sep = "_"),
    nombre_apellidos = str_c(nombre_norm,
                             apellido_norm,
                             sep = "_"),
    
    
    # Añadir una variable para indicar de qué intercambio se trata
    intercambio = "2024_4") %>% 
  
  # Eliminar filas sin datos
  filter(!is.na(nombre)) %>% 
  
  # Selección de variables
  select(intercambio, nombre, apellido, genero, uni, conocimiento_intercambio,
         prendas_entrada, prendas_salida, puntos_entrada, puntos_salida,
         hora_inscripcion,
         
         nombre_apellido, nombre_apellidos)


# Intercambio noviembre 2024 -------------------------------------------------------

intercambio_24_11 <- read_xlsx("00_datos/originales/intercambio_24_11.xlsx") %>% 
  
  # Normalizar y limpiar los nombres de las variables
  clean_names() %>% 
  
  rename(conocimiento_intercambio = como_te_has_enterado_del_evento,
         uni = cual_es_tu_universidad,
         prendas_entrada = numero_de_prendas_entrada,
         prendas_salida = prendas_salida,
         hora_inscripcion = marca_temporal) %>% 
  
  # Variables nuevas, edición de categorías y corrección errores
  mutate(
    
    puntos_salida = as.numeric(puntos_salida),
    
    # Por qué conocen el intercambio:
    conocimiento_intercambio = case_when(
      str_detect(conocimiento_intercambio,
                 "RRSS o correo de la universidad") ~
        "Comunicación UC3M",
      str_detect(conocimiento_intercambio,
                 "Mis amigas participan en MUCAM") ~ 
        "Amigo organizador",
      str_detect(conocimiento_intercambio,
                 "Me lo han contado por ahí") ~ 
        "Amigos",
      str_detect(conocimiento_intercambio,
                 "@mucam_uc3m|MUCAM") ~ 
        "Redes MUCAM",
      str_detect(conocimiento_intercambio,
                 "Whatsapp") ~ 
        "Amigos",
      T ~ NA),
    
    # Ver si han venido otras veces:
    repetidor = case_when(
      repetidor == "No :)" ~ "Primera vez",
      repetidor == "Si, os conocí en el último" ~ "Segunda vez",
      repetidor == "Sii!!! Varias veces" ~ "Varias veces"),
    
    # Normalizar los nombres y apellidos y juntarlos en una única variable:
    # 1. Extraer primer apellido compuesto
    primer_apellido = str_extract(
      apellido,
      regex("^(de_la|de_los|de_las|de_el|del|de|la|las|el|los|)\\s*\\w+",
            ignore_case = TRUE)) %>%
      str_replace_all("\\s+", "_") %>%
      str_to_lower(),
    # 2. Normalizar ambas versiones
    across(c(nombre, apellido, primer_apellido), 
           ~ str_to_lower(.) %>% 
             stri_trans_general("Latin-ASCII") %>% 
             str_replace_all("\\s+", "_"),
           .names = "{.col}_norm"),  # Crea variables normalizadas
    # 3. Combinar variables
    nombre_apellido = str_c(nombre_norm,
                            primer_apellido_norm,
                            sep = "_"),
    nombre_apellidos = str_c(nombre_norm,
                             apellido_norm,
                             sep = "_"),
    
    
    # Añadir una variable para indicar de qué intercambio se trata
    intercambio = "2024_11") %>% 
  
  # Eliminar filas sin datos
  filter(!is.na(nombre)) %>% 
  
  # Selección de variables
  select(intercambio, nombre, apellido, genero, uni, conocimiento_intercambio,
         prendas_entrada, prendas_salida, puntos_entrada, puntos_salida,
         hora_inscripcion,
         
         repetidor,
         nombre_apellido, nombre_apellidos)


# Intercambio abril 2025 --------------------------------------------------

### Hoja con la información de la inscripción ###

intercambio_25_0 <- read_csv("00_datos/originales/intercambio_25_4_inscripcion.csv") %>% 
  
  # Normalizar y limpiar los nombres de las variables
  clean_names() %>% 
  
  rename(conocimiento_intercambio = como_te_has_enterado_del_evento,
         uni = cual_es_tu_universidad,
         repetidor = has_venido_otras_veces,
         hora_inscripcion = f) %>% 
  
  # Variables nuevas, edición de categorías y corrección errores
  mutate(
    
    # Poner la hora de inscripción en el formato adecuado
    hora_inscripcion = lubridate::dmy_hms(hora_inscripcion,
                                          tz = "Europe/Madrid"),
    
    
    # Ver si han venido otras veces:
    repetidor = case_when(
      repetidor == "No :)" ~ "Primera vez",
      repetidor == "Si, os conocí en el último" ~ "Segunda vez",
      repetidor == "Sii!!! Varias veces" ~ "Varias veces"),
    
    # Por qué conocen el intercambio:
    conocimiento_intercambio = case_when(
      str_detect(conocimiento_intercambio,
                 "Correo o comunicación|RRSS:") ~
        "Comunicación UC3M",
      str_detect(conocimiento_intercambio,
                 "Mis amigas participan en MUCAM") ~
        "Amigo organizador",
      str_detect(conocimiento_intercambio,
                 "Me lo han contado por ahí|Whatsapp") ~
        "Amigos",
      str_detect(conocimiento_intercambio,
                 "participar en mucam|Me incorporé a MUCAM") ~
        "Redes MUCAM",
      str_detect(conocimiento_intercambio,
                 "cartel|corchos|panfleto") ~
        "Material impreso",
      T ~ NA),
    
    # Normalizar los nombres y apellidos y juntarlos en una única variable:
    # 0. Eliminar espacios extra al principio y final del nombre y el apellido
    nombre = str_trim(nombre),
    apellidos = str_trim(apellido),
    # 1. Extraer primer apellido compuesto
    primer_apellido = str_extract(
      apellido,
      regex("^(de_la|de_los|de_las|de_el|del|de|la|las|el|los|)\\s*\\w+",
            ignore_case = TRUE)) %>%
      str_replace_all("\\s+", "_") %>%
      str_to_lower(),
    # 2. Normalizar ambas versiones
    across(c(nombre, apellido, primer_apellido), 
           ~ str_to_lower(.) %>% 
             stri_trans_general("Latin-ASCII") %>% 
             str_replace_all("\\s+", "_"),
           .names = "{.col}_norm"),  # Crea variables normalizadas
    # 3. Combinar variables
    nombre_apellido = str_c(nombre_norm,
                            primer_apellido_norm,
                            sep = "_"),
    nombre_apellidos = str_c(nombre_norm,
                             apellido_norm,
                             sep = "_"),
    
    
    # Añadir variable para ver de qué intercambio se trata
    intercambio = "2025_4") %>% 

    # Eliminar filas sin datos
    filter(!is.na(nombre)) %>% 
    
    # Selección de variables
    select(intercambio, nombre, apellidos, genero, uni, conocimiento_intercambio,
           hora_inscripcion,
           
           repetidor,
           nombre_apellido, nombre_apellidos)

  
### Hoja con la información de entrada ###

intercambio_25_1 <- read.csv("00_datos/originales/intercambio_25_4_entrada.csv") %>% 
  clean_names() %>% 
  
  # Añadir un prefijo para distinguir las prendas de entrada de las de salida
  rename_with(
    ~ paste0("entrada_", .),
    .cols = starts_with("prenda")) %>% 
  
  # Eliminar espacios extra al principio y final del nombre y el apellido
  mutate(
    nombre = str_trim(nombre),
    apellidos = str_trim(apellidos)) %>% 
  
  # Eliminar filas sin datos
  filter(!is.na(nombre)) %>% 
  
  # Seleccionar las variables
  select(nombre, apellidos,
         puntos_entrada, prendas_entrada = no_prendas, puntos_extra, puntos_antes,
         hora_entrada, 
         fiel_intercambio = fiel_al_intercambio, 
         starts_with("entrada_prenda"))
  
  
### Hoja con la información de salida ###

intercambio_25_2 <- read.csv("00_datos/originales/intercambio_25_4_salida.csv") %>% 
  clean_names() %>% 
  
  # Añadir un prefijo para distinguir las prendas de entrada de las de salida
  rename_with(
    ~ paste0("salida_", .),
    .cols = starts_with("prenda")) %>% 
  
  # Eliminar espacios extra al principio y final del nombre y el apellido
  mutate(
    nombre = str_trim(nombre),
    apellidos = str_trim(apellidos)) %>% 
  
  # Eliminar filas sin datos
  filter(!is.na(nombre)) %>% 
  
  # Seleccionar las variables
  select(nombre, apellidos,
         puntos_salida = puntos_restantes,
         prendas_salida = no_prendas, 
         hora_salida,
         starts_with("salida_prenda"),
         -salida_prenda_10)


## Juntar las tres hojas en una ##
#(Como iba mal el forms, hay gente que se ha apuntado varias veces, lo que da problemas)

intercambio_25_0 <- intercambio_25_0 %>% 
  distinct(nombre, apellidos, .keep_all = TRUE)
intercambio_25_1 <- intercambio_25_1 %>% 
  distinct(nombre, apellidos, .keep_all = TRUE)
intercambio_25_2 <- intercambio_25_2 %>% 
  distinct(nombre, apellidos, .keep_all = TRUE)

intercambio_25 <-  left_join(intercambio_25_0, intercambio_25_1,
                             by = c("nombre", "apellidos")) %>% 
  left_join(intercambio_25_2, by = c("nombre", "apellidos")) %>% 
  rename(apellido = apellidos)


# Juntar y guardar bases enteras ---------------------------------------------------

intercambios_juntos <- 
    bind_rows(intercambio_23, 
              intercambio_24_4, 
              intercambio_24_11, 
              intercambio_25) %>% 
  write.csv("00_datos/intercambios_juntos.csv")
  

# Guardar bases anonimizadas ----------------------------------------------

intercambios_juntos<- 
  bind_rows(intercambio_23, 
            intercambio_24_4, 
            intercambio_24_11, 
            intercambio_25) %>%
  select(-nombre, -apellido, -nombre_apellido, -nombre_apellidos) %>% 
  write.csv("00_datos/intercambios_juntos_anonimo.csv")

