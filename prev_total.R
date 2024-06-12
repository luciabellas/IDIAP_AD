#PREVALENTES 
fechas_embarazo <- `repositori_LuciaVH_N_2023-06-09` %>% mutate(id_emb = row_number())
# Inicio base fechas embarazo
embarazo <- fechas_embarazo %>% select(idp, ctanca, cod_emb,id_emb, dinici, dfi)

#se une la BD de embarazo con la de farmacos prescrito 
farmacos <- `farmacs_prescrits_LuciaVH_N_2023-06-09` %>% 
  full_join(fechas_embarazo, relationship = "many-to-many")

##Filtramos las columnas de factuados por antidepresivos 
dispensed_drugs <-  `farmacs_facturats_LuciaVH_N_2023-06-09`%>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A"))%>%
  mutate(data_dispensed = dat)%>%
  select(idp,cod,env,data_dispensed)

#Calculamos denominador de prevalentes por trimestrs 
denominadorTrimestres <- farmacos %>%
  mutate(
    deno_EMB = between(dinici, as.Date("2011-01-01"), as.Date("2020-06-30")) &
      between(dfi, as.Date("2011-01-01"), as.Date("2020-06-30")), 1, 0)

# DEFINIR INTERVALOS 
#definimos los intervalos para cada uno de los periodos de interés y observación 
#3 meses antes 
intervalo_embarazo <- interval(denominadorTrimestres$dinici, denominadorTrimestres$dfi)
intervalo_prescripcion <- interval (denominadorTrimestres$dat, denominadorTrimestres$dbaixa)


# Solapamiento de intervalo de prescripcion: Si hay solapamiento expo: 1, sino expo: 0
numeradorTrimestres <- denominadorTrimestres %>%
  mutate(
    expo_3mB = ifelse(int_overlaps(intervalo_embarazo, intervalo_prescripcion), 1, 0),
    ## Convertimos las exposiciones a 0 si no hay denominador para esa exposición 
    expo_3mB = expo_3mB * deno_EMB)

numeradorTrimestres_preva <- numeradorTrimestres%>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A"))%>% 
  group_by(id_emb, cod)

#Calculamos adherencia en prevalentes 

#creamos BD adherencia
adherencia <- numeradorTrimestres_preva%>%
  full_join(dispensed_drugs, relationship = "many-to-many")

adherencia_fechas_prevalentes <- adherencia %>%
  mutate(ad_3mb = if_else(between(data_dispensed, dinici, dfi), 1, 0)) 

Adherencia_multiplicado_prevalentes <- adherencia_fechas_prevalentes%>% 
  mutate(
    #si hay solapameiento etre el periodo de interes y periodo de prescripcion=1, sino 0 
    ad_3mb=ad_3mb*expo_3mB)%>%
  filter(ad_3mb == 1)%>%
  select("id_emb", "cod", "idp", "dinici", "dfi", "dat", "dbaixa", "data_dispensed")%>%
  distinct(id_emb, dinici, dfi, dat, dbaixa, data_dispensed, .keep_all = TRUE)%>%
  arrange(id_emb, data_dispensed)


#Diferencia de días entre prescripciones 
Adherencia_multiplicado_prevalentes <- Adherencia_multiplicado_prevalentes %>%
  group_by(id_emb) %>%
  mutate(
    prev_dbaixa = lag(dbaixa),
    cuenta = ifelse(is.na (prev_dbaixa) | prev_dbaixa >= dat | as.numeric(dat - prev_dbaixa) < 62, 1, 0)
  ) %>%
  ungroup()%>%
  filter(cuenta == 1)


#Creamos columnas de duracion de tratamiento y diferencia de dias  
Adherencia_selected_prevalentes_columnas <- Adherencia_multiplicado_prevalentes %>% 
  mutate(
    diferencia_dias = as.numeric(data_dispensed - lag(data_dispensed, default = first(dbaixa)), units = "days"), 
    duracion_tto = as.numeric(pmin(data_dispensed, dbaixa) - dinici)
  ) %>%
  select(id_emb, idp, dinici, dfi, dat, data_dispensed, dbaixa, duracion_tto, diferencia_dias, cod) 


#Creamos la columna de ordered failure times 
OFT <- Adherencia_selected_prevalentes_columnas%>%
  # filter(dbaixa > data_dispensed) %>%
  # filter(pmin(dat, dbaixa) <= data_dispensed) %>%
  group_by(id_emb) %>%
  mutate(
    evento = ifelse((diferencia_dias >= 62),1, 0),
    first_event_duracion = ifelse(any(evento == 1), duracion_tto[evento == 1][1], NA_real_),
    OFT = ifelse(any(evento == 1), first_event_duracion, max(duracion_tto, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  select(-first_event_duracion) 


# Paso 2: Crear una fila única por `id_emb`
resumen_oft<- OFT %>%
  group_by(id_emb) %>%
  summarise(
    dinici = first(dinici),
    dfi = first(dfi),
    evento = max(evento),  # Si hay al menos un evento 1, se queda con 1
    OFT = first(OFT),      # OFT es el mismo para todas las filas dentro de un id_emb
    cod = first(cod)       # Seleccionar otros campos relevantes
  ) %>%
  ungroup()


#Creamos la base de datos para el KP 
survival_time <- resumen_oft %>%
  mutate(tiempo_semanas = OFT/7)  
km_grupo_c <- survfit(Surv(tiempo_semanas, evento) ~ 1, data = survival_time)

plot(km_grupo_c, col = "#FF3490", main = "Figure 4: Persistence with antidepressants, all users", 
     xlab = "Time (weeks)", ylab = "Proportion in persistence", lwd = 2)
summary(km_grupo_c)

# Obtener las estimaciones de supervivencia
summary_km <- summary(km_grupo_c)

# Ver los tiempos, las supervivencias y los grupos
summary_km$time
summary_km$surv
median(summary_km$time)
median(summary_km$surv)
mediana_tiempo <- summary(km_grupo_c)$time[which.min(abs(summary(km_grupo_c)$surv - 0.5))]
# 31.28571
# Obtener la función de supervivencia acumulada en la semana 40
km_summary <- summary(km_grupo_c)
tiempo_40 <- 40  # Semana 40
tiempo_26 <- 26  # Semana 26

indice_tiempo_40 <- which(km_summary$time >= tiempo_40)[1]
indice_tiempo_26 <- which(km_summary$time >= tiempo_26)[1]

# Obtener la función de supervivencia acumulada en la semana 40
supervivencia_en_40 <- km_summary$surv[indice_tiempo_40]

# Calcular el porcentaje de persistencia en la semana 40
porcentaje_persistencia_40 <- supervivencia_en_40 * 100

# Obtener la función de supervivencia acumulada en la semana 26
supervivencia_en_26 <- km_summary$surv[indice_tiempo_26]

# Calcular el porcentaje de persistencia en la semana 40
porcentaje_persistencia_40 <- supervivencia_en_40 * 100
# 22.4126

# Calcular el porcentaje de persistencia en la semana 26
porcentaje_persistencia_26 <- supervivencia_en_26 * 100
# 59.64754


