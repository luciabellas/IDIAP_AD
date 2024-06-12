##ESTUDIO DE ADHERENCIA 
#Adherencia primaria 
install.packages("dplyr")
install.packages("lubridate")
install.packages("exactci")
install.packages ('lubridate')
install.packages ('dplyr')
install.packages('devtools')
install.packages('Desc')
install.packages("epitools")
install.packages("exact2x2")
install.packages("DescTools")
install.packages("survival")
library(survival)
library(exact2x2)
library (dplyr)
library(stringr)
library (lubridate)
library(stats)
library(epitools)
library(DescTools)

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


# DEFINIR INTERVALOS  #definimos los intervalos para cada uno de los periodos de interés y observación 
 #3 meses antes 
intervalo_embarazo <- interval(denominadorTrimestres$dinici, denominadorTrimestres$dfi)
intervalo_prescripcion <- interval (denominadorTrimestres$dat, denominadorTrimestres$dbaixa)
intervalo_NOWASH <- interval (denominadorTrimestres$dinici-years(1), denominadorTrimestres$dinici)

#Calculamos denominador de prevalentes por trimestrs 
denominadorTrimestres <- farmacos %>%
 mutate(
     deno_EMB = between(dinici, as.Date("2011-01-01"), as.Date("2020-06-30")) &
      between(dfi, as.Date("2011-01-01"), as.Date("2020-06-30")), 1, 0, 
    EmB_washout = ifelse(int_overlaps(intervalo_NOWASH, intervalo_prescripcion), 1,0))

# Solapamiento de intervalo de prescripcion: Si hay solapamiento expo: 1, sino expo: 0
numeradorTrimestres <- denominadorTrimestres %>%
  mutate(
    expo_3mB = ifelse(int_overlaps(intervalo_embarazo, intervalo_prescripcion), 1, 0),
    ## Convertimos las exposiciones a 0 si no hay denominador para esa exposición 
    expo_3mB = expo_3mB * deno_EMB*EmB_washout)

numeradorTrimestres_preva <- numeradorTrimestres%>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A"))%>% 
  group_by(id_emb, cod)

#Calculamos adherencia en prevalentes 
#trimestres adherencia

#creamos BD adherencia
adherencia <- numeradorTrimestres_preva%>%
  full_join(dispensed_drugs, relationship = "many-to-many")


adherencia <- adherencia %>%
  mutate(
    data_dispensed = as.Date(data_dispensed, format="%Y-%m-%d"),
    dinici = as.Date(dinici, format="%Y-%m-%d"),
    dfi = as.Date(dfi, format="%Y-%m-%d")
  )

# Now apply the mutate with between function
adherencia <- adherencia %>%
  mutate(ad_3mb = if_else(between(data_dispensed, dinici, dfi), 1, 0))

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



OFT <- Adherencia_selected_prevalentes_columnas %>%
  group_by(id_emb) %>%
  mutate(
    evento = ifelse((diferencia_dias >= 62) & (dfi >= data_dispensed), 1, 0),
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
km_grupo_A <- survfit(Surv(tiempo_semanas, evento) ~ 1, data = survival_time)

plot(km_grupo_A, col = "#FF4400", main = "antidepressants", 
     xlab = "Time (weeks)", ylab = "Probability of adherence", lwd = 2)

###EMBARAZOS INCIDENTES 
# DEFINIR INTERVALOS 
#definimos los intervalos para cada uno de los periodos de interés y observación 
intervalo_embarazo <- interval(denominadorTrimestres$dinici, denominadorTrimestres$dfi)
intervalo_prescripcion <- interval (denominadorTrimestres$dat, denominadorTrimestres$dbaixa)
intervalo_WASH <- interval (denominadorTrimestres$dinici-years(1), denominadorTrimestres$dinici)

denom_ITS_Inci <- farmacos %>%
  mutate(   deno_EMB = between(dinici, as.Date("2011-01-01"), as.Date("2020-06-30")) &
              between(dfi, as.Date("2011-01-01"), as.Date("2020-06-30")), 1, 0,
    EmB_washout = ifelse(!int_overlaps(intervalo_WASH, intervalo_prescripcion), 1,0))

deno_grupoN <- denom_ITS_Inci%>%
  mutate(
    deno_EMB = if_else(str_detect(cod,"^N06A"), deno_EMB * EmB_washout, deno_EMB))

numeradorTrimestres_Inci <- deno_grupoN %>% 
  mutate(
    #si hay solapameiento etre el periodo de interes y periodo de prescripcion=1, sino 0 
    E3mB_Inci = ifelse(int_overlaps(intervalo_embarazo, intervalo_prescripcion), 1,0))%>%
  #multiplicamos por los 
  mutate(
    E3mB_Inci=E3mB_Inci*deno_EMB*EmB_washout)%>%
  select("id_emb", "cod", "idp", "dinici","dfi", "dat", "dbaixa", "E3mB_Inci")

numeradorTrimestres_Inci_grupN <- numeradorTrimestres_Inci%>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A"))%>% 
  group_by(id_emb, cod)

#creamos BD adherencia
adherencia <- numeradorTrimestres_Inci_grupN%>%
  full_join(dispensed_drugs, relationship = "many-to-many")

#trimestres adherencia
adherencia_fechas <- adherencia %>%
  mutate(ad_EMB = if_else(between(data_dispensed, dinici, dfi), 1, 0)) 

Adherencia_multiplicado <- adherencia_fechas%>% 
  mutate(
    #si hay solapameiento etre el periodo de interes y periodo de prescripcion=1, sino 0 
    ad_EMB=ad_EMB*E3mB_Inci)%>%
  filter(ad_EMB == 1)%>%
  select("id_emb", "cod", "idp", "dinici", "dfi", "dat", "dbaixa", "data_dispensed")%>%
  distinct(id_emb, dinici, dfi, dat, dbaixa, data_dispensed, .keep_all = TRUE)%>%
  arrange(id_emb, data_dispensed)


#Eliminamos en primer lugar aquellas filas en cuyo mes se solape dfi y data dispensed (reintroducciones tras embarazo)
Adherencia_selected_incidentes <- Adherencia_multiplicado%>%
  group_by(id_emb) %>%
  mutate(
    prev_dbaixa = lag(dbaixa),
    cuenta = ifelse(is.na (prev_dbaixa) | prev_dbaixa >= dat | as.numeric(dat - prev_dbaixa) < 61, 1, 0)
  ) %>%
  ungroup()%>%
  filter(cuenta == 1)




Adherencia_selected_incidentes_columnas <- Adherencia_selected_incidentes %>%
  select(id_emb, idp, dinici, dfi, dat, dbaixa, data_dispensed) %>%
  group_by(id_emb) %>%
  arrange(id_emb, data_dispensed) %>%
  mutate(
    diferencia_dias = as.numeric(difftime(data_dispensed, lag(data_dispensed, default = NA), units = "days")),
    diferencia_dias = ifelse(is.na(diferencia_dias), 0, diferencia_dias),
    evento = ifelse(diferencia_dias >= 62, 1, 0)
  ) %>%
  select(id_emb, idp, dinici, dfi, dat, dbaixa, data_dispensed, diferencia_dias, evento) %>%
  ungroup()



# 
# duracion_tto = if_else(data_dispensed > dinici | dbaixa > dinici,
#                        as.numeric(difftime(pmin(data_dispensed, dbaixa), dat, units = "days")),
#                        0)

# OFT_inci <- Adherencia_selected_incidentes_columnas %>%
#       mutate(duracion_tto = ifelse(evento == 1),
#   # &(data_dispensed > dinici & dbaixa > dinici),  
#          as.numeric(difftime(pmin(data_dispensed, dbaixa), dinici, units = "days")),  
#          ifelse(
#              dbaixa > data_dispensed & pmin(dat, dbaixa) <= data_dispensed,
#              as.numeric(difftime(pmin(dfi, data_dispensed), dat, units = "days")),
#              as.numeric(difftime(pmin(dfi, dbaixa), dat, units = "days"))))



OFT_inci <- Adherencia_selected_incidentes_columnas %>%
  filter(dbaixa > data_dispensed) %>%
  filter(pmin(dat, dbaixa) <= data_dispensed) %>%
  mutate(
    evento = ifelse((diferencia_dias >= 62) & (dfi >= data_dispensed), 1, 0),
    duracion_tto = ifelse(
      evento == 1,
      as.numeric(difftime(data_dispensed, dat, units = "days")),  
      ifelse(
        dbaixa > data_dispensed, 
        as.numeric(difftime(dfi, dat, units = "days")),
        as.numeric(difftime(dbaixa,  dat, units = "days"))
      )
    )
  ) %>%
  group_by(id_emb) %>%
  mutate(cumulative_sum = cumsum(duracion_tto)) %>%
  mutate(OFT = ifelse(evento == 0, cumulative_sum, 
                      ifelse(evento == 1, lag(cumulative_sum, default = 0) + duracion_tto, duracion_tto))) %>%
  ungroup()

# Inspección del resultado final
head(OFT_inci)


OFT_inci <- OFT_inci %>%
  group_by(id_emb) %>%
  mutate(cumulative_sum = cumsum(duracion_tto)) %>%
  mutate(OFT = ifelse(evento == 1, duracion_tto, cumulative_sum)) %>%
  ungroup()


# Now try the summarise function again
resumen_oft_inci <- OFT_inci %>%
  group_by(id_emb) %>%
  summarise(
    dinici = first(dinici),
    dfi = first(dfi),
    evento = max(evento),  
    OFT = max(duracion_tto)
  )

#Creamos la base de datos para el KP 
survival_time_inci <- resumen_oft_inci %>%
  mutate(tiempo_semanas = OFT/7)  # Crear columna 'evento' basada en la condición

# Calcular las estimaciones de Kaplan-Meier
km_grupo_B <- survfit(Surv(tiempo_semanas, evento) ~ 1, data = survival_time_inci)


# Gráfico de Kaplan-Meier
plot(km_grupo_A, col = "#FF4400", main = "Figure 5: Persistence with antidepressants",
     xlab = "Time (weeks)", ylab = "Proportion of persistence", lwd = 2)
lines(km_grupo_B, col = "#4682B4", lwd = 2)

# Leyenda en la esquina inferior izquierda con texto pequeño
legend("bottomleft", legend = c("Prior Prescription", "No prior prescription"),
       col = c("#FF4400", "#4682B4"), lty = 1, lwd = 1, text.col = c("#000", "#000"),
       cex = 0.7)



#logranktest
# Suponiendo que survival_time y survival_time_inci son data frames que ya existen
# Añadir una columna "grupo" a cada data frame antes de combinarlos
survival_time <- survival_time %>%
  mutate(grupo = "survival_time")%>%
  select("id_emb", "tiempo_semanas", "evento", "OFT", "grupo")

survival_time_inci <- survival_time_inci %>%
  mutate(grupo = "survival_time_inci")%>%
  select("id_emb", "tiempo_semanas", "evento", "OFT", "grupo")

# Combinar los data frames con rbind
datos_combinados <- rbind(survival_time, survival_time_inci)

# Verificar la estructura de datos_combinados
str(datos_combinados)

# Ver los tiempos, las supervivencias y los grupos
summary_km$time
summary_km$surv
summary_km$strata

# Crear un data frame con los tiempos, las probabilidades de supervivencia y los grupos
df_persistencia <- data.frame(
  tiempo = summary_km$time,
  persistencia = summary_km$surv * 100
  # grupo = summary_km$strata
)

# Ver el data frame
print(df_persistencia)

# Calcular las diferencias entre las curvas de supervivencia usando varios métodos
log_rank_combinado <- survdiff(Surv(tiempo_semanas, evento) ~ grupo, data = datos_combinados)
# # # Call:
# Call:
#   survdiff(formula = Surv(tiempo_semanas, evento) ~ grupo, data = datos_combinados)
# 
# N Observed Expected (O-E)^2/E (O-E)^2/V
# grupo=survival_time      4099      816      484       228       421
# grupo=survival_time_inci 1716      290      622       177       421
# 
# Chisq= 421  on 1 degrees of freedom, p= <2e-16 

# Cargar la librería de supervivencia
library(survival)

# Ajustar un modelo de regresión de Cox
fit <- coxph(Surv(tiempo_semanas, evento) ~ grupo, data = datos_combinados)

# Resumen del modelo para obtener el HR
summary(fit)

# Call:
#   coxph(formula = Surv(tiempo_semanas, evento) ~ grupo, data = datos_combinados)
# 
# n= 5815, number of events= 1106 
# 
# coef exp(coef) se(coef)     z
# gruposurvival_time_inci -1.32769   0.26509  0.06917 -19.2
# Pr(>|z|)    
# gruposurvival_time_inci   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95
# gruposurvival_time_inci    0.2651      3.772    0.2315
# upper .95
# gruposurvival_time_inci    0.3036
# 
# Concordance= 0.653  (se = 0.007 )
# Likelihood ratio test= 425  on 1 df,   p=<2e-16
# Wald test            = 368.5  on 1 df,   p=<2e-16
# Score (logrank) test = 421.2  on 1 df,   p=<2e-16
# 
# > 

# Obtener la función de supervivencia acumulada en la semana 40
km_summaryA <- summary(km_grupo_A)
km_summaryB <- summary(km_grupo_B)

tiempo_40 <- 40  # Semana 40
tiempo_26 <- 26  # Semana 26

indice_tiempo_40_A <- which(km_summaryA$time >= tiempo_40)[1]
indice_tiempo_26_A <- which(km_summaryA$time >= tiempo_26)[1]

indice_tiempo_40_B <- which(km_summaryB$time >= tiempo_40)[1]
indice_tiempo_26_B <- which(km_summaryB$time >= tiempo_26)[1]


# Obtener la función de supervivencia acumulada en la semana 40
supervivencia_en_40A <- km_summaryA$surv[indice_tiempo_40_A]
supervivencia_en_40B <- km_summaryB$surv[indice_tiempo_40_B]


# Calcular el porcentaje de persistencia en la semana 40
porcentaje_persistencia_40A <- supervivencia_en_40A * 100
# 13.04567
porcentaje_persistencia_40B <- supervivencia_en_40B * 100
# 43.91617

# Obtener la función de supervivencia acumulada en la semana 26
supervivencia_en_26A <- km_summaryA$surv[indice_tiempo_26_A]
supervivencia_en_26B <- km_summaryB$surv[indice_tiempo_26_B]


# Calcular el porcentaje de persistencia en la semana 26
porcentaje_persistencia_26A <- supervivencia_en_26A * 100
# 51.15003
porcentaje_persistencia_26b <- supervivencia_en_26B * 100
# 87.58793
# Mostrar el porcentaje de persistencia en la semana 40
print(porcentaje_persistencia_40)

# Ver los tiempos, las supervivencias y los grupos
summary_km$time
summary_km$surv
median(summary_km$time)
median(summary_km$surv)
#Mediana grupo A 
mediana_tiempoA <- summary(km_grupo_A)$time[which.min(abs(summary(km_grupo_A)$surv - 0.5))]
# 26.57143

#Mediana grupo B
mediana_tiempob <- summary(km_grupo_B)$time[which.min(abs(summary(km_grupo_B)$surv - 0.5))]
# 39.85714
