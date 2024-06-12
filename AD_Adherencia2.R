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
install.packages(binom)
install.packages("DescTools")
library(exact2x2)
library (dplyr)
library(stringr)
library (lubridate)
library(stats)
library(epitools)
library(binom)
library(DescTools)
#GENERAL 
#ADHERENCIA EN GLOBAL EN TODOS LOS EMBARAZOS 
fechas_embarazo <- `repositori_LuciaVH_N_2023-06-09` %>% mutate(id_emb = row_number())

# Inicio base fechas embarazo
embarazo <- fechas_embarazo %>% select(idp, ctanca, cod_emb,id_emb)

##Filtramos las columnas de factuados por antidepresivos 
dispensed_drugs <-  `farmacs_facturats_LuciaVH_N_2023-06-09`%>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A"))%>%
  mutate(data_dispensed = dat)%>%
  select(idp,cod,env,data_dispensed)

#se une la BD de embarazo con la de farmacos prescrito 
farmacos <- dispensed_drugs %>% 
  full_join(fechas_embarazo, relationship = "many-to-many")%>%
  select(idp, id_emb, cod, data_dispensed, dinici, dfi)

##denominador prevalencia: embarazadas por periodo de tiempo
# Calcular el denominador de prevalencia para embarazadas
denominadorEMB <- farmacos %>%
  mutate(deno_embarazo = if_else(between(dinici, as.Date("2011-01-01"), as.Date("2020-06-30")) &
                                   between(dfi, as.Date("2011-01-01"), as.Date("2020-06-30")), 1, 0))

intervalo_emb<- interval (denominadorEMB$dinici, denominadorEMB$dfi)

numeradoremb  <- denominadorEMB %>%
  mutate(
    expo_EMB = if_else(between(data_dispensed, dinici, dfi), 1, 0),
    ## Convertimos las exposiciones a 0 si no hay denominador para esa exposición 
    expo_EMB = expo_EMB * deno_embarazo
  ) 

numeradoremb <- numeradoremb %>%
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A")) %>%
  filter(expo_EMB == 1)%>%  
  distinct(id_emb, .keep_all = T)

n <- nrow(numeradoremb)
prev_exp <- n/99605
poisson.exact(n, 99605, conf.level = 0.95)
#Número de embarazadas a las que se les ha prescrito 
N_embarazadas <- embarazo %>%
  distinct(idp, .keep_all = T)

numeradorpregnan <- numeradoremb %>%
  distinct(idp, .keep_all = T)

numeradopregnancollapsed <- numeradorpregnan%>%
  summarise(across(c("expo_EMB"), sum))

prevalencia_embarazadas <- numeradopregnancollapsed / 76459 
poisson.exact(nrow(numeradorpregnan), nrow(N_embarazadas), conf.level = 0.95)

#Adherencia primaria en mujeres prevalentes y mujeres incidentes.
#Dividimos por trimestres 
#se añaden columnas que calculan fechas 6 meses antes, 1 mes antes 
#columna 1 mes despues + 6 meses después 
fechas_embarazo <- `repositori_LuciaVH_N_2023-06-09` %>% mutate(three_monthB = dinici - 90,  # 3 meses antes                                                                
                                                                firstT = dinici + 90, 
                                                                secondT = dinici + 180, 
                                                                three_monthA = dfi + 90,  # 3 meses después
                                                                six_monthA = dfi +180,
                                                                id_emb = row_number())
# Inicio base fechas embarazo
embarazo <- fechas_embarazo %>% select(idp, ctanca, cod_emb,id_emb, three_monthB, dinici, dfi,              
                                       firstT, secondT, dfi,three_monthB, six_monthA)

#se une la BD de embarazo con la de farmacos prescrito 
farmacos <- `farmacs_prescrits_LuciaVH_N_2023-06-09` %>% 
  full_join(fechas_embarazo, relationship = "many-to-many")

##Seleccionamos columnas que nos interesen de farmacos prescritos y filtramos por antidepresivos
prescrip_drugs <- farmacos %>%
  select(idp, cod, dat, dbaixa, ctanca, cod_emb,id_emb, three_monthB, dinici, firstT, secondT, dfi, three_monthA, six_monthA) 
  # mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  # filter(grupo_N %in% paste0("N06A"))

##Filtramos las columnas de factuados por antidepresivos 
dispensed_drugs <-  `farmacs_facturats_LuciaVH_N_2023-06-09`%>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A"))%>%
  mutate(data_dispensed = dat)%>%
  select(idp,cod,env,data_dispensed)

#Calculamos denominador de prevalentes por trimestrs 
denominadorTrimestres <- prescrip_drugs %>%
  ##si la fecha 6mB esta entre inicio de datos disponibles y fecha de inicio: entra denominador
  ##sino no entra
  mutate(##si la fecha 3mB está entre inicio de periodo de observacion y dinici: entra en el denominador
         deno_3mB = if_else(between(three_monthB, as.Date("2011-01-01"), dinici), 1, 0),
         ##si la fecha 1mB está entre inicio de periodo de observacion y dinici: entra en el denominador
         #sino no entra: O 
         #y si no es anterior ni posterior a 1/2011 o 06/2011 respectivamente
         deno_firstT = if_else((dfi>= dinici) & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")) & between(dinici, as.Date("2010-12-31"), as.Date("2020-06-01")), 1, 0),
         ##si la fecha fin esta entre segundo y tercer trimestre: entra en 2T. Si ffin en primer t: no entra
         #y si no es anterior ni posterior a 1/2011 o 06/2011 respectivamente
         deno_secondT = if_else((between(dfi, firstT, secondT) | between(dfi, secondT, dfi)) & !between(dfi, dinici, firstT) & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")), 1, 0),
         ##Si la fecha fin es posterior a segundo trimestre y se encuentra entre 01/2011 y junio 2020
         #entra como denominador de 3er trimestre 
         deno_thirdT = if_else(dfi >= secondT & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")), 1, 0),
         ##Si el periodo de observacion entra en periodo de estudio: entra en 1mB  
         ##Si el periodo de observacion entra en periodo de estudio: entra en 3mB  
         deno_3mA = if_else(three_monthA < as.Date("2020-06-01"), 1, 0))

# DEFINIR INTERVALOS 
#definimos los intervalos para cada uno de los periodos de interés y observación 
#3 meses antes 
intervalo_3mB <- interval(denominadorTrimestres$three_monthB, denominadorTrimestres$dinici)
#primer trimestre: dinici-firstT 
intervalo_1T <- interval(denominadorTrimestres$dinici, denominadorTrimestres$firstT)
#segundo trimestre: firstT - secondt
intervalo_2T <- interval(denominadorTrimestres$firstT, denominadorTrimestres$secondT)
#tercer trimestre: secondt - dfi 
intervalo_3T <- interval(denominadorTrimestres$secondT, denominadorTrimestres$dfi)
#3 meses despues 
intervalo_3mA <- interval(denominadorTrimestres$dfi, denominadorTrimestres$three_monthA)
intervalo_prescripcion <- interval (prescrip_drugs$dat, prescrip_drugs$dbaixa)


#nueva base de datos denom
#calculamos el denominador total 
denom_nocolapsado <- denominadorTrimestres %>%  
  distinct(id_emb, .keep_all = T) %>%
  #seleccionamos las columnas que nos interesan
  select("idp","id_emb","deno_3mB","deno_firstT",
         "deno_secondT","deno_thirdT","deno_3mA") %>%
  #Filtra las filas duplicadas basadas en todas las columnas seleccionadas. 
  #Esto asegura que solo se consideren combinaciones únicas de "idp" e "id_emb".
  distinct() 

#sumamos los 0 y 1 de las columnas 
denom_colapsado <- denom_nocolapsado%>% 
  #sumamos el total de 1 en cada columna para calcular el total de denominadores 
  summarise(across(c("deno_3mB","deno_firstT","deno_secondT","deno_thirdT","deno_3mA"),sum))

# Solapamiento de intervalo de prescripcion: Si hay solapamiento expo: 1, sino expo: 0
numeradorTrimestres <- denominadorTrimestres %>%
  mutate(
    expo_3mB = ifelse(int_overlaps(intervalo_3mB, intervalo_prescripcion), 1, 0),
    expo_1T = ifelse(int_overlaps(intervalo_1T, intervalo_prescripcion), 1, 0),
    expo_2T = ifelse(int_overlaps(intervalo_2T, intervalo_prescripcion), 1, 0),
    expo_3T = ifelse(int_overlaps(intervalo_3T, intervalo_prescripcion), 1, 0),
    expo_3mA = ifelse(int_overlaps(intervalo_3mA, intervalo_prescripcion), 1, 0),
    ## Convertimos las exposiciones a 0 si no hay denominador para esa exposición 
    expo_3mB = expo_3mB * deno_3mB,
    expo_1T = expo_1T * deno_firstT,
    expo_2T = expo_2T * deno_secondT,
    expo_3T = expo_3T * deno_thirdT,
    expo_3mA = expo_3mA * deno_3mA)

numeradorTrimestres_preva <- numeradorTrimestres%>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A"))%>% 
  group_by(id_emb, cod)

#Calculamos adherencia en prevalentes 
#trimestres adherencia

#creamos BD adherencia
adherencia <- numeradorTrimestres_preva%>%
  full_join(dispensed_drugs, relationship = "many-to-many")

adherencia_fechas_prevalentes <- adherencia %>%
  mutate(ad_3mb = if_else(between(data_dispensed, three_monthB, dinici), 1, 0),
         ad_1T = if_else(between(data_dispensed, dinici, firstT), 1, 0),
         ad_2T = if_else(between(data_dispensed, firstT, secondT), 1, 0),
         ad_3T = if_else(between(data_dispensed, secondT, dfi), 1, 0),
         ad_3mA = if_else(between(data_dispensed, dfi, three_monthA), 1, 0)) 

Adherencia_multiplicado_prevalentes <- adherencia_fechas_prevalentes%>% 
  mutate(
    #si hay solapameiento etre el periodo de interes y periodo de prescripcion=1, sino 0 
    ad_3mb=ad_3mb*expo_3mB,
    ad_1T=ad_1T*expo_1T, 
    ad_2T= ad_2T*expo_2T,
    ad_3T=ad_3T*expo_1T, 
    ad_3mA= ad_3mA*expo_3mA)%>%
  select("id_emb", "cod", "idp", "ad_3mb","ad_1T","ad_2T","ad_3T","ad_3mA")

## Al agrupar por id_emb y cod y obtener el valor máximo para ciertas columnas
num_grupoNmax_preva <- Adherencia_multiplicado_prevalentes %>%
  group_by(id_emb, cod) %>%
  summarise(across(c("ad_3mb","ad_1T","ad_2T","ad_3T","ad_3mA"), max))

NUM_grupo_preva <- num_grupoNmax_preva%>% 
  group_by(id_emb)%>%  
  summarise(
    across(c("ad_3mb","ad_1T","ad_2T","ad_3T","ad_3mA"), max))

#sumamos a través de las columnas para calcular Ns 
N_colapsado_preva <- NUM_grupo_preva %>%
  #sumamos todos los denominadores
  summarise(across(c("ad_3mb","ad_1T","ad_2T","ad_3T","ad_3mA")
                   ,~sum(., na.rm = T)))

#Calculamos  reintroducción
N_reintroduccion <- NUM_grupo_preva %>% 
  mutate(reintro = if_else(ad_3mA == 1 & ad_3mb == 1 & (ad_1T == 0 | ad_2T == 0 | ad_3T == 0), 1, 0))%>%
  #sumamos todos los denominadores
  summarise(across(c("reintro")
                   ,~sum(., na.rm = T)))

poisson.exact(1603, 5494, conf.level = 0.95)

#Calculamos INCIDENTES ADHERENTES 
#Creamos los intervalos para definir la incidencia 
#Definimos intervalo de prescripción 
intervalo_prescrip <- interval (prescrip_drugs$dat, prescrip_drugs$dbaixa)
#DEFINIMOS INTERVALOS DE OSBERVACION
#3MESES ANTES  three monthB- dinici:   
intervalo_i3mB <- interval(prescrip_drugs$three_monthB, prescrip_drugs$dinici) 
#primer trimestre: dinici-firstT 
intervalo_i1T <- interval(prescrip_drugs$dinici, prescrip_drugs$firstT)
#segundo trimestre: firstT - secondt
intervalo_i2T <- interval(prescrip_drugs$firstT, prescrip_drugs$secondT)
#tercer trimestre: secondt - dfi 
intervalo_i3T <- interval(prescrip_drugs$secondT, prescrip_drugs$dfi)
#1 mes despues_ dfi: onemonthA
intervalo_i1mA <- interval(prescrip_drugs$dfi, prescrip_drugs$one_monthA)
#3 meses despues dfi: three monthA 
intervalo_i3mA <- interval(prescrip_drugs$dfi, prescrip_drugs$three_monthA)
#6 meses despues dfi- sixmonthA 
intervalo_i6mA <- interval(prescrip_drugs$dfi, prescrip_drugs$six_monthA)

#INTERVALOS DE WASHOUT 
#Hay uno para cada intervalo porque tiene que incluir el lo antes + trimestre anterior 
#3 meses antes
intervalo_w3mB <- interval(prescrip_drugs$dinici -years(1), prescrip_drugs$three_monthB) 
#primer trimestre: 1 año antes embarazo - dinici
intervalo_w1T <- interval(prescrip_drugs$dinici- years (1), prescrip_drugs$dinici)
#segundo trimestre: 1 año antes de embarazo hsata fecha de fin de primer trimestrs
intervalo_w2T <- interval(prescrip_drugs$dinici - years(1), prescrip_drugs$firstT)
#tercer trimestre: 1 año antes del embarazo hasta fecha fin de segundo trimestre
intervalo_w3T <- interval(prescrip_drugs$dinici-years(1), prescrip_drugs$secondT)
# despues_ dfi:  1 año antes del embarazo - despues del embarazo 
intervalo_wA <- interval(prescrip_drugs$dinici-years(1), prescrip_drugs$dfi)
##Calculamos denominador prevalentes 

#Si hay solapamiento entre el periodo de embarazo  
denom_ITS_Inci <- prescrip_drugs %>%
  mutate(##si la fecha 3mB está entre inicio de periodo de observacion y dinici: entra en el denominador
    #sino no entra: O 
    deno_3mB = if_else(between(three_monthB, as.Date("2011-01-01"), dinici), 1, 0),
    ##si la fecha fin esta en primer, segundo o tercer trimestre, entra en denominador 1T
    #y si no es anterior ni posterior a 1/2011 o 06/2011 respectivamente
    deno_firstT = if_else((dfi>= dinici) & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")) & between(dinici, as.Date("2010-12-31"), as.Date("2020-06-01")), 1, 0),
    ##si la fecha fin esta entre segundo y tercer trimestre: entra en 2T. Si ffin en primer t: no entra
    #y si no es anterior ni posterior a 1/2011 o 06/2011 respectivamente
    deno_secondT = if_else((between(dfi, firstT, secondT) | between(dfi, secondT, dfi)) & !between(dfi, dinici, firstT) & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")), 1, 0),
    ##Si la fecha fin es posterior a segundo trimestre y se encuentra entre 01/2011 y junio 2020
    #entra como denominador de 3er trimestre 
    deno_thirdT = if_else(dfi >= secondT & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")), 1, 0),
    ##Si el periodo de observacion entra en periodo de estudio: entra en 3mB  
    deno_3mA = if_else(three_monthA < as.Date("2020-06-01"), 1, 0),
    ##Si el periodo de observacion entra en periodo de estudio: entra en 6mB
    deno_6mA = if_else(six_monthA < as.Date("2020-06-01"), 1, 0),
    EmB_washout = ifelse(!int_overlaps(intervalo_w3mB, intervalo_prescrip), 1,0), 
    E1T_washout = ifelse(!int_overlaps(intervalo_w1T, intervalo_prescrip), 1,0), 
    E2T_washout = ifelse(!int_overlaps(intervalo_w2T, intervalo_prescrip), 1,0), 
    E3T_washout = ifelse(!int_overlaps(intervalo_w3T, intervalo_prescrip), 1,0), 
    EmA_washout = ifelse(!int_overlaps(intervalo_wA, intervalo_prescrip), 1,0))

#denominador por trimestres para antidepresivos en incidencia 
deno_grupoN <- denom_ITS_Inci%>%
  mutate(
    deno_3mB = if_else(str_detect(cod,"^N06A"), deno_3mB * EmB_washout, deno_3mB),
    deno_firstT = if_else(str_detect(cod,"^N06A"), deno_firstT * E1T_washout, deno_firstT),
    deno_secondT = if_else(str_detect(cod,"^N06A"), deno_secondT * E2T_washout, deno_secondT),
    deno_thirdT = if_else(str_detect(cod,"^N06A"), deno_thirdT * E3T_washout, deno_thirdT),
    deno_3mA = if_else(str_detect(cod,"^N06A"), deno_3mA * EmA_washout, deno_3mA),
    deno_6mA = if_else(str_detect(cod,"^N06A"), deno_6mA * EmA_washout, deno_6mA))

##Numerador para las prevalentes 
numeradorTrimestres_Inci <- deno_grupoN %>% 
  mutate(
    #si hay solapameiento etre el periodo de interes y periodo de prescripcion=1, sino 0 
    E3mB_Inci = ifelse(int_overlaps(intervalo_3mB, intervalo_prescrip), 1,0),
    E1T_Inci = ifelse(int_overlaps(intervalo_1T, intervalo_prescrip), 1,0),
    E2T_Inci = ifelse(int_overlaps(intervalo_2T, intervalo_prescrip), 1, 0),
    E3T_Inci = ifelse(int_overlaps(intervalo_3T, intervalo_prescrip), 1, 0),
    E3mA_Inci = ifelse(int_overlaps(intervalo_3mA, intervalo_prescrip), 1, 0))
##Numerador en las incidentes 
numeradorTrimestres_Inci <- deno_grupoN %>% 
  mutate(
    #si hay solapameiento etre el periodo de interes y periodo de prescripcion=1, sino 0 
    E3mB_Inci = ifelse(int_overlaps(intervalo_i3mB, intervalo_prescrip), 1,0),
    E1T_Inci = ifelse(int_overlaps(intervalo_i1T, intervalo_prescrip), 1,0),
    E2T_Inci = ifelse(int_overlaps(intervalo_i2T, intervalo_prescrip), 1, 0),
    E3T_Inci = ifelse(int_overlaps(intervalo_i3T, intervalo_prescrip), 1, 0),
    E3mA_Inci = ifelse(int_overlaps(intervalo_i3mA, intervalo_prescrip), 1, 0))%>%
  #multiplicamos por los 
  mutate(
    E3mB_Inci=E3mB_Inci*deno_3mB*EmB_washout,
    E1T_Inci=E1T_Inci*deno_firstT*E1T_washout, 
    E2T_Inci= E2T_Inci*deno_secondT*E2T_washout,
    E3T_Inci=E3T_Inci*deno_thirdT*E3T_washout, 
    E3mA_Inci= E3mA_Inci*deno_3mA*EmA_washout)%>%
  select("id_emb", "cod", "idp", "three_monthB","dinici", "firstT", "secondT", "dfi", "three_monthA", "E3mB_Inci",
         "E1T_Inci","E2T_Inci","E3T_Inci","E3mA_Inci")

numeradorTrimestres_Inci_grupN <- numeradorTrimestres_Inci%>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A"))%>% 
  group_by(id_emb, cod)

#creamos BD adherencia
adherencia <- numeradorTrimestres_Inci_grupN%>%
  full_join(dispensed_drugs, relationship = "many-to-many")

#trimestres adherencia
adherencia_fechas <- adherencia %>%
  mutate(ad_3mb = if_else(between(data_dispensed, three_monthB, dinici), 1, 0),
         ad_1T = if_else(between(data_dispensed, dinici, firstT), 1, 0),
         ad_2T = if_else(between(data_dispensed, firstT, secondT), 1, 0),
         ad_3T = if_else(between(data_dispensed, secondT, dfi), 1, 0),
         ad_3mA = if_else(between(data_dispensed, dfi, three_monthA), 1, 0)) 

Adherencia_multiplicado <- adherencia_fechas%>% 
  mutate(
    #si hay solapameiento etre el periodo de interes y periodo de prescripcion=1, sino 0 
    ad_3mb=ad_3mb*E3mB_Inci,
    ad_1T=ad_1T*E1T_Inci, 
    ad_2T= ad_2T*E2T_Inci,
    ad_3T=ad_3T*E3T_Inci, 
    ad_3mA= ad_3mA*E3mA_Inci)%>%
  select("id_emb", "cod", "idp", "ad_3mb","ad_1T","ad_2T","ad_3T","ad_3mA")

# Al agrupar por id_emb y cod y obtener el valor máximo para ciertas columnas
num_grupoNmax <- Adherencia_multiplicado %>%
  group_by(id_emb, cod) %>%
  summarise(
    across(c("ad_3mb","ad_1T","ad_2T","ad_3T","ad_3mA"), max))

#al agruparlo por id_emb, nos quedamos con el valor mínimo 
#así nos quitaremos del denominador
#los N que hayan sido prescritos el año anterior. 
NUM_grupoN<- num_grupoNmax%>% 
  group_by(id_emb)%>%  
  summarise(
    across(c("ad_3mb","ad_1T","ad_2T","ad_3T","ad_3mA"), max))

#sumamos a través de las columnas para calcular Ns 
N_colapsado <- NUM_grupoN %>%
  #sumamos todos los denominadores
  summarise(across(c("ad_3mb","ad_1T","ad_2T","ad_3T","ad_3mA")
                   ,~sum(., na.rm = T)))

#INCIDENCIA ACUMULADA 
incidenciagrupoN <- N_colapsado/AD_denocolapsado*100 

#INTERVALOS DE CONFIANZA AD 
# Vectores de numeradores
numerator_vectors <- list(
  N_colapsado$E3mB_Inci,
  N_colapsado$E1T_Inci,
  N_colapsado$E2T_Inci,
  N_colapsado$E3T_Inci,
  N_colapsado$E3mA_Inci,
  N_colapsado$E6mA_Inci
)

# Vectores de denominadores
denominator_vectors <- list(
  AD_denocolapsado$deno_3mB,
  AD_denocolapsado$deno_firstT,
  AD_denocolapsado$deno_secondT,
  AD_denocolapsado$deno_thirdT,
  AD_denocolapsado$deno_3mA,
  AD_denocolapsado$deno_6mA
)

# Reemplazar NA con 0 en todos los vectores
#Por que me salen NA aqui??? 
numerator_vectors <- lapply(numerator_vectors, function(x) { x[is.na(x)] <- 0; return(x) })
denominator_vectors <- lapply(denominator_vectors, function(x) { x[is.na(x)] <- 0; return(x) })

# Calcular los intervalos de confianza exactos de Poisson para todos los vectores
conf_int_exact <- lapply(1:length(numerator_vectors), function(i) {
  poisson.exact(sum(numerator_vectors[[i]]), sum(denominator_vectors[[i]]), conf.level = 0.95)
})

# Imprimir los intervalos de confianza
for (i in 1:length(conf_int_exact)) {
  print(conf_int_exact[[i]])
  cat("\n")
}

#Adherencia durante la fase de implementación (MPR)
#Persitencia y discontinuación (Kaplan-Meier)