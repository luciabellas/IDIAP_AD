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
library(binom)
library(dplyr)
library(exact2x2)
library (dplyr)
library(stringr)
library (lubridate)
library(stats)
library(epitools)
library(binom)
library(DescTools)
#GENERAL 
#PREVALENCIA 
#se añaden columnas que calculan fechas 6 meses antes, 1 mes antes 
#columna 1 mes despues + 6 meses después 
fechas_embarazo <- `repositori_LuciaVH_N_2023-06-09` %>% mutate(six_monthB= dinici- 180,
                                                                three_monthB = dinici - 90,  # 3 meses antes
                                                                one_monthB= dinici - 30,
                                                                firstT = dinici + 90, 
                                                                secondT = dinici + 180, 
                                                                one_monthA = dfi+ 30,
                                                                three_monthA = dfi + 90,  # 3 meses después
                                                                six_monthA = dfi +180,
                                                                id_emb = row_number())
# Inicio base fechas embarazo
embarazo <- fechas_embarazo %>% select(idp, ctanca, cod_emb,id_emb, six_monthB, three_monthB, one_monthB, dinici, dfi,              
                                       firstT, secondT, dfi, one_monthA,three_monthB, six_monthA)

#se une la BD de embarazo con la de farmacos prescrito 
farmacos <- `farmacs_prescrits_LuciaVH_N_2023-06-09` %>% 
  full_join(fechas_embarazo, relationship = "many-to-many")

##Seleccionamos columnas que nos interesen 
prescrip_drugs <- farmacos %>%
  select(idp, cod, dat, dbaixa, ctanca, cod_emb,id_emb, six_monthB, three_monthB, one_monthB, dinici, firstT, secondT, dfi, 
         one_monthA, three_monthA, six_monthA)

##denominador prevalencia: embarazadas por periodo de tiempo
#todos aquellos periodos de observacion completos (-6 m, -1m, 1t,2t,3t,1m+,6+)entraran como denominador 
#si no tienen denominador: no entraran. 
#creamos nuevas columnas en forma de 0 y 1 que nos servirán para calcular posteriormente el denominador 
denominadorTrimestres <- prescrip_drugs %>%
  ##si la fecha 6mB esta entre inicio de datos disponibles y fecha de inicio: entra denominador
  ##sino no entra
  mutate(deno_6mB = if_else(between(six_monthB, as.Date("2011-01-01"), dinici), 1, 0),
         ##si la fecha 3mB está entre inicio de periodo de observacion y dinici: entra en el denominador
         #sino no entra: O 
         deno_3mB = if_else(between(three_monthB, as.Date("2011-01-01"), dinici), 1, 0),
         ##si la fecha 1mB está entre inicio de periodo de observacion y dinici: entra en el denominador
         #sino no entra: O 
         deno_1mB = if_else(between(one_monthB, as.Date("2011-01-01"), dinici), 1, 0),
         ##si la fecha fin esta en primer, segundo o tercer trimestre, entra en denominador 1T
         #y si no es anterior ni posterior a 1/2011 o 06/2011 respectivamente
         deno_firstT = if_else((dfi>= dinici) & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")) & between(dinici, as.Date("2010-12-31"), as.Date("2020-06-01")), 1, 0),
         ##si la fecha fin esta entre segundo y tercer trimestre: entra en 2T. Si ffin en primer t: no entra
         #y si no es anterior ni posterior a 1/2011 o 06/2011 respectivamente
         deno_secondT = if_else((between(dfi, firstT, secondT) | between(dfi, secondT, dfi)) & !between(dfi, dinici, firstT) & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")), 1, 0),
         ##Si la fecha fin es posterior a segundo trimestre y se encuentra entre 01/2011 y junio 2020
         #entra como denominador de 3er trimestre 
         deno_thirdT = if_else(dfi >= secondT & between(dfi, as.Date("2011-01-01"), as.Date("2020-06-01")), 1, 0),
         ##Si el periodo de observacion entra en periodo de estudio: entra en 1mB  
         deno_1mA = if_else(one_monthA < as.Date("2020-06-01"), 1, 0),
         ##Si el periodo de observacion entra en periodo de estudio: entra en 3mB  
         deno_3mA = if_else(three_monthA < as.Date("2020-06-01"), 1, 0),
         ##Si el periodo de observacion entra en periodo de estudio: entra en 6mB
         deno_6mA = if_else(six_monthA < as.Date("2020-06-01"), 1, 0))

#nueva base de datos denom
#calculamos el denominador total 
denom_nocolapsado <- denominadorTrimestres %>%  
  distinct(id_emb, .keep_all = T) %>%
  #seleccionamos las columnas que nos interesan
  select("idp","id_emb","deno_6mB","deno_3mB", "deno_1mB","deno_firstT",
         "deno_secondT","deno_thirdT","deno_1mA","deno_3mA","deno_6mA") %>%
  #Filtra las filas duplicadas basadas en todas las columnas seleccionadas. 
  #Esto asegura que solo se consideren combinaciones únicas de "idp" e "id_emb".
  distinct() 

#sumamos los 0 y 1 de las columnas 
denom_colapsado <- denom_nocolapsado%>% 
  #sumamos el total de 1 en cada columna para calcular el total de denominadores 
  summarise(across(c("deno_6mB","deno_3mB", "deno_1mB",
                     "deno_firstT","deno_secondT","deno_thirdT","deno_1mA","deno_3mA","deno_6mA"),sum))

#Numerador de prevalencia
# DEFINIR INTERVALOS 
#definimos los intervalos para cada uno de los periodos de interés y observación 
#6 meses antes : desde fecha six_monthB a dinici
intervalo_6mB <- interval(denominadorTrimestres$six_monthB, denominadorTrimestres$dinici)
#3 meses antes 
intervalo_3mB <- interval(denominadorTrimestres$three_monthB, denominadorTrimestres$dinici)
#1 meses antes : desde fecha one_monthB a dinici
intervalo_1mB <- interval(denominadorTrimestres$one_monthB, denominadorTrimestres$dinici)
#primer trimestre: dinici-firstT 
intervalo_1T <- interval(denominadorTrimestres$dinici, denominadorTrimestres$firstT)
#segundo trimestre: firstT - secondt
intervalo_2T <- interval(denominadorTrimestres$firstT, denominadorTrimestres$secondT)
#tercer trimestre: secondt - dfi 
intervalo_3T <- interval(denominadorTrimestres$secondT, denominadorTrimestres$dfi)
#1 mes despues_ dfi: onemonthA
intervalo_1mA <- interval(denominadorTrimestres$dfi, denominadorTrimestres$one_monthA)
#3 meses despues 
intervalo_3mA <- interval(denominadorTrimestres$dfi, denominadorTrimestres$three_monthA)
#6 meses despues dfi- sixmonthA 
intervalo_6mA <- interval(denominadorTrimestres$dfi, denominadorTrimestres$six_monthA)
#intervalo de prescripcion va desde inincio dat inici a dbaixa 
intervalo_prescripcion <- interval(denominadorTrimestres$dat, denominadorTrimestres$dbaixa)

# Solapamiento de intervalo de prescripcion: Si hay solapamiento expo: 1, sino expo: 0
numeradorTrimestres <- denominadorTrimestres %>%
  mutate(
    expo_6mB = ifelse(int_overlaps(intervalo_6mB, intervalo_prescripcion), 1, 0),
    expo_3mB = ifelse(int_overlaps(intervalo_3mB, intervalo_prescripcion), 1, 0),
    expo_1mB = ifelse(int_overlaps(intervalo_1mB, intervalo_prescripcion), 1, 0), 
    expo_1T = ifelse(int_overlaps(intervalo_1T, intervalo_prescripcion), 1, 0),
    expo_2T = ifelse(int_overlaps(intervalo_2T, intervalo_prescripcion), 1, 0),
    expo_3T = ifelse(int_overlaps(intervalo_3T, intervalo_prescripcion), 1, 0),
    expo_1mA = ifelse(int_overlaps(intervalo_1mA, intervalo_prescripcion), 1, 0),
    expo_3mA = ifelse(int_overlaps(intervalo_3mA, intervalo_prescripcion), 1, 0),
    expo_6mA = ifelse(int_overlaps(intervalo_6mA, intervalo_prescripcion), 1, 0),
    ## Convertimos las exposiciones a 0 si no hay denominador para esa exposición 
    expo_6mB = expo_6mB * deno_6mB,
    expo_3mB = expo_3mB * deno_3mB,
    expo_1mB = expo_1mB * deno_1mB,
    expo_1T = expo_1T * deno_firstT,
    expo_2T = expo_2T * deno_secondT,
    expo_3T = expo_3T * deno_thirdT,
    expo_1mA = expo_1mA * deno_1mA,
    expo_3mA = expo_3mA * deno_3mA,
    expo_6mA = expo_6mA * deno_6mA
  )


#creamos bd numerador
num <- numeradorTrimestres %>% 
  #agrupamos por id de embarazo y codigo del farmaco 
  group_by(id_emb, cod) %>%
  #sumamos cada una de las filas 
  summarise(across(c("expo_6mB","expo_3mB","expo_1mB","expo_1T","expo_2T","expo_3T",
                     "expo_1mA","expo_3mA","expo_6mA"), max))


# Antidepresivos: tots
antidepresivos_num_nocolapsado <- num %>%
  mutate(grupo_N = substr(cod, start = 1, stop = 3)) %>%
  filter(grupo_N %in% c("N06")) %>%
  group_by(id_emb, grupo_N) %>%
  summarise(across(c("expo_6mB", "expo_3mB", "expo_1mB", "expo_1T", "expo_2T", "expo_3T",
                     "expo_1mA", "expo_3mA", "expo_6mA"), max)) %>%
  group_by(id_emb) %>%
  summarise(across(c("expo_6mB", "expo_3mB", "expo_1mB", "expo_1T", "expo_2T", "expo_3T",
                     "expo_1mA", "expo_3mA", "expo_6mA"), max))

#colapsamos Para obtener el N de episodios de embarazo en cada etapa del embarazo 
AD_numcolapsado <- antidepresivos_num_nocolapsado%>%
  #sumamos el total de 1 en cada una de las filas de exposicion 
  summarise(across(c("expo_6mB","expo_3mB", "expo_1mB","expo_1T","expo_2T","expo_3T","expo_1mA",
                     "expo_3mA","expo_6mA"),sum))

#calculo de prevalencia total para todo el grupo N 
prevalencia_total_AD <- AD_numcolapsado/denom_colapsado*100      

# #Intervalos de confianza para cada uno de los periodos 
# los eventos (en este caso, los eventos "1" de los vectores) 
# siguen una distribución de Poisson con una tasa lambda desconocida y 
# después estimamos esa tasa lambda a partir de los datos disponibles 
#Vectores que contienen los valores 0 y 1 por cada periodo de tiempo
#Numeradores de AD
vector_AD6mb <- antidepresivos_num_nocolapsado$expo_6mB
vector_AD3mb <- antidepresivos_num_nocolapsado$expo_3mB
vector_AD1mb <- antidepresivos_num_nocolapsado$expo_1mB
vector_AD1T <- antidepresivos_num_nocolapsado$expo_1T
vector_AD2T <- antidepresivos_num_nocolapsado$expo_2T
vector_AD3T <- antidepresivos_num_nocolapsado$expo_3T
vector_AD1mA <- antidepresivos_num_nocolapsado$expo_1mA
vector_AD3mA <- antidepresivos_num_nocolapsado$expo_3mA
vector_AD6mA <- antidepresivos_num_nocolapsado$expo_6mA
#Denominadores
#mismos vectores que previamente
# Calcular el intervalo de confianza exacto de Poisson
conf_int_exact_6mB <- poisson.exact(sum(vector_AD6mb), sum(vector_DENOM6mb), conf.level = 0.95)
print(conf_int_exact_6mB)
conf_int_exact_3mB <- poisson.exact(sum(vector_AD3mb), sum(vector_DENOM3mb), conf.level = 0.95)
print(conf_int_exact_3mB)
conf_int_exact_1mB <- poisson.exact(sum(vector_AD1mb), sum(vector_DENOM1mb), conf.level = 0.95)
print(conf_int_exact_1mB)
conf_int_exact_1T<- poisson.exact(sum(vector_AD1T), sum(vector_DENOM1T), conf.level = 0.95)
print(conf_int_exact_1T)
conf_int_exact_2T<- poisson.exact(sum(vector_AD2T), sum(vector_DENOM2T), conf.level = 0.95)
print(conf_int_exact_2T)
conf_int_exact_3T <- poisson.exact(sum(vector_AD3T), sum(vector_DENOM3T), conf.level = 0.95)
print(conf_int_exact_3T)
conf_int_exact_1mA <- poisson.exact(sum(vector_AD1mA), sum(vector_DENOM1mA), conf.level = 0.95)
print(conf_int_exact_1mA)
conf_int_exact_3mA <- poisson.exact(sum(vector_AD3mA), sum(vector_DENOM3mA), conf.level = 0.95)
print(conf_int_exact_3mA)
conf_int_exact_6mA <- poisson.exact(sum(vector_AD6mA), sum(vector_DENOM6mA), conf.level = 0.95)
print(conf_int_exact_6mA)

#INCIDENCIA  

#Dividimos por trimestres 
fechas_embarazo <- `repositori_LuciaVH_N_2023-06-09` %>% mutate(three_monthB = dinici - 90,                                                                three_monthB = dinici - 90,  # 3 meses antes
                                                                firstT = dinici + 90, 
                                                                secondT = dinici + 180, 
                                                                three_monthA = dfi + 90,  # 3 meses después
                                                                six_monthA = dfi +180,
                                                                id_emb = row_number())
#se une la BD de embarazo con la de farmacos prescrito 
farmacos <- `farmacs_prescrits_LuciaVH_N_2023-06-09` %>% 
  full_join(fechas_embarazo, relationship = "many-to-many")

##Seleccionamos columnas que nos interesen 
prescrip_drugs <- farmacos %>%
  select(idp, cod, dat, dbaixa, ctanca, cod_emb,id_emb, three_monthB, dinici, 
         firstT, secondT, dfi, three_monthA, six_monthA)

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

#Calculamos denominadores 
#Si hay solapamiento entre el periodo de em 
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

#denominador por trimestres para antidepresivos 
deno_grupoN <- denom_ITS_Inci %>%
  mutate(
    deno_3mB = if_else(str_detect(cod,"^N06A"), deno_3mB * EmB_washout, deno_3mB),
    deno_firstT = if_else(str_detect(cod,"^N06A"), deno_firstT * E1T_washout, deno_firstT),
    deno_secondT = if_else(str_detect(cod,"^N06A"), deno_secondT * E2T_washout, deno_secondT),
    deno_thirdT = if_else(str_detect(cod,"^N06A"), deno_thirdT * E3T_washout, deno_thirdT),
    deno_3mA = if_else(str_detect(cod,"^N06A"), deno_3mA * EmA_washout, deno_3mA),
    deno_6mA = if_else(str_detect(cod,"^N06A"), deno_6mA * EmA_washout, deno_6mA))

#nos quedamos con el denominador máximo
#Al agurpar por id_emb y cod
deno_grupoNmax <- deno_grupoN %>%  
  select("idp", "id_emb", "cod", "deno_3mB", "deno_firstT", "deno_secondT",
         "deno_thirdT","deno_3mA","deno_6mA") %>% 
  group_by(id_emb, cod) %>%
  #nos quedamos con el valor máximo de cada uno de los códigos 
  summarise(across(c("deno_3mB", "deno_firstT", "deno_secondT","deno_thirdT","deno_3mA","deno_6mA"), max))

#al agruparlo por id_emb, nos quedamos con el valor mínimo 
#así nos quitaremos del denominador
#los N que hayan sido prescritos el año anterior. 
deno_grupoNmin <- deno_grupoNmax%>%
  group_by(id_emb)%>%  
  summarise(across(c("deno_3mB", "deno_firstT", "deno_secondT","deno_thirdT","deno_3mA","deno_6mA"), min))%>%
  select("deno_3mB", "deno_firstT", "deno_secondT","deno_thirdT","deno_3mA","deno_6mA")

#colapsamos las columnas 
AD_denocolapsado <- deno_grupoNmin %>%
  summarise(across(c("deno_3mB", "deno_firstT", "deno_secondT", "deno_thirdT", "deno_3mA", "deno_6mA"), 
                   ~ sum(., na.rm = TRUE)))

#NUMERADOR PARA AD  
#Calculamos si el episodio de solapamiento coincide 
numeradorTrimestres_Inci <- deno_grupoN %>% 
  mutate(
    #si hay solapameiento etre el periodo de interes y periodo de prescripcion=1, sino 0 
    E3mB_Inci = ifelse(int_overlaps(intervalo_i3mB, intervalo_prescrip), 1,0),
    E1T_Inci = ifelse(int_overlaps(intervalo_i1T, intervalo_prescrip), 1,0),
    E2T_Inci = ifelse(int_overlaps(intervalo_i2T, intervalo_prescrip), 1, 0),
    E3T_Inci = ifelse(int_overlaps(intervalo_i3T, intervalo_prescrip), 1, 0),
    E3mA_Inci = ifelse(int_overlaps(intervalo_i3mA, intervalo_prescrip), 1, 0),
    E6mA_Inci= ifelse(int_overlaps(intervalo_i6mA, intervalo_prescrip), 1, 0))%>%
  #multiplicamos por los 
  mutate(
    E3mB_Inci=E3mB_Inci*deno_3mB*EmB_washout,
    E1T_Inci=E1T_Inci*deno_firstT*E1T_washout, 
    E2T_Inci= E2T_Inci*deno_secondT*E2T_washout,
    E3T_Inci=E3T_Inci*deno_thirdT*E3T_washout, 
    E3mA_Inci= E3mA_Inci*deno_3mA*EmA_washout, 
    E6mA_Inci=E6mA_Inci*deno_6mA*EmA_washout)%>%
  select("id_emb", "cod", "idp", "E3mB_Inci","E1T_Inci","E2T_Inci","E3T_Inci","E3mA_Inci", "E6mA_Inci")

# Al agrupar por id_emb y cod y obtener el valor máximo para ciertas columnas
num_grupoNmax <- numeradorTrimestres_Inci %>%
  group_by(id_emb, cod) %>%
  summarise(
    across(c("E3mB_Inci", "E1T_Inci", "E2T_Inci", "E3T_Inci", "E3mA_Inci", "E6mA_Inci"), max))

#al agruparlo por id_emb, nos quedamos con el valor mínimo 
#así nos quitaremos del denominador
#los N que hayan sido prescritos el año anterior. 
NUM_grupoN<- num_grupoNmax%>% 
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A")) %>%
  group_by(id_emb)%>%  
  summarise(
    across(c("E3mB_Inci", "E1T_Inci", "E2T_Inci", "E3T_Inci", "E3mA_Inci", "E6mA_Inci"), max))

#sumamos a través de las columnas para calcular Ns 
N_colapsado <- NUM_grupoN %>%
  #sumamos todos los denominadores
  summarise(across(c("E3mB_Inci", "E1T_Inci","E2T_Inci","E3T_Inci","E3mA_Inci","E6mA_Inci")
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

# CARACTERÍSTICAS SOCIODEMOGRÁFICAS DE LAS MADRES 
#se añaden columnas que calculan fechas 6 meses antes, 1 mes antes 
#columna 1 mes despues + 6 meses después 
fechas_embarazo <- `repositori_LuciaVH_N_2023-06-09` %>% mutate(id_emb = row_number())

# Inicio base fechas embarazo
embarazo <- fechas_embarazo %>% select(idp, ctanca, cod_emb,id_emb, dinici, dfi)

#se une la BD de embarazo con la de farmacos prescrito 
farmacos <- `farmacs_prescrits_LuciaVH_N_2023-06-09` %>% 
  full_join(fechas_embarazo, relationship = "many-to-many") 


# Seleccionar las columnas de interés
prescrip_drugs <- farmacos %>%
  select(idp, cod, dat, dbaixa, ctanca, cod_emb, id_emb, dinici, dfi, durada)

# Calcular el denominador de prevalencia para embarazadas
denominadorEMB <- prescrip_drugs %>%
  mutate(deno_embarazo = if_else(between(dinici, as.Date("2011-01-01"), as.Date("2020-06-30")) &
                                   between(dfi, as.Date("2011-01-01"), as.Date("2020-06-30")), 1, 0))

# DEFINIR INTERVALOS 
#definimos los intervalos para cada uno de los periodos de interés y observación 
#episodio de embarazo 
intervalo_emb <- interval(prescrip_drugs$dinici, prescrip_drugs$dfi)
#intervalo de prescripcion va desde inincio dat inici a dbaixa 
intervalo_heaprescripcion <- interval(denominadorEMB$dat, denominadorEMB$dbaixa)
#códigos de embarazo a los que se han prescrito farmacos del grupo N 

# Solapamiento de intervalo de prescripcion: Si hay solapamiento expo: 1, sino expo: 0
numeradoremb  <- denominadorEMB %>%
  mutate(
    expo_EMB = ifelse(int_overlaps(intervalo_emb, intervalo_prescripcion), 1, 0),
    ## Convertimos las exposiciones a 0 si no hay denominador para esa exposición 
    expo_EMB = expo_EMB * deno_embarazo
  )

numeradoremb <- numeradoremb %>%
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A")) %>%
  filter(expo_EMB == 1)%>%  
  distinct(id_emb, .keep_all = T)%>%
  select(idp, id_emb, cod, dat, ctanca, durada, dinici, dfi, expo_EMB)
#deben ser 14815 embarazos expuestos 

n <- nrow(numeradoremb)
prev_exp <- n/99605
poisson.exact(n, 99605, conf.level = 0.95)
#Número de embarazadas a las que se les ha prescrito 
N_embarazadas <- embarazo %>%
  mutate(deno_embarazo = if_else(between(dinici, as.Date("2011-01-01"), as.Date("2020-06-30")) &
                                   between(dfi, as.Date("2011-01-01"), as.Date("2020-06-30")), 1, 0)) %>%  
  distinct(idp, .keep_all = T)

numeradorpregnan <- numeradoremb %>%
  distinct(idp, .keep_all = T)

numeradopregnancollapsed <- numeradorpregnan%>%
  summarise(across(c("expo_EMB"), sum))
prevalencia_embarazadas <- numeradopregnancollapsed / N_embarazadas_coll 
poisson.exact(nrow(numeradorpregnan), nrow(N_embarazadas), conf.level = 0.95)

#Media y mediana edad madres en el episodio de embarazo 
edad <- numeradoremb %>%
  left_join(`poblacio_LuciaVH_N_2023-06-09`, relationship = "many-to-many") %>%
  mutate(edad = as.numeric(dinici-dnaix)/365)

# Media y mediana de la variable "edad"
media_edad <- mean(edad$edad)
desviacion_edad <- sd(edad$edad)
mediana <- median(edad$edad)
IQR(edad$edad)
q1 <- quantile(edad$edad, 0.25)
q3 <- quantile(edad$edad, 0.75)

# Media y mediana de la variable "durada"
media_dur <- mean(edad$durada)
desviacion_dur <- sd(edad$durada)
mediana <- median(edad$durada)
IQR(edad$durada)
q1d<- quantile(edad$durada, 0.25)
q3d <- quantile(edad$durada, 0.75)


#ESTATUS SOCIOECONÓMICO 

#QMEDEA/RURALITAT 
qmedea <- numeradoremb %>%
  left_join(`variables_socioeconomiques_LuciaVH_N_2023-06-09`, relationship = "many-to-many")

#Proporcion e intervalos de confianza
tabla.qmedea <- table(qmedea$qmedea)
proporciones <- prop.table(tabla.qmedea)
proporciones
tabla.ruralidad <- table(qmedea$ruralitat)
tabla.ruralidad_df <- as.data.frame(tabla.ruralidad)
prop_rurarit <- prop.table(tabla.ruralidad)
prop_rurarit
#qemedea
prop_u1  <- 0.09011366
prop_u2  <- 0.12962503
prop_u3 <- 0.14203691 
prop_u4 <- 0.16296316 
prop_u5 <- 0.18879514 
#rural
prop_U <- 0.799595005
prop_R <- 0.199190010
# Tamaño de muestra
n <- nrow(numeradoremb)
# Desviación estándar para cada proporción
se_u1 <- sqrt(prop_u1 * (1 - prop_u1) / n)
se_u2 <- sqrt(prop_u2 * (1 - prop_u2) / n)
se_u3 <- sqrt(prop_u3 * (1 - prop_u3) / n)
se_u4 <- sqrt(prop_u4 * (1 - prop_u4) / n)
se_u5 <- sqrt(prop_u5 * (1 - prop_u5) / n)

se_R <- sqrt(prop_R * (1 - prop_R) / n)
se_U <- sqrt(prop_U * (1 - prop_U) / n)

# Valor crítico Z para un nivel de confianza del 95%
z <- qnorm(0.975)

# Calcular los intervalos de confianza para las proporciones
ci_u1 <- c(prop_u1 - z * se_u1, prop_u1 + z * se_u1)*100
ci_u2 <- c(prop_u2 - z * se_u2, prop_u2 + z * se_u2)*100
ci_u3 <- c(prop_u3 - z * se_u3, prop_u3 + z * se_u3)*100
ci_u4 <- c(prop_u4 - z * se_u4, prop_u4 + z * se_u4)*100
ci_u5 <- c(prop_u5 - z * se_u5, prop_u5 + z * se_u5)*100
ci_U <- c(prop_U - z * se_U, prop_U + z * se_U)*100
ci_R <- c(prop_R - z * se_R, prop_R + z * se_R)*100

#TABAQUISMO
tabaquismo <- numeradoremb %>%
  left_join(`tabaquisme_LuciaVH_N_2023-06-09`, relationship = "many-to-many") %>%
  mutate(tab = if_else(dat >= (dinici - years(1)), val, NA))%>%  
  group_by(idp, id_emb) %>%
  summarise(across(c("tab"), max)) 
ntotal <- nrow(tabaquismo)

#Proporcion e intervalos de confianza
tabla.tabaco <- table(tabaquismo$tab)
calculo_missing <- sum(!complete.cases(tabaquismo$tab))
prop_0  <- 1247/ntotal
prop_1  <- 412/ntotal
prop_2  <- 340/ntotal 
prop_missing <- calculo_missing/ntotal

# Desviación estándar para cada proporción
se_0 <- sqrt(prop_0 * (1 - prop_0) / n)
se_1 <- sqrt(prop_1 * (1 - prop_1) / n)
se_2 <- sqrt(prop_2 * (1 - prop_2) / n)
se_missing <- sqrt(prop_missing * (1 - prop_missing) / n)

# Valor crítico Z para un nivel de confianza del 95%
z <- qnorm(0.975)

# Calcular los intervalos de confianza para las proporciones
ci_0 <- c(prop_0 - z * se_0, prop_0 + z * se_0)
ci_1 <- c(prop_1 - z * se_1, prop_1 + z * se_1)
ci_2 <- c(prop_2 - z * se_2, prop_2 + z * se_2)
ci_missing <- c(prop_missing - z * se_missing, prop_missing + z * se_missing)

##RESULTADO GESTACIONAL 
#Codi tancament 
table(numeradoremb$ctanca)
ctanca.tabla <- table(numeradoremb$ctanca)
prop.table(ctanca.tabla)
aborto_total_n <- 3448 + 214
aborto_total_prop <- 0.2327370908+0.0144448194

#Intervalos de confianza 
# Datos de las proporciones
prop_coditancament <- prop_coditancament %>% 
  mutate(prop_avortament_total= prop_coditancament$prop_avortament+prop_coditancament$prop_ive )

# Tamaño de muestra
n <- nrow(numeradoremb)

# Desviación estándar para cada proporción
se_part <- sqrt(prop_part * (1 - prop_part) / n)
se_cesaria <- sqrt(prop_cesaria * (1 - prop_cesaria) / n)
se_prematuritat <- sqrt(prop_prematuritat * (1 - prop_prematuritat) / n)
se_avortament <- sqrt(prop_avortament * (1 - prop_avortament) / n)
se_ive <- sqrt(prop_ive * (1 - prop_ive) / n)
se_mf <- se_ive <- sqrt(prop_mf* (1 - prop_mf) / n)
se_emb_ect <- sqrt(prop_emb_ect * (1 - prop_emb_ect) / n)
se_mh <- se_mh <- sqrt(prop_mh * (1 - prop_mh) / n)
se_abort_total <-  sqrt(aborto_total_prop * (1 - aborto_total_prop) / n)

# Valor crítico Z para un nivel de confianza del 95%
z <- qnorm(0.975)

# Calcular los intervalos de confianza para las proporciones
ci_part <- c(prop_part - z * se_part, prop_part + z * se_part)*100
ci_cesaria <- c(prop_cesaria - z * se_cesaria, prop_cesaria + z * se_cesaria)*100
ci_prematuritat <- c(prop_prematuritat - z * se_prematuritat, prop_prematuritat + z * se_prematuritat)*100
ci_avortament <- c(prop_avortament - z * se_avortament, prop_avortament + z * se_avortament)*100
ci_ive <- c(prop_ive - z * se_ive, prop_ive + z * se_ive)*100
ci_mf <- c(prop_mf - z * se_mf, prop_mf+ z * se_mf)*100
ci_emb_ect <- c(prop_emb_ect - z * se_emb_ect, prop_emb_ect + z * se_emb_ect)*100
ci_mh <-c(prop_mh - z * se_mh, prop_mh + z * se_mh)*100
ci_abort_total <- c(aborto_total_prop - z * se_abort_total, aborto_total_prop + z * se_abort_total)*100


#COMORBILIDADES 
intervalo_emb <- (denominadorEMB$dinici-denominadorEMB$dfi)
intervalo_prescripcion <- (denominadorEMB$dat-denominadorEMB$dbaixa)

numeradoremb  <- denominadorEMB %>%
  mutate(
    expo_EMB = ifelse(int_overlaps(intervalo_emb, intervalo_prescripcion), 1, 0),
    ## Convertimos las exposiciones a 0 si no hay denominador para esa exposición 
    expo_EMB = expo_EMB * deno_embarazo
  )

numeradoremb <- numeradoremb %>%
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A")) %>%
  filter(expo_EMB == 1)%>%  
  distinct(id_emb, .keep_all = T)%>%
  select(idp, id_emb, cod, ctanca, durada, dinici, dfi, expo_EMB)

CIE <- `diagnostics_LuciaVH_N_2023-06-09` %>% 
  mutate(dx=cod)%>% 
  select(idp, dat, dbaixa, agr, dx)

CIE <- numeradoremb %>%
  left_join(CIE, relationship = "many-to-many") %>%
  mutate(Diagnost = if_else(dat >= (dinici - years(1)), dx, NA))%>%
  
  #N, proporciones
  tabla.comorb <- table(CIE$dx)
tabla.comorb_df <- as.data.frame(tabla.comorb)
tabla.comorb_df <- tabla.comorb_df %>% 
  mutate(frecuencia_relativa=(Freq/n)*100)

#intervalos de confianza
prop_0  <- 97.23 /100
prop_1  <- 39.68/100
prop_2  <- 36.70/100
prop_3  <- 27.16/100
prop_4  <- 25.44/100
prop_5  <-  20.92/100
prop_6  <-  17.97/100
prop_7  <-  1.84/100


# Desviación estándar para cada proporción
se_0 <- sqrt(prop_0 * (1 - prop_0) / n)
se_1 <- sqrt(prop_1 * (1 - prop_1) / n)
se_2 <- sqrt(prop_2 * (1 - prop_2) / n)
se_3 <- sqrt(prop_3 * (1 - prop_3) / n)
se_4 <- sqrt(prop_4 * (1 - prop_4) / n)
se_5 <- sqrt(prop_5 * (1 - prop_5) / n)
se_6 <- sqrt(prop_6 * (1 - prop_6) / n)
se_7 <- sqrt(prop_7 * (1 - prop_7) / n)


# Valor crítico Z para un nivel de confianza del 95%
z <- qnorm(0.975)

# Calcular los intervalos de confianza para las proporciones
ci_0 <- c(prop_0 - z * se_0, prop_0 + z * se_0)*100
ci_1 <- c(prop_1 - z * se_1, prop_1 + z * se_1)*100
ci_2 <- c(prop_2 - z * se_2, prop_2 + z * se_2)*100
ci_3 <- c(prop_3 - z * se_3, prop_3 + z * se_3)*100
ci_4 <- c(prop_4 - z * se_4, prop_4 + z * se_4)*100
ci_5 <- c(prop_5 - z * se_5, prop_5 + z * se_5)*100
ci_6 <- c(prop_6 - z * se_6, prop_6 + z * se_6)*100
ci_7 <- c(prop_7 - z * se_7, prop_7 + z * se_7)*100


#ALCOHOLISMO 



###CALCULAMOS FÁRMACOS MÁS PRESCRITOS 
#Reiniciar numeradoremb y poner este filtro 
numeradoremb <- numeradoremb %>%
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A")) %>%
  filter(expo_EMB == 1)%>%  
  distinct(id_emb,cod, .keep_all = T)%>%
  select(idp, id_emb, cod, dat, dbaixa, ctanca, durada, dinici, dfi, expo_EMB)
#Salen 17099

# Calcular el número total de prescripciones
total_prescripciones <- 17099

# Primero, cuenta cuántas prescripciones hay para cada código de medicamento distinto
prescripciones_por_cod <- numeradoremb %>%
  group_by(cod) %>%
  summarise(num_prescripciones = n_distinct(id_emb))

# Calcular la frecuencia relativa para cada código de medicamento
prescripciones_por_cod <- prescripciones_por_cod %>%
  mutate(frecuencia_relativa = num_prescripciones / total_prescripciones*100)

# Calcular los intervalos de confianza para las proporciones
# Utilizando el método exacto de Clopper-Pearson para intervalos de confianza
ci <- binom.confint(prescripciones_por_cod$num_prescripciones, total_prescripciones, method = "exact")

# Agregar los intervalos de confianza a los resultados
prescripciones_por_cod <- prescripciones_por_cod %>%
  mutate(IC_lower = ci$lower,
         IC_upper = ci$upper)

# Mostrar el resultado con frecuencias relativas en notación decimal
prescripciones_por_cod <- prescripciones_por_cod %>%
  mutate(frecuencia_relativa = sprintf("%.4f", frecuencia_relativa),
         IC_lower = sprintf("%.4f", IC_lower),
         IC_upper = sprintf("%.4f", IC_upper))


# Especificar la ruta y el nombre del archivo Excel
ruta_archivo <- "/Users/luciabellas_/Desktop/ANALISIS2_TESIS/COD.csv"

# Exportar la tabla a un archivo de Excel
write.csv(prescripciones_por_cod, ruta_archivo)

#POR ATC 
# Crear una columna para la categoría ATC basada en el código de medicamento
numeradoremb <- numeradoremb %>%
  mutate(ATC = case_when(
    substr(cod, 1, 5) == "N06AA" ~ "Non-selective monoamine reuptake inhibitors",
    substr(cod, 1, 5) == "N06AB" ~ "Selective serotonin reuptake inhibitors",
    substr(cod, 1, 5) == "N06AF" ~ "Monoamine oxidase inhibitors, non-selective",
    substr(cod, 1, 5) == "N06AG" ~ "Monoamine oxidase A inhibitors",
    substr(cod, 1, 5) == "N06AX" ~ "Other antidepressants",
    TRUE ~ "Other"
  ))

# Filtrar para mantener solo las categorías relevantes
numeradoremb <- numeradoremb %>%
  filter(ATC != "Other")

# Contar cuántas prescripciones hay para cada categoría ATC distinta
prescripciones_por_ATC <- numeradoremb %>%
  group_by(ATC) %>%
  summarise(num_prescripciones = n_distinct(id_emb))



# Calcular la frecuencia relativa para cada categoría ATC
prescripciones_por_ATC <- prescripciones_por_ATC %>%
  mutate(frecuencia_relativa = num_prescripciones / total_prescripciones*100)

# Calcular los intervalos de confianza para las proporciones
ci <- binom.confint(prescripciones_por_ATC$num_prescripciones, total_prescripciones, method = "exact")

# Agregar los intervalos de confianza a los resultados
prescripciones_por_ATC <- prescripciones_por_ATC %>%
  mutate(IC_lower = ci$lower,
         IC_upper = ci$upper)

# Mostrar el resultado con frecuencias relativas en notación decimal
prescripciones_por_ATC <- prescripciones_por_ATC %>%
  mutate(frecuencia_relativa = sprintf("%.4f", frecuencia_relativa),
         IC_lower = sprintf("%.4f", IC_lower),
         IC_upper = sprintf("%.4f", IC_upper))

print(prescripciones_por_ATC)
# Especificar la ruta y el nombre del archivo Excel
ruta_archivo <- "/Users/luciabellas_/Desktop/ANALISIS2_TESIS/ATC.csv"

# Exportar la tabla a un archivo de Excel
write.csv(prescripciones_por_ATC, ruta_archivo)



#PLURIPRESCRIPCION 
numeradoremb <- numeradoremb %>%
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A")) %>%
  filter(expo_EMB == 1) %>%
  distinct(id_emb, cod, .keep_all = TRUE) %>%
  select(idp, id_emb, cod, dat, dbaixa, ctanca, durada, dinici, dfi, expo_EMB)

# Función para comprobar la superposición de un mes
overlap_one_month <- function(start1, end1, start2, end2) {
  return((start1 <= end2) & (end1 >= start2))
}

# Filtrar prescripciones que se superponen con el embarazo en un mes
numeradoremb[, superposicion := overlap_one_month(dat, dbaixa, dinici - 60, dfi + 60), by = id_emb]

# Contar el número de prescripciones por embarazo
prescriptions_count <- numeradoremb[superposicion == TRUE, .N, by = id_emb]

# Clasificar los embarazos por número de prescripciones
prescriptions_count[, category := fifelse(N == 1, '1 prescripcion', 
                                          fifelse(N == 2, '2 prescripciones', '3 o más prescripciones'))]

# Contar el número de embarazos por categoría
result <- prescriptions_count[, .N, by = category]

# Calcular el porcentaje y el IC al 95%
total_embarazos <- sum(result$N)

result <- result %>% 
  mutate(Porcentaje= N/total_embarazos*100)

# Utilizando el método exacto de Clopper-Pearson para intervalos de confianza
ci <- binom.confint(result$N, total_embarazos, method = "exact")

# Agregar los intervalos de confianza a los resultados
result <- result %>%
  mutate(IC_lower = ci$lower*100,
         IC_upper = ci$upper*100)
# Especificar la ruta y el nombre del archivo Excel
ruta_archivo <- "/Users/luciabellas_/Desktop/ANALISIS2_TESIS/comedicacion.csv"

# Exportar la tabla a un archivo de Excel
write.csv(result, ruta_archivo)


#Alcoholimetro 
#Media y mediana edad madres en el episodio de embarazo 

numeradoremb  <- denominadorEMB %>%
  mutate(
    expo_EMB = ifelse(int_overlaps(intervalo_emb, intervalo_prescripcion), 1, 0),
    ## Convertimos las exposiciones a 0 si no hay denominador para esa exposición 
    expo_EMB = expo_EMB * deno_embarazo
  )

numeradoremb <- numeradoremb %>%
  mutate(grupo_N = substr(cod, start = 1, stop = 4)) %>%
  filter(grupo_N %in% paste0("N06A")) %>%
  filter(expo_EMB == 1)%>%  
  distinct(id_emb, .keep_all = T)%>%
  select(idp, id_emb, cod, dat, ctanca, durada, dinici, dfi, expo_EMB)

OH_data <- `variables_cliniques_LuciaVH_N_2023-06-09`%>%
  mutate(dato_oh=cod, 
         fecha_oh=dat)%>%
  select(idp, dato_oh, fecha_oh,val)%>%
  filter(dato_oh=="ALRIS")

OH <- numeradoremb %>%
  full_join(OH_data, relationship = "many-to-many") %>%  
  distinct(id_emb, .keep_all=TRUE)  

summary(OH$val)
tabla_oh <- table(OH$val)
# 10532  2758   154 
# 1371
ntotal<-nrow(numeradoremb)
prop_table <- tabla_oh/ntotal

# Calculate confidence intervals using binom.confint
conf_intervals_binom <- binom.confint(tabla_oh, ntotal, methods = "wilson")

# Combine the proportions and their confidence intervals into a data frame
prop_ci_table_binom <- data.frame(
  Proportion = prop_table*100,
  CI_Lower = conf_intervals_binom$lower * 100,
  CI_Upper = conf_intervals_binom$upper * 100
)

alcoholismo <- prop_ci_table_binom %>% 
  mutate(N=tabla_oh, 
         missing=1371)

# Especificar la ruta y el nombre del archivo Excel
ruta_archivo <- "/Users/luciabellas_/Desktop/ANALISIS2_TESIS/alcohol.csv"

# Exportar la tabla a un archivo de Excel
write.csv(alcoholismo, ruta_archivo)
