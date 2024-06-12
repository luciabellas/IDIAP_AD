install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(data.table)

# Transformación de datos
CIT <- CIT %>% select(prevalence_start_date, prevalence)
CIT$prevalence <- as.numeric(CIT$prevalence)
CIT$prevalence_start_date <- as.Date(CIT$prevalence_start_date)
colnames(CIT) <- c("Fecha", "PREV_cit")

FLUOX <- FLUOX %>% select(prevalence_start_date, prevalence)
FLUOX$prevalence <- as.numeric(FLUOX$prevalence)
FLUOX$prevalence_start_date <- as.Date(FLUOX$prevalence_start_date)
colnames(FLUOX) <- c("Fecha", "PREV_fluox")

PAR <- PAR %>% select(prevalence_start_date, prevalence)
PAR$prevalence <- as.numeric(PAR$prevalence)
PAR$prevalence_start_date <- as.Date(PAR$prevalence_start_date)
colnames(PAR) <- c("Fecha", "PREV_par")

SER <- SER %>% select(prevalence_start_date, prevalence)
SER$prevalence <- as.numeric(SER$prevalence)
SER$prevalence_start_date <- as.Date(SER$prevalence_start_date)
colnames(SER) <- c("Fecha", "PREV_ser")

#Datos combinados 
#Juntamos todas las columnas en un único dataset 
trigraph <- CIT %>%mutate( 
  PREV_fluox=FLUOX$PREV_fluox, 
  PREV_par=PAR$PREV_par, 
  PREV_SER=SER$PREV_ser)

trigraph$Fecha <- as.Date(trigraph$Fecha)

# Cargar el paquete ggplot2 si aún no está cargado
ggplot(trigraph, aes(x = Fecha)) +
  geom_line(aes(y = PREV_cit, color = "Citalopram"), linetype = "solid") +
  geom_line(aes(y = PREV_par, color = "Paroxetine"), linetype = "solid") +
  geom_line(aes(y = PREV_fluox, color = "Fluoxetine"), linetype = "solid") +
  geom_line(aes(y = PREV_SER, color = "Sertraline"), linetype = "solid") +
  scale_color_manual(values = c("Citalopram" = "#FF7F00", "Paroxetine" = "#24693D", "Fluoxetine" = "#B22222", "Sertraline"="#9932CC")) +
  labs(title = "Figure 1: Prevalence of citalopram, paroxetine, fluoxetine and sertraline over time",
       x = "Time points (months)",
       y = "Prevalence") +
  theme_minimal() + 
  theme(legend.position = "top",
        legend.text = element_text(size = 13, color = "black", face = "bold"),
        legend.title = element_text(size = 13, color = "black", face = "bold"),
        plot.title = element_text(size = 18, color = "#5F9EA0", face = "bold"),
        axis.title = element_text(size = 13, color = "#5F9EA0", face = "bold"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "#5F9EA0"),
        axis.text.y = element_text(color = "#5F9EA0", size = 10, hjust = 1)) +
  scale_x_date(labels = scales::date_format("%Y %B"),
               breaks = scales::breaks_pretty(n = 15)) 