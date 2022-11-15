##1.Librería, Cargar paquetes

pacman::p_load(sjPlot, 
               tidyverse,
               heaven,
               magrittr,
               srvyr,
               survey,
               dplyr,
               summarise,
               sjmisc,
               forcats,
               car,
               sjlabelled,
               sjplot,
               reshape)
# install.packages("haven")
# library(haven)

##2.Cargar base de datos
datos <- read_dta("input/Base de datos Full VI EME (extracto).dta")

names(datos)

##Datos procesados

datos_proc <- datos %>%  select(Enc_rph, ganancia_final_mensual,conta_completa,
                                registro_SII, CISE, region, Factor_EME)

##3.Recodificamos datos

#Recodificamos conta_completa 

datos_proc <- mutate(datos_proc,
       conta_completa =
case_when(conta_completa == 1 ~ "Sí",
          conta_completa == 2 ~ "No"))

#Recodificamos registro del SII

datos_proc <- mutate(datos_proc,
       registro_SII =
case_when(registro_SII == 1 ~ "Sí",
          registro_SII == 2 ~ "No"))

#Recodificamos CISE

datos_proc <- mutate(datos_proc,
              CISE =
case_when(CISE == 0 ~ "Cuenta propia",
          CISE == 1 ~ "Empleador"))

##4.Hacemos la recodificación región por zonas

datos_proc <- mutate(datos_proc,
              Macrozona =
case_when(region %in% c(1,2,3,15)~ "Macrozona Norte",
          region %in% c(4,5,6,7,8,16)~"Macrozona Central",
          region %in% c(9,14,10)~"Macrozona Sur",
          region %in% c(11,12)~ "Macrozona Austral",
          TRUE ~ NA_character_))

##5. Generar Tabla

objeto_encuesta <- as_survey_design(datos_proc, 
                                    ids = Enc_rph,
                                    weights = Factor_EME)
  

Tabla_por_Macrozona <- objeto_encuesta %>%
  group_by(Macrozona) %>%
  summarise(CISE = survey_total(vartype = "ci",na.rm = T),
            conta_completa = survey_prop(vartype = "ci", na.rm = T),
            Factor_EME = survey_prop(vartype = "ci", na.rm = T),
            ganancia_final_mensual = survey_prop(vartype = "ci", na.rm = T))
  
  

  
  
