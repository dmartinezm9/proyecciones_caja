write_xlsx(merge_sd_rc_caja, "C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Outputs/sd_rc_merge.xlsx")
write_xlsx(bbdd_inv, "C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Outputs/inv.xlsx")
write_xlsx(bbdd_remun, "C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Outputs/remun.xlsx")
write_xlsx(bbdd_bbss, "C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Outputs/bbss.xlsx")
write_xlsx(df3, "C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Outputs/bbdd_append.xlsx")

####################
##### Paquetes #####
####################

paquetes <- c("tidyverse","data.table","writexl","readxl", "lubridate", 
              "icesTAF","stringr","gtools","RJDBC","tidyr","eeptools", 
              "doBy", "haven","profvis","pryr","tictoc","evaluate","tidyfast", "testit")
# detectar paquetes que no estan instalados
miss_paquetes <- paquetes[!paquetes %in% installed.packages()[,1]]

# instalar paquetes que no estan instalados
if(length(miss_paquetes) > 0){
  install.packages(miss_paquetes)
}

# aplicar library a los paquetes instalados
invisible(lapply(paquetes,library,character.only = TRUE))

# eliminar objetos temporales
rm(miss_paquetes,paquetes)

#########################################
#########################################
bbdd_sd_rc <- read_excel("C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Inputs/PIAPIMCertCompAnDev_2024_GNGRGLMN_01042024.xlsx")

# Nos quedamos con las observaciones que tengan 0 y 8 en la GENERICA
bbdd_sd_rc <- bbdd_sd_rc %>% filter(grepl("^0|^8.",GENERICA))

##### SD:

# Se crea una variable MES
bbdd_sd_rc$MES <- ifelse(bbdd_sd_rc$GENERICA == "8. SERVICIO DE LA DEUDA PUBLICA", (bbdd_sd_rc$PIM - bbdd_sd_rc$EJE_ENE - bbdd_sd_rc$EJE_FEB - bbdd_sd_rc$EJE_MAR)/9, 0)
#bbdd_sd_rc$PROY_DEV_FEB <- bbdd_sd_rc$MES
#bbdd_sd_rc$PROY_DEV_MAR <- bbdd_sd_rc$MES
bbdd_sd_rc$PROY_DEV_ABR <- bbdd_sd_rc$MES
bbdd_sd_rc$PROY_DEV_MAY <- bbdd_sd_rc$MES
bbdd_sd_rc$PROY_DEV_JUN <- bbdd_sd_rc$MES
bbdd_sd_rc$PROY_DEV_JUL <- bbdd_sd_rc$MES
bbdd_sd_rc$PROY_DEV_AGO <- bbdd_sd_rc$MES
bbdd_sd_rc$PROY_DEV_SET <- bbdd_sd_rc$MES
bbdd_sd_rc$PROY_DEV_OCT <- bbdd_sd_rc$MES
bbdd_sd_rc$PROY_DEV_NOV <- bbdd_sd_rc$MES
bbdd_sd_rc$PROY_DEV_DIC <- bbdd_sd_rc$MES

##### RC:
bbdd_sd_rc <- bbdd_sd_rc %>% mutate(PROY_DEV_DIC = ifelse(bbdd_sd_rc$GENERICA == "0. RESERVA DE CONTINGENCIA", PIM, PROY_DEV_DIC))

# Creamos la variable PROY_ANUAL_MAR. Agregar los demas ejecutados "EJE_FEB, EJE_MAR" para los prox meses
bbdd_sd_rc <- bbdd_sd_rc %>% mutate(PROY_ANUAL_ABR =  EJE_ENE + EJE_FEB + EJE_MAR + PROY_DEV_ABR + PROY_DEV_MAY + 
                                      PROY_DEV_JUN + PROY_DEV_JUL + PROY_DEV_AGO + PROY_DEV_SET + PROY_DEV_OCT + PROY_DEV_NOV + PROY_DEV_DIC)

# Creamos el identificador del ID y del equipo
bbdd_sd_rc <- mutate(bbdd_sd_rc, EQUIPO = "4. G0 y G8")

# Agregamos la variable TIPO_GASTO = REGULAR
bbdd_sd_rc <- mutate(bbdd_sd_rc, TIPO_GASTO = "1. GASTO RECURRENTE")

# Creamos TIPO_ACTIVIDAD
bbdd_sd_rc <- mutate(bbdd_sd_rc, TIPO_ACTIVIDAD = "15. DEUDA")

# Colapsamos
bbdd_sd_rc <- bbdd_sd_rc %>% group_by(EQUIPO, NIVEL_GOBIERNO = NIVEL_GOB, SECTOR = COD_SECTOR, PLIEGO = COD_PLIEGO,
                                      EJECUTORA = COD_ENTIDAD, FUENTE, RUBRO, CATEGORIA = CATEGORIA_GASTO,
                                      GENERICA, TIPO_GASTO, TIPO_ACTIVIDAD) %>% 
  summarise(PIA_2024 = sum(PIA, na.rm = T),
            PIM_2024 = sum(PIM, na.rm = T),
            CERTIFICADO = sum(CE_ANUAL, na.rm = T),
            COMPROMISO = sum(CO_ANUAL, na.rm = T),
            
            REAL_DEV_ENE = sum(EJE_ENE, na.rm = T),
            REAL_DEV_FEB = sum(EJE_FEB, na.rm = T),
            REAL_DEV_MAR = sum(EJE_MAR, na.rm = T),
            
            PROY_DEV_ABR = sum(PROY_DEV_ABR, na.rm = T),
            PROY_DEV_MAY = sum(PROY_DEV_MAY, na.rm = T),
            PROY_DEV_JUN = sum(PROY_DEV_JUN, na.rm = T),
            PROY_DEV_JUL = sum(PROY_DEV_JUL, na.rm = T),
            PROY_DEV_AGO = sum(PROY_DEV_AGO, na.rm = T),
            PROY_DEV_SET = sum(PROY_DEV_SET, na.rm = T),
            PROY_DEV_OCT = sum(PROY_DEV_OCT, na.rm = T),
            PROY_DEV_NOV = sum(PROY_DEV_NOV, na.rm = T),
            PROY_DEV_DIC = sum(PROY_DEV_DIC, na.rm = T),
            PROY_ANUAL_ABR = sum(PROY_ANUAL_ABR, na.rm = T))

# Generamos el ID para esta base
bbdd_sd_rc <- bbdd_sd_rc %>%  mutate(ID = paste0(str_extract(EQUIPO,"\\d*\\."),
                                                 str_extract(NIVEL_GOBIERNO,"\\d*\\."),
                                                 str_extract(SECTOR,"\\d*\\."),
                                                 str_extract(PLIEGO,"\\d*\\."),
                                                 str_extract(EJECUTORA,"\\d*\\."),
                                                 str_extract(FUENTE,"\\d*\\."),
                                                 str_extract(RUBRO,"\\d*\\."),
                                                 str_extract(CATEGORIA,"\\d*\\."),
                                                 str_extract(GENERICA,"\\d*\\."),
                                                 str_extract(TIPO_GASTO,"[[:alpha:]]*")))

bbdd_sd_rc <- bbdd_sd_rc %>% select(ID, everything())

##### ENERO, CAJA Y ESPACIO FISCAL####
######################################

# Importamos la base de enero caja para traer PROY_DEV_ENE, PROY_ANUAL_ENE Y CAJA
bbdd_caja <- read_excel("C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Inputs/Consolidado_Proy_Marzo_13.03.2024 Comité de Caja_v2.xlsx")

# Renombramos los tipo_gasto y NIVEL_GOBIERNO a un unico formato:
#bbdd_caja <- bbdd_caja %>%
#  mutate(TIPO_GASTO = fct_recode(TIPO_GASTO, "REGULAR" = "1. GASTO RECURRENTE", "COVID" = "2. GASTO CORONAVIRUS", "FEN"="3. GASTO FEN", "REACTIVACION" = "3. GASTO REACTIVACIÓN"))
#bbdd_caja <- bbdd_caja %>%
#  mutate(NIVEL_GOBIERNO = fct_recode(NIVEL_GOBIERNO, "3. GOBIERNOS REGIONALES" = "3. MANCOMUNIDAD REGIONAL"))

bbdd_caja <- bbdd_caja %>% filter(grepl("^0|^8.",GENERICA))

#bbdd_caja %>% group_by(GENERICA) %>% summarise(n())

# Cambiamos G0 y G8 por 4. G0 y G8 para fines del ID
bbdd_caja <- mutate(bbdd_caja, EQUIPO = "4. G0 y G8")

bbdd_caja <- bbdd_caja %>%
  mutate(TIPO_GASTO = fct_recode(TIPO_GASTO, "1. GASTO RECURRENTE" = "REGULAR"))
  
# Generamos el ID para la base de enero solo de g0 y g8
bbdd_caja <- bbdd_caja %>%  mutate(ID = paste0(str_extract(EQUIPO,"\\d*\\."),
                                               str_extract(NIVEL_GOBIERNO,"\\d*\\."),
                                               str_extract(SECTOR,"\\d*\\."),
                                               str_extract(PLIEGO,"\\d*\\."),
                                               str_extract(EJECUTORA,"\\d*\\."),
                                               str_extract(FUENTE,"\\d*\\."),
                                               str_extract(RUBRO,"\\d*\\."),
                                               str_extract(CATEGORIA,"\\d*\\."),
                                               str_extract(GENERICA,"\\d*\\."),
                                               str_extract(TIPO_GASTO,"[[:alpha:]]*")))

# Nos quedamos con las variables que queremos: ID, PROY_DEV_ENE, ANUAL EQUIPO Y CAJA
bbdd_caja <- select(bbdd_caja, all_of(c("ID","PROY_DEV_MAR","PROY_ANUAL_MAR")))

table(duplicated(bbdd_caja$ID))
table(duplicated(bbdd_sd_rc$ID))

# Hacemos el merge:
merge_sd_rc_caja <- merge(bbdd_sd_rc, bbdd_caja, by = "ID", all.x = TRUE)
merge_sd_rc_caja <- select(merge_sd_rc_caja, -ID)

merge_sd_rc_caja <- mutate(merge_sd_rc_caja, EQUIPO = "G0 y G8")
names(merge_sd_rc_caja)
##################################################################################


###################
### Inversiones ###
###################

# Especifica la ruta del Excel
bbdd_inv <- read_excel("C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Inputs/inversiones_v2.xlsx")

# Modificmos mancomunidades regionales -> regionales
bbdd_inv <- bbdd_inv %>%
  mutate(NIVEL_GOBIERNO = fct_recode(NIVEL_GOBIERNO, "2. GOBIERNOS REGIONALES" = "2. MANCOMUNIDAD REGIONAL", "3. GOBIERNOS LOCALES" = "3. MANCOMUNIDAD LOCAL"))

# Modificamos las categorias de TIPO_GASTO para uniformizar con las demas df
bbdd_inv <- bbdd_inv %>%
  mutate(TIPO_GASTO = fct_recode(TIPO_GASTO, "4. GASTO FEN" = "FEN", "3. GASTO REACTIVACION" = "REACTIVACION", "2. GASTO CORONAVIRUS" = "COVID", "1. GASTO RECURRENTE" = "REGULAR"))

# Colapsamos
bbdd_inv <- bbdd_inv %>% group_by(EQUIPO, NIVEL_GOBIERNO, SECTOR, PLIEGO,
                                  EJECUTORA, FUENTE, RUBRO, CATEGORIA,
                                  GENERICA, TIPO_GASTO, TIPO_ACTIVIDAD) %>% 
  summarise(PIA_2024 = sum(PIA_2024, na.rm = T),
            PIM_2024 = sum(PIM_2024, na.rm = T),
            CERTIFICADO = sum(CERTIFICADO, na.rm = T),
            COMPROMISO = sum(COMPROMISO_ANUAL, na.rm = T),
            
            REAL_DEV_ENE = sum(REAL_DEV_ENE, na.rm = T),
            REAL_DEV_FEB = sum(REAL_DEV_FEB, na.rm = T),
            REAL_DEV_MAR = sum(REAL_DEV_MAR, na.rm = T),
            
            PROY_DEV_ABR = sum(PROY_DEV_ABR, na.rm = T),
            PROY_DEV_MAY = sum(PROY_DEV_MAY, na.rm = T),
            PROY_DEV_JUN = sum(PROY_DEV_JUN, na.rm = T),
            PROY_DEV_JUL = sum(PROY_DEV_JUL, na.rm = T),
            PROY_DEV_AGO = sum(PROY_DEV_AGO, na.rm = T),
            PROY_DEV_SET = sum(PROY_DEV_SET, na.rm = T),
            PROY_DEV_OCT = sum(PROY_DEV_OCT, na.rm = T),
            PROY_DEV_NOV = sum(PROY_DEV_NOV, na.rm = T),
            PROY_DEV_DIC = sum(PROY_DEV_DIC, na.rm = T),
            PROY_ANUAL_ABR = sum(PROY_ANUAL_ABR, na.rm = T),
            
            PROY_DEV_MAR = sum(PROY_DEV_MAR, na.rm = T),
            PROY_ANUAL_MAR = sum(PROY_ANUAL_MAR, na.rm =T)) # Cambiar a PROY_ANUAL_MAR cuando se tenga la informacion

#############
### BBySS ###
#############

# Especifica la ruta del Excel
bbdd_bbss <- read_excel(paste0("C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Inputs/bbss_v2.xlsx"))

# Creamos el identificador del equipo
bbdd_bbss <- mutate(bbdd_bbss, EQUIPO = "Bienes y Servicios")

# Modificamos las categorias de TIPO_GASTO para uniformizar con las demas df
bbdd_bbss <- bbdd_bbss %>%
  mutate(TIPO_GASTO = fct_recode(TIPO_GASTO, "4. GASTO FEN" = "3. GASTO FEN", "3. GASTO REACTIVACION" = "2. GASTO REACTIVACION", "2. GASTO CORONAVIRUS" = "1. GASTO COVID-19", "1. GASTO RECURRENTE" = "0. GASTO REGULAR"))

#1. GASTO RECURRENTE
#2. GASTO CORONAVIRUS
#3. GASTO REACTIVACION
#4. GASTO FEN 

# Modificmos mancomunidades regionales -> regionales
bbdd_bbss <- bbdd_bbss %>%
  mutate(NIVEL_GOBIERNO = fct_recode(NIVEL_GOBIERNO, "2. GOBIERNOS REGIONALES" = "2. MANCOMUNIDAD REGIONAL", "3. GOBIERNOS LOCALES" = "3. MANCOMUNIDAD LOCAL"))

# Colapsamos
bbdd_bbss <- bbdd_bbss %>% group_by(EQUIPO, NIVEL_GOBIERNO, SECTOR, PLIEGO,
                                    EJECUTORA, FUENTE, RUBRO, CATEGORIA,
                                    GENERICA, TIPO_GASTO, TIPO_ACTIVIDAD) %>% 
  summarise(PIA_2024 = sum(PIA_2024, na.rm = T),
            PIM_2024 = sum(PIM_2024, na.rm = T),
            CERTIFICADO = sum(CERTIFICADO, na.rm = T),
            COMPROMISO = sum(COMPROMISO, na.rm = T),
            
            REAL_DEV_ENE = sum(REAL_DEV_ENE, na.rm = T),
            REAL_DEV_FEB = sum(REAL_DEV_FEB, na.rm = T),
            REAL_DEV_MAR = sum(REAL_DEV_MAR, na.rm = T),
            
            PROY_DEV_ABR = sum(PROY_DEV_ABR, na.rm = T),
            PROY_DEV_MAY = sum(PROY_DEV_MAY, na.rm = T),
            PROY_DEV_JUN = sum(PROY_DEV_JUN, na.rm = T),
            PROY_DEV_JUL = sum(PROY_DEV_JUL, na.rm = T),
            PROY_DEV_AGO = sum(PROY_DEV_AGO, na.rm = T),
            PROY_DEV_SET = sum(PROY_DEV_SET, na.rm = T),
            PROY_DEV_OCT = sum(PROY_DEV_OCT, na.rm = T),
            PROY_DEV_NOV = sum(PROY_DEV_NOV, na.rm = T),
            PROY_DEV_DIC = sum(PROY_DEV_DIC, na.rm = T),
            PROY_ANUAL_ABR = sum(PROY_ANUAL_ABR, na.rm = T),
            
            PROY_DEV_MAR = sum(PROY_DEV_MAR, na.rm = T),
            PROY_ANUAL_MAR = sum(PROY_ANUAL_MAR, na.rm =T))


#########################
### 5- Remuneraciones ###
#########################
#write_xlsx(pruebaprueba, "C:/Users/DIEGO/Desktop/consolidado optimista/Outputs/pruebaprueba.xlsx")

bbdd_remun <- read_excel(paste0("C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Inputs/remun_v3_locaciones.xlsx"))

# Creamos el identificador del equipo
bbdd_remun <- mutate(bbdd_remun, EQUIPO = "Remuneraciones")
names(bbdd_remun)

# Modificamos las categorias de TIPO_GASTO para uniformizar con las demas df
bbdd_remun <- bbdd_remun %>%
  mutate(TIPO_GASTO = fct_recode(TIPO_GASTO, "4. GASTO FEN" = "2. GASTO FEN", "3. GASTO REACTIVACION" = "3. GASTO REACTIVACIÓN"))

# Modificmos mancomunidades regionales -> regionales
bbdd_remun <- bbdd_remun %>%
  mutate(NIVEL_GOBIERNO = fct_recode(NIVEL_GOBIERNO, "3. GOBIERNOS LOCALES" = "3. MANCOMUNIDAD LOCAL"))

# Colapsamos
bbdd_remun <- bbdd_remun %>% group_by(EQUIPO, NIVEL_GOBIERNO, SECTOR, PLIEGO,
                                      EJECUTORA, FUENTE, RUBRO, CATEGORIA = CATEGORIA_GTO,
                                      GENERICA, TIPO_GASTO, TIPO_ACTIVIDAD) %>% 
  summarise(PIA_2024 = sum(PIA_2024, na.rm = T),
            PIM_2024 = sum(PIM_2024, na.rm = T),
            CERTIFICADO = sum(CERTIFICADO, na.rm = T),
            COMPROMISO = sum(COMPROMISO, na.rm = T),
            
            REAL_DEV_ENE = sum(REAL_DEV_ENE, na.rm = T), # Actualizar el de proyeccion en la derecha con el REAL_DEV_ENE
            REAL_DEV_FEB = sum(REAL_DEV_FEB, na.rm = T),
            REAL_DEV_MAR = sum(REAL_DEV_MAR, na.rm = T),
            PROY_DEV_ABR = sum(PROY_DEV_ABR, na.rm = T),
            PROY_DEV_MAY = sum(PROY_DEV_MAY, na.rm = T),
            PROY_DEV_JUN = sum(PROY_DEV_JUN, na.rm = T),
            PROY_DEV_JUL = sum(PROY_DEV_JUL, na.rm = T),
            PROY_DEV_AGO = sum(PROY_DEV_AGO, na.rm = T),
            PROY_DEV_SET = sum(PROY_DEV_SET, na.rm = T),
            PROY_DEV_OCT = sum(PROY_DEV_OCT, na.rm = T),
            PROY_DEV_NOV = sum(PROY_DEV_NOV, na.rm = T),
            PROY_DEV_DIC = sum(PROY_DEV_DIC, na.rm = T),
            PROY_ANUAL_ABR = sum(PROY_ANUAL_ABR, na.rm = T), # Cambiar a PROY_ANUAL_FEB cuando se tenga la informacion
            
            PROY_DEV_MAR = sum(PROY_DEV_MAR, na.rm = T),
            PROY_ANUAL_MAR = sum(PROY_ANUAL_MAR, na.rm =T))

##### APPEND:
df1<-rbind(merge_sd_rc_caja,bbdd_bbss)
df2<-rbind(df1,bbdd_inv)
df3<-rbind(df2,bbdd_remun)

write_xlsx(df3, "C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Outputs/bbdd_append_v3.xlsx")





############################################################################################################################################
###########################################
# Para appendear y colapsar con la de caja y espacio fiscal!!!!!!!! #

df3$PROY_ANUAL_FEB_2024_CAJA<-0

###########################################
### Importar caja
#PROY_ANUAL_ENE_2024_CAJA

# Importamos la base de enero caja para traer PROY_DEV_ENE, PROY_ANUAL_ENE Y CAJA
bbdd_caja <- read_excel("C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Inputs/caja_usare_v4.xlsx")
names(bbdd_caja)
names(df3)
# Renombramos los tipo_gasto y NIVEL_GOBIERNO a un unico formato:
#bbdd_caja <- bbdd_caja %>%
#  mutate(TIPO_GASTO = fct_recode(TIPO_GASTO, "REGULAR" = "1. GASTO RECURRENTE", "COVID" = "2. GASTO CORONAVIRUS", "FEN"="3. GASTO FEN", "REACTIVACION" = "3. GASTO REACTIVACIÓN"))
bbdd_caja <- bbdd_caja %>%
  mutate(NIVEL_GOBIERNO = fct_recode(NIVEL_GOBIERNO, "2. GOBIERNOS REGIONALES" = "2. MANCOMUNIDAD REGIONAL"))

df_prueba<-rbind(df3,bbdd_caja)

names(df_prueba)
names(bbdd_caja)

# Colapsamos

df_prueba <- df_prueba %>% group_by(EQUIPO, NIVEL_GOBIERNO, SECTOR, PLIEGO,
                                    EJECUTORA, FUENTE, RUBRO, CATEGORIA,
                                    GENERICA, TIPO_GASTO) %>% 
  summarise(PIA_2024 = sum(PIA_2024, na.rm = T),
            PIM_2024 = sum(PIM_2024, na.rm = T),
            CERTIFICADO = sum(CERTIFICADO, na.rm = T),
            COMPROMISO = sum(COMPROMISO, na.rm = T),
            
            REAL_DEV_ENE = sum(REAL_DEV_ENE, na.rm = T),
            REAL_DEV_FEB = sum(REAL_DEV_FEB, na.rm = T),
            REAL_DEV_MAR = sum(REAL_DEV_MAR, na.rm = T),
            
            PROY_DEV_ABR = sum(PROY_DEV_ABR, na.rm = T),
            PROY_DEV_MAY = sum(PROY_DEV_MAY, na.rm = T),
            PROY_DEV_JUN = sum(PROY_DEV_JUN, na.rm = T),
            PROY_DEV_JUL = sum(PROY_DEV_JUL, na.rm = T),
            PROY_DEV_AGO = sum(PROY_DEV_AGO, na.rm = T),
            PROY_DEV_SET = sum(PROY_DEV_SET, na.rm = T),
            PROY_DEV_OCT = sum(PROY_DEV_OCT, na.rm = T),
            PROY_DEV_NOV = sum(PROY_DEV_NOV, na.rm = T),
            PROY_DEV_DIC = sum(PROY_DEV_DIC, na.rm = T),
            
            PROY_ANUAL_ABR = sum(PROY_ANUAL_ABR, na.rm = T),
            PROY_DEV_MAR = sum(PROY_DEV_MAR, na.rm = T),
            PROY_ANUAL_MAR_2024_CAJA= sum(PROY_ANUAL_MAR_2024_CAJA, na.rm = T),
names(df_prueba)

write_xlsx(df_prueba, "C:/Users/DIEGO/Desktop/Consolidacion_proyecciones/Consolidacion_abril/Outputs/proyecciones_consolidadas_f.xlsx")
