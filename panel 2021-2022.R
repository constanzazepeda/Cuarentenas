rm(list=ls())
library(sf)
library(readxl)
library(dplyr)
library(data.table)
#install.packages("stringi")
library(stringi)
library(mapview)
library(sf)
library(stringr)
library(lubridate)
library(tidyr)
source("functions.R")
#panel_2020<- readRDS("panel_cuarentenas_final.rds")


# zonas censales
zc<- st_read("insumos/espec-pap.shp")

# paso a paso
cuarentenas_pap<- read_excel("cuarentenas_2021_2022.xlsx", sheet = "pasoapaso")
# fases de imacto bajo, medio, alto
cuarentenas_fases<- read_excel("cuarentenas_2021_2022.xlsx", sheet = "fases")

fechas<- read_excel("cuarentenas_2021_20221.xlsx", sheet = "fechas")

fechas<- fechas %>% 
  mutate(fecha_cod = as.character(fecha_cod)) %>% 
  mutate(fecha = dmy(as.character(paste0(dia,"/",mes,"/",yr)))) %>%  
  select(fecha_cod, fecha)

# Paso zonas a mayusculas y sin tilde -------------------------------------
# uarentenas_pap<- may_cap_letter(cuarentenas_pap, zona)
# cuarentenas_fases<- may_cap_letter(cuarentenas_fases, zona)

cuarentenas_pap<- cuarentenas_pap %>%  
  may_cap_letter(., zona) %>%  
  cod_com_character(., codigo_comuna)

cuarentenas_fases<- cuarentenas_fases %>%  
  may_cap_letter(., zona) %>%  
  cod_com_character(., codigo_comuna)


unique(cuarentenas_pap$cod_com)
colnames(cuarentenas_fases)

# 
# unique(pap_espec$zona[!is.na(pap_espec$zona) ])
# unique(cuarentenas_pap$zona[!is.na(cuarentenas_pap$zona) ])
# unique(cuarentenas_fases$zona[!is.na(cuarentenas_fases$zona) ])
# 
# cuarentenas_pap<- cod_com_character(cuarentenas_pap, codigo_comuna)
# cuarentenas_fases<- cod_com_character(cuarentenas_fases, codigo_comuna)



# zonas normales -------------------------------------------------

zc_df<- st_drop_geometry(zc) %>% 
  filter(is.na(zona)) %>% 
  mutate(urbano = ifelse(AREA == 1,1,0),
         RURAL = ifelse(AREA == 2,1,0)) %>% 
  rename(gcod = COD_INE_16) %>% 
  mutate(cod_com = ifelse(str_length(COMUNA)==4, 
                          paste0(0,COMUNA), 
                          COMUNA)) %>%  
  dplyr::select(gcod, cod_com, urbano)

zc_rural<- zc_df %>%  
  filter(urbano == 0 ) 

zc_urb<- zc_df %>%  
  filter(urbano == 1 ) 

# zonas especiales --------------------------------------------------------

zonas_especiales<- zc %>% 
  st_drop_geometry() %>% 
  filter(!is.na(zona)) %>%
  mutate(urbano = ifelse(AREA == 1,1,0),
         RURAL = ifelse(AREA == 2,1,0)) %>% 
  rename(gcod = COD_INE_16) %>% 
  mutate(cod_com = ifelse(str_length(COMUNA)==4, 
                          paste0(0,COMUNA), 
                          COMUNA)) %>%  
  dplyr::select(gcod, cod_com, zona, urbano)
  

head(zonas_especiales)
head(cuarentenas_pap)


# separo por rural y urbano -----------------------------------------------

# rural
pap_rural <- cuarentenas_pap %>%  
  filter(zona == "RURAL" | zona == "TOTAL") %>%  
  select(-c(codigo_region, region_residencia, codigo_comuna, comuna_residencia, zona))

fases_rural<- cuarentenas_fases %>% 
  filter(zona == "RURAL" | zona == "TOTAL") %>%  
  select(-c(codigo_region, region_residencia, codigo_comuna, comuna_residencia, zona))


zc_rural_pap<- zc_rural %>%  
  select(gcod, cod_com, urbano) %>% 
  left_join(pap_rural, by = "cod_com") %>%  
  pivot_longer(!c(gcod, cod_com, urbano), 
               names_to = "fecha_cod", 
               values_to = "pap") %>% 
  left_join(fechas, by  = "fecha_cod") %>%  
  mutate(fase = NA)

zc_rural_fase<- zc_rural %>%  
  select(gcod, cod_com, urbano) %>% 
  left_join(fases_rural, by = "cod_com") %>%  
  pivot_longer(!c(gcod, cod_com, urbano), 
               names_to = "fecha_cod", 
               values_to = "fase") %>% 
  left_join(fechas, by  = "fecha_cod") %>%  
  mutate(pap = NA)

colnames(zc_rural_pap)
colnames(zc_rural_fase)

zonas_normales_rural<- rbind(zc_rural_pap, zc_rural_fase) 
saveRDS(zonas_normales_rural, "resultados intermedios/zonas_normales_rural.rds")

# urbano 
pap_urb <- cuarentenas_pap %>%  
  filter(zona == "URBANA" | zona == "TOTAL") %>%  
  select(-c(codigo_region, region_residencia, codigo_comuna, comuna_residencia, zona))

fases_urb<- cuarentenas_fases %>% 
  filter(zona == "URBANA" | zona == "TOTAL") %>%  
  select(-c(codigo_region, region_residencia, codigo_comuna, comuna_residencia, zona))

zc_urb_pap<- zc_urb %>%  
  select(gcod, cod_com, urbano) %>% 
  left_join(pap_urb, by = "cod_com") %>%  
  pivot_longer(!c(gcod, cod_com, urbano), 
               names_to = "fecha_cod", 
               values_to = "pap") %>% 
  left_join(fechas, by  = "fecha_cod") %>%  
  mutate(fase = NA)

zc_urb_fase<- zc_urb %>%  
  select(gcod, cod_com, urbano) %>% 
  left_join(fases_urb, by = "cod_com") %>%  
  pivot_longer(!c(gcod, cod_com, urbano), 
               names_to = "fecha_cod", 
               values_to = "fase") %>% 
  left_join(fechas, by  = "fecha_cod") %>%  
  mutate(pap = NA)

colnames(zc_urb_pap)
colnames(zc_urb_fase)

zonas_normales_urb<- rbind(zc_urb_pap, zc_urb_fase) 
saveRDS(zonas_normales_urb, "resultados intermedios/zonas_normales_urb.rds")

# zonas especiales 

zc_esp_pap <- cuarentenas_pap %>%  
  filter(!(zona == "URBANA" | zona == "TOTAL" | zona == "RURAL")) %>%  
  select(-c(codigo_region, region_residencia, codigo_comuna, comuna_residencia))

zc_esp_fases<- cuarentenas_fases %>% 
  filter(!(zona == "URBANA" & zona == "TOTAL" & zona == "RURAL")) %>%  
  select(-c(codigo_region, region_residencia, codigo_comuna, comuna_residencia))


zc_espec_pap<- zonas_especiales %>%  
  left_join(zc_esp_pap, by = c("cod_com" , "zona")) %>%  
  pivot_longer(!c(gcod, cod_com, urbano, zona), 
               names_to = "fecha_cod", 
               values_to = "pap") %>% 
  left_join(fechas, by  = "fecha_cod") %>%  
  dplyr::select(-zona) %>%  
  mutate(fase = NA)

zc_espec_fases<- zonas_especiales %>%  
  left_join(zc_esp_fases, by = c("cod_com" , "zona")) %>%  
  pivot_longer(!c(gcod, cod_com, urbano, zona), 
               names_to = "fecha_cod", 
               values_to = "fase") %>% 
  left_join(fechas, by  = "fecha_cod") %>% 
  dplyr::select(-zona) %>%  
  mutate(pap = NA)


res_pap<- rbind(zc_espec_pap, zc_urb_pap, zc_rural_pap)
res_fases<- rbind(zc_espec_fases, zc_urb_fase, zc_rural_fase)

saveRDS(res_pap, "resultados intermedios/res_pap.rds")
saveRDS(res_fases, "resultados intermedios/res_fases.rds")

# comprobacion
res_pap %>%  
  group_by(gcod) %>%  
  summarise(n=n()) %>%  
  group_by(n) %>%  
  summarise(n=n())

res_fases %>%  
  group_by(gcod) %>%  
  summarise(n=n()) %>%  
  group_by(gcod) %>%  
  summarise(n=n())


sum(is.na(res_pap$pap))
sum(is.na(res_fases$fase))

res_pap %>%  
  distinct(gcod) %>%  
  nrow()


# unir panel paso a paso y fases  -----------------------------------------

zc_all<- st_drop_geometry(zc) %>% 
  mutate(urbano = ifelse(AREA == 1,1,0),
         RURAL = ifelse(AREA == 2,1,0)) %>% 
  #filter(urbano == 1 ) %>% 
  rename(gcod = COD_INE_16) %>% 
  mutate(cod_com = ifelse(str_length(COMUNA)==4, 
                          paste0(0,COMUNA), 
                          COMUNA)) %>%  
  dplyr::select(gcod, urbano)


fecha_inicio<- min(res_pap$fecha)
fecha_fin<- max(res_fases$fecha)
freq = "day"

panel_zc<- make_panel_zc(zc_all, fecha_inicio, fecha_fin , freq)

saveRDS(panel_zc, "resultados intermedios/panel_zc.rds")

panel_cuarentena_bruta<- panel_zc %>%  
  rename(fecha = date_seq) %>% 
  left_join(res_pap, by = c("gcod", "fecha")) %>% 
  left_join(res_fases, by = c("gcod", "fecha"))

saveRDS(panel_cuarentena_bruta, "resultados intermedios/panel_cuarentena_bruta_2021_2022.rds")

panel_cuarentena_pruebas<- panel_cuarentena %>%  
  mutate(tiene_dato = ifelse(!is.na(fecha_cod.x)| !is.na(fecha_cod.y), 1, 0))

sum(is.na(panel_cuarentena_pruebas$tiene_dato))

