
rm(list=ls())
library(dplyr)



# Consolida panel 2020 y 2021 - 2022 --------------------------------------


panel_2020<- readRDS("panel_cuarentenas_2020.rds")
panel_2021_2022<- readRDS("resultados intermedios/panel_cuarentena_bruta_2021_2022.rds")

panel_20<- panel_2020 %>%  
  rename(gcod = GEOCODIGO, 
         fecha = date) %>%  
  dplyr::select(gcod, fecha, cuarentena)


panel_21_22<- panel_2021_2022 %>%
  mutate(pap = ifelse(is.na(pap.x), pap.y, pap.x), 
         fase = ifelse(is.na(fase.x), fase.y, fase.x)) %>% 
  dplyr::select(gcod, fecha, pap, fase) %>%  
  mutate(cuar_pap = ifelse(pap>=3 | is.na(pap), 0, 1), 
         cuar_fase = ifelse(fase == "Bajo" | fase == "Medio" | is.na(fase), 0,0), 
         cuarentena = cuar_pap + cuar_fase) %>%  
  select(gcod, fecha, cuarentena)
  

# Unir paneles ------------------------------------------------------------

panel_all_cuarentenas<- rbind(panel_20, panel_21_22)

saveRDS(panel_all_cuarentenas, "resultados intermedios/panel_all_cuarentenas.rds")



