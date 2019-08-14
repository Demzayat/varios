library(tidyverse)
library(grid)
library(gridExtra)

descripciones <- read_delim("/Users/demian/Documents/Rcran/hackaton_paso/PASO2019/SmartMatic/120819-054029/descripcion_postulaciones.dsv",delim = "|") 
descripcion_region <- read_delim("/Users/demian/Documents/Rcran/hackaton_paso/PASO2019/SmartMatic/120819-054029/descripcion_regiones.dsv",delim = "|") 
total_agrupados <- read_delim("/Users/demian/Documents/Rcran/hackaton_paso/PASO2019/SmartMatic/120819-054029/mesas_totales_agrp_politica.dsv",delim = "|") 

blanco <-  read_delim("/Users/demian/Documents/Rcran/hackaton_paso/PASO2019/SmartMatic/120819-054029/mesas_totales.dsv",delim = "|")

pais<- read_delim("/Users/demian/Documents/Rcran/hackaton_paso/PASO2019/SmartMatic/120819-054029/mesas_totales_lista.dsv",delim = "|") 
  
caba <- pais %>% 
  filter(CODIGO_DISTRITO == "01")  %>% 
  mutate(CODIGO_CATEGORIA = recode(CODIGO_CATEGORIA,
                                   "000100000000000" = "Presidente",
                                   "000201000000000" = "Senadores",
                                   "000301000000000" = "Diputados",
                                   "000501000000000" = "Jefe de Gob.",
                                   "000801000000000" = "Legisladores Caba",
                                   "000901001000000" = "Junta Comunal 01",
                                   "000901002000000" = "Junta Comunal 02",
                                   "000901003000000" = "Junta Comunal 03",
                                   "000901004000000" = "Junta Comunal 04",
                                   "000901005000000" = "Junta Comunal 05",
                                   "000901006000000" = "Junta Comunal 06",
                                   "000901007000000" = "Junta Comunal 07",
                                   "000901008000000" = "Junta Comunal 08",
                                   "000901009000000" = "Junta Comunal 09",
                                   "000901010000000" = "Junta Comunal 10",
                                   "000901011000000" = "Junta Comunal 11",
                                   "000901012000000" = "Junta Comunal 12",
                                   "000901013000000" = "Junta Comunal 13",
                                   "000901014000000" = "Junta Comunal 14",
                                   "000901015000000" = "Junta Comunal 15"),
         CODIGO_AGRUPACION = recode(CODIGO_AGRUPACION,
                                    "01-187" = "Autodeterminacion y Libertad",
                                    "01-501" = "FIT",
                                    "01-502" = "Frente de Todos",
                                    "01-503" = "Juntos por el Cambio",
                                    "01-504" = "Consenso Federal",
                                    "13" = "MAS",
                                    "87" = "Libertad y Dignidad",
                                    "88" = "Dignidad Popular",
                                    "5" = "Democrata Cristiana"))


caba_migrante <- caba%>% 
  filter(str_detect(caba$CODIGO_MESA, ".E"))

caba_nacional <- caba %>% 
  filter(str_detect(caba$CODIGO_MESA, ".E", negate = TRUE))

caba_migrante_voto <- caba_migrante %>% 
  group_by(CODIGO_SECCION, CODIGO_CATEGORIA,CODIGO_MESA, CODIGO_AGRUPACION ) %>% 
  summarize(votos = sum(VOTOS_LISTA)) %>% 
  mutate(comuna = str_sub(CODIGO_SECCION, -2)) 


jefe_gob <- caba_migrante_voto %>% 
  filter(CODIGO_CATEGORIA == "Jefe de Gob.") %>% 
  group_by( comuna, CODIGO_AGRUPACION) %>% 
  summarise(total = sum(votos)) %>% 
  spread(key = comuna, value = total)
  
jefe_gob_porc <- caba_migrante_voto %>% 
  filter(CODIGO_CATEGORIA == "Jefe de Gob.") %>% 
  group_by( comuna, CODIGO_AGRUPACION) %>% 
  summarise(total = sum(votos)) %>% 
  mutate(porc = round(total /sum(total)*100,2)) %>% 
  select(-total)
  spread(key = comuna, value = porc)

jefe_gob_unido <- left_join(jefe_gob, jefe_gob_porc, by = "CODIGO_AGRUPACION") %>% 
  select(CODIGO_AGRUPACION,"01.x", "01.y", "02.x", "02.y", "03.x", "03.y",
         "04.x", "04.y", "05.x", "05.y", "06.x", "06.y", "07.x", "07.y",
         "08.x", "08.y", "09.x", "09.y", "10.x", "10.y", "11.x", "11.y", 
         "12.x", "12.y", "13.x", "13.y", "14.x", "14.y", "15.x", "15.y")


# caba no migrantes -------------------------------------------------------

caba_nacional_voto <- caba_nacional %>% 
  group_by(CODIGO_SECCION, CODIGO_CATEGORIA,CODIGO_MESA, CODIGO_AGRUPACION ) %>% 
  summarize(votos = sum(VOTOS_LISTA)) %>% 
  mutate(comuna = str_sub(CODIGO_SECCION, -2)) 


jefe_gob_nac <- caba_nacional_voto %>% 
  filter(CODIGO_CATEGORIA == "Jefe de Gob.") %>% 
  group_by( comuna, CODIGO_AGRUPACION) %>% 
  summarise(total = sum(votos)) %>% 
  spread(key = comuna, value = total)

jefe_gob_porc_nac <- caba_nacional_voto %>% 
  filter(CODIGO_CATEGORIA == "Jefe de Gob.") %>% 
  group_by( comuna, CODIGO_AGRUPACION) %>% 
  summarise(total = sum(votos)) %>% 
  mutate(porc = round(total /sum(total)*100,2)) %>% 
  select(-total) 
  spread(key = comuna, value = porc)

jefe_gob_unido_nacional <- left_join(jefe_gob_nac, jefe_gob_porc_nac, by = "CODIGO_AGRUPACION") %>% 
  select(CODIGO_AGRUPACION,"01.x", "01.y", "02.x", "02.y", "03.x", "03.y",
         "04.x", "04.y", "05.x", "05.y", "06.x", "06.y", "07.x", "07.y",
         "08.x", "08.y", "09.x", "09.y", "10.x", "10.y", "11.x", "11.y", 
         "12.x", "12.y", "13.x", "13.y", "14.x", "14.y", "15.x", "15.y")

# graficos ----------------------------------------------------------------
paleta <- c("#800080", "#008000", "#DC143C", "#FF0000", "#1E90FF", "#FFD700", "#A9A9A9", "#FF8C00" )
nacional <- ggplot()+
  geom_col(data = jefe_gob_porc_nac, aes(x = CODIGO_AGRUPACION, y = porc, fill = CODIGO_AGRUPACION))+
  facet_wrap(~comuna)+
  labs(title = "Porcentaje voto nacionales por comuna", x = "", subtitle = "Jefe de Gobierno")+
  scale_fill_manual (values = paleta)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

migrante <- ggplot()+
  geom_col(data = jefe_gob_porc, aes(x = CODIGO_AGRUPACION, y = porc, fill = CODIGO_AGRUPACION))+
  facet_wrap(~comuna)+
  labs(title = "Porcentaje voto migrantes por comuna", x = "", subtitle = "Jefe de Gobierno")+
  scale_fill_manual (values = paleta)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(nacional, migrante, nrow = 1)
