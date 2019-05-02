library(tidyverse)
library (lubridate)

# Carga de la base y division por presidente ------------------------------

infojus <- read_csv("base-infoleg-normativa-nacional.csv")
options(scipen=999) #elimina notacion cientifica
infojus <- infojus %>% 
  mutate(presidente = case_when(fecha_sancion >= "1983-12-10" & fecha_sancion <= "1989-07-08" ~ "Alfonsin",
                                fecha_sancion >= "1989-07-09" & fecha_sancion <= "1999-12-09" ~ "Menem",
                                fecha_sancion >= "1999-12-10" & fecha_sancion <= "2001-12-20" ~ "De la Rua",
                                fecha_sancion >= "2001-12-21" & fecha_sancion <= "2001-12-30" ~ "Rodriguez Saa",
                                fecha_sancion >= "2001-12-31" & fecha_sancion <= "2003-05-24" ~ "Duhalde",
                                fecha_sancion >= "2003-05-25" & fecha_sancion <= "2007-12-09" ~ "Kirchner",
                                fecha_sancion >= "2007-12-10" & fecha_sancion <= "2015-12-09" ~ "Fernandez de Kirchner",
                                fecha_sancion >= "2015-12-10" & fecha_sancion <= "2019-12-10" ~ "Macri",
                                TRUE ~ "Otros"))


# Leyes y decretos desde Alfonsin -----------------------------------------

lista <- c("artículo 99, inciso 3", "artículo 99, incisos 1 y 3", "artículo 99, incisos 2 y 3", "artículo 99, incisos 1, 2 y 3")

#detectando DNU en cada link de Ley y Decreto a partir del 24-08-1994
infojus_leyes <- infojus %>%
  mutate(texto_original = replace(texto_original, id_norma == 43004, NA)) %>%  #eliminando el error de pagina
  filter(tipo_norma == "Ley"| tipo_norma == "Decreto",
         !is.na(texto_original),
         fecha_sancion >= "1994-08-24")  %>%       #desde la entrada en vigencia de la CN 1994
  mutate(DNU = str_detect(htmlToText(texto_original), paste(lista, collapse = "|")))

## Cuadro por presidente y por ano leyes y decretos
periodo <- infojus %>% 
  group_by(presidente, tipo_norma, year(fecha_sancion)) %>% 
  summarise(cant = n()) %>% 
  filter(tipo_norma == "Ley"| tipo_norma == "Decreto") %>% 
  filter(presidente !="Otros") %>% 
  #spread(key = tipo_norma, value = cant) %>% 
  rename(Ano = `year(fecha_sancion)`) %>% 
  arrange(desc(Ano)) %>% 
  ungroup() %>% 
  group_by(tipo_norma) %>% 
  mutate(prom = round(mean(cant),0))

 #grafico presidente y ano de ley y decreto
ggplot(data = periodo , aes(x= Ano, y = cant))+
  geom_col(aes(y = cant, fill = presidente), position = position_stack( reverse = TRUE))+
  geom_text(aes(label = cant, y = cant), size = 2.5, position = position_stack(vjust =1.05))+
  scale_x_continuous(breaks = seq(1983,2019,1), labels = str_sub(seq(1983, 2019,1), start = -2))+
  facet_wrap(~tipo_norma, nrow = 3, scales = "free")+
  geom_hline(aes(yintercept = prom), colour = "grey50")


# Analisis de Decretos Menem -------------------------------------------------------

menem <-  infojus %>% 
  filter(presidente == "Menem", tipo_norma == "Decreto") %>% 
  filter(!is.na(modifica_a)) %>% 
  mutate(texto_original = replace(texto_original, id_norma == 43004, NA)) %>%  #eliminando el error de pagina
  filter(!is.na(texto_original))

menem_limpio <- menem %>% 
  filter(str_detect(texto_resumido, "LEY"),
         str_detect(texto_resumido, "PROMULGACION", negate = TRUE),
         str_detect(texto_resumido, "PROMULGASE", negate = TRUE),
         str_detect(titulo_resumido, "PROMULGACION", negate = TRUE),
         str_detect(texto_resumido, "REGLAMENTACION", negate = TRUE),
         str_detect(titulo_resumido, "REGLAMENTACION", negate = TRUE)) %>% 
  filter(!is.na(texto_original))

menem_dnu <- menem %>% 
  mutate(DNU =str_detect(htmlToText(texto_original), "artículo 99, inciso 3"))


# Analisis DNU Leyes y Decretos por presidente ----------------------------

#agregar DNU a tipo de norma en reemplazo de Decreto, filtrar 1995 a 2018
infojus_dnu <- infojus_leyes %>% 
  mutate(tipo_norma = case_when(DNU == TRUE ~ "DNU", 
                                TRUE ~ tipo_norma)) %>% 
  filter(fecha_sancion >= "1995-01-01" & fecha_sancion <= "2018-12-31")

save(infojus_dnu, file = "infojus_dnu.rdata") #guardando para evitar 20 mins de procesamiento


# Cuadro y Grafico de DNU por presidente ----------------------------------

## DNU, Decreto y Leyes por ano y presidente
periodo_dnu <- infojus_dnu %>% 
  group_by(presidente, tipo_norma, year(fecha_sancion)) %>% 
  summarise(cant = n()) %>% 
  rename(Ano = `year(fecha_sancion)`) %>% 
  arrange(desc(Ano)) %>% 
  ungroup() %>% 
  group_by(tipo_norma) %>% 
  mutate(prom = round(mean(cant),0))

## Graficando cuadro de Ley, Decreto, DNU por ano y presidente

ggplot(data = periodo_dnu , aes(x= Ano, y = cant))+
  geom_col(aes (fill = presidente), position = position_stack( reverse = TRUE))+
  geom_text(aes(label = cant, y = cant), size = 2.5, position = position_stack(vjust =1.05))+
  scale_x_continuous(breaks = seq(1995,2018,1), labels = str_sub(seq(1995, 2018,1), start = -2))+
  facet_wrap(~tipo_norma, nrow = 3, scales = "free")+
  geom_hline(aes(yintercept = prom), colour = "grey50")+
  labs(title = "Leyes, Decretos y DNU por año y presidente", x = "Año", y="")+
  geom_text(aes(x=1993, y = prom+3, label = prom), size = 2.5)+
  theme_bw()


# Cuadro DNU por presidente paa compararlo con G.A.------------------------------------

xpresidente <- infojus_dnu %>% 
  group_by(presidente, tipo_norma) %>% 
  summarise(cant = n())
