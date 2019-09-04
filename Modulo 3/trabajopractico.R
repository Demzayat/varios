library(tidyverse)
library(sf)
library(leaflet)


# leyendo los barrios informales del repo nacional ------------------------

villas <- st_read(dsn = "/Users/demian/Downloads/CABA Datos", 
                  layer = "CABA_GA_WGS84")
#barrios_populares <- st_read(dsn = "/Users/demian/Downloads/barrios-populares", 
                             layer = "barrios-populares")
#barrios_caba <- barrios_populares %>% 
  filter(provincia == "CIUDAD AUTONOMA DE BUENOS AIRES")
ggplot()+
  geom_sf(data = barrios_caba)
  geom_sf(data = comunas, fill = NA)

#comunas <- st_read("/Users/demian/Downloads/CABA_comunas.geojson")
#censo <- st_read(dsn = "/Users/demian/Downloads/radios_caba_censo_2010", 
                 layer = "radios_censo_2010")
radios_caba <- st_read("/Users/demian/Downloads/radios_censo_caba.geojson")

ggplot()+
  geom_sf(data = radios_caba, fill = NA, col = "grey40")+
  geom_sf(data = villas, col = "red")

# censo 2010 --------------------------------------------------------------


censo2010 <- read_csv2("/Users/demian/Documents/Rcran/Censo/CNPHyV-2010.csv/PERSONA.csv")
hogar2010 <- read_csv2("/Users/demian/Documents/Rcran/Censo/CNPHyV-2010.csv/HOGAR.csv")
vivienda2010 <- read_csv2("/Users/demian/Documents/Rcran/Censo/CNPHyV-2010.csv/VIVIENDA.csv")
radio2010 <- read_csv2("/Users/demian/Documents/Rcran/Censo/CNPHyV-2010.csv/RADIO.csv")
frac2010 <- read_csv2("/Users/demian/Documents/Rcran/Censo/CNPHyV-2010.csv/FRAC.csv")
depto2010 <- read_csv2("/Users/demian/Documents/Rcran/Censo/CNPHyV-2010.csv/DPTO.csv")
prov2010 <- read_csv2("/Users/demian/Documents/Rcran/Censo/CNPHyV-2010.csv/PROV.csv")


vivienda_hogar <- vivienda2010 %>% 
  select(VIVIENDA_REF_ID, RADIO_REF_ID) %>% 
  left_join(hogar2010, by = "VIVIENDA_REF_ID")
  
save(vivienda_hogar, file = "vivienda_hogar.rds")
censo2010 <- censo2010 %>% 
  inner_join(, by = "HOGAR_REF_ID")

radio_provincia_caba <- radio2010 %>% 
  left_join(frac2010, by = "FRAC_REF_ID") %>% 
  left_join(depto2010, by = "DPTO_REF_ID") %>% 
  left_join(prov2010, by = "PROV_REF_ID") %>% 
  filter(PROV_REF_ID == 1)
  
vivienda_hogar <- vivienda_hogar %>% 
  inner_join(radio_provincia_caba, by = "RADIO_REF_ID")

# EMPEZAR DESDE EL CENSO --------------------------------------------------


# cargar ------------------------------------------------------------------

load(file = "/Users/demian/Documents/Cursos/Big Data e IT/Modulo3/TP/censo2010caba.rds")
radio_persona_caba <- radio_persona_caba %>% 
  rename("relacion_jefe" = P01,
         "sexo" = P02,
         "edad" = P03,
         "nacimiento" = P05,
         "pais" = P06,
         "lee_escribe" = P07,
         "usa_comput" = P12,
         "escolaridad" = P08,
         "nivel" = P09,
         "completo" = P10,
         "actividad" = CONDACT,
         "material_piso" = H05,
         "material_techo" = H06,
         "revestimientos" = H07)
radio_persona_caba <- radio_persona_caba %>% 
  mutate(IDFRAC = as.numeric(IDFRAC),
         IDRADIO = as.numeric(IDRADIO)) %>% 
  unite(CO_FRAC_RA, DPTO_REF_ID, IDFRAC, IDRADIO, sep = "_", remove = FALSE)

# datos geo
censo_geo <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/informacion-censal-por-radio/CABA_rc.geojson")
censo_geo_radio <- censo_geo %>% 
  select(CO_FRAC_RA = RADIO_ID)

# uniendo censo personas a radio censal
radio_persona_caba <- radio_persona_caba %>% 
  left_join(censo_geo_radio)


# eligiendo radios con barrio emergencia ----------------------------------

barrios_radios <- radios_caba %>% 
  filter(st_intersects(x = ., 
                y = villas, sparse = FALSE))


barrios_unidos <- st_union(barrios_caba)
ggplot()+
  geom_sf(data = censo_geo_radio, fill = NA, col = "grey40")+
  geom_sf(data =barrios_unidos, fill = "red")
  

# Comuna 4 ----------------------------------------------------------------

comuna4_geo <- radios_caba%>% 
  filter(COMUNA == 4) %>% 
  select(RADIO_ID)
comuna4_villas <- villas %>% 
  filter(DTO == "Comuna 4") %>% 
  select(FNA)

ggplot()+
  geom_sf(data = comuna4_geo, fill = NA, col = "grey40")+
  geom_sf(data = comuna4_villas, fill = "red")+
  labs(title = "Comuna 4. Radios censales y Asentamientos")+
  theme_minimal()

radios_villas_4 <- st_intersects(comuna4_geo, comuna4_villas)
radios_villas_4 <- lengths(radios_villas_4) > 0

comuna4 <- cbind(comuna4_geo, radios_villas_4) %>% 
  filter(radios_villas_4 == TRUE)

ggplot()+
  geom_sf(data = comuna4, fill = "red")+
  geom_sf(data = comuna4_geo, fill = NA)+
  geom_sf(data = comuna4_villas, fill = "blue")


# Uniendo con los datos del censo -----------------------------------------
radio_persona_caba$CO_FRAC_RA <- factor(radio_persona_caba$CO_FRAC_RA)

comuna4_censo_barrios <- radio_persona_caba %>% 
  filter(CO_FRAC_RA %in% comuna4$RADIO_ID)


# probando corregir los barrios -------------------------------------------
barrios <- st_read(dsn = "/Users/demian/Downloads/CABA Datos", 
                   layer = "CABA_GA_WGS84", quiet = TRUE)
barrios_bis <- st_geometry(barrios)
barrios_bis <- barrios_bis + c(0.00055,-0.00035)
barrios_bis_centroides <- st_centroid(barrios_bis)
barrios_bis <- (barrios_bis - barrios_bis_centroides)*0.97 + barrios_bis_centroides
barrios_bis <- st_sf(barrios_bis, crs = 4326)
barrios$geometry <- st_geometry(barrios_bis)


comuna4_barrios <- barrios%>% 
  filter(DTO == "Comuna 4") 

ggplot()+
  geom_sf(data = comuna4_geo, fill = NA, col = "grey40")+
  geom_sf(data = comuna4_barrios, fill = "green")+
  labs(title = "Comuna 4.")+
  theme_minimal()
  

# Niños en edad escolar ---------------------------------------------------

edad_escolar <- comuna4_censo_barrios %>% 
  filter(edad >= 6 & edad <= 12)

nivel_primario <- comuna4_censo_barrios %>% 
  filter(nivel == 2 & escolaridad == 1)

edad_noasiste <- edad_escolar %>% 
  filter(escolaridad == 2 | escolaridad == 3) %>% 
  filter(completo == 2 | completo == 0)

#

# Sumando las escuelas ----------------------------------------------------

escuelas <-  st_read(dsn = "establecimientos-educativos-zip",
                     layer = "establecimientos-educativos") %>% 
  st_transform(4326) 
primarios <- escuelas %>% 
  filter(NIVMOD == "PriCom") %>% 
  mutate(CUEANEXO = as.numeric(as.character(CUEANEXO)))

ggplot()+
  geom_sf(data = radios_caba, col = NA)+
  geom_sf(data = primarios)


mapa_escuelas <- leaflet(primarios_vacantes %>% filter (COMUNA == 4)) %>% 
  setView(lng =-58.399685,lat = -34.654402, zoom = 13) %>% 
  addTiles() %>% 
  addTopoJSON(comuna4_villas) %>% 
  addMarkers(label = ~as.character(total), 
             popup = ~NOMBRE_ABR)
htmlwidgets::saveWidget(mapa_escuelas, file = "mapa_escuelas.html")


# Vacantes ----------------------------------------------------------------


vacantes17 <- read_csv2("inscripciones-escolares-2017.csv", locale = locale(encoding = "latin1") ) %>% 
  filter(NIVEL == "PRIMARIO") %>% 
  group_by(ESTABLECIMIENTO, CUE_ANEXO) %>% 
  summarise(primero_17 = n())

vacantes16 <- read_csv2("inscripciones-escolares-2016.csv", locale = locale(encoding = "UTF-8") )%>% 
  filter(NIVEL == "PRIMARIO") %>% 
  group_by(ESTABLECIMIENTO) %>% 
  summarise(primero_16 = n())  


vacantes_prom <- vacantes17 %>% 
  bind_cols(., vacantes16) %>% 
  select(-ESTABLECIMIENTO1) %>% 
  mutate(total = round((primero_17+primero_16)/2*7,0)) %>% 
  ungroup() %>% 
  select(-primero_17, -primero_16, -ESTABLECIMIENTO)

primarios_vacantes <- primarios %>% 
  left_join(vacantes_prom, by = c("CUEANEXO" = "CUE_ANEXO")) %>% 
  select(CUEANEXO, DOM_EDIFIC, NOMBRE_ABR, DE, COMUNA, total)

primarios_vacantes <- primarios_vacantes %>% 
  mutate(long = st_coordinates(primarios_vacantes)[,1],
         lat = st_coordinates(primarios_vacantes)[,2])

leaflet(st_union(comuna4_geo)) %>% 
  addTiles() %>% 
  addPolygons() %>% 
  addMarkers(map = primarios_vacantes,
    lng = primarios_vacantes$long,
    lat = primarios_vacantes$lat,  
    label = ~as.character(total),
    popup = ~NOMBRE_ABR)

         