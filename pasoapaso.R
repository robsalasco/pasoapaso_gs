library(httr)
library(rvest)
library(stringr)
library(jsonlite)
library(sf)
library(stringi)
library(tmap)
library(dplyr)
library(purrr)

paso <- read_html("https://e.infogram.com/81277d3a-5813-46f7-a270-79d1768a70b2") %>% 
  html_nodes(xpath="/html/body/script") %>% 
  .[1] %>% 
  html_text() %>% 
  str_extract("(?<=window.infographicData=).*(?=;$)")  %>% 
  fromJSON()
  
data <- matrix(paso$elements$content$content$entities$`3f026fbf-998f-4ae2-852b-94fa3a2f71d4`$props$chartData$data,
       nrow = 346) %>% 
  as_tibble(.name_repair = ~ c("comuna","paso","estado")) %>% 
  slice(-1) %>% 
  mutate(comuna = str_to_upper(comuna)) %>%
  mutate(paso= factor(paso, labels = c("Cuarentena", "Paso 2", "Paso 3", "Paso 4")))
  
gs <- st_read("https://raw.githubusercontent.com/robsalasco/precenso_2016_geojson_chile/master/Extras/GRAN_SANTIAGO.geojson")

data_sf <- inner_join(gs, data, by=c("NOM_COMUNA"="comuna"))

data_sf %>% st_write("quarantine.geojson", delete_dsn = TRUE)

tmap_mode("plot")

plot_q <- tm_shape(data_sf) + 
  tm_polygons(col="paso", 
              title=paste0("Plan Paso a Paso Gran Santiago (", format(Sys.Date(),"%d-%m-%Y"),")"), 
              palette = "-Blues",  
              border.col = "black", 
              border.alpha = 0.5,
              lwd=1) + 
  tm_text(text = "NOM_COMUNA", size = 0.3) +
  tm_credits("Fuente: Ministerio de Salud; Gobierno de Chile (2021)", position=c("right", "bottom"), size=0.55) +
  tm_layout(legend.width=1,
            inner.margins = c(0.1, 0.1, 0.10, 0.1), 
            frame=FALSE) +
  tm_compass(type = "8star", position = c(.85, .80))

tmap_save(plot_q, "quarantine.png", width=6, height = 6, units="in")
