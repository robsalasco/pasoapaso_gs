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
  as_tibble() %>% 
  slice(-1) %>% 
  set_names(c("comuna","paso","estado")) %>% 
  mutate(comuna = str_to_upper(comuna))
  
gs <- st_read("https://raw.githubusercontent.com/robsalasco/precenso_2016_geojson_chile/master/Extras/GRAN_SANTIAGO.geojson")

data_sf <- inner_join(gs, data, by=c("NOM_COMUNA"="comuna"))

data_sf %>% st_write("quarantine.geojson")

tmap_mode("plot")

plot_q <- tm_shape(data_sf) + 
  tm_polygons(col="paso") + 
  tm_text(text = "NOM_COMUNA", size = 0.3)

tmap_save(plot_q, "quarantine.png", width=6, height = 6, units="in")
