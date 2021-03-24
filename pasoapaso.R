library(httr)
library(rvest)
library(stringr)
library(jsonlite)
library(sf)
library(stringi)
library(mapsf)
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

data_sf <- data_sf %>% st_transform(32719)

mf_init(x = data_sf, theme = "dark", expandBB = c(.09,.09,.09,.2),
        export = "png", filename = "quarantine.png", res = 300, width = 2000, height = 2000) 
mf_shadow(data_sf, col = "grey30", add = TRUE)
mf_map(x = data_sf, 
       var = "paso",
       type = "typo",
       pal = "Sunset", 
       leg_title = NULL, 
       add = TRUE,
       leg_pos = "topleft")
mf_label(x = data_sf, var = "NOM_COMUNA", overlap=FALSE, cex = 0.35, halo = TRUE, col = "white", r = 0.05, bg = "grey40")
mf_inset_on(x = "worldmap", pos = "right")
mf_worldmap(data_sf, col = "black")
mf_inset_off()
mf_title(paste0("Plan Paso a Paso Gran Santiago (", format(Sys.Date(),"%d-%m-%Y"),")"))
mf_credits("Ministerio de Salud, Gobierno de Chile (2021)", pos = "bottomright")
mf_scale(pos = "bottomleft")
mf_arrow('topright')
dev.off()
