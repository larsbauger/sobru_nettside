# HEADER --------------------------------------------
#
# Author: Lars Bauger
# Copyright (c) Lars Bauger, 2025
# Email:  lars.bauger@usn.no
# 
# Date: 2025-03-31
#
# Script Name:sobru_descriptives
#
# Script Description:dette scriptet bearbeider og lager presentasjoner av forekomst av utenforsskap i vår region.
#
#
# Notes:
#
# Loading libraries
library(tidyverse)

# loading data
library(readxl)
df <- read_excel(here::here("data/trial.xlsx"))



# gjøre det om til longform
df_long <- df |> 
  pivot_longer(
    cols = 3:58, # velger kommuner
    names_to = "region",
    values_to = "value"
  ) |> 
  pivot_wider(
    names_from = cat,
    values_from = value) |> 
  janitor::clean_names() |> 
      select(region, everything()) # Flytt municipality til første kolonne

df_long <- df_long |> 
  mutate(neet_tot_prop = round((neet_tot_pop / tot_pop) * 100, 1),
         neet_exc_imm_prop = round((neet_pop_exc_imm / tot_pop_exc_imm) * 100, 1),
         neet_immig_prop = round((neet_pop_immig / tot_pop_immig) * 100, 1),
         region = str_remove(region, "K-\\d{4} ")) #fjerner unødvendig tekst

df_long
unique(df_long$region)

#LAge kart over regionen----
library(csmaps)
library(sf)


norway <-  csmaps::nor_municip_map_b2024_default_sf


region_usn <- norway |> 
  dplyr::filter(location_code >= "municip_nor3301" & location_code <= "municip_nor3338"|
           location_code >= "municip_nor3901" & location_code <= "municip_nor3911"|
           location_code >= "municip_nor4001" & location_code <= "municip_nor4036") |> 
  mutate(fylke = case_when(
    location_code >= "municip_nor3301" & location_code <= "municip_nor3338" ~ "Buskerud",
    location_code >= "municip_nor3901" & location_code <= "municip_nor3911" ~ "Vestfold",
    location_code >= "municip_nor4001" & location_code <= "municip_nor4036" ~ "Telemark",
    TRUE ~ NA_character_
  ))



# Lag en ny dataframe med kommunenavn og kommunenummer
Sys.setlocale(locale = 'no_NB.utf8')
kommuner <- tibble(
  region = c("Drammen", "Kongsberg", "Ringerike", "Hole", "Lier", "Øvre Eiker", "Modum", "Krødsherad", "Flå", "Nesbyen", "Gol", "Hemsedal", "Ål", "Hol", "Sigdal", "Flesberg", "Rollag", "Nore og Uvdal", "Horten", "Holmestrand", "Tønsberg", "Sandefjord", "Larvik", "Færder", "Porsgrunn", "Skien", "Notodden", "Siljan", "Bamble", "Kragerø", "Drangedal", "Nome", "Midt-Telemark", "Seljord", "Hjartdal", "Tinn", "Kviteseid", "Nissedal", "Fyresdal", "Tokke", "Vinje"),
  Kommunenr = c(3301, 3303, 3305, 3310, 3312, 3314, 3316, 3318, 3320, 3322, 3324, 3326, 3328, 3330, 3332, 3334, 3336, 3338, 3901, 3903, 3905, 3907, 3909, 3911, 4001, 4003, 4005, 4010, 4012, 4014, 4016, 4018, 4020, 4022, 4024, 4026, 4028, 4030, 4032, 4034, 4036)
)

# Legg til en kolonne med kommunenummer i df basert på location_code
region_usn <- region_usn %>%
  mutate(Kommunenr = as.numeric(str_extract(location_code, "\\d{4}")))




# Slå sammen df med kommuner for å legge til kommunenavn
region_usn <- region_usn |> 
  left_join(kommuner, by = "Kommunenr")

#slå sammen datasettet med neet data"
df_desc <- left_join(df_long, region_usn)

#utforske telemark
df_desc |> 
  filter(year == 2023 & fylke == "Telemark")

#lage kartet
# Konverter til et romlig objekt
library(mapview)
library(leafpop)

df_desc_sf <- st_as_sf(df_desc, sf_column_name = "geometry")

df_desc_sf_2023 <-  df_desc_sf|> 
  filter(!is.na(location_code) &
           year ==2023) 

df_desc_sf_2019 <- df_desc_sf|> 
  filter(!is.na(location_code) &
           year ==2019)
  
kart_2023 <- df_desc_sf_2023 |> 
  mapview(zcol = c("neet_tot_prop", "region"),
          legend = list(TRUE, FALSE),
          popup = popupTable(
            df_desc_sf_2023,
            zcol = c(
              "region",
              "neet_tot_prop",
              "neet_exc_imm_prop",
              "neet_immig_prop"
            )),
          layer.name = c("Andel av 15-29 år som er NEET 2023", "Kommune"))
kart_2023

kart_2019 <- df_desc_sf_2019 |> 
  mapview(zcol = c("neet_tot_prop", "region"),
          legend = list(TRUE, FALSE),
          popup = popupTable(
            df_desc_sf_2019,
            zcol = c(
              "region",
              "neet_tot_prop",
              "neet_exc_imm_prop",
              "neet_immig_prop"
            )),
          layer.name = c("Andel av 15-29 år som er NEET 2019", "Kommune"))

kart_2019
library(leaflet.extras2)
leafsync::sync(kart_2019,kart_2023)
kart_2019|kart_2023

###Kart med andre farger---
kart_2019_color <- df_desc_sf_2019 |> 
  mapview(zcol = c("neet_tot_prop", "region"),
          legend = list(TRUE, FALSE),
          col.regions= paletteer::paletteer_c("grDevices::Temps", 30),
          popup = popupTable(
            df_desc_sf_2019,
            zcol = c(
              "region",
              "neet_tot_prop",
              "neet_exc_imm_prop",
              "neet_immig_prop"
            )),
          layer.name = "Andel av 15-29 år som er NEET 2019",
          native.crs = FALSE)
kart_2019_color

#KArt med Ggplot - disse er statiske?
library(ggiraph)
p <- ggplot(data = df_desc_sf_2023, mapping = aes(geometry = geometry,
                                fill = neet_tot_prop)) + 
  geom_sf() +
  coord_sf() +
  geom_sf_interactive(aes(data_id = region,
                          tooltip = glue::glue('{neet_tot_prop}')),
                      linewidth = 0.1)+
  scale_fill_viridis_c() + theme_void()
p
girafe(ggobj = p)


#kart med leaflet----
library(leaflet)
coords <- st_coordinates(df_desc_sf_2023$geometry)
lng <- mean(coords[,1])
lat <- mean(coords[,2])
#definere fargepalett
pal <- colorNumeric(palette = "viridis", domain = df_desc_sf_2023$neet_tot_prop)
#lage riktig popup info
labels <- sprintf(
  "<strong>%s</strong><br/>%g%% NEET",
  df_desc_sf_2023$region, df_desc_sf_2023$neet_tot_prop
) |>  lapply(htmltools::HTML)

leaflet(df_desc_sf_2023) %>% addTiles() %>%
  setView(lng, lat, zoom = 7) |> 
  addPolygons(fillColor = ~pal(neet_tot_prop),
              stroke = TRUE, 
              fillOpacity = 0.7,
              opacity = 0.2,
              color = "white",
              label = labels,
              dashArray = "3",
              highlight = highlightOptions(weight = 5,
                                           color = "#666", 
                                           bringToFront = TRUE)) |> 
  addLegend("bottomright", pal = pal, 
            values = ~neet_tot_prop, title = "Andel Neet 15-29 i 2023",
            opacity = 0.7)


#Gscord
norge_fylker <- giscoR::gisco_get_nuts(country = "Norway",
                       nuts_level = 3,
                       year = "2024",
                       epsg = 3035) |> 
  as.tibble() |> 
  janitor::clean_names()

norge_alt<- giscoR::gisco_get_nuts(country = "Norway")
norge_fylker |> 
  ggplot(aes(geometry = geometry))+
  geom_sf()

# stolpediagram til document
barplot <- df_desc |> 
  filter(year == 2023 &
         fylke %in% c("Buskerud", "Vestfold", "Telemark")) |> 
  ggplot(aes(x = reorder(region, neet_tot_prop), y = neet_tot_prop, fill = region)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual(values=ggsci::pal_igv()(41)) +
  theme_classic() +
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Kommuner i Buskerud, Vestfold og Telemark", 
       y = "Andel NEET blant 15-29 åringer i 2023")+
  geom_hline(yintercept = 9.9, linetype = "dashed", color = "steelblue", size =0.7) +
  annotate("text", x = 15, y = 13, label = "Stiplet blå linje representerer landsgjennomsnitt", color = "black", hjust = 0) +
  annotate("rect", xmin = 6, xmax = 24, ymin = 12.5, ymax = 13.5, alpha = 0.2, fill = "steelblue")
plotly::ggplotly(barplot)



p2 <- df_desc |> 
  filter(year == 2023 & fylke %in% c("Buskerud", "Vestfold", "Telemark")) |> 
  ggplot(aes(x = reorder(region, neet_tot_prop), y = neet_tot_prop, fill = region)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = ggsci::pal_igv()(40)) +
  theme_classic() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Kommuner i Buskerud, Vestfold og Telemark", 
       y = "Andel Neet blant 15-29") +
  geom_hline(yintercept = 9.9, linetype = "dashed", color = "steelblue", size = 0.7) +
  annotate("text", x = 15, y = 14, label = "Stiplet blå linje representerer landsgjennomsnitt", color = "black", hjust = 0) +
  annotate("rect", xmin = 14.5, xmax = 17.5, ymin = 13.5, ymax = 14.5, alpha = 0.2, fill = "steelblue")
plotly::ggplotly(p2)
