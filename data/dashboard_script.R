# HEADER --------------------------------------------
#
# Author:     Lars  Bauger
# Copyright     Copyright 2025 - Lars Bauger
# Email:      lars.bauger@usn.no
#
# Date:     2025-11-03
#
# Script Name: dashboard_Script
#
# Script Description: Dette scriptet brukes for å kvalitetssikre de tingene som skal inn i de forskjellige delene på sobru nettsiden. 
#
#
# SETUP ------------------------------------
cat("\014")                 # Clears the console
options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
rm(list = ls())             # Remove all variables of the work space
## load up the packages we will need:  (uncomment as required)
Sys.setlocale("LC_ALL","no_NB.utf8")

library(tidyverse)
library(mapview)
library(leaflet)
library(leafpop)
library(sf)
library(here)
#Laste opp data
# loading data

df_kjonn <- read_rds(here::here("data/kjonn_neet"))
str(df_kjonn)

df_kjonn_clean <- df_kjonn |>
  filter(!is.na(komnr)) |>
  mutate(tid = as.integer(tid),
         neet_andel = round(neet_andel, 2)
  )
df_insp <- df_kjonn |> 
  filter(region == "Bø")

df_innv <- read_rds(here::here("data/innvandring_neet"))


#LAge kart over regionen----
library(csmaps)
library(sf)
df_kjonn_clean |> 
  filter(region == "Porsgrunn",
         kjonn == "Begge kjønn",
         alder == "15-29 år") |>
  arrange(tid) |> 
  print()

norway_2024 <-  csmaps::nor_municip_map_b2024_default_sf |> 
  mutate(komnr = str_extract(location_code, "\\d{4}"))
norway_2020 <-  csmaps::nor_municip_map_b2020_default_sf |> 
  mutate(komnr = str_extract(location_code, "\\d{4}"))
norway_2019 <-  csmaps::nor_municip_map_b2019_default_sf |> 
  mutate(komnr = str_extract(location_code, "\\d{4}"))


kart_2024 <- norway_2024 |> 
  left_join(df_kjonn_clean, by = "komnr")
kart_2020 <- norway_2020 |> 
  left_join(df_kjonn_clean, by = "komnr")
kart_2019 <- norway_2019 |> 
  left_join(df_kjonn_clean, by = "komnr")
kart_2020 |>
  filter(region == "Porsgrunn",
         kjonn == "Begge kjønn",
         alder == "15-29 år") |>
  arrange(tid) |> 
  print()


library(dplyr)
library(sf)

# 1) Finn felles kolonner (så rbind ikke feiler)
common_cols <- Reduce(intersect, list(
  names(kart_2019),
  names(kart_2020),
  names(kart_2024)
))

# 2) Forbered hvert kart: velg samme kolonner, merk med grenseår,
#    valider geometri og harmoniser CRS
prep_kart <- function(x, grense_ar, target_crs = 4326) {
  x %>%
    select(all_of(common_cols)) %>%
    mutate(grense_ar = grense_ar, .before = 1) %>%
    st_make_valid() %>%
    st_transform(target_crs)
}

# Velg en felles CRS – her bruker jeg WGS84 (EPSG:4326) for enkelhet
target_crs <- 4326

k19 <- prep_kart(kart_2019, 2019, target_crs)
k20 <- prep_kart(kart_2020, 2020, target_crs)
k24 <- prep_kart(kart_2024, 2024, target_crs)

# 3) Radbind (sf har egen rbind-metode, dette bevarer geometrien)
major <- do.call(rbind, list(k19, k20, k24))

# Sjekk raskt


major |> 
  filter(region == "Porsgrunn",
         kjonn == "Begge kjønn",
         alder == "15-29 år") |> 
  arrange(tid) |> 
  print(n = Inf)

#lagre dataset
saveRDS(major, here::here("data/kart_fullstendig"))

sobru_reg <- major |> 
  filter(fylkesnavn %in% c("Telemark", "Buskerud", "Vestfold"))
saveRDS(sobru_reg, here::here("data/kart_sobru_reg"))



# Les data
sobru_reg <- read_rds(here::here("data/kart_sobru_reg")) #|>
  st_make_valid() |>
  st_transform(4326)

df_kjonn <- read_rds(here::here("data/kjonn_neet")) 
df_kjonn$kommunenr_2023 <- as.numeric(df_kjonn$kommunenr_2023)
#lage kartet
norway <-  csmaps::nor_municip_map_b2020_default_sf


# Konverter til et romlig objekt
# Legg til en kolonne med kommunenummer i df basert på location_code
kartinfo <- norway |> 
  mutate(kommunenr_2023 = as.numeric(str_extract(location_code, "\\d{4}")))
# Slå sammen df med kommuner for å legge til kommunenavn
df_desc <- df_kjonn |> 
  left_join(kartinfo, by = "kommunenr_2023")


df_desc_sf <- st_as_sf(df_desc, sf_column_name = "geometry")

df_desc_sf_2024 <-  df_desc_sf|> 
  filter(!is.na(location_code) &
           tid ==2024) 

df_desc_sf_2019 <- df_desc_sf|> 
  filter(!is.na(location_code) &
           tid ==2019)

neet_2024 <- df_desc_sf_2024 |> 
  filter(tid == 2024,
         kjonn == "Begge kjønn",
         alder == "15-29 år")
kart_2024 <- neet_2024 |> 
  filter(tid == 2024,
         kjonn == "Begge kjønn",
         alder == "15-29 år") |> 
  mapview(zcol = c("neet_andel", "region"),
          legend = list(TRUE, FALSE),
          popup = popupTable(neet_2024,
                             zcol = c(
                               "region",
                               "neet_andel")),
          layer.name = c("Andel av 15-29 år som er NEET 2023", "Kommune"))

kart_2024
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


# gammel brukt før --------------------------------------------------------


```{r, lage kartet}
#lage kartet
# Konverter til et romlig objekt
#LAge kart over regionen----
library(csmaps)
library(sf)
library(mapview)
library(leafpop)

df_desc_sf <- st_as_sf(df_desc, sf_column_name = "geometry")

df_desc_sf_2024 <-  df_desc_sf|> 
  filter(!is.na(location_code) &
           tid ==2024,
         alder == "15-29 år",
         kjonn == "Begge kjønn") 

df_desc_sf_2019 <- df_desc_sf|> 
  filter(!is.na(location_code) &
           tid ==2019,
         alder == "15-29 år",
         kjonn == "Begge kjønn")

kart_2024 <- df_desc_sf_2024 |> 
  mapview(zcol = c("neet_andel", "region"),
          legend = list(TRUE, FALSE),
          popup = popupTable(
            df_desc_sf_2024,
            zcol = c(
              "region",
              "neet_andel")),
          layer.name = c("Andel av 15-29 år som er NEET 2023", "Kommune"))
kart_2024
```

