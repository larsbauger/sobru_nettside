# HEADER --------------------------------------------
#
# Author: Lars Bauger
# Copyright (c) Lars Bauger, 2025
# Email:  lars.bauger@usn.no
# 
# Date: 2025-03-31
#
# Script Name:data_prep
#
# Script Description:klargjør data fra SSB API løsning
#
#
# Notes:
#
# Loading libraries
Sys.setlocale("LC_ALL","no_NB.utf8")
library(PxWebApiData)
library(tidyverse)

# loading data
## henter inn data fra alle bosatte i Norge, ALLE tilgjenglige år (fra 2008) med kjønn, alder og region

arbeidsstyrke_alle <- ApiData12(13556,  
                                ContentsCode = "Bosatte", 
                                Tid = TRUE,
                                HovArbStyrkStatus =c("TOT3", "NEET"),
                                Region = TRUE)

df_nasjonalt <- arbeidsstyrke_alle |> 
  select(region, Region, Tid, alder, `prioritert arbeidsstyrkestatus`, kjønn, value) |> 
  janitor::clean_names() |>
  mutate(region = str_remove(region, "\\s*\\(.*\\)"),
         prioritert_arbeidsstyrkestatus = str_remove(prioritert_arbeidsstyrkestatus, "\\s*\\(.*\\)")
  ) |> 
  filter(!value <=0)

wide_nasj <- df_nasjonalt |> 
  pivot_wider(names_from = prioritert_arbeidsstyrkestatus, values_from = value) |> 
  rename(tot= `I alt`,
         komnr = region_2)

fylkeskart <- readxl::read_excel(here::here("data/kommune_fylke.xlsx"), sheet = 1) |>
  janitor::clean_names() |>
  select(kommunenavn, kommunenr_2023, fylkesnavn) |>
  mutate(kommunenr_2023 = as.character(kommunenr_2023),
         kommunenr_2023 = str_pad(kommunenr_2023, width = 4, pad = "0"))


kommune_gammel <- readxl::read_excel(here::here("data/kom_gam.xlsx"), sheet = 1) |> 
  janitor::clean_names() |> 
  mutate(gammel = as.character(gammel),
         gammel = str_pad(gammel, width = 4, pad = "0"))
kombinert <- left_join(kommune_gammel, fylkeskart, by = c("region" = "kommunenavn"))

df_med_kommuner <- left_join(wide_nasj, kombinert, by = "region")

## legge inn fylkestilhørighet manuelt -------------------------------------


df_med_kommuner_fullstendig <- df_med_kommuner |>
  mutate(fylkesnavn = case_when(
      # Samler alternative fylkesnavn til én felles betegnelse
      fylkesnavn %in% c("Finnmark", "Finnmark – Finnmárku – Finmarkku") ~ "Finnmark",
      fylkesnavn %in% c("Troms", "Troms – Romsa – Tromssa") ~ "Troms",
      fylkesnavn %in% c("Nordland", "Nordland - Nordlánnda") ~ "Nordland",
      fylkesnavn %in% c("Trøndelag", "Trøndelag - Trööndelage") ~ "Trøndelag",
      TRUE ~ fylkesnavn
    ),
    fylkesnavn = case_when(
    # TELEMARK
    gammel %in% c("0822", "0821") ~ "Telemark",
    region %in% c("Midt-Telemark") ~ "Telemark",
    komnr == "0821" ~ "Telemark",
    
    # TRØNDELAG
    gammel %in% c("1601", "5025") ~ "Trøndelag",
    region %in% c("Namsos", "Hemne", "Snillfjord", "Agdenes", "Bjugn", "Roan", "Meldal", "Orkdal", "Klæbu",
                  "Snåsa", "Levanger - Levangke", "Trondheim - Tråante", "Verran", "Namdalseid", "Fosnes", "Vikna",
                  "Nærøy", "Rissa", "Røros", "Røyrvik", "Leksvik", "Mosvik", "Namsos - Nåavmesjenjaelmie",
                  "Røros - Rosse", "Snåase - Snåsa", "Raarvihke - Røyrvik", "Indre Fosen", "Heim", "Orkland", "Nærøysund") ~ "Trøndelag",
    
    # INNLANDET
    region %in% c("Våler (Hedmark)", "Os (Hedmark)") ~ "Innlandet",
    
    # ØSTFOLD
    region %in% c("Rømskog", "Våler (Østfold)", "Trøgstad", "Våler", "Spydeberg", "Askim", "Eidsberg", "Indre Østfold") ~ "Østfold",
    
    # AKERSHUS
    region %in% c("Rygge", "Nes (Akershus)", "Hobøl", "Ski", "Oppegård", "Lillestrøm", "Sørum", "Fet", "Skedsmo",
                  "Nordre Follo", "Nes", "Os") ~ "Akershus",
    
    # BUSKERUD
    region %in% c("Nedre Eiker", "Røyken", "Hurum", "Svelvik", "Nes (Buskerud)") ~ "Buskerud",
    komnr %in% c("3040", "3322") ~ "Buskerud",
    
    # VESTFOLD
    region %in% c("Hof", "Re", "Andebu", "Stokke", "Nøtterøy", "Tjøme", "Færder",
                   "Lardal") ~ "Vestfold",
    komnr =="0713" ~ "Vestfold",
    
    # AGDER
    region %in% c("Mandal", "Songdalen", "Søgne", "Marnardal", "Audnedal") ~ "Agder",
    
    # ROGALAND
    region %in% c("Forsand", "Finnøy", "Rennesøy") ~ "Rogaland",
    
    # VESTLAND
    region %in% c("Jondal", "Odda", "Granvin", "Fusa", "Sund", "Fjell", "Meland", "Radøy", "Lindås",
                  "Flora", "Os (Hordaland)", "Balestrand", "Leikanger", "Gaular", "Jølster", "Førde", "Naustdal",
                  "Vågsøy", "Selje", "Eid", "Hornindal", "Kinn", "Bjørnafjorden", "Alver", "Sunnfjord", "Stad") ~ "Vestland",
    
    # MØRE OG ROMSDAL
    region %in% c("Ørskog", "Norddal", "Sande (Møre og Romsdal)", "Stordal", "Skodje", "Herøy (Møre og Romsdal)",
                  "Nesset", "Midsund", "Sandøy", "Fræna", "Eide", "Halsa", "Herøy", "Fjord", "Hustadvika") ~ "Møre og Romsdal",
    komnr =="1514" ~ "Møre og Romsdal",
    
    # FINNMARK
    region %in% c("Porsanger - Porsángu - Porsanki", "Kárásjohka - Karasjok", "Kárá?johka - Karasjok",
                  "Hammerfest", "Kvalsund", "Hammerfest - Hámmerfeasta") ~ "Finnmark",
    
    # TROMS
    region %in% c("Harstad", "Berg", "Torsken", "Tjeldsund", "Lavangen", "Skånland", "Bjarkøy",
                  "Dielddanuorri - Tjeldsund", "Storfjord", "Gáivuotna - Kåfjord", "Lenvik", "Nordreisa", "Senja",
                  "Harstad - Hárstták", "Loabák - Lavangen", "Storfjord - Omasvuotna - Omasvuono",
                  "Gáivuotna - Kåfjord - Kaivuono", "Nordreisa - Ráisa - Raisi", "Gratangen - Rivtták") ~ "Troms",
    
    # NORDLAND
    region %in% c("Sørfold - Fuolldá", "Rana - Raane", "Tranøy", "Herøy (Nordland)", "Bø (Nordland)", "Sortland",
                  "Divtasvuodna - Tysfjord", "Ballangen", "Hamarøy - Hábmer", "Evenes - Evenássi", "Hattfjelldal",
                  "Fauske", "Hamarøy", "Tysfjord", "Evenes", "Aarborte - Hattfjelldal", "Fauske - Fuossko",
                  "Sortland - Suortá", "Hábmer - Hamarøy") ~ "Nordland",
    komnr == "1867" ~ "Nordland",
    region == "Hele landet"  ~ "Norge",
    TRUE ~ fylkesnavn))

df_med_kommuner_fullstendig |>
  filter(is.na(fylkesnavn)) |>
  pull(region) |>
  unique()

df_med_kommuner_fullstendig |>
  pull(fylkesnavn) |>
  unique()
df_ins <- df_med_kommuner_fullstendig |> 
  filter(is.na(fylkesnavn))


df_fullstendig_ryddet <- df_med_kommuner_fullstendig|> 
  filter(!is.na(fylkesnavn))

lookup_kommune_fylke <- df_fullstendig_ryddet |>
  select(komnr, region, fylkesnavn) |>
  distinct()  

# Lagre som CSV
readr::write_csv(lookup_kommune_fylke, "data/kommune_fylke_lookup.csv")

# beregne NEET andel
df_fullstendig_ryddet <- df_fullstendig_ryddet |> 
  mutate(neet_andel = (NEET/tot)*100)

# lagre kjønn datasett som et objekt --------------------------------------------


saveRDS(df_fullstendig_ryddet, here::here("data/kjonn_neet"))


# lage datasett med kartinfo
df_kjonn_clean <- df_fullstendig_ryddet |>
  filter(!is.na(komnr)) |>
  mutate(tid = as.integer(tid),
         neet_andel = round(neet_andel, 2)
  )
df_insp <- df_fullstendig_ryddet |> 
  filter(region == "Bø")



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

## Innvandring
# Sjekk hvilke Region-verdier som er gyldige
arbeidsstyrke_innvandring <- ApiData12(13563,
                                            ContentsCode = "Bosatte", 
                                            Tid = TRUE,
                                            HovArbStyrkStatus =c("TOT3", "NEET2"),
                                            Alder = "15-29",
                                            Region =TRUE) |> 
  janitor::clean_names()

arbeidsstyrke_innvandring <- arbeidsstyrke_innvandring |> 
  select(region, region_2, tid, alder, prioritert_arbeidsstyrkestatus, 
         innvandringskategori, value) |> 
  mutate(region = str_remove(region, "\\s*\\(.*\\)"),
         prioritert_arbeidsstyrkestatus = str_remove(prioritert_arbeidsstyrkestatus, "\\s*\\(.*\\)")
  ) |> 
  filter(!value <=0)


arbeidsstyrke_innvandring |>
  pull(innvandringskategori) |> 
  unique()

wide_nasj_innvandring <- arbeidsstyrke_innvandring |> 
  pivot_wider(names_from = prioritert_arbeidsstyrkestatus, values_from = value) |> 
  rename(tot= `I alt`,
         komnr = region_2,
         NEET = `Utenfor arbeid, utdanning og arbeidsmarkedstiltak` )

# Laster inn kommune-fylke-lookup fil lagd tidligere i kjønnsfila
lookup <- read_csv("data/kommune_fylke_lookup.csv") 


# Slår sammen fylkesnavn inn i hoveddatasettet
wide_nasj_innvandring_fullstendig <- wide_nasj_innvandring |>
  left_join(lookup |> select(komnr, fylkesnavn), by = "komnr") |>
  relocate(fylkesnavn, .after = region)   # flytter fylkesnavn rett etter region for oversikt

#lager et ryddig datasett hvor SSB fylkestilhørigheten er fjernet. Dette for å 
# unngå problemer fra fusjonering oppsplitting osv.  
df_innvandring_clean <- wide_nasj_innvandring_fullstendig |> 
  filter(!is.na(fylkesnavn)) |> 
  mutate(neet_andel = (NEET/tot)*100)



# lagre innvandrings datasett som et objekt --------------------------------------------


saveRDS(df_innvandring_clean, here::here("data/innvandring_neet"))
