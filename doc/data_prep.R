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
                  "Nordre Follo", "Nes", "Nesbyen", "Os") ~ "Akershus",
    
    # BUSKERUD
    region %in% c("Nedre Eiker", "Røyken", "Hurum", "Svelvik", "Nes (Buskerud)") ~ "Buskerud",
    
    # VESTFOLD
    region %in% c("Hof", "Re", "Andebu", "Stokke", "Nøtterøy", "Tjøme", "Færder", "Sande (Vestfold)",
                  "Sande", "Lardal", "Bø") ~ "Vestfold",
    
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
