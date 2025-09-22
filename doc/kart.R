# HEADER --------------------------------------------
#
# Author: Lars Bauger
# Copyright (c) Lars Bauger, 2025
# Email:  lars.bauger@usn.no
# 
# Date: 2025-03-31
#
# Script Name:kart
#
# Script Description:Der lager jeg kartet som skal brukes senere i inforgrafsiden
#
# Notes:
#
# Loading libraries
library(tidyverse)
library(here)

# loading data

df_kjonn <- read_rds(here::here("data/kjonn_neet"))
str(df_kjonn)

df_kjonn_clean <- df_kjonn |>
  filter(!is.na(komnr)) |>
  mutate(tid = as.integer(tid),
    neet_andel = round(neet_andel, 2)
  )
df_insp <- df_kjonn |> 
  filter(fylkesnavn == "Telemark")

df_innv <- read_rds(here::here("data/innvandring_neet"))


#LAge kart over regionen----
library(csmaps)
library(sf)


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
  left_join(df_kjonn_clean, by = c("komnr", "geometry"))


# Eksempel: join på 'region'
major <- kart_2024 |>
  left_join(
    kart_2020 |>
      st_drop_geometry() |>
      # velg bare kolonnene du faktisk vil hente fra 2020
      select(region, everything()) |>
      # valgfritt: gi 2020-kolonnene et suffiks for å skille dem fra 2024
      rename_with(~ paste0(.x, "_2020"), -region),
    by = "region"
  )

major <- kart_2024 |> 
  left_join(kart_2020, by = "region")

df_insp <- kart_2019 |> 
  filter(fylkesnavn == "Telemark")
#### SJEKK UT VEIEN VIDERE HERFRA-----
region_usn <- norway |>
  filter(location_code >= "municip_nor3301" & location_code <= "municip_nor3338" |
           location_code >= "municip_nor3901" & location_code <= "municip_nor3911" |
           location_code >= "municip_nor4001" & location_code <= "municip_nor4036") |>
  mutate(fylkesnavn = case_when(
    location_code >= "municip_nor3301" & location_code <= "municip_nor3338" ~ "Buskerud",
    location_code >= "municip_nor3901" & location_code <= "municip_nor3911" ~ "Vestfold",
    location_code >= "municip_nor4001" & location_code <= "municip_nor4036" ~ "Telemark",
    TRUE ~ NA_character_
  )) |>
  mutate(komnr = str_extract(location_code, "\\d{4}"))

df_2024 <- df_kjonn_clean |> 
  filter(tid == 2024)

kartdata <- norway |>
  left_join(df_kjonn_clean, by = "komnr")




library(shiny)
library(leaflet)


# 4. Shiny-app med flere filtervalg
ui <- fluidPage(
  titlePanel("NEET-andel per kommune"),
  sidebarLayout(
    sidebarPanel(
      selectInput("aar", "Velg år", choices = sort(unique(kartdata$tid))),
      selectInput("kjonn", "Velg kjønn", choices = unique(kartdata$kjonn)),
      selectInput("fylke", "Velg fylke", choices = unique(kartdata$fylkesnavn)),
      selectInput("alder", "Velg aldersgruppe", choices = unique(kartdata$alder))
    ),
    mainPanel(leafletOutput("kart"))
  )
)

server <- function(input, output) {
  output$kart <- renderLeaflet({
    kartdata_filtered <- kartdata |>
      filter(
        tid == input$aar,
        kjonn == input$kjonn,
        fylkesnavn == input$fylke,
        alder == input$alder
      )
    
    leaflet(kartdata_filtered) |>
      addTiles() |>
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", neet_andel, na.color = "#cccccc")(neet_andel),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0(
          "<strong>", region, "</strong><br>",
          "År: ", tid, "<br>",
          "Kjønn: ", kjonn, "<br>",
          "Fylke: ", fylkesnavn, "<br>",
          "Alder: ", alder, "<br>",
          "NEET-andel: ", neet_andel, "%"
        )
      )
  })
}

shinyApp(ui, server)




###
region_usn <- norway |> 
  dplyr::filter(location_code >= "municip_nor3301" & location_code <= "municip_nor3338"|
                  location_code >= "municip_nor3901" & location_code <= "municip_nor3911"|
                  location_code >= "municip_nor4001" & location_code <= "municip_nor4036") |>
  mutate(fylkesnavn = case_when(
    location_code >= "municip_nor3301" & location_code <= "municip_nor3338" ~ "Buskerud",
    location_code >= "municip_nor3901" & location_code <= "municip_nor3911" ~ "Vestfold",
    location_code >= "municip_nor4001" & location_code <= "municip_nor4036" ~ "Telemark",
    TRUE ~ NA_character_
  ))

# Legg til en kolonne med kommunenummer i df basert på location_code
region_usn <- region_usn %>%
  mutate(komnr = str_extract(location_code, "\\d{4}"))

print(region_usn)
print(df_kjonn)
# Slå sammen df med kommuner for å legge til kommunenavn

df_desc_sf <- df_kjonn |>
  left_join(region_usn |> select(komnr, geometry, fylkesnavn, location_code), by = "komnr") |>
  filter(!is.na(geometry)) |>  # fjerner kommuner uten kartdata
  st_as_sf(sf_column_name = "geometry")


#lage kartet
# Konverter til et romlig objekt
#KArt alternativ 1, med bare NEET


df_desc_sf_2024 <-  df_desc_sf|> 
  filter(!is.na(location_code) &
           tid ==2024 &
           kjonn == "Begge kjønn" &
           alder == "15-29 år") 

#kart med leaflet----
library(leaflet)
coords <- st_coordinates(df_desc_sf_2024$geometry)
lng <- mean(coords[,1])
lat <- mean(coords[,2])
#definere fargepalett
pal <- colorNumeric(palette = "viridis", domain = df_desc_sf_2024$neet_andel)


# Definerer fargepalett: grønn (lav) til rød (høy)
library(RColorBrewer)
pal2 <- colorNumeric(
  palette = rev(brewer.pal(11, "RdYlGn")),  # reverserer så grønn er lavest
  domain = df_desc_sf_2024$neet_andel
)

#lage riktig popup info
labels <- sprintf(
  "<strong>%s</strong><br/>%g%% NEET",
  df_desc_sf_2024$region, df_desc_sf_2024$neet_andel
) |>  lapply(htmltools::HTML)

leaflet <- leaflet(df_desc_sf_2024) %>% addTiles() %>%
  setView(lng, lat, zoom = 7) |> 
  addPolygons(fillColor = ~pal2(neet_andel),
              stroke = TRUE, 
              fillOpacity = 0.7,
              opacity = 0.2,
              color = "white",
              label = labels,
              dashArray = "3",
              highlight = highlightOptions(weight = 5,
                                           color = "#666", 
                                           bringToFront = TRUE)) |> 
  addLegend("bottomright", pal = pal2, 
            values = ~neet_andel, title = "Andel Neet 15-29 i 2024",
            opacity = 0.7)

leaflet


# shiny eksempel ----------------------------------------------------------

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(here)

# Last inn data
df_kjonn <- read_rds(here("data/kjonn_neet"))
norway <- csmaps::nor_municip_map_b2024_default_sf

# Filtrer regionen
region_usn <- norway |>
  filter(location_code >= "municip_nor3301" & location_code <= "municip_nor3338" |
           location_code >= "municip_nor3901" & location_code <= "municip_nor3911" |
           location_code >= "municip_nor4001" & location_code <= "municip_nor4036") |>
  mutate(fylkesnavn = case_when(
    location_code >= "municip_nor3301" & location_code <= "municip_nor3338" ~ "Buskerud",
    location_code >= "municip_nor3901" & location_code <= "municip_nor3911" ~ "Vestfold",
    location_code >= "municip_nor4001" & location_code <= "municip_nor4036" ~ "Telemark",
    TRUE ~ NA_character_
  )) |>
  mutate(komnr = str_extract(location_code, "\\d{4}"))

# Slå sammen med df_kjonn
df_desc_sf <- df_kjonn |>
  left_join(region_usn |> select(komnr, geometry, fylkesnavn, location_code), by = "komnr") |>
  filter(!is.na(geometry)) |>
  st_as_sf(sf_column_name = "geometry")

# UI
ui <- fluidPage(
  titlePanel("NEET-andel i regionen over tid"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Velg år:", min = 2008, max = 2024, value = 2024, sep = "")
    ),
    mainPanel(
      leafletOutput("neetMap", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$neetMap <- renderLeaflet({
    df_filtered <- df_desc_sf |>
      filter(tid == input$year,
             kjonn == "Begge kjønn",
             alder == "15-29 år")
    
    pal <- colorNumeric(
      palette = rev(brewer.pal(11, "RdYlGn")),
      domain = df_filtered$neet_andel
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g%% NEET",
      df_filtered$region, df_filtered$neet_andel
    ) |> lapply(htmltools::HTML)
    
    leaflet(df_filtered) |>
      addTiles() |>
      setView(lng = 9.5, lat = 59.3, zoom = 7) |>
      addPolygons(
        fillColor = ~pal(neet_andel),
        stroke = TRUE,
        fillOpacity = 0.7,
        opacity = 0.2,
        color = "white",
        label = labels,
        dashArray = "3",
        highlight = highlightOptions(weight = 5, color = "#666", bringToFront = TRUE)
      ) |>
      addLegend("bottomright", pal = pal, values = ~neet_andel,
                title = paste("Andel NEET 15–29 år i", input$year),
                opacity = 0.7)
  })
}

# Kjør appen
shinyApp(ui, server)


## kart alternativ 2 med mer detaljer
library(mapview)
library(leafpop)



df_desc_sf_2019 <- df_desc_sf|> 
  filter(!is.na(location_code) &
           year ==2019)

kart_2024 <- df_desc_sf_2024 |> 
  mapview(zcol = c("neet_andel", "region"),
          legend = list(TRUE, FALSE),
          popup = popupTable(
            df_desc_sf_2024,
            zcol = c(
              "region",
              "neet_andel"
            )),
          layer.name = c("Andel av 15-29 år som er NEET 2024", "Kommune"))
kart_2024

# SHiny forsøk


# 1. Lag en tabell med alle år og alle kommuner med geometri
alle_aar <- unique(df_kjonn$tid)
alle_kommuner <- region_usn |> select(komnr, geometry, location_code, fylkesnavn)

# Utvid kartdata til alle år
kart_panel <- expand_grid(
  tid = alle_aar,
  komnr = alle_kommuner$komnr
) |>
  left_join(alle_kommuner, by = "komnr")
str(df_kjonn)
str(kart_panel)
table(df_kjonn$tid)
table(kart_panel$tid)
df_desc_sf |> count(tid)
# 2. Join på df_kjonn for å få med NEET-data
df_desc_sf <- kart_panel |>
  left_join(df_kjonn, by = c("komnr", "tid", "fylkesnavn")) |>
  st_as_sf(sf_column_name = "geometry")



library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer)

df_desc_sf_shiny <- df_desc_sf |> 
  filter(fylkesnavn.x %in% c("Telemark", "Vestfold", "Buskerud"))
df_desc_sf |> count(tid)

# UI
ui <- fluidPage(
  titlePanel("NEET-andel 15–29 år over tid"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Velg år:", 
                  min = min(as.numeric(df_desc_sf_shiny$tid)), 
                  max = max(as.numeric(df_desc_sf_shiny$tid)), 
                  value = 2024, 
                  step = 1,
                  sep = "")
    ),
    mainPanel(
      leafletOutput("neetMap", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dynamisk filtrering basert på valgt år
  filtered_data <- reactive({
    df_desc_sf_shiny |> 
      filter(tid == input$year,
             kjonn == "Begge kjønn",
             alder == "15-29 år",
             !is.na(location_code))
  })
  
  output$neetMap <- renderLeaflet({
    df <- filtered_data()
    
    # Fargepalett
    pal <- colorNumeric(
      palette = rev(brewer.pal(11, "RdYlGn")),
      domain = df$neet_andel
    )
    
    # Popup
    labels <- sprintf(
      "<strong>%s</strong><br/>%g%% NEET",
      df$region, df$neet_andel
    ) |> lapply(htmltools::HTML)
    
    coords <- st_coordinates(df$geometry)
    lng <- mean(coords[,1])
    lat <- mean(coords[,2])
    
    leaflet(df) |>
      addTiles() |>
      setView(lng, lat, zoom = 7) |>
      addPolygons(
        fillColor = ~pal(neet_andel),
        stroke = TRUE,
        fillOpacity = 0.7,
        opacity = 0.2,
        color = "white",
        label = labels,
        dashArray = "3",
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          bringToFront = TRUE
        )
      ) |>
      addLegend("bottomright", 
                pal = pal, 
                values = ~neet_andel, 
                title = paste("Andel NEET 15–29 i", input$year),
                opacity = 0.7)
  })
}

# Kjør appen
shinyApp(ui, server)



#Feilsøking
library(tidyverse)

# Sørg for at begge datasett har riktig format
df_kjonn <- df_kjonn |> 
  mutate(komnr = str_pad(as.character(komnr), 4, pad = "0"),
         tid = as.character(tid))

kart_panel <- kart_panel |> 
  mutate(komnr = str_pad(as.character(komnr), 4, pad = "0"),
         tid = as.character(tid))

# Lag en oversikt over alle kombinasjoner i kartdata
kart_keys <- kart_panel |> distinct(komnr, tid)

# Lag en oversikt over alle kombinasjoner i df_kjonn
data_keys <- df_kjonn |> distinct(komnr, tid)

# Finn hvilke kombinasjoner som finnes i kart, men ikke i data
mangler_data <- kart_keys |> 
  anti_join(data_keys, by = c("komnr", "tid"))

# Finn hvilke kombinasjoner som finnes i data, men ikke i kart
mangler_kart <- data_keys |> 
  anti_join(kart_keys, by = c("komnr", "tid"))

# Skriv ut
print("Kombinasjoner som finnes i kart, men mangler i df_kjonn:")
print(mangler_data)

print("Kombinasjoner som finnes i df_kjonn, men mangler i kart:")
print(mangler_kart)

# Sjekk hvilke komnr som finnes i kartet
gyldige_komnr <- unique(kart_panel$komnr)
gyldige_komnr

# Filtrer df_kjonn til kun gyldige kommuner
df_kjonn_riktig <- df_kjonn |> 
  mutate(komnr = str_pad(as.character(komnr), 4, pad = "0")) |>
  filter(komnr %in% gyldige_komnr)
