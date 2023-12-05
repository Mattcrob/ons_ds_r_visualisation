library(tidyverse)
library(data.table)
library(sf)
library(tmap)
library(dplyr)
library(readr)
library(stringr)
library(plotly)
library(leaflet)

# Loads in Ames (need to replace with one provided to us)
data(ames, package = "modeldata")

ames_sf <- st_as_sf(x = ames, coords = c('Longitude', 'Latitude'), crs = 27700)

tmap::tmap_mode("view")
tmap::qtm(ames_sf)

#Plot it:
ggplot_object <- ggplot(ames_sf) +
  geom_sf(aes(color = Neighborhood)) +
  theme(legend.position = 'none')

ggplot_object
ggplotly(ggplot_object)

ames_bounds <- sf::st_read("Ames_-_Precincts_2.shp", quiet = TRUE)
ames_bounds <- ames_bounds[2:25,]

##### ----- try with plotly ----- #####

plot_ly(
    ames,
    lat = ~Latitude,
    lon = ~Longitude,
    marker = list(alpha = 0.5),
    mode = 'markers',
    type = 'scattermapbox',
    color = ~Sale_Price,
    text = paste0("Sale Price: $", ames$Sale_Price),
    hoverinfo = text
    ) %>%
  add_trace(
    ames_bounds,
    type = 'scatter',
    colors = 'Purples'
  ) %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom = 11,
      center = list(lon = -93.62, lat = 42.02)),
    showlegend = FALSE)


  plot_ly(
    data = ames,
    lat = ~Latitude,
    lon = ~Longitude,
    marker = list(alpha = 0.5),
    mode = 'markers',
    type = 'scattermapbox',
    color = ~Sale_Price,
    text = ~Sale_Price,
    hoverinfo = text

  ) %>%
  plot_ly(data = ames_bounds)
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom = 11,
      center = list(lon = -93.62, lat = 42.02)),
    showlegend = FALSE) %>%
  plot_ly(data = ames_bounds)


plot_ly(ames_bounds,
        type = 'scatter'
        ) %>%
  add_trace(
    ames,
    lat = ames$Latitude,
    lon = ames$Longitude,
    marker = list(alpha = 0.5),
    mode = 'markers',
    type = 'scattermapbox',
    color = ames$Sale_Price,
    text = paste0("Sale Price: $", ames$Sale_Price),
    hoverinfo = text
    )





##### ----- try with leaflet ----- #####
ames_map <- st_transform(ames_sf, crs = '+proj=longlat +datum=WGS84')

pal_sale_price <- colorNumeric(
  palette = "Reds",
  domain = ames$Sale_Price)

leaflet() %>%
  addTiles() %>%
  setView(lng = -93.633403, lat = 42.022857, zoom = 13) %>%
  addCircles(data = ames, lng = ames$Longitude,
                   lat = ames$Latitude,
                   color = pal_sale_price(ames$Sale_Price),
                   radius = 20,
                   stroke = FALSE,
                   fillOpacity = 1) %>%
  addPopups(lng = ames$Longitude,
            lat = ames$Latitude,
            popup = ~ames$Sale_Price)
  addLabelOnlyMarkers(lng = ames$Longitude,
                      lat = ames$Latitude,
                      label = ames$Sale_Price)




