# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(sf) 
library(tidyverse)
library(readxl)
library(lubridate)
library(bcdata)
library(bcmaps)
library(raster)
library(RStoolbox)
library(ggsflabel)

#### HILLSHADE ####

  hs <- raster("openSTARS/Data/DEM/hs.tif")

#### FIELD STATION DATA ####

  sites <- read_sf("openSTARS/Data/Sites/sites.shp")

#### FRESHWATER ATLAS  ####

  bowron <- read_sf("fieldDATA/bowron_ws.sqlite") %>% st_transform(3005)
    write_sf(bowron, "geoData/bowron.sqlite")
  
  # Streams
  stream <- bcdc_query_geodata("freshwater-atlas-stream-network") %>% 
    filter(WITHIN(bowron)) %>% 
    collect() 
    write_sf(stream, "geoData/stream.sqlite")
  
  # Rivers
  riv <- bcdc_query_geodata("freshwater-atlas-rivers") %>% 
    filter(WITHIN(bowron)) %>% 
    collect() 
    write_sf(riv, "geoData/riv.sqlite")
  
  # Lakes
  lak <- bcdc_query_geodata("freshwater-atlas-lakes") %>% 
    filter(WITHIN(bowron)) %>% 
    collect() 
    write_sf(lak, "geoData/lak.sqlite")
  
  # Wetlands
  wet <- bcdc_query_geodata("freshwater-atlas-wetlands") %>% 
    filter(WITHIN(bowron)) %>% 
    collect() 
    write_sf(wet, "geoData/wet.sqlite")
  
  # Glaciers 
  gl <- bcdc_query_geodata("freshwater-atlas-glaciers") %>% 
    filter(WITHIN(bowron)) %>% 
    collect()
    write_sf(gl, "geoData/gl.sqlite")
  
  # Assessment Watersheds (Names)
  faaw <- bcdc_query_geodata("freshwater-atlas-assessment-watersheds") %>% 
    filter(WITHIN(bowron)) %>% 
    collect() 
    write_sf(faaw, "geoData/faaw.sqlite")
    
  # Watersheds
  faw <- bcdc_query_geodata("freshwater-atlas-watersheds") %>% 
    filter(INTERSECTS(bowron)) %>% 
    collect() 
    write_sf(faw, "geoData/faw.sqlite")
    
#### BEC ####

  # BEC
  bec <- bcmaps::bec() 

  # Clip to Bowron
  bec_bow <- st_intersection(bec, bowron) 

  # Dissolve by Zone
  bec_bow_dissolve <- bec_bow %>% 
    group_by(ZONE, ZONE_NAME) %>% 
    summarise()

  # Calculate Area
  bec_bow_dissolve$area_km2 <- round(st_area(bec_bow_dissolve)/(1000*1000),0) 
  
  # Grab Colors
  bec_col <- tibble(ZONE=names(bec_colors()),
         BEC_COL=bec_colors())
  
  # Join colors to polygons
  bec_bow_dissolve <- left_join(bec_bow_dissolve, bec_col) 
  
  # Add BEC ZONE to sites
  sites <- st_join(sites, y = bec_bow_dissolve)
  
  # Count Sites per Zone
  site_count <- sites %>% 
    st_drop_geometry()%>% 
    group_by(ZONE) %>% 
    summarise(n = n()) 

  # Join Site Count to Polygons
  bec_bow_dissolve <- full_join(bec_bow_dissolve, site_count)
  
#### WATERSHED NAME ####

  sites <- st_join(sites, y = dplyr::select(faaw,GNIS_NAME_1))
  
  faaw_dissolve <- faaw %>% 
    dplyr::group_by(GNIS_NAME_1) %>%
    dplyr::summarise() 
  
#### WATERSHED ORDER ####

  # Add watershed order column
  sites <- st_join(sites, y = dplyr::select(faw,WATERSHED_ORDER))
  
  # Count Sites per Zone
  site_count_order <- sites %>% 
    st_drop_geometry()%>% 
    mutate(STREAM_ORDER = WATERSHED_ORDER) %>% 
    group_by(STREAM_ORDER) %>% 
    summarise(n = n())
  
  stream_dissolve <- stream %>% 
    group_by(STREAM_ORDER) %>% 
    summarise()
  
  # Join Site Count to Polygons
  stream_dissolve <- full_join(stream_dissolve, site_count_order)
  
#### MEGA PLOT ####
  
  plot <- ggR(hs) +
    geom_sf(data = bec_bow_dissolve, aes(fill = ZONE), color = NA, alpha = 0.5) +
    scale_fill_manual("BEC Zone",
                      values = bec_bow_dissolve$BEC_COL,
                      labels = paste0(bec_bow_dissolve$ZONE_NAME,
                                      " [",bec_bow_dissolve$ZONE,"]",
                                      "\nArea: ", bec_bow_dissolve$area_km2, " km2",
                                      "\nSites: ", bec_bow_dissolve$n,"\n")) +
    geom_sf(data = stream_dissolve, size = 0.5, alpha = 0.5, aes(color = factor(STREAM_ORDER))) +
    scale_color_brewer("Stream Order",
                       labels = paste0("S", stream_dissolve$STREAM_ORDER, ", Sites: ", stream_dissolve$n)) +
    # geom_sf(data = stream, size = 0.5, alpha = 0.5, aes(color = factor(STREAM_ORDER))) +
    geom_sf(data = riv, color = NA, fill = "dark blue") +
    geom_sf(data = lak, color = NA, fill = "dark blue") +
    geom_sf(data = wet, color = NA, fill = "green") +
    geom_sf(data = gl, color = NA, fill = "yellow") +
    geom_sf(data = sites, aes(fill = ZONE), shape = 21, size = 2) +
    geom_sf_text_repel(data = sites, aes(label = tolower(ShortName)), color = "black") +
    geom_sf(data = bowron, fill = NA, color = "black") +
    # geom_sf_text(data = faaw_dissolve, aes(label = GNIS_NAME_1), size = 4) +
    
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    
    labs(title = "Bowron Watershed") +
    theme_void(); plot

  ggsave("fieldDATA/Outputs/figs_location/Map_BEC_Sb.png", width = 8, height = 8)  
  