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

###############################################
### THIS SCRIPT CLEANS THE 2019 FIELD DATA #### 
###############################################

#### Import Metadata of Field Sites #### 

  metadf <- readxl::read_excel("fieldDATA/Data/bowron_2019/2019 plot card data.xlsx", col_names = T)
  metadf <- read.csv("fieldDATA/Data/bowron_2019/2019 plot card data_UTM10.csv")
  metadf <- metadf %>% mutate(ShortName = toupper(ShortName))
  
#### Clean and Summarise h2o Temperatures #### 
  
  # Read all h2o files
  list_h2o <- list.files("fieldDATA/Data/bowron_2019/Climate_h2o/", recursive = T, full.names = T)

  # Create dump file
  h2o_df <- tibble()

  # Loop sites
  for(i in list_h2o){
  
    # i = list_h2o[1]
    # Progress indicator
    print(i)
  
    # Grab region and site names from file path
    fullname <- str_split(string = i, " ")[[1]][1]
    fullname <- str_split(string = fullname, pattern = "//")[[1]][2]
    site <- str_split(fullname, "/")[[1]][2]
    site <- sub(pattern = "SN", replacement = "", x = site)
    site <- toupper(site)
    
    # Read data
    df_Temp <- read_excel(path = i, skip = 1)
    
    # Grab only Date and Temp (some have min max temp aslo...)
    df_Temp <- df_Temp %>% 
      select(contains("Date"), contains("Temp,")) %>% 
      mutate(Site = site)
    names(df_Temp) <- c("Date", "Temp", "Site")

    # Filter August    
    df_Temp <- df_Temp %>% 
      mutate(Month = lubridate::month(Date)) %>% 
      filter(Month == 8)
    
    # Mean daily
    df_Temp <- df_Temp %>% 
      mutate(Date = as.Date(Date)) %>% 
      group_by(Site, Date) %>% 
      summarise(minDailyTemp = min(Temp),
                meanDailyTemp = mean(Temp),
                maxDailyTemp = max(Temp))
    
    # Summarise
    df_Temp_mean <- df_Temp %>% 
      group_by(Site) %>% 
      summarise(
        h2omean = mean(meanDailyTemp),
        h2omin = mean(minDailyTemp),
        h2omax = mean(maxDailyTemp))

    # Plot and save  
    # ppp <- ggplot(df_Temp) +
    #   geom_line(aes(Date, Temp)) +
    #   geom_hline(yintercept = df_Temp_mean %>% select(contains("mean")) %>% pull()) +
    #   ggtitle(site); ppp
    # 
    # ggsave(plot = ppp, filename = paste0("fieldDATA/Outputs/figs/h2o_",site,".png"))

    h2o_df <- bind_rows(h2o_df, df_Temp_mean)
    }
  
  
  metadf_h2o <- merge(metadf, h2o_df, by.x = "ShortName", by.y = "Site") %>%
      select(ShortName, Northing, Easting, contains("h2o"))
    
  metadf_h2o_sf <- st_as_sf(metadf_h2o, coords = c("Easting", "Northing"), crs = 32610, agr = "constant")
  
    
#### Clean and Summarise Temp/Rh Temperatures #### 
  
  # Read all h2o files
  list_rh <- list.files("fieldDATA/Data/bowron_2019/Climate_rh/", recursive = F, full.names = T, pattern = ".xlsx")
  
  # Create dump file
  Trh_df <- tibble()
  
  # Loop sites
  for(i in list_rh){
    
    # i = list_rh[10]
    # Progress indicator
    print(i)
    
    # Grab region and site names from file path
    site <- str_split(string = i, "/")[[1]][5]
    site <- str_split(string = site, pattern = "_")[[1]][1]
    site <- toupper(site)
    
    # Read data
    df_Temp <- read_excel(path = i, skip = 1)
    
    # Grab only Date and Temp (some have min max temp aslo...)
    df_Temp <- df_Temp %>% 
      select(contains("Date"), contains("Temp,"), contains("RH,")) %>% 
      mutate(Site = site)
    
    names(df_Temp) <- c("Date", "Temp", "RH", "Site")
    
    # Filter August    
    df_Temp <- df_Temp %>% 
      mutate(Month = month(Date)) %>% 
      filter(Month == 8)
    
    # Mean daily
    df_Temp <- df_Temp %>% 
      mutate(Date = as.Date(Date)) %>% 
      group_by(Site, Date) %>% 
      summarise(minDailyTemp = min(Temp),
                meanDailyTemp = mean(Temp),
                maxDailyTemp = max(Temp),
                minDailyRH = min(RH),
                meanDailyRH = mean(RH),
                maxDailyRH = max(RH))
    
    # Summarise
    df_Temp_mean <- df_Temp %>% 
      group_by(Site) %>%
      summarise(
        Tmean = mean(meanDailyTemp),
        Tmin = mean(minDailyTemp),
        Tmax = mean(maxDailyTemp),
        RHmean = mean(meanDailyRH),
        RHmin = mean(minDailyRH),
        RHmax = mean(maxDailyRH))
    
    # Plot and save  
    # ppp <- ggplot(df_Temp) +
    #   geom_line(aes(Date, Temp)) +
    #   geom_line(aes(Date, RH)) +
    #   # geom_hline(yintercept = df_Temp_mean %>% pull()) +
    #   ggtitle(site); ppp
    # 
    # ggsave(plot = ppp, filename = paste0("fieldDATA/Outputs/figs/TempRH_",site,".png"))
    
    Trh_df <- bind_rows(Trh_df, df_Temp_mean)
  }
  
  
  metadf_rh <- merge(x = metadf, y = Trh_df, by.x = "ShortName", by.y = "Site") %>%
    select(ShortName, Northing, Easting, contains("h2o"))
  
  metadf_rh_sf <- st_as_sf(metadf_rh, coords = c("Easting", "Northing"), crs = 32610, agr = "constant")
  
  
##### MERGE H2O and TEMP RH ####
  
  # all <- merge(metadf_h2o, metadf_rh, all = T)
  # all_sf <- st_as_sf(all, coords = c("Location (WGS84) Lon", "Location (WGS84) Lat"), crs = 4326, agr = "constant")
  # all_sf
  
  st_write(metadf_h2o_sf, "openSTARS/Data/Sites/sites.shp", delete_dsn = T)
  st_write(metadf_rh_sf, "openSTARS/Data/ClimateObs/FieldObs.shp", delete_dsn = T)

