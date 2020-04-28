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
library(patchwork)

sites <- read_sf("openSTARS/SNN/bowron2019_dm.ssn/sites.shp")
preds <- read_sf("openSTARS/SNN/bowron2019_dm.ssn/preds_o.shp")


rbind(sites, preds)

names(preds)
names(sites)

# compare <- function(s = sites, p = preds, c = avEleA) { 
s = sites
p = preds
c = c("avEleA","avSloA", "avEasA","avNorA","avIrradA","avTmp","avTotPpA","avGrdtA","smRdsA","LA","WA","GA","NewA","OldA","RegrwA","NewFA","OldFA","RegrwFA" )

# New facet label names for supp variable
supp.labs <- c("Mean Elevation",
               "Mean Slope",
               "Mean Eastness",
               "Mean Northness",
               "Total Irradiance",
               "Mean Air Temp",
               "Total Precip.",
               "Mean Stream Gradient",
               "Sum of Roads",
               "Percent Lakes",
               "Percent Wetlands",
               "Percent Glaciers",
               "0-5 Year Cutblock (%)",
               ">10 Year Cutblock (%)",
               "5-10 Year Cutblock (%)",
               "0-5 Year Fire (%)",
               ">10 Year Fire (%)",
               "5-10 Year Fire (%)")

names(supp.labs) <- c

s_f <- s %>% st_drop_geometry() %>% dplyr::select(c) %>% mutate(source = "sites")
p_f <- p %>% st_drop_geometry() %>% dplyr::select(c) %>% mutate(source = "preds")
f <- rbind(s_f, p_f)
f_l <- f %>% pivot_longer(cols = -source, names_to = "Measurement", values_to = "Values")

f_l %>% 
  # dplyr::filter(Measurement %in% c("avEleA","avSloA", "avEasA","avNorA","avIrradA","avGrdtA")) %>% 
  ggplot() + 
  ggridges::geom_density_ridges(aes(Values, source, fill = source), quantile_lines = TRUE, quantiles = 2, alpha = 0.8, show.legend = F) +
  facet_wrap(~Measurement, ncol=2, scales = "free_x", labeller = labeller(Measurement = supp.labs)) + 
  scale_fill_manual(values = c("light blue", "light green")) +
  theme_bw() +
  theme(aspect.ratio = 0.2) 
ggsave("fieldDATA/Outputs/site_summary/sites_pred_all.png", width = 6, heigh = 10)


f_l %>% 
  dplyr::filter(Measurement %in% c("avEleA","avSloA", "avEasA","avNorA","avIrradA","avGrdtA")) %>% 
  ggplot() + 
    ggridges::geom_density_ridges(aes(Values, source, fill = source), quantile_lines = TRUE, quantiles = 2, alpha = 0.8, show.legend = F) +
    facet_wrap(~Measurement, ncol=1, scales = "free_x", labeller = labeller(Measurement = supp.labs)) + 
    scale_fill_manual(values = c("light blue", "light green")) +
    theme_bw() +
    theme(aspect.ratio = 0.2) 
ggsave("fieldDATA/Outputs/site_summary/sites_pred_terrain.png", width = 6, heigh = 12)

f_l %>% 
  dplyr::filter(Measurement %in% c("avTmp","avTotPpA")) %>% 
  ggplot() + 
  ggridges::geom_density_ridges(aes(Values, source, fill = source), quantile_lines = TRUE, quantiles = 2, alpha = 0.8, show.legend = F) +
  facet_wrap(~Measurement, ncol=1, scales = "free_x", labeller = labeller(Measurement = supp.labs)) + 
  scale_fill_manual(values = c("light blue", "light green")) +
  theme_bw() +
  theme(aspect.ratio = 0.2) 
ggsave("fieldDATA/Outputs/site_summary/sites_pred_temp.png", width = 6, heigh = 12)

f_l %>% 
  dplyr::filter(Measurement %in%               c("smRdsA","LA","WA","GA")) %>% 
  ggplot() + 
  ggridges::geom_density_ridges(aes(Values, source, fill = source), quantile_lines = TRUE, quantiles = 2, alpha = 0.8, show.legend = F) +
  facet_wrap(~Measurement, ncol=1, scales = "free_x", labeller = labeller(Measurement = supp.labs)) + 
  scale_fill_manual(values = c("light blue", "light green")) +
  theme_bw() +
  theme(aspect.ratio = 0.2) 
ggsave("fieldDATA/Outputs/site_summary/sites_pred_rd_fwa.png", width = 6, heigh = 12)

f_l %>% 
  dplyr::filter(Measurement %in% 
                  c("NewA","OldA","RegrwA","NewFA","OldFA","RegrwFA" )) %>% 
  ggplot() + 
  ggridges::geom_density_ridges(aes(Values, source, fill = source), quantile_lines = TRUE, quantiles = 2, alpha = 0.8, show.legend = F) +
  facet_wrap(~Measurement, ncol=1, scales = "free_x", labeller = labeller(Measurement = supp.labs)) + 
  scale_fill_manual(values = c("light blue", "light green")) +
  theme_bw() +
  theme(aspect.ratio = 0.2) 
ggsave("fieldDATA/Outputs/site_summary/sites_pred_for.png", width = 6, heigh = 12)


