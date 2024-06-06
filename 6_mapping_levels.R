#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Authors: Briana Barajas, Fletcher McConnell, Rosemary Juarez, Vanessa Salgado
# Project: Mapping Tree Species' Drought Sensitivity Under Climate Change
# Institution: Bren School of Environmental Science & Management - UCSB
# Date: 2024-06-07
# Purpose: Map species CWD sensitivity, as categorical sensitivity level
#
# Input files:
# - combined_predictions.csv
# - species_metadata.csv
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#===============================================================================
# 1) Pkg imports ---------
#===============================================================================
library(tidyverse)
library(here)
library(ggplot2)
library(terra)
library(tidyterra)
library(leaflet)
library(leafem)


#===============================================================================
# 1) Load data ---------
#===============================================================================
# 1. Species CWD predictions
combined_pred_raw <- read_csv(here("~/../../capstone/climatree/output/repo-and-slides/combined_predictions_slides.csv"))

# 2. Species metadata
meta_df <- read_csv("~/../../capstone/climatree/output/repo-and-slides/species_metadata.csv")


#===============================================================================
# 3) Calculate sensitivity levels ---------
#===============================================================================

# create a df with custom sens levels, grouped by each species
combined_pred <- data.frame()

for (i in unique(combined_pred_raw$sp_code)) {
  
  single_spp <- combined_pred_raw %>%
    filter(sp_code == i) %>%
    as.data.frame()
  
  quant <- quantile(single_spp$drought_sens[single_spp$drought_sens < 0], probs = c(0.25,0.75))
  
  single_spp <- single_spp %>% 
    mutate(sens_level = case_when(
      drought_sens >= 0 ~ "Least Concern",
      drought_sens <= quant[1] ~ "High Sensitivity",
      drought_sens > quant[1] & drought_sens <= quant[2] ~ "Moderate Sensitivity",
      drought_sens > quant[2] & drought_sens < 0 ~ "Low Sensitivity")) %>% 
    mutate(sens_level = factor(sens_level,
                               levels = c("High Sensitivity", "Moderate Sensitivity", 
                                          "Low Sensitivity", "Least Concern"), ordered = TRUE)) %>% 
    left_join(meta_df, join_by(sp_code)) %>%
    dplyr::select(all_of(c("longitude", "latitude", "drought_sens",
                    "sens_level", "sp_code", "scientific_name", "common_name")))
  
  combined_pred <- rbind(combined_pred, single_spp)
  
}

rm(combined_pred_raw)

# list species with only negative values
all_neg <- combined_pred %>% group_by(sp_code) %>% filter(all(drought_sens < 0))
all_neg_code <- unique(all_neg$sp_code)
all_neg_sci_name <- unique(all_neg$scientific_name)
all_neg_common_name <- unique(all_neg$common_name)



#===============================================================================
# 3) Plot Sensitivity levels ---------
#===============================================================================
species <- "pcgl"

# filter to single species
single_spp <- combined_pred %>% 
  filter(sp_code == species) %>% 
  dplyr::select(longitude, latitude, sens_level)

# create raster
rast <- tidyterra::as_spatraster(single_spp, crs = "+proj=longlat +datum=WGS84")
rast <- stars::st_as_stars(rast)


if(species %in% all_neg_code){
  leaflet() %>% 
    addTiles() %>% 
    addStarsImage(rast, colors = c("#8C2F0E", "#E7844C", "#F0B28F"),
                  opacity = 0.8, project = TRUE) %>% 
    addLegend(colors = c("#8C2F0E", "#E7844C", "#F0B28F"),
              labels = c("High", "Moderate", "Low"),
              values = values(rast),
              title = "Drought Sensitivity")
} else {
  leaflet() %>% 
    addTiles() %>% 
    addStarsImage(rast, colors = c("#8C2F0E", "#E7844C", "#F0B28F", "#144D6F"),
                  opacity = 0.8, project = TRUE) %>% 
    addLegend(colors = c("#8C2F0E", "#E7844C", "#F0B28F", "#144D6F"),
              labels = c("High", "Moderate", "Low", "Least Concern"),
              values = values(rast),
              title = "Drought Sensitivity")
}

