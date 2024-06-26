#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Authors: Briana Barajas, Fletcher McConnell, Rosemary Juarez, Vanessa Salgado
# Project: Mapping Tree Species' Drought Sensitivity Under Climate Change
# Institution: Bren School of Environmental Science & Management - UCSB
# Date: 2024-06-07
# Purpose: Run scripts 3-5
#
# Input files:
# - site_pet_cwd_std.csv
# - site_ave_clim.gz
# - site_summary.csv
# - species_metadata.csv
#
# Output files:
# - combined_predictions.csv: final predictions for all species
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Load necessary libraries
library(dplyr)
library(MASS)
library(tidyverse)
library(broom)
library(purrr)
library(margins)
library(tidylog)
library(fixest)
library(gstat)
library(sf)
library(units)
library(dtplyr)
library(marginaleffects)
library(dbplyr)
library(RSQLite)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(stringr)
library(raster)
library(rgdal)
library(viridis)
library(patchwork)
library(effects)
library(dplR)
library(terra)
select <- dplyr::select


### Define path
data_dir <- "~/../../capstone/climatree/raw_data/"
input_dir <- "~/../../capstone/climatree/input/external/"
output_dir <- "~/../../capstone/climatree/output/intermediate-output/"

###########################################
###########################################


# START Script 1 Pre-Processing steps

###########################################
###########################################

# 1. Site-level regressions
flm_df <- read_csv(paste0(input_dir, 'site_pet_cwd_std.csv'))

# 2. Historic site-level climate
ave_site_clim <- read_rds(paste0(input_dir, "site_ave_clim.gz"))
flm_df <- flm_df %>% 
  left_join(ave_site_clim, by = c("collection_id"))

# 3. Site information
site_df <- read_csv(paste0(input_dir, 'site_summary.csv'))
site_df <- site_df %>% 
  select(collection_id, sp_id, latitude, longitude)
site_df <- site_df %>% 
  rename(species_id = sp_id) %>% 
  mutate(species_id = str_to_lower(species_id))

# # 4. Species information
sp_info <- read_csv(paste0(input_dir, 'species_metadata.csv'))
sp_info <- sp_info %>% 
  select(species_id, genus, gymno_angio, family)
site_df <- site_df %>% 
  left_join(sp_info, by = "species_id")

# Merge back into main flm_df
 flm_df <- flm_df %>% 
   left_join(site_df, by = "collection_id") %>% 
   filter(species_id %in% c("psme", "pcgl", "pisy", "pcab", "tsme", "abal", "quro",
                            "lasi", "piec", "pifl", "laly", "pist", "quve",
                            "pipo", "pire", "pied", "quma", "auch", "pico", "pial",
                            "pcen","pcma", "pilo", "pila", "quico", "fasy", "psma")) # <-------------------------- can choose species to run through script here
                            
                       
  
# define species_id column to iterate through for for loop
spp_code_list <- unique(flm_df$species_id)

assign("spp_code_list", spp_code_list, envir = .GlobalEnv)

# initiate empty data frame to store final species predictions (spp_predictions_species.csv)
combined_predictions <- data.frame()

###########################################
###########################################

# END Script 1 Pre-Processing steps

##########################################
##########################################


# Set the file paths for the three scripts
script1_path <- "3_run_regressions.R"
script2_path <- "4_sens_predictions.R"
script3_path <- "5_mapping.R"

# Source the three scripts for the current species
source(script1_path, local = TRUE)
source(script2_path, local = TRUE)
source(script3_path, local = TRUE)
  



