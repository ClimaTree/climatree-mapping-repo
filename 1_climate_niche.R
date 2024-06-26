#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Authors: Briana Barajas, Fletcher McConnell, Rosemary Juarez, Vanessa Salgado
# Project: Mapping Tree Species' Drought Sensitivity Under Climate Change
# Institution: Bren School of Environmental Science & Management - UCSB
# Date: 2024-06-07
# Purpose: Extract and standardize climate data across species ranges
#
# Input files:
# - TerraClimate19611990_def.nc: rasters of historic cwd
# - TerraClimate19611990_pet.nc: rasters of historic pet
# - site_summary.csv: site attributes
# - itrdbsites_pet.csv: monthly site pet
# - itrdbsites_def.csv monthly site cwd
# - merged_ranges_dissolve.shp: shapefiles tree species' ranges
# - cmip5_cwdaet_end.Rdat: cwd predictions using CMIP5
# - cmip5_cwdaet_start.Rdat: cwd predictions using CMIP5
#
# Output files:
# - clim_niche.csv
# - sp_clim_predictions.gz
# - site_ave_clim.gz
# - site_an_clim.gz
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Package imports --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(dbplyr)
library(RSQLite)
library(ggplot2)
library(sf)
library(rgeos)
library(stringr)
library(raster)
library(readr)
library(tmap)
library(tictoc)
library(terra)
library(data.table)
select <- dplyr::select


library(furrr)
n_cores <- 8
future::plan(multisession, workers = n_cores)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load data --------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define path
data_dir <- "~/../../capstone/climatree/raw_data/"
output_dir <- "~/../../capstone/climatree/output/new-output/"

# 1. Historic climate raster
# clim_file <- paste0(data_dir, 'HistoricCWD_AETGrids_Annual.Rdat')
# load(clim_file)
# cwd_historic <- mean(cwd_historic)
# aet_historic <- mean(aet_historic)
# pet_historic <- aet_historic + cwd_historic
# names(cwd_historic) = "cwd"
# names(pet_historic) = "pet"
# 
# # 2. Data on historic baseline temp and precip
# temps_historic <- raster(paste0(data_dir, "monthlycrubaseline_tas"))
# names(temps_historic) = "temp"
# temps_historic <- resample(temps_historic, cwd_historic)
# clim_historic <- raster::brick(list(cwd_historic, pet_historic, temps_historic))
# 
# # 3. Site-specific historic climate data
# site_clim_csv <- paste0(data_dir, 'essentialcwd_data.csv')
# site_clim_df <- read_csv(site_clim_csv)
# site_clim_df <- site_clim_df %>% 
#   mutate("site_id" = as.character(site)) %>% 
#   rename(location_id = site_id)#,
# # precip = ppt) 

# 1. Add terraclimate raster data of historic climates
cwd_tc <- rast(paste0(data_dir,"TerraClimate19611990_def.nc")) %>%
  sum()
pet_tc <- rast(paste0(data_dir,"TerraClimate19611990_pet.nc")) %>%
  sum()
clim_tc <- rast(list("cwd" = cwd_tc, "pet" = pet_tc))

# load species information for sites (for join)
site_smry <- read_csv(paste0(data_dir, 'site_summary.csv')) %>% 
  select(latitude, collection_id)

# 2. Add terraclimate site-month-year data
tc_pet <- read_csv(paste0(data_dir,"itrdbsites_pet.csv"))
tc_cwd <- read_csv(paste0(data_dir,"itrdbsites_def.csv"))
site_clim_df <- tc_pet %>%
  left_join(tc_cwd, by = c("collection_id", "Month", "year")) %>%
  rename(month = Month,
         pet = pet,
         cwd = def) %>% 
  left_join(site_smry, by = "collection_id")

# 3. Load species information for sites
site_smry <- read_csv(paste0(data_dir, 'site_summary.csv'))
site_smry <- site_smry %>%
  select(collection_id, sp_id, latitude) %>% 
  mutate(location_id = collection_id) %>% 
  mutate(sp_code = tolower(sp_id)) %>% 
  select(-sp_id) 


# convert site_clim_df to data table for calculation below
setDT(site_clim_df)

# 4. Add water year
site_clim_df[,water_year:=year]
site_clim_df[(latitude>=0) & (month>=10),water_year:=year+1] # Northern hemisphere water year is october through september
site_clim_df[(latitude<0) & (month>=7),water_year:=year+1] # Southern hemisphere water year is July through June
site_clim_df <- site_clim_df %>% 
  as_tibble() %>% 
  select(-year) %>% 
  rename(year = water_year)

# 5. Calculate site-level annual climate
site_clim_df = site_clim_df %>%
  group_by(collection_id, year) %>%
  summarise(cwd.an = sum(cwd),
            pet.an = sum(pet),
            .groups = "drop")

# # 4. Load species information for sites
# site_smry <- read_csv(paste0(data_dir, 'site_summary.csv'))
# site_smry <- site_smry %>%
#   select(collection_id, sp_id) %>% 
#   mutate(location_id = collection_id) %>% 
#   mutate(sp_code = tolower(sp_id)) %>% 
#   select(-sp_id)


## NOTE: FIA data not included in replication data repository
# site_smry_fia <- read_csv(paste0(wdir, 'out/dendro/site_summary_fia.csv'))
# site_smry_fia <- site_smry_fia %>% 
#   select(collection_id, location_id = plot_cn, sp_id = species_id) %>% 
#   mutate(sp_code = tolower(sp_id)) %>% 
#   select(-sp_id)
# site_smry <- rbind(site_smry, site_smry_fia)


# 6. Species range maps
range_file <- paste0(data_dir, 'merged_ranges_dissolve.shp')
range_sf <- st_read(range_file)

# 7. Climate projections from CMIP5
cmip_end <- load(paste0(data_dir, 'cmip5_cwdaet_end.Rdat'))
pet_cmip_end <- aet_raster + cwd_raster
cwd_cmip_end <- cwd_raster
names(cwd_cmip_end) <- NULL # Resetting this due to strange names in file from CMIP processing
# Convert raster objects to terra objects
pet_cmip_end <- rast(pet_cmip_end)
cwd_cmip_end <- rast(cwd_cmip_end)
rm(cwd_raster)
rm(aet_raster)


cmip_start <- load(paste0(data_dir, 'cmip5_cwdaet_start.Rdat'))
pet_cmip_start <- aet_raster + cwd_raster
cwd_cmip_start <- cwd_raster
names(cwd_cmip_start) <- NULL # Resetting this due to strange names in file from CMIP processing
# Convert raster objects to terra objects
pet_cmip_start <- rast(pet_cmip_start)
cwd_cmip_start <- rast(cwd_cmip_start)
rm(cwd_raster)
rm(aet_raster)


clim_tc <- resample(clim_tc, cwd_cmip_start, method = "bilinear")


# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Visually inspect data -----------------------------------------------
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# tmap_mode("view")
# tm_shape(clim_tc) +
#   tm_raster() +
#   tm_facets(as.layers = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Summarize species niches -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Pull and organize climate distribution for species
# pull_clim <- function(spp_code){
#   print(spp_code)
#   # Pull relevant range map
#   sp_range <- range_sf %>%
#     filter(sp_code == spp_code) %>% 
#     rasterize(cwd_historic, getCover=TRUE)
#   sp_range[sp_range==0] <- NA
#   
#   # Pull cwd and aet values
#   cwd_vals <- cwd_historic %>% 
#     mask(sp_range) %>% 
#     as.data.frame(xy = TRUE) %>% 
#     drop_na()
#   
#   pet_vals <- pet_historic %>% 
#     mask(sp_range) %>% 
#     as.data.frame(xy = TRUE) %>% 
#     drop_na()
#   
#   temp_vals <- temps_historic %>% 
#     mask(sp_range) %>% 
#     as.data.frame(xy = TRUE) %>% 
#     drop_na()
#   
#   # Combine into tibble
#   clim_vals <- cwd_vals %>% 
#     left_join(pet_vals, by = c("x", "y")) %>% 
#     left_join(temp_vals, by = c("x", "y"))
#   
#   return(clim_vals)
# }

# # updated pull_clim function using terra, instead of raster
pull_clim <- function(spp_code, clim_raster){
  print(spp_code)
  
  # Pull relevant range map
  sp_range <- range_sf %>%
    filter(sp_code == spp_code)
  
  # Pull clim values
  clim_vals <- clim_raster %>% 
    mask(mask = sp_range, touches = TRUE) %>% 
    as.data.frame(xy = TRUE) %>% 
    drop_na()
  
  return(clim_vals)
}

species_list <- range_sf %>%
  pull(sp_code) %>% 
  unique() %>% 
  enframe(name = NULL) %>% 
  select(sp_code = value) %>% 
  arrange(sp_code) %>% 
  drop_na()

pull_clim_tc <- partial(.f = pull_clim, clim_raster = clim_tc)

clim_df_tc <- species_list %>%
  mutate(clim_vals = map(sp_code,.f = pull_clim_tc))


# clim_df <- species_list %>% 
#   mutate(clim_vals = future_map(sp_code, 
#                                 .f = pull_clim,
#                                 .options = furrr_options(packages = c( "dplyr", "raster", "sf")),
#                                 .progress = TRUE))


## Summarize mean and sd of each species' climate
niche_df <- clim_df_tc %>% 
  unnest(clim_vals) %>% 
  group_by(sp_code) %>% 
  summarize(pet_mean = mean(pet),
            pet_sd = sd(pet),
            cwd_mean = mean(cwd),
            cwd_sd = sd(cwd))
#temp_mean = mean(temp),
#temp_sd = sd(temp))



## Export species niche description
write.csv(niche_df, paste0(output_dir, "clim_niche.csv"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Standardize historic climate -------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sp_standardize <- function(val, sp_mean, sp_sd){
  std_val <- (val - sp_mean) / sp_sd
  return(std_val)
}

sp_std_historic_df <- function(hist_clim_vals, pet_mean, pet_sd, cwd_mean, cwd_sd){
  hist_clim_vals <- hist_clim_vals %>% 
    mutate_at(vars(starts_with("cwd")), 
              ~sp_standardize(.x, cwd_mean, cwd_sd)) %>% 
    mutate_at(vars(starts_with("pet")), 
              ~sp_standardize(.x, pet_mean, pet_sd)) #%>% 
  #mutate_at(vars(starts_with("temp")), 
  # ~sp_standardize(.x, temp_mean, temp_sd))
  return(hist_clim_vals)
}


sp_std_future_df <- function(cmip_df, hist_clim_vals, pet_mean, pet_sd, cwd_mean, cwd_sd){
  valid_locations <- hist_clim_vals %>% select(x,y)
  cmip_df <- valid_locations %>% 
    left_join(cmip_df, by = c("x", "y"))
  cmip_df <- cmip_df %>% 
    mutate_at(vars(starts_with("cwd")), 
              ~sp_standardize(.x, cwd_mean, cwd_sd)) %>% 
    mutate_at(vars(starts_with("pet")), 
              ~sp_standardize(.x, pet_mean, pet_sd)) #%>% 
  #mutate_at(vars(starts_with("temp")), 
  # ~sp_standardize(.x, temp_mean, temp_sd))
  return(cmip_df)
}


clim_df <- clim_df_tc %>% 
  left_join(niche_df, by = "sp_code")


clim_df <- clim_df %>% 
  mutate(clim_historic_sp = future_pmap(list(hist_clim_vals = clim_vals,
                                             pet_mean = pet_mean,
                                             pet_sd = pet_sd,
                                             cwd_mean = cwd_mean,
                                             cwd_sd = cwd_sd),
                                        #temp_mean = temp_mean,
                                        #temp_sd = temp_sd),
                                        .f = sp_std_historic_df,
                                        .options = furrr_options(packages = c( "dplyr"))))
# NOTE: May no longer need this dataframe???


hist_clim_vals = (clim_df[1,]$clim_vals)[[1]]
pet_mean = (clim_df[1,]$pet_mean)[[1]]
pet_sd = (clim_df[1,]$pet_sd)[[1]]
cwd_mean = (clim_df[1,]$cwd_mean)[[1]]
cwd_sd = (clim_df[1,]$cwd_sd)[[1]]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Pull CMIP projections -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pet_end_df <- pet_cmip_end %>% 
  as.data.frame(xy = TRUE) %>%
  drop_na() %>% 
  as_tibble() %>%
  rename_with(~stringr::str_replace(., "layer.", "pet_cmip_end"), 
              starts_with('layer.'))

cwd_end_df <- cwd_cmip_end %>% 
  as.data.frame(xy = TRUE) %>%
  drop_na() %>% 
  as_tibble() %>%
  rename_with(~stringr::str_replace(., "layer.", "cwd_cmip_end"), 
              starts_with('layer.'))


pet_start_df <- pet_cmip_start %>% 
  as.data.frame(xy = TRUE) %>%
  drop_na() %>% 
  as_tibble() %>%
  rename_with(~stringr::str_replace(., "layer.", "pet_cmip_start"), 
              starts_with('layer.'))

cwd_start_df <- cwd_cmip_start %>% 
  as.data.frame(xy = TRUE) %>%
  drop_na() %>% 
  as_tibble() %>%
  rename_with(~stringr::str_replace(., "layer.", "cwd_cmip_start"), 
              starts_with('layer.'))


# ## Illustrate forawrd/backward conversion between df and raster
# crs_template <- crs(cwd_future)
# raster_template <- cwd_df %>% select(x,y)
# cwd_df <- cwd_df %>% 
#   drop_na()
# cwd_df2 <- raster_template %>% 
#   left_join(cwd_df, by = c("x", "y"))
# cwd_rast2 <- rasterFromXYZ(cwd_df2, crs = crs_template)

## Combine PET and CWD projections
cmip_df <- cwd_end_df %>% 
  full_join(pet_end_df, by = c("x", "y")) %>% 
  full_join(cwd_start_df, by = c("x", "y")) %>% 
  full_join(pet_start_df, by = c("x", "y"))


## Nest CMIP data
cmip_df <- cmip_df %>%
  mutate(idx = 1) %>% 
  group_by(idx) %>% 
  nest() %>% 
  ungroup() %>% 
  select(data)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Summarize cmip climate for each species ------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Cross species list with nested cmip data
sp_cmip_clim <- clim_df %>% 
  mutate(cmip_df = cmip_df$data)


sp_cmip_clim <- sp_cmip_clim %>% 
  mutate(clim_cmip_sp = future_pmap(list(cmip_df = cmip_df,
                                         hist_clim_vals = clim_vals,
                                         pet_mean = pet_mean,
                                         pet_sd = pet_sd,
                                         cwd_mean = cwd_mean,
                                         cwd_sd = cwd_sd), 
                                    .f = sp_std_future_df,
                                    .options = furrr_options(packages = c( "dplyr")))) %>% 
  select(-cmip_df)


# ## Check final result as raster
#  species = "pcgl"
#  test_clim <- (sp_cmip_clim %>% filter(sp_code == species) %>% pull(clim_cmip_sp))[[1]]
#  crs_template <- crs(cwd_cmip_end)
#  raster_template <- cwd_cmip_end %>% as.data.frame(xy = TRUE) %>% select(x,y)
#  test_clim <- raster_template %>%
#    left_join(test_clim, by = c("x", "y"))
#  test_clim <- rasterFromXYZ(test_clim, crs = crs_template)
#  range <- range_sf %>% filter(sp_code == species)
#  tmap_mode("view")
# # 
#  tm_shape(test_clim$cwd_cmip_end1) +
#    tm_raster(palette = "-RdYlGn") +
#    tm_facets(as.layers = TRUE) +
#    tm_shape(range) + 
#    tm_fill(col = "lightblue")
# # 
#  tm_shape(test_clim$cwd_cmip_end1) +
#    tm_raster(palette = "-RdYlGn") +
#    tm_facets(as.layers = TRUE) +
#    tm_shape(test_clim$cwd_cmip_start1) +
#    tm_raster(palette = "-RdYlGn") +
#    tm_facets(as.layers = TRUE) +
#    tm_shape(range) + 
#    tm_fill(col = "lightblue")
# 

## Export predictions
write_rds(sp_cmip_clim, paste0(output_dir, "sp_clim_predictions.", compress = "gz"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Apply species standardization to site-level data -----------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# niche_df <- niche_df %>% 
#   select(sp_code = sp_code, sp_pet_mean = pet_mean, sp_pet_sd = pet_sd, sp_cwd_mean = cwd_mean, sp_cwd_sd = cwd_sd)

# ===============================================================
# Left off here, new data does not have the same column names
# ===============================================================

# Calculate site-level annual climate
# site_clim_df = site_clim_df %>%
#   group_by(collection_id, year) %>%
#   summarise(
#     #aet.an = sum(aet),
#     cwd.an = sum(cwd),
#     pet.an = sum(pet),
#     #temp.an = mean(tmean),
#     .groups = "drop")

### Calculate site-level, average, historic, relative climate (for second stage)
## TODO: Note - dropping CANA323 because it has null climate data for a few months each year. might want to dig into this
site_clim_df <- site_clim_df %>% 
  filter(collection_id != "CANA323")

ave_site_clim_df <- site_clim_df %>% 
  filter(year < 1980) %>% 
  group_by(collection_id) %>% 
  summarise(cwd.ave = mean(cwd.an),
            pet.ave = mean(pet.an),
            cwd.sd = sd(cwd.an),
            pet.sd = sd(pet.an)) %>% 
  #temp.ave = mean(temp.an),
  #temp.sd = sd(temp.an)) %>% 
  ungroup()

spstd_site_clim_df <- site_smry %>% 
  left_join(ave_site_clim_df, by = "collection_id") %>% 
  group_by(sp_code) %>% 
  nest(data = c(collection_id, 
                cwd.ave, 
                pet.ave)) %>%  
  #temp.ave)) %>% 
  left_join(niche_df, by = ("sp_code")) %>%
  drop_na() # Dropping some species due to NA niche data

spstd_site_clim_df <- spstd_site_clim_df %>% 
  mutate(site_clim = future_pmap(list(hist_clim_vals = data,
                                      pet_mean = pet_mean,
                                      pet_sd = pet_sd,
                                      cwd_mean = cwd_mean,
                                      cwd_sd = cwd_sd),
                                 #temp_mean = temp_mean,
                                 #temp_sd = temp_sd),
                                 .f = sp_std_historic_df,
                                 .options = furrr_options(packages = c( "dplyr"))))

spstd_site_clim_df <- spstd_site_clim_df %>% 
  unnest(site_clim) %>% 
  rename(cwd.spstd = cwd.ave, 
         pet.spstd = pet.ave) %>%  
  #temp.spstd = temp.ave) %>% 
  mutate(cwd.sd = cwd.sd / cwd_sd,
         pet.sd = pet.sd / pet_sd) %>% 
  #temp.sd = temp.sd / temp_sd) %>% 
  ungroup() %>% 
  select(collection_id, location_id, cwd.spstd, 
         pet.spstd, 
         #temp.spstd, 
         cwd.sd, 
         pet.sd) 
#temp.sd)

spstd_site_clim_df <- spstd_site_clim_df %>% 
  left_join(ave_site_clim_df %>% select(collection_id, 
                                        cwd.ave, 
                                        pet.ave), by = "collection_id") 
#temp.ave), by = "location_id")

spstd_site_clim_df <- spstd_site_clim_df %>% 
  select(-location_id)

write_rds(spstd_site_clim_df, 
          paste0(output_dir, "site_ave_clim.", compress = "gz"))



### Calculate site-level, annual, historic, relative climate (for first stage) 
an_site_clim_df <- site_smry %>% 
  left_join(site_clim_df, by = "collection_id") %>% 
  group_by(sp_code) %>% 
  nest() %>% 
  left_join(niche_df, by = "sp_code") %>% 
  drop_na()

an_site_clim_df <- an_site_clim_df %>% 
  mutate(site_clim = future_pmap(list(hist_clim_vals = data,
                                      pet_mean = pet_mean,
                                      pet_sd = pet_sd,
                                      cwd_mean = cwd_mean,
                                      cwd_sd = cwd_sd),
                                 #temp_mean = temp_mean,
                                 #temp_sd = temp_sd),
                                 .f = sp_std_historic_df,
                                 .options = furrr_options(packages = c( "dplyr"))))

an_site_clim_df <- an_site_clim_df %>% 
  unnest(site_clim) %>% 
  rename(cwd.an.spstd = cwd.an, 
         pet.an.spstd = pet.an) %>%  
  #temp.an.spstd = temp.an) %>% 
  #ungroup() %>% 
  select(
    # -aet.an, 
    -pet_mean, 
    -pet_sd, -cwd_mean, -cwd_sd, -data, -sp_code)

an_site_clim_df <- an_site_clim_df %>%
  select(-location_id)

write_rds(an_site_clim_df, 
          paste0(output_dir, "site_an_clim.", compress = "gz"))


# ## Exploring source of dropped sites - seems to be entirely driven by sites for species with no range maps
# an_site_clim_df %>% pull(collection_id) %>% unique() %>% length()
# site_clim_df %>% pull(collection_id) %>% unique() %>% length()
# # clim_sites <- clim_df %>% pull(collection_id) %>% unique()
# test_sites <- test %>% pull(collection_id) %>% unique()
# an_site_clim_df %>% unnest(cols = c(data)) %>% pull(collection_id) %>% unique() %>% length()
# an_site_clim_df %>% unnest(cols = c(data)) %>% drop_na() %>% pull(collection_id) %>% unique() %>% length()

