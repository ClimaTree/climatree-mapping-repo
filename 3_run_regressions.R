#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Authors: Briana Barajas, Fletcher McConnell, Rosemary Juarez, Vanessa Salgado
# Project: Mapping Tree Species' Drought Sensitivity Under Climate Change
# Institution: Bren School of Environmental Science & Management - UCSB
# Date: 2024-06-07
# Purpose: Estimate species sensitivity fluctuation under historic climate
#
# Input files:
# - site_pet_cwd_std.csv
# - site_ave_clim.gz
# - site_summary.csv
# - species_metadata.csv
#
# Output files (species specific):
# - ss_bootstrap
# - site_pet_cwd_std_augmented
# - mc_sample
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Package imports --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
 library(fixest)
 # library(raster)
 library(sp)
 library(sf)
 library(rnaturalearth)
 library(rnaturalearthdata)
 library(patchwork)
 library(tidyverse)
 library(dtplyr)
 library(prediction)
 library(tictoc)
 library(furrr)
 library(snow)
 library(profvis)
 library(tmap)
 library(tidylog)
# set.seed(5597)
# 
# select <- dplyr::select
# 
# n_mc <- 1000
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Import data --------------------------------------------------------
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ### Define path
#  data_dir <- "~/../../capstone/climatree/raw_data/"
#  output_dir <- "~/../../capstone/climatree/output/new-output/"
# 
#  # 1. Site-level regressions
#  flm_df <- read_csv(paste0(output_dir, 'site_pet_cwd_std.csv'))
# 
#  # 2. Historic site-level climate
#  ave_site_clim <- read_rds(paste0(output_dir, "site_ave_clim.gz"))
#  flm_df <- flm_df %>%
#    left_join(ave_site_clim, by = c("collection_id"))
# 
#  # 3. Site information
#  site_df <- read_csv(paste0(data_dir, 'site_summary.csv'))
#  site_df <- site_df %>%
#    select(collection_id, sp_id, latitude, longitude)
#  site_df <- site_df %>%
#    rename(species_id = sp_id) %>%
#    mutate(species_id = str_to_lower(species_id))
# 
#  # # 4. Species information
#  sp_info <- read_csv(paste0(data_dir, 'species_metadata.csv'))
#  sp_info <- sp_info %>%
#    select(species_id, genus, gymno_angio, family)
#  site_df <- site_df %>%
#    left_join(sp_info, by = "species_id")
# 
#  # Merge back into main flm_df
#  flm_df <- flm_df %>%
#    left_join(site_df, by = "collection_id") %>%
#    filter(species_id %in% c("pcgl", "psme", "pisy")) # <-------------------------- can choose species to run through script here
# 
# # define species_id column to iterate through for for loop
# spp_code_list <- unique(flm_df$species_id)

# store flm_df data frame for loop
original_flm_df <- flm_df

# for loop for iterating through species in flm_df data frame
for(species in spp_code_list) {
  
  n_mc <- 1000
  
  flm_df <- original_flm_df %>% filter(species_id == species)
  
  print(nrow(flm_df))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Prep and trim data -----------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Add weighting based on inverse of first stage variance
flm_df <- flm_df %>% 
  mutate(cwd_errorweights = 1 / (std.error_cwd.an),
         errorweights2 = sqrt(ntrees),
         pet_errorweights = 1 / (std.error_pet.an),
         int_errorweights = 1 / (std.error_intercept))

# Identify and trim extreme outliers
cwd_est_bounds = quantile(flm_df$estimate_cwd.an, c(0.01, 0.99),na.rm=T)
pet_est_bounds = quantile(flm_df$estimate_pet.an, c(0.01, 0.99),na.rm=T)
cwd_spstd_bounds = quantile(flm_df$cwd.spstd, c(0.01, 0.99), na.rm = T)
pet_spstd_bounds = quantile(flm_df$pet.spstd, c(0.01, 0.99), na.rm = T)

# flm_df <- flm_df %>%
#   mutate(outlier = (estimate_cwd.an<cwd_est_bounds[1]) |
#            (estimate_cwd.an>cwd_est_bounds[2]) |
#            (estimate_pet.an<pet_est_bounds[1]) |
#            (estimate_pet.an>pet_est_bounds[2]) |
#            (cwd.spstd<cwd_spstd_bounds[1]) |
#            (cwd.spstd>cwd_spstd_bounds[2]) |
#            (pet.spstd<pet_spstd_bounds[1]) |
#            (pet.spstd>pet_spstd_bounds[2]))


flm_df <- flm_df %>%
  mutate(outlier = (estimate_cwd.an<cwd_est_bounds[1]) |
           (estimate_cwd.an>cwd_est_bounds[2]) |
           (estimate_pet.an<pet_est_bounds[1]) |
           (estimate_pet.an>pet_est_bounds[2]))

# Save out full flm_df to simplify downstream scripts and ensure consistency
flm_df %>% write.csv(paste0(output_dir, "site_pet_cwd_std_augmented_", species, ".csv"))

# Trim outliers
trim_df <- flm_df %>% 
  filter(outlier==0) %>% 
  drop_na()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Spatial autocorrelation of trim_df ---------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
site_points=st_as_sf(trim_df,coords=c("longitude","latitude"),crs="+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

vg <-variogram(estimate_cwd.an~1, site_points, cutoff = 1500, width = 10)
vg.fit <- fit.variogram(vg, model = vgm(1, "Sph", 900, 1))
plot(vg, vg.fit)
# print(paste0("Range before hitting sill (km): "), as.character(vg.fit[2,3]))

vg.range = vg.fit[2,3] * 1000


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Quick test of primary regression ---------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
formula = as.formula("estimate_cwd.an ~ cwd.spstd + (cwd.spstd^2) + pet.spstd + (pet.spstd^2)")
mod_data <- trim_df
cwd_mod <- feols(formula, data = mod_data, weights = mod_data$cwd_errorweights,
                 vcov = conley(cutoff = vg.range/1000, distance = "spherical"))
summary(cwd_mod)

marg_fx_df <- function(mod){
  inc <- 0.1
  min <- -2.5
  max <- 2.5
  cwd_pred <- predictions(mod, newdata = datagrid(pet.spstd = 0, cwd.spstd = seq(min,max,inc))) %>% 
    mutate(variation = "cwd")
  pet_pred <- predictions(mod, newdata = datagrid(pet.spstd = seq(min,max,inc), cwd.spstd = 0)) %>% 
    mutate(variation = "pet")
  return(rbind(cwd_pred, pet_pred))
}


preds <- marg_fx_df(cwd_mod)

cwd_mfx_plot <- preds %>% 
  filter(variation == "cwd") %>% 
  ggplot(aes(x = cwd.spstd)) + 
  geom_line(aes(y = estimate)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2)
cwd_mfx_plot

pet_mfx_plot <- preds %>% 
  filter(variation == "pet") %>% 
  ggplot(aes(x = pet.spstd)) + 
  geom_line(aes(y = estimate)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2)
pet_mfx_plot

formula = as.formula("estimate_pet.an ~ cwd.spstd + pet.spstd + (cwd.spstd^2) + (pet.spstd^2)")
pet_mod <- feols(formula, weights = mod_data$pet_errorweights, data = mod_data,
                 vcov = conley(cutoff = vg.range/1000, distance = "spherical"))
summary(pet_mod)
preds <- marg_fx_df(pet_mod)

cwd_mfx_plot <- preds %>% 
  filter(variation == "cwd") %>% 
  ggplot(aes(x = cwd.spstd)) + 
  geom_line(aes(y = estimate)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2)
cwd_mfx_plot

pet_mfx_plot <- preds %>% 
  filter(variation == "pet") %>% 
  ggplot(aes(x = pet.spstd)) + 
  geom_line(aes(y = estimate)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2)
pet_mfx_plot

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Identify spatially proximate blocks of sites ---------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
site_list <- trim_df %>%
  pull(collection_id) %>%
  unique()
n_sites <- length(site_list)

site_dist=st_distance(site_points)
rownames(site_dist)=site_points$collection_id
colnames(site_dist)=site_points$collection_id
# save(site_dist,file=paste0(wdir,"out/site_distances.Rdat"))
# load(paste0(wdir,"out/site_distances.Rdat"))

dist_df <- as_tibble(site_dist) %>% 
  drop_units() 

dist_df <- dist_df %>%
  lazy_dt() %>% 
  mutate(collection_id = names(dist_df)) %>% 
  # select(collection_id, site_list) %>% 
  # filter(collection_id %in% site_list) %>% 
  mutate(across(.cols = !collection_id, ~(.x < vg.range))) %>% 
  # mutate(across(.cols = !collection_id, ~ifelse((.x < range), collection_id, "DROP"))) %>% 
  as_tibble()

block_list <- c()
for (site in site_list){
  block_sites <- dist_df %>% 
    filter(get(site) == TRUE) %>% 
    pull(collection_id)
  block_list[site] <- list(block_sites)
}
save(block_list,file=paste0(output_dir,"spatial_blocks_old.Rdat"))
# load(file=paste0(wdir,"out/second_stage/spatial_blocks.Rdat"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Create block bootstrap draws  ---------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
draw_blocks <- function(site_sample){
  samp <- site_sample %>% pull(samp)
  blocked_draw <- (block_list[samp] %>% unlist() %>% unname())[1:n_sites]
  return(blocked_draw)  
}

n_obs = n_mc * n_sites
block_draw_df <- tibble(boot_id = rep(1:n_mc, each = n_sites)) %>% 
  lazy_dt() %>% 
  mutate(samp = sample(site_list,size=n_obs,replace=TRUE)) %>%
  group_by(boot_id) %>% 
  nest()

block_draw_df <- block_draw_df %>% 
  mutate(sites = map(.x = data, .f = draw_blocks)) %>% # COULD PARALLELIZE HERE?
  select(boot_id, sites) %>% 
  as_tibble() %>% 
  unnest(sites) 

block_draw_df <- block_draw_df %>% 
  rename(collection_id = sites)

## Identify number of draws needed for each site
n_draws <- block_draw_df %>% 
  group_by(collection_id) %>% 
  tally() %>% 
  rename(n_draw = n)

trim_df <- trim_df %>% 
  left_join(n_draws, by = "collection_id")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Random draws of coefs from first stage ---------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Function to create random draws of first stage coefficients
draw_coefs <- function(n, cwd_est, pet_est, int_est, cwd_ste, pet_ste, int_ste, 
                       cwd_pet_cov, pet_int_cov, cwd_int_cov){
  mu <- c("cwd_coef" = cwd_est, 
          "pet_coef" = pet_est, 
          "int_coef" = int_est)
  vcov <- matrix(data = NA, nrow = 3, ncol = 3)
  vcov[1,1] <- cwd_ste^2
  vcov[2,2] <- pet_ste^2
  vcov[3,3] <- int_ste^2
  vcov[1,2] <- cwd_pet_cov
  vcov[2,1] <- cwd_pet_cov
  vcov[1,3] <- cwd_int_cov
  vcov[3,1] <- cwd_int_cov
  vcov[2,3] <- pet_int_cov
  vcov[3,2] <- pet_int_cov
  
  draw <- mvrnorm(n, mu, vcov)
  draw <- as_tibble(draw)
  draw$iter_idx <- seq(1,n)
  draw <- draw %>% select(iter_idx, cwd_coef, pet_coef, int_coef)
  return(draw)
}

## Create needed number (n_draw) of random draws of first stage coefficients for each site
trim_df <- trim_df %>% 
  drop_na()

mc_df <- trim_df %>%
  mutate(coef_draws = pmap(list(n = trim_df$n_draw + 1, 
                                cwd_est = trim_df$estimate_cwd.an, 
                                pet_est = trim_df$estimate_pet.an,
                                int_est = trim_df$estimate_intercept, 
                                cwd_ste = trim_df$std.error_cwd.an,
                                pet_ste = trim_df$std.error_pet.an, 
                                int_ste = trim_df$std.error_intercept,
                                cwd_pet_cov = trim_df$cov_cwd_pet, 
                                cwd_int_cov = trim_df$cov_int_cwd,
                                pet_int_cov = trim_df$cov_int_pet), 
                           draw_coefs))


## Unnest to create dataframe of n_site X n_draw coefficient estimates
mc_df <- mc_df %>% 
  unnest(coef_draws) %>% 
  select(collection_id, iter_idx, cwd_coef, pet_coef, int_coef, cwd.spstd, 
         pet.spstd, latitude, longitude, cwd_errorweights, pet_errorweights, int_errorweights)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merge first stage draws back to bootstrap dataframe -------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
block_draw_df <- block_draw_df %>% 
  group_by(collection_id) %>% 
  mutate(iter_idx = 1:n())

block_draw_df <- block_draw_df %>% 
  left_join(mc_df, by = c("collection_id", "iter_idx"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Export first stage draws to pull summary stats -------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 block_draw_df %>% 
   select(boot_id, collection_id, cwd_coef, pet_coef, int_coef, cwd.spstd, pet.spstd) %>% 
   write_rds(paste0(output_dir, "mc_sample_", species, "."), compress = "gz")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run bootstrap estimation of second stage model -------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Function defining main second stage model
run_ss <- function(data, outcome = "cwd_coef"){
  if (outcome == "cwd_coef") {
    error_weights = data$cwd_errorweights
  } else if (outcome == "pet_coef") {
    error_weights = data$pet_errorweights
  } else if (outcome == "int_coef") {
    error_weights = data$int_errorweights
  }
  formula <- as.formula(paste(outcome, " ~ cwd.spstd + I(cwd.spstd^2) + pet.spstd + I(pet.spstd^2)"))
  mod <- lm(formula, data=data, weights = error_weights)
  # mod <- lm(formula, data=data)
  coefs <- mod %>% 
    tidy() %>% 
    pull(estimate) 
  return(coefs)
}


## Function to run second stage model for first stage CWD, 
## PET and Intercept terms and organize coefficients
bs_ss <- function(data){
  # data <- data %>% # Needed to add this since block bootstrap is returning nested tibble
  #   unnest(cols = c(data))
  cwd_mod <- data %>% 
    run_ss(outcome = "cwd_coef")
  pet_mod <- data %>% 
    run_ss(outcome = "pet_coef")
  int_mod <- data %>% 
    run_ss(outcome = "int_coef")
  # return(list("int_mod" = c(int_mod),
  #             "cwd_mod" = c(cwd_mod),
  #             "pet_mot" = c(pet_mod)))
  return(list(int_int = int_mod[1],
              int_cwd = int_mod[2],
              int_cwd2 = int_mod[3],
              int_pet = int_mod[4],
              int_pet2 = int_mod[5],
              cwd_int = cwd_mod[1],
              cwd_cwd = cwd_mod[2],
              cwd_cwd2 = cwd_mod[3],
              cwd_pet = cwd_mod[4],
              cwd_pet2 = cwd_mod[5],
              pet_int = pet_mod[1],
              pet_cwd = pet_mod[2],
              pet_cwd2 = pet_mod[3],
              pet_pet = pet_mod[4],
              pet_pet2 = pet_mod[5]))
}

bs_constant <- function(data){
  # data <- data %>% # Needed to add this since block bootstrap is returning nested tibble
  #   unnest(cols = c(data))
  const_sens <- data %>% 
    summarise(cwd_const_sens = weighted.mean(cwd_coef, cwd_errorweights),
              pet_const_sens = weighted.mean(pet_coef, pet_errorweights),
              int_const_sens = weighted.mean(int_coef, pet_errorweights))
  return(const_sens)
}


## Create dataframe holding bootstrap samples
boot_df <- block_draw_df %>%
  select(-iter_idx) %>% 
  group_by(boot_id) %>% 
  nest()


## Estimate second stage models
boot_df <- boot_df %>% 
  mutate(const_sens = map(.x = data, .f = bs_constant)) %>%
  unnest_wider(const_sens) %>%
  mutate(estimates = map(.x = data, .f = bs_ss)) %>% 
  unnest_wider(estimates) %>% 
  select(-data) %>% 
  ungroup()


## Save out bootstrapped coefficients
write_rds(boot_df, paste0(output_dir, "ss_bootstrap_", species, ".rds"))


print(paste0("Processing of tree species ", species, " is complete."))


}

