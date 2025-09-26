##################################################################################
##################################################################################

# Author: Jo-Hannes Nowé
# Email: johannes.nowe@vliz.be
# Date: 2025-09-26
# Script Name: ~/duc42_ga/02_code/04_train_GAM_model.R
# Script Description: train a GAM model based on all the data
# SETUP ------------------------------------
cat("\014")                          # Clears the console
rm(list = ls())                      # Remove all variables of the work space
source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## ----load-packages------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(parsnip)
library(tidymodels)
library(geosphere)
library(future)
library(mgcv)
library(tidysdm)
library(lubridate)
library(bundle)
library(sf)
library(terra)
library(lwgeom)
library(ggplot2)

## ----load-data----------------------------------------------------------------
chunk03 <- readRDS(file.path(processed_dir, "output_chunk03.rds"))

chunk03 |> head()

## ----lod-------------------------------------------------------------------------
#The formula for length of day is daylength(lat, day of year)
chunk03 <- chunk03 |>
  dplyr::mutate(lod = geosphere::daylength(deploy_latitude, lubridate::yday(time)))

## ----prep-model-df------------------------------------------------------------

# Convert target to factor (if not yet)
chunk03_prep <- chunk03 |>
  dplyr::mutate(
    acoustic_detection = factor(acoustic_detection, levels = c(1, 0)), #tidymodels assume first level is level of interest
    habitat = as.factor(habitat)) |>
  sf::st_drop_geometry() |>
  dplyr::select(c(acoustic_detection,
                  min_dist_owf,
                  min_dist_shipwreck,
                  elevation,
                  sst,
                  lod)) # select the columns we use for the modelling
#Leave out habitat until NA is fixed, then also need to edit formula

## ----model-recipe-train-------------------------------------------------------

model_recipe <- recipes::recipe(acoustic_detection ~ ., data = chunk03_prep) |> 
  step_normalize( # use 'normalize' instead of 'scale' to have all variables centered around the mean
    all_numeric_predictors()) #The columns created with the step_novel have zero variance and cannot be scaled. 
#recipes::step_novel(habitat) |> #ensures that habitat classes not found during training will be turned to NA during predicting
#recipes::step_unknown(habitat) |> #ensures the model knows how to deal with NA values
#step_dummy(all_nominal_predictors(), one_hot = TRUE) #one_hot = true ensures that all variables get their own dummy


## ----gam-model-formulation----------------------------------------------------
gam_wf <- 
  workflow() %>%
  add_recipe(model_recipe) %>%            # assume model_recipe pre‐processes habitat etc.
  add_model(
    gen_additive_mod() %>%
      set_engine(
        "mgcv",
        family = binomial(link = "logit"),
        method = "REML") %>%
      set_mode("classification"),
    formula = acoustic_detection ~
      s(min_dist_owf, k = 20, bs = "tp") +
      s(elevation, k = 10, bs = "tp") +
      te(sst, lod, k = c(10,10), bs = c("tp", "cc")) +
      s(min_dist_shipwreck, k = 20, bs = "tp")
  )

## ----fit-model----------------------------------------------------------------
start_time <- Sys.time()
# +- 4min
gam_fitted_model <- fit(gam_wf, data = chunk03_prep)
end_time <- Sys.time()
print(end_time - start_time)

## ----save-results-------------------------------------------------------------
# bundle and then save
mod_bundle <- bundle::bundle(gam_fitted_model)
saveRDS(mod_bundle, file.path(mod_dir, "gam_fitted_model.rds"))


