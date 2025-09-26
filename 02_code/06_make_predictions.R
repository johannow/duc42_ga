##################################################################################
##################################################################################

# Author: Jo-Hannes Nowé
# Email: johannes.nowe@vliz.be
# Date: 2025-09-26
# Script Name: ~/duc42_ga/02_code/06_make_predictions.R
# Script Description: predict on the environmental rasters using the model
# SETUP ------------------------------------
cat("\014")                          # Clears the console
rm(list = ls())                      # Remove all variables of the work space
source("02_code/folder_structure.R") # Create relative paths

##################################################################################
##################################################################################

## -----------------------------------------------------------------------------
library(bundle)
library(terra)
library(tidymodels)
## -----------------------------------------------------------------------------
# Load model (not sure about the name of saved object)
# model <- readRDS(file.path(processed_dir, "xgb_minimal_final.rds"))
model <- readRDS(file.path(mod_dir, "gam_fitted_model.rds"))
model <- unbundle(model)

## -----------------------------------------------------------------------------
bathy <- terra::rast(file.path(processed_dir, "bathy_rast.nc"))
habitats <- terra::rast(file.path(processed_dir, "habitats_rast.tif"))
lat <- terra::rast(file.path(processed_dir, "lat_rast.nc"))
lon <- terra::rast(file.path(processed_dir, "lon_rast.nc"))
lod <- terra::rast(file.path(processed_dir, "lod_rast.nc"))
OWF_dist <- terra::rast(file.path(processed_dir, "OWF_dist_rast.nc"))
shipwreck_dist <- terra::rast(file.path(processed_dir, "shipwreck_dist_rast.nc"))
sst <- terra::rast(file.path(processed_dir, "sst_rast.nc"))

## ----prediction-function------------------------------------------------------
#Function to use in the terra::predict()
predprob <- function(...) predict(...,type="prob")$.pred_1

## ----predict------------------------------------------------------------------
bathy <- terra::resample(bathy, sst) #need to align to allow predictions
lod2 <- c(lod, lod) #because we have two years of data
r_zero <- OWF_dist  
values(r_zero) <- 0
r_zero <- terra::mask(r_zero, OWF_dist) #Now we have a raster with all distance to owf = 0
#extract model predictor names
predictor_names <-
    model |> 
    extract_fit_engine() |>
    formula() |>
    all.vars()
  
predictions <- list()
predictions_all_owf <- list()
diff_owf <- list()
  # make monthly predictions
  for(i in 1:nlyr(sst)){
    predictors <- c(bathy, sst[[i]], lod2[[i]], OWF_dist, shipwreck_dist)
    predictors_all_owf <- c(bathy, sst[[i]], lod2[[i]], r_zero, shipwreck_dist) #use a raster with distance to OWF = 0 to mimick all OWF
    names(predictors) <- c("elevation", "sst", "lod", "min_dist_owf", "min_dist_shipwreck")
    names(predictors_all_owf) <- c("elevation", "sst", "lod", "min_dist_owf", "min_dist_shipwreck")
  
    # Predict
    predictions[[i]] <- terra::predict(predictors, model = model, fun = predprob)
    predictions_all_owf[[i]] <- terra::predict(predictors_all_owf, model = model, fun = predprob)
    
    #the difference in suitability when a owf everywhere
    diff_owf[[i]] <- predictions_all_owf[[i]] - predictions[[i]]
    #if positive it means the suitability improved when an owf was placed on the spot
  }
  predictions <- terra::rast(predictions)
  predictions_all_owf <- terra::rast(predictions_all_owf)
  diff_owf <- terra::rast(diff_owf)
  
terra::plot(diff_owf)

terra::writeCDF(x = predictions,
                filename = file.path(pred_dir,"predictions_current.nc"),
                varname = "Encounter rate",
                overwrite = TRUE)
terra::writeCDF(x = predictions_all_owf,
                filename = file.path(pred_dir,"predictions_all_owf.nc"),
                varname = "Encounter rate",
                overwrite = TRUE)
terra::writeCDF(x = diff_owf,
                filename = file.path(pred_dir,"diff_owf.nc"),
                varname = "Difference in encounter rate",
                overwrite = TRUE)
