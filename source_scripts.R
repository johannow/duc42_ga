# This script sources all code chunks in '02_code'in the correct order
source("02_code/folder_structure.R")
rm(list=ls())
# Chunk 01
source("02_code/01_detection_data_to_hourly_presences.R")

# Chunk 02
source("02_code/02_download_environmental_covariates.R")

# Chunk 03
source("02_code/03_couple_env_data_to_detections.R")

# Chunk 04
source("02_code/04_train_GAM_model.R")

# Chunk 05
source("02_code/05_create_env_grids_to_predict.R")
