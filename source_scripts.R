# This script sources all code chunks in '02_code'in the correct order

rm(list=ls())
# Chunk 01
source("02_code/01_detection_data_to_hourly_presences.R")

# Chunk 02
source("02_code/02_download_environmental_covariates.R")

# Chunk 03
source("02_code/03_couple_env_data_to_detections.R")

