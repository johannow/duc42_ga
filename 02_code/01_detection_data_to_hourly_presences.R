
# author: Lotte Pohl, lotte.pohl@vliz.be
# purpose: process raw acoustic detections and deployments data to a monthly aggregation of presences per station.
# Note: JHN, your header looks way nicer so I will upgrade :)


rm(list = ls()) # clear environment
## ----setup-folders------------------------------------------------------------
# Source preparation script to create relative paths
source("02_code/folder_structure.R")

## ----load-packages------------------------------------------------------------
# unsure still how .renv works with loading packages - 
#so for now loading them like this - change in the future

# install.packages(c("mregions2", "knitr", "dplyr", 
# "remotes", "leaflet", "sf", "ggplot2", "plotly"))
# remotes::install_github("inbo/etn@v2.3-beta", force = TRUE)

library(dplyr)
library(knitr)
library(tidyr)
library(lubridate)
library(sf)
# getwd()

## ----parameters----------------------------------------------------------------
start <- "2021-01-01"
end <- "2022-12-31"

## ----read raw data----------------------------------------------------------------

# Belgian EEZ --> will be the bounding box
BPNS <- 
  sf::st_read(file.path(raw_dir, "BPNS.gpkg")) 

### ----acoustic-detections--------------------------------------------------

detections_raw <- base::readRDS("01_data/01_raw_data/detections.rds") 
            
detections_month <- 
  detections_raw |>
      dplyr::mutate(
       month = lubridate::month(date_time))  |>
      dplyr::group_by(month, station_name) |>
      dplyr::summarise(n_dets = dplyr::n(), .groups = "drop",
      n_animals = n_distinct(animal_id) %>% as.integer(),
      lat = deploy_latitude |> mean(na.rm = T),
      lon = deploy_longitude |> mean(na.rm = T)) |>
  # make into sf object
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) 

# save the monthly detections dataframe
base::saveRDS(detections_month, file.path(processed_dir, "detections_month.rds"))


### ----acoustic_deployments-------------------------------------------------

deployments <-
  base::readRDS(file.path(raw_dir, "deployments.rds"))

## ----create-time-index--------------------------------------------------------
# Create a time index for each day from start to end
time_index <- 
  tidyr::tibble(time = seq(
    from = as.Date(start),
    to = as.Date(end),
    by = "day"))

## ----stations-days------------------------------------------------------------
stations <- 
  deployments |>
    dplyr::group_by(station_name) |>
    dplyr::summarise(deploy_latitude = deploy_latitude |> mean(na.rm = T),
                     deploy_longitude = deploy_longitude |> mean(na.rm = T))

stations_days <- 
  time_index |>
    tidyr::crossing(station_name = stations$station_name)


## ----detections-days----------------------------------------------------------
# Merge detection data with station activity times
detections_days <- 
  detections_raw |>
    dplyr::mutate(time = as.Date(date_time)) |>
    dplyr::select(time, station_name) |>
    dplyr::distinct() |>
    dplyr::mutate(value_det = 1) # 1 for presence

#detections_days |> slice_sample(n = 10) |> knitr::kable()

## ----deployments-days---------------------------------------------------------
deployments_days <-
  time_index |>
    dplyr::left_join(deployments |>
              dplyr::select(station_name, deploy_date_time, recover_date_time) |>
              sf::st_drop_geometry() |>
              dplyr::mutate(value_deploy = 0,
                            deploy_date = as.Date(deploy_date_time),
                            recover_date = as.Date(recover_date_time)),
    by = dplyr::join_by(between(time, deploy_date, recover_date)))|>
    dplyr::select(-c(deploy_date_time, recover_date_time, deploy_date, recover_date))

#deployments_days |> dplyr::slice_sample(n = 10) |> knitr::kable()


## ----merge-dataframes---------------------------------------------------------
time_stations_deployments_detections <-
  stations_days |>
  left_join(detections_days, by = c("time", "station_name"))  |>
  left_join(deployments_days, 
    by = c("time", "station_name")) |>
  dplyr::mutate(value = coalesce(value_det, value_deploy)) |>
  dplyr::select(time, station_name, value) 

# don't pivot but only retain actively listening stations
output_chunk01 <- 
  time_stations_deployments_detections |>
      dplyr::filter(!is.na(value)) |>
      dplyr::left_join(stations, by = join_by(station_name))
    # tidyr::pivot_wider(names_from = station_name, values_from = value) 

## ----save-outputs-------------------------------------------------------------
# as RDS
base::saveRDS(output_chunk01, file.path(processed_dir, "output_chunk01.rds"))
base::saveRDS(stations, file.path(processed_dir, "stations.rds"))

