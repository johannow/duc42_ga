
# author: Lotte Pohl, lotte.pohl@vliz.be
# purpose: download environmental data

rm(list = ls()) # clear environment

## ----folder-structure---------------------------------------------------------
# Source preparation script to create relative paths
## for now only folders right?

source("02_code/folder_structure.R")

## ----R-packages---------------------------------------------------------------

# options(repos = c(CRAN = "https://cloud.r-project.org"))
# if (!requireNamespace("rerddap", quietly = TRUE)) {
#   install.packages("rerddap")
# }

# if (!requireNamespace("tidyterra", quietly = TRUE)) {
#   install.packages("tidyterra")
# }

# if (!requireNamespace("emodnet.wfs", quietly = TRUE)) {
# #   install.packages("emodnet.wfs")
#   install.packages("emodnet.wfs", repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))
# }


## ----load-packages------------------------------------------------------------

# unsure still how .renv works with loading packages - so for now loading them like this - change in the future
# install.packages("emodnet.wfs", repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))

# remotes::install_github("ropensci/rerddap")

## For CMEMS data import
# install.packages("CopernicusMarine")  # For CMEMS data access
# install.packages("ncdf4")            # For netCDF file handling
# install.packages("reticulate") 
library(reticulate)
library(CopernicusMarine)
library(ncdf4)
library(dplyr)
library(knitr)
library(readr)
library(emodnet.wfs)
library(rerddap)
library(tidyr)
library(lubridate)
library(sf)
library(terra)

#setwd("C:/Users/lotte.pohl/OneDrive - VLIZ/Documents/repositories/dto-bioflow_wp4_duc2/02_code/01_boosted_regression_trees") # set working directory to the folder where the script is located -> same as qmd rendering

## ----geospatial-data----------------------------------------------------------
# BPNS
BPNS <- 
  sf::st_read(file.path(raw_dir, "BPNS.gpkg")) 

## ----variables----------------------------------------------------------------
lon_min <- 2.2
lon_max <- 3.4
lat_min <- 51
lat_max <- 51.9

start <- "2021-01-01"
end <- "2022-12-31"

## ----shipwrecks---------------------------------------------------------------

shipwrecks <- 
    readr::read_delim(file.path(raw_dir, "wreck-export.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
        dplyr::select(Easting, Northing, `Gezonken op`, Code, Bouwjaar) |>
        sf::st_as_sf(coords = c("Easting", "Northing"), crs = 32631) |>
        sf::st_transform(4326) |>
    dplyr::bind_cols(
        readr::read_delim(file.path(raw_dir, "wreck-export.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
        dplyr::select(Easting, Northing) |>
        sf::st_as_sf(coords = c("Easting", "Northing"), crs = 32631) |>
        sf::st_transform(4326) |>
        sf::st_coordinates() |> # Extract coordinates as a matrix |>
        tidyr::as_tibble() |> # Convert coordinates matrix to tibble
        dplyr::rename(lon = X, lat = Y))


## ----wfs-human----------------------------------------------------------------
# initiate WFS client
wfs_human <- emodnet.wfs::emodnet_init_wfs_client(service = "human_activities")

# inspect available layers
# wfs_human |> emodnet.wfs::emodnet_get_wfs_info() |> View()

OWF <- 
    wfs_human |> 
        emodnet.wfs::emodnet_get_layers(layers = "windfarmspoly", crs = 4326) |>
        purrr::pluck("windfarmspoly") 

## ----erddap-url---------------------------------------------------------------
# This is the url where the EMODnet ERDDAP server is located
erddap_url <- "https://erddap.emodnet.eu/erddap/"

# # Inspect all available datasets
# erddap_datasets <- rerddap::ed_datasets(url = erddap_url)


## ----habitats-ERDDAP----------------------------------------------------------
# potential TODO: put rasterisation into a function (for now only habitats and bathy)
##look for available datasets
# rerddap::ed_search(query = "seabed habitats", url = erddap_url) 
habitats_dataset_id <- "biology_8777_429f_a47a_d420"

# # get info on dataset
habitats_info <- rerddap::info(datasetid = habitats_dataset_id, url = erddap_url) 
# habitats_info

# fetch dataset
habitats_erddap <- 
  rerddap::griddap(datasetx = habitats_info, 
                   longitude = c(lon_min, lon_max), 
                   latitude = c(lat_min, lat_max))

# wrangle
habitats <- 
    # retain the 'data' col
    habitats_erddap$data |>
    # make an sf obj
    mutate(lat = latitude, lon = longitude) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    # retain habitats inside BPNS
    mutate(within_BPNS = sf::st_within(geometry, BPNS) |> lengths() > 0) |>
    filter(within_BPNS) |>
    select(-within_BPNS)


### ----habitats-rasterise-------------------------------------------------------
habitats_vect <- terra::vect(habitats)  # terra's format for vector data

# Define raster extent, resolution and CRS
template_raster <- terra::rast(
  extent = ext(habitats_vect),
  resolution = 0.01,  #0.0001 set resolution based on desired cell size
  crs = crs(habitats_vect)
)

habitats_rast <- 
  terra::rasterize(habitats_vect, 
                   template_raster, 
                   field = "eusm_benthos_eunis2019ID")
# To make it a categorical spatraster
#the categories are the different found classes without NA
unique_habitats <- unique(habitats_vect$eusm_benthos_eunis2019ID)%>%na.omit()
#todo: Change habitat to the correct names when we get this list
levels(habitats_rast) <- data.frame(id = unique_habitats, habitat = unique_habitats)


## ----erddap-bathy-------------------------------------------------------------
# #look for available datasets
# rerddap::ed_search(query = "bathymetry", url = erddap_url) #|> View()
bathy_dataset_id <- "dtm_2020_v2_e0bf_e7e4_5b8f"

# # get info on dataset
bathy_info <- rerddap::info(datasetid = bathy_dataset_id, url = erddap_url) 
# bathy_info

# fetch dataset
bathy_erddap <- rerddap::griddap(datasetx = bathy_info, longitude = c(2.2, 3.4), latitude = c(51, 51.9))

# wrangle
bathy <- 
    # retain the 'data' col
    bathy_erddap$data |>
    # make an sf obj
    mutate(lat = latitude, lon = longitude) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    # retain habitats inside BPNS
    mutate(within_BPNS = sf::st_within(geometry, BPNS) |> lengths() > 0) |>
    filter(within_BPNS) |>
    select(-within_BPNS)



## ----bathy-rasterise----------------------------------------------------------

bathy_vect <- terra::vect(bathy)  # terra's format for vector data

# Define raster extent, resolution and CRS
template_raster <- terra::rast(
  extent = ext(bathy_vect),
  resolution = 0.01,  #0.0001 set resolution based on desired cell size
  crs = crs(bathy_vect)
)

bathy_rast <- terra::rasterize(bathy_vect, template_raster, field = "elevation")

# plot(bathy_rast)


## ----cmems-sst----------------------------------------------------------------
# this is a dataset with daily resolution and the highest spatial res there currently is available

## 0. define parameters
dataset_id <- "IFREMER-ATL-SST-L4-NRT-OBS_FULL_TIME_SERIE"
variable_name <- "analysed_sst"
start_time <- paste0(start, "T00:00:00")
end_time <- paste0(end, "T23:59:00")

output_path <- processed_dir

# following this tutorial: https://help.marine.copernicus.eu/en/articles/8638253-how-to-download-data-via-the-copernicus-marine-toolbox-in-r
library(reticulate)

#step1: virtual environment
virtualenv_create(envname = "CopernicusMarine")

# # only 1st time
# virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))

reticulate::use_virtualenv("CopernicusMarine", required = TRUE)

cmt <- import("copernicusmarine")
# log in


# query the data
cmt$subset(
  dataset_id= dataset_id,
  # dataset_version="202406",
  variables=list(variable_name),
#   longitude = c(2.2, 3.4), latitude = c(51, 51.9)
  minimum_longitude=lon_min - 1,
  maximum_longitude=lon_max + 0.5,
  minimum_latitude=lat_min - 0.5,
  maximum_latitude=lat_max + 0.5,
  start_datetime= start_time,
  end_datetime= end_time,
#   minimum_depth=-40,
#   maximum_depth=0,
  # force_download = TRUE,
  output_directory = output_path
)

## ----sst-time-----------------------------------------------------------------
sst_rast <- terra::rast(file.path(output_path, "IFREMER-ATL-SST-L4-NRT-OBS_FULL_TIME_SERIE_analysed_sst_1.21E-3.89E_50.51N-52.39N_2021-01-01-2022-12-31.nc"), subds = variable_name)

# convert time: already POSIXCT so no conversion needed
time_sst <- terra::time(sst_rast)
summary(time_sst)

# convert temperature from Kelvin to Celcius
sst_rast <- sst_rast - 273.15

## ----sst-BPNS-crop------------------------------------------------------------
# Crop and mask the raster to the BPNS extent
sst_rast_BPNS <- sst_rast |> 
  crop(BPNS) |>          # Trim to bounding box
  mask(BPNS, touches = TRUE)             # Mask values outside polygon


## ----save-outputs-------------------------------------------------------------

sf::st_write(shipwrecks, file.path(processed_dir,"shipwrecks.gpkg"), delete_dsn = TRUE)

#OWF
sf::st_write(OWF, file.path(processed_dir,"OWF.gpkg"), delete_dsn = TRUE)

#seabed habitats
# sf::st_write(habitats, file.path(processed_dir,"habitats.gpkg"), delete_dsn = TRUE)

#tif, netcdf does not retain categories information
terra::writeRaster(habitats_rast, file.path(processed_dir,"habitats_rast.tif"), overwrite = TRUE)

# bathymetry
# sf::st_write(bathy, file.path(processed_dir,"bathy.gpkg"), delete_dsn = TRUE)

## netcdf
terra::writeCDF(bathy_rast, file.path(processed_dir,"bathy_rast.nc"), varname = "elevation", overwrite = TRUE)

# sst
terra::writeCDF(sst_rast_BPNS, file.path(processed_dir,"sst_rast.nc"), varname = variable_name, overwrite = TRUE, metags = list()) #throws error because it cannot write the link 'http://copernicus.marine.eu/''

## tiff
# terra::writeRaster(sst_rast_BPNS, file.path(processed_dir,"sst_rast.tiff"), overwrite = TRUE)


