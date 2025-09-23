# Define different folders
library(here)

raw_dir      <- here("01_data", "01_raw_data")
processed_dir<- here("01_data", "02_processed_data")
code_brt_dir <- here("02_code", "01_boosted_regression_trees")
code_na_dir  <- here("02_code", "02_network_analysis")
docs_brt_dir <- here("02_code", "01_boosted_regression_trees", "documentation")
docs_na_dir  <- here("02_code", "02_network_analysis", "documentation")
func_dir     <- here("03_functions")
res_brt_dir  <- here("04_results", "01_brt")
res_na_dir   <- here("04_results", "02_na")
fig_brt_dir  <- here("04_results", "01_brt", "figures_tables")
fig_na_dir   <- here("04_results", "02_na", "figures_tables")
mod_brt_dir  <- here("04_results", "01_brt", "models")
mod_gam_dir  <- here("04_results", "01a_GAM", "models")
pred_gam_dir  <- here("04_results", "01a_GAM", "predictions")
mod_na_dir   <- here("04_results", "02_na", "models")
spat_brt_dir <- here("04_results", "01_brt", "geospatial_layers")
spat_na_dir  <- here("04_results", "02_na", "geospatial_layers")


folder_struct <- c(raw_dir,
                 processed_dir,
                 code_brt_dir,
                 code_na_dir,
                 docs_brt_dir,
                 docs_na_dir,
                 func_dir,
                 res_brt_dir,
                 res_na_dir,
                 fig_brt_dir,
                 fig_na_dir,
                 mod_brt_dir,
                 mod_gam_dir,
                 mod_na_dir,
                 pred_gam_dir,
                 spat_brt_dir,
                 spat_na_dir)


# # because it clashes with the wd needed for the qmd rendering, uncomment for now
# #Check for their existence, create if missing
# for(i in 1:length(folder_struct)){
#   if(!dir.exists(folder_struct[i])){
#     # If not, create the folder
#     dir.create(folder_struct[i],recursive = TRUE)
#     cat("Folder created:", folder_struct[i], "\n")
#   } else {
#     cat("Folder already exists:", folder_struct[i], "\n")
#   }
# }