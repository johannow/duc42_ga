raw_dir      <- file.path("01_data", "01_raw_data")
processed_dir<- file.path("01_data", "02_processed_data")
code_dir <- file.path("02_code")
func_dir     <- file.path("04_functions")
mod_dir  <- file.path("03_results", "01_models")
pred_dir  <- file.path("03_results", "02_predictions")

folder_struct <- c(raw_dir,
                 processed_dir,
                 code_dir,
                 func_dir,
                 mod_dir,
                 pred_dir)


# #Check for their existence, create if missing
for(i in 1:length(folder_struct)){
  if(!dir.exists(folder_struct[i])){
    # If not, create the folder
    dir.create(folder_struct[i],recursive = TRUE)
    cat("Folder created:", folder_struct[i], "\n")
  } else {
    cat("Folder already exists:", folder_struct[i], "\n")
  }
}
