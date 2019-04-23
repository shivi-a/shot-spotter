
# Load neccessary dependencies

library(tidyverse)

# Read in shot file and modify factor levels of type for plotting

shot <- read_csv(file = "camdencounty_nj.csv") %>% mutate(type = fct_recode(type, "Multiple Gunshots" = "Multiple_Gunshots",
                           "Single Gunshot" = "Single_Gunshot",
                           "Gunshot or Firecracker" = "Gunshot_or_Firecracker"))

# Create temporary file as precursor to RDS

shot_file <- tempfile()

# Write data originally from CSV to RDS file for manipulation by the Shiny App

write_rds(shot, "shot-spotter/shot_file.rds")