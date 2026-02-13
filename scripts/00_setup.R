# **************************************** #
# MHDP project
# Script for setting up the project files and packages
# Oct 2025
# **************************************** #

#Load required packages

library(pacman)
p_load(
  tidyverse,     # data wrangling and visualization
  janitor,       # additional data cleaning functions
  skimr,         # data summary
  psych,         # descriptive statistics
  digest,        # hashing algorithm for unique IDs
  here,          # project oriented workflow
  naniar,        # assess and visualize missingness
  scales,        # easily convert proportions to percents  
  flextable,     # converting tables to pretty images
  gtsummary,     # summary statistics and tests 
  table1,        # for summary EDA tables
  readxl,        # read and write excel files
  glue,          # glue together strings
  lubridate,     # working with date variables
  writexl,       # write excel files
  googledrive,   # interact with Google drive
  googlesheets4, # import data from Google sheets
  quarto,        # dynamic report generation
  sf,            # spatial data handling
  tmap,          # thematic mapping
  ggspatial,     # spatial data visualization
  patchwork,     # Easy combining of plots and charts 
  viridis,       # color palettes for visualizations
  ggthemes,      # additional themes for ggplot2
  ggthemr,       # more themes for ggplot2 form ggthemr
  gt             # beautiful tables
  )           

# create project directories/folders if they do not exist
if(!dir.exists(here::here("data"))) {
  dir.create(here::here("data"))
}
if(!dir.exists(here::here("outputs"))) {
  dir.create(here::here("outputs"))
}
if(!dir.exists(here::here("scripts"))) {
  dir.create(here::here("scripts"))
}

print("Project setup complete. Required packages loaded and directories created.")
