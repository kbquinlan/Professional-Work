#################################################################
################# Fragile State Index Data Pull #################
#################################################################

###################### SET  UP ENVIRONMENT ###########################
# Load libraries
# ALL OF THE FOLLOWING PACKAGES ARE REQUIRED FOR THIS SCRIPT WITH THE EXCEPTION
# OF ONES COMMENTED OUT
library(data.table)
library(dtplyr)
library(tidyverse)
library(janitor)
library(readxl)
#library(writexl)
library(countrycode)
#library(acled.api) # LOOK OUT FOR UPDATES TO THIS PACKAGE AS IT IS A DEVELOPMENT PACKAGE
#library(vdemdata) # LOOK TO UPDATE THIS EVERY YEAR AS NEW DATA IS RELEASED; DEVELOPMENT PACKAGE AS WELL
#library(WDI) # Package for pulling in World Bank data
library(rvest) # For web scraping data
library(openxlsx) # For reading in Excel files from web pages

############## CREATE DATAFRAME OF STANDARDIZED COUNTRY NAMES ################
# Use the vdem package to create a dataframe of standardized country names
# These names are the closest to CEPPS as well as with regions
# This will be used to standardize country names in the ACLED data
country_names <- vdemdata::vdem |> 
  # Filter to the most recent year
  filter(year == max(year)) |>
  # Select country_name, country_text_id, and region
  distinct(country_cepps = country_name, iso3c = country_text_id, 
           region = e_regionpol_6C) |> 
  # Add in iso3n codes, change some country names, and make region codes names
  mutate(iso3n = countrycode(country_cepps, "country.name", "iso3n"), 
         country_cepps = case_when(str_detect(country_cepps, "Palestine") ~ "West Bank/Gaza", 
                                   str_detect(country_cepps, "Burma") ~ "Burma", 
                                   country_cepps == "Kyrgyzstan" ~ "Kyrgyz Republic",
                                   TRUE ~ country_cepps), 
         region_cepps = case_when(region == 1 ~ "Eurasia", 
                                  region == 2 ~ "Latin America and the Caribbean", 
                                  region == 3 ~ "Middle East and North Africa",
                                  region == 4 ~ "Africa", 
                                  region == 5 ~ "Western Europe and North America", 
                                  region == 6 ~ "Asia")) |> 
  # Remove region codes column
  select(-region) |> 
  # Run distinct again to remove duplicates
  distinct(country_cepps, iso3n, .keep_all = TRUE) |> 
  # Arrange by country
  arrange(country_cepps)

######################### FRAGILE STATES INDEX ################################
# Find Excel files within web page
fsi_excel_links <- read_html("https://fragilestatesindex.org/excel/") |> 
  html_nodes(xpath = ".//a[contains(@href, '.xlsx')]") |> 
  html_attr("href") |> 
  # Remove white space from vector
  str_trim() |> 
  # Files are duplicated; return unique files
  unique()

# Run for loop that brings in the FSI data for the last 10 years
# Create empty list to store data frames from Excel download
fsi_list <- list()

# Set current year variable to 2023 as the files go in descending order
current_year <- 2023

# Run for loop to bring in the data
# NEED TO REWRITE THIS TOP SECTION TO BETTER AUTOMATE PROCESS
for (i in 1:(length(fsi_excel_links)-7)) {
  # Read in each Excel for the last ten years worth of data
  fsi_list[[i]] <- read.xlsx(fsi_excel_links[i], sheet = 1) |> 
    # Set as tibble
    as_tibble() |> 
    # Clean up column names
    clean_names() |> 
    # Alter year column as there are some issues in certain files and 
    # create iso3n column
    mutate(year = current_year,
           iso3n = countrycode(country, origin = "country.name", destination = "iso3n")) |> 
    # Filter out missing iso3n codes
    filter(!is.na(iso3n)) |> 
    # Left join with country_names df
    left_join(country_names, by = "iso3n")
  
  # Subtract 1 from year
  current_year <- current_year - 1
}

# Bind data frames in fsi_list
fsi <- bind_rows(fsi_list)

# Clean up environment
rm(country_names, fsi_excel_links, fsi_list, current_year)