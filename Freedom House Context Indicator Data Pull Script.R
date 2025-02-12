##############################################################
############### Freedom House Data Pull ######################
#############################################################

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

######################### FREEDOM HOUSE DATA PULL #############################
# Find the most recent link to the Freedom House data
fh_link <- "https://freedomhouse.org/reports/publication-archives"

# Find Excel files 
fh_links <- read_html(fh_link) |> 
  html_nodes(xpath = ".//a[contains(@href, '.xlsx')]") |> 
  html_attr("href")

# Pull in the Freedom House data for the latest update; should be first file from
# the fh_links list (already for 10 years worth of data)
fh <- read.xlsx(fh_links[1], sheet = 2, startRow = 2) |> 
  # Set as tibble
  as_tibble() |> 
  # Clean up column names
  clean_names() |> 
  # Set data pull timestamp and creat iso3n column for joining
  mutate(time_stamp = Sys.Date(), 
         iso3n = countrycode(country_territory, origin = "country.name",
                             destination = "iso3n")) |>
  # Filter out missing iso3n codes
  filter(!is.na(iso3n)) |>
  # Left join with country names table
  left_join(country_names, by = "iso3n")

# Clean up environment 
rm(country_names, fh_link, fh_links)