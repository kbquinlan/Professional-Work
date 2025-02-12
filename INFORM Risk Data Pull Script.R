######################################################################
#################### INFORM Risk Data Pull ###########################
######################################################################

# Load libraries
library(data.table)
library(tidyverse)
library(dtplyr)
library(janitor)
library(openxlsx)
library(rvest)
library(countrycode)

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

######################## INFORM Risk Index Data ########################
# Set up link
inform_link <- "https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Risk/Results-and-data/moduleId/1782/id/469/controller/Admin/action/Results"

# Find Excel files
inform_files <- read_html(inform_link) |> 
  html_nodes(xpath = ".//a[contains(@href, '.xlsx')]") |> 
  html_attr("href") |> 
  unique()

# Find column names
# inform_colnames <- read.xlsx(paste0("https://drmkc.jrc.ec.europa.eu", inform_files[1]), sheet = 2, startRow = 2) |> 
#   clean_names() |> 
#   colnames()

# Pull in data and fix column names following extraction
inform <- read.xlsx(paste0("https://drmkc.jrc.ec.europa.eu", inform_files[2])) |> 
  clean_names() |> 
  # Select specific variables
  filter(indicator_name %in% c("INFORM Risk Index", "Hazard & Exposure Index", 
                               "Natural Hazard", "Physical exposure to earthquakes", 
                               "Physical exposure to river floods", "Physical exposure to tsunamis", 
                               "Physical exposure to tropical cyclones", "Physical exposure to coastal flood", 
                               "Droughts probability and historical impact", "Hazard & Exposure Index - Epidemic", 
                               "Human Hazard", "Violent Conflict probability Score", 
                               "Current conflicts", "Vulnerability Index", 
                               "Socio-Economic Vulnerability", "Poverty & Development", 
                               "Inequality", "Economic Dependency", "Vulnerable Groups", 
                               "Uprooted people", "Health Conditions", 
                               "Health of children under-five", "Recent shocks", 
                               "Food Security", "Others Vulnerable Groups", 
                               "Lack of Coping Capacity Index", "Institutional", 
                               "Disaster Risk Reduction", "Governance", 
                               "Infrastructure", "Communication", "Physical Infrastructure", 
                               "Access to Health Care")) |> 
  # Narrow down columns
  select(iso3, inform_year, indicator_name, indicator_score) |> 
  # Pivot data into wider format
  pivot_wider(names_from = indicator_name, values_from = indicator_score) |> 
  # Clean up column names
  clean_names()

# Change column names
# colnames(inform_current) <- inform_colnames

# Join with country_names dataset
inform_final <- inform |> 
  left_join(country_names, by = c("iso3" = "iso3c"))

# Clean up environment
rm(country_names, inform_link, inform_files, 
   inform)