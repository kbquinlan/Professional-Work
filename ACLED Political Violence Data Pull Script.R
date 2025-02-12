##########################################################################
################# ACLED Political Violence Data Pull #####################
##########################################################################
# Load libraries
library(data.table)
library(dtplyr)
library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(countrycode)
library(acled.api) # LOOK OUT FOR UPDATES TO THIS PACKAGE AS IT IS A DEVELOPMENT PACKAGE
#library(vdemdata) # LOOK TO UPDATE THIS EVERY YEAR AS NEW DATA IS RELEASED; DEVELOPMENT PACKAGE AS WELL

############## CREATE DATAFRAME OF STANDARDIZED COUNTRY NAMES ################
# Use the vdem package to create a dataframe of standardized country names
# This will be used to standardize country names in the ACLED data
country_names <- vdemdata::vdem |> 
  # Filter to the most recent year
  filter(year == max(year)) |>
  # Select country_name, country_text_id, an region
  distinct(country_cepps = country_name, iso3c = country_text_id, 
           region = e_regionpol_6C) |> 
  # Addin iso3n codes, change some country names, and make region codes names
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


######################## ACLED DATA PULL ##############################

# Create a variable that has the date range for the last full 13 months
# Use for pulling in the data from the ACLED API
# This will automatically update the data pull date ranges when the script is run
start_date <- floor_date(min(seq.Date(from = Sys.Date() - 395, to = Sys.Date(), by = "day")), "month")
end_date <- floor_date(max(seq.Date(from = Sys.Date() - 395, to = Sys.Date(), by = "day")), "month") - 1

# Using the acled.api package, pull the data for the last 13 months using start_date and end_date
# WARNING: This will pull in a large amount of data and may take a long time to run
# First test of data pull was 349,013 rows; a lot conflict, a lot of data!
acled <- acled.api(email.address = "{email}", 
                   access.key = "{passcode}", 
                   start.date = start_date, 
                   end.date = end_date,
                   all.variables = TRUE) |> 
  # Set as tibble
  as_tibble()

# Set time_stamp for data pull and join with country_names df
acled <- acled |> 
  mutate(time_stamp = Sys.Date()) |> 
  left_join(country_names, by = c("iso" = "iso3n"))

# Create dataset specifically for Non-State Armed Groups from ACLED
# Use inter codes 2, 3, 4
#acled_nsag <- filter(acled, inter1 %in% c(2, 3, 4) | 
#inter2 %in% c(2, 3, 4))
# Save the data to an RDS file
# saveRDS(acled, "~/Desktop/acled_2023_202401.rds")
# 
# # Write the data to an csv file as well
# write_csv(acled, "~/Desktop/acled_2023_202401.csv")

#################### VDEM DATA LOAD ###################################

# Set up first year for data load; set to ten years before the max year of Vdem data
#first_year <- max(vdemdata::vdem$year) - 10

# From the vdemata package, load the VDem data with the variables: 
# Accountability Index, Clean Elections Index, Rule of Law Index, Core Civil Society Index, 
# Effective state control of territory, diagonal, horizontal, and vertical accountability indices
#vdem <- vdemdata::vdem |>
# Set as tibble
#as_tibble() |> 
# Filter to the ten years prior variable
#filter(year >= first_year) |>
# Select specific variables of interest as mentioned above
#select(country = country_name, iso3c = country_text_id, region = e_regionpol_6C, 
#year, regime = v2x_regime, account_index = v2x_accountability_osp, 
#vertaccount_index = v2x_veracc_osp, diagaccount_index = v2x_diagacc_osp, 
#horizaccount_index = v2x_horacc_osp, ruleoflaw_index = v2x_rule, 
#corecs_index = v2xcs_ccsi, partipcomp_index = v2x_partip, state_territory = v2svstterr, 
#cleanelec_index = v2xel_frefair, women_suffrage = v2fsuffrage) |> 
# Create regime categorical variable, rename some countries, and change region
# codes to text
#mutate(regime_cat = case_when(regime == 0 ~ "Closed Autocracy",
#regime == 1 ~ "Electoral Autocracy",
#regime == 2 ~ "Electoral Democracy",
#regime == 3 ~ "Liberal Democracy"), 
#country = case_when(str_detect(country, "Palestine") ~ "West Bank/Gaza", 
#str_detect(country, "Burma") ~ "Burma", 
#country == "Kyrgyzstan" ~ "Kyrgyz Republic",
#TRUE ~ country), 
#region = case_when(region == 1 ~ "Eurasia", 
#region == 2 ~ "Latin America and the Caribbean", 
#region == 3 ~ "Middle East and North Africa",
#region == 4 ~ "Africa", 
#region == 5 ~ "Western Europe and North America", 
#region == 6 ~ "Asia"))
# Clean up environment
rm(country_names, start_date, end_date)
