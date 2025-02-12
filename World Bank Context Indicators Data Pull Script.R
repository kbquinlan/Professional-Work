############################################################################
##################### WORLD BANK DATA PULL SCRIPT ##########################
############################################################################

###################### SET  UP ENVIRONMENT #################################
# Load libraries
# ALL OF THE FOLLOWING PACKAGES ARE REQUIRED FOR THIS SCRIPT WITH THE EXCEPTION
# OF ONES COMMENTED OUT
library(data.table)
library(dtplyr)
library(tidyverse)
#library(janitor)
#library(readxl)
#library(writexl)
library(countrycode)
#library(acled.api) # LOOK OUT FOR UPDATES TO THIS PACKAGE AS IT IS A DEVELOPMENT PACKAGE
#library(vdemdata) # LOOK TO UPDATE THIS EVERY YEAR AS NEW DATA IS RELEASED; DEVELOPMENT PACKAGE AS WELL
library(WDI) # Package for pulling in World Bank data
#library(rvest) # For web scraping data
#library(openxlsx) # For reading in Excel files from web pages

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


# Set first year
first_year <- 2013

########################## WORLD BANK DATA PULL ###############################

# Using the WDI package, pull in the World Bank data for the following variables:
# GINI Index, GNI, vulnerable employment, total urban population, total rural population, 
# percent youth population
wb_data <- WDI(country = "all", indicator = c("NY.GNP.MKTP.CD", "NY.GDP.PCAP.CD", 
                                              "SL.EMP.VULN.ZS", 
                                              "SP.URB.TOTL", "SP.RUR.TOTL",
                                              "SI.POV.GINI", "SP.POP.1014.FE",
                                              "SP.POP.1014.MA", "SP.POP.1519.FE",
                                              "SP.POP.1519.MA", "SP.POP.2024.FE",
                                              "SP.POP.2024.MA", "SP.POP.2529.FE",
                                              "SP.POP.2529.MA", "SP.POP.TOTL"),
               #"SP.POP.TOTL.FE.IN", 
               # "SP.POP.TOTL.MA.IN", "SP.POP.1519.FE.5Y", 
               # "SP.POP.1519.MA.5Y", "SP.POP.2024.FE.5Y",
               # "SP.POP.2024.MA.5Y"), 
               start = first_year, end = first_year + 10, extra = TRUE) |> 
  # Set as tibble
  as_tibble() |> 
  # Rename columns
  rename(gni = NY.GNP.MKTP.CD, gdp_per_cap = NY.GDP.PCAP.CD, 
         vuln_employ = SL.EMP.VULN.ZS, urban_pop = SP.URB.TOTL, 
         rural_pop = SP.RUR.TOTL, gini = SI.POV.GINI, 
         female_1014_pop = SP.POP.1014.FE, male_1014_pop = SP.POP.1014.MA,
         female_1519_pop = SP.POP.1519.FE, male_1519_pop = SP.POP.1519.MA, 
         female_2024_pop = SP.POP.2024.FE, male_2024_pop = SP.POP.2024.MA,
         female_2529_pop = SP.POP.2529.FE, 
         male_2529_pop = SP.POP.2529.MA, total_pop = SP.POP.TOTL) |> 
  # female_pop = SP.POP.TOTL.FE.IN, 
  # male_pop = SP.POP.TOTL.MA.IN, percent_female_1519 = SP.POP.1519.FE.5Y, 
  # percent_male_1519 = SP.POP.1519.MA.5Y, percent_female_2024 = SP.POP.2024.FE.5Y, 
  # percent_male_2024 = SP.POP.2024.MA.5Y) |> 
  # Find populations for youth and set date for data pull
  # mutate(youth_female = round((female_pop*(percent_female_1519/100)) + (female_pop*(percent_female_2024/100)), digits = 0), 
  #        youth_male = round((male_pop*(percent_male_1519/100)) + (male_pop*(percent_male_2024/100)), digits = 0), 
  #        total_youth = youth_female + youth_male, 
  #        iso3n = countrycode(country, "country.name", "iso3n"),
  #        time_stamp = Sys.Date()) |> 
  mutate(total_youth = female_1014_pop + male_1014_pop + female_1519_pop + 
           male_1519_pop + female_2024_pop + male_2024_pop + 
           female_2529_pop + male_2529_pop, 
         iso3n = countrycode(country, "country.name", "iso3n"),
         time_stamp = Sys.Date()) |> 
  # Filter out NAs from iso3n column
  filter(!is.na(iso3n)) |> 
  # Left join with country_names
  left_join(country_names, by = "iso3n")

# Due to duplication occurring during the data pull, 
# data is separated into two equal data frames based on when
# individual data points were last internally updated by World Bank
# First data set
wb_data1 <- filter(wb_data, lastupdated == min(lastupdated)) |> 
  # Select specific variables
  select(country:gini, total_pop:lending, iso3n:region_cepps)

# Second data set
wb_data2 <- filter(wb_data, lastupdated == max(lastupdated)) |> 
  # Select specific variables
  select(iso3n, year, female_1014_pop:male_2529_pop, total_youth)

# Create final merged wb_data_final data set
wb_data_final <- inner_join(wb_data1, wb_data2, 
                            # Join on iso3n and year
                            by = c("iso3n", "year")) |> 
  # Remove status and lastupdated columns
  select(-c(status, lastupdated)) |> 
  # Arrange by country and year for organizational purposes
  arrange(country, year)

# Clean up environment
rm(country_names, first_year, wb_data, wb_data1, wb_data2)