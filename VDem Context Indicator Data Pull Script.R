#######################################################################
################ V-Dem Context Indicator Data Pull ####################
#######################################################################

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
# devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata) # LOOK TO UPDATE THIS EVERY YEAR AS NEW DATA IS RELEASED; DEVELOPMENT PACKAGE AS WELL
#library(WDI) # Package for pulling in World Bank data
#library(rvest) # For web scraping data
#library(openxlsx) # For reading in Excel files from web pages

#################### VDEM DATA LOAD ###################################

# Set up first year for data load; set to ten years before the max year of Vdem data
first_year <- max(vdemdata::vdem$year) - 10

# From the vdemata package, load the VDem data with the variables: 
# Accountability Index, Clean Elections Index, Rule of Law Index, Core Civil Society Index, 
# Effective state control of territory, diagonal, horizontal, and vertical accountability indices
vdem <- vdemdata::vdem |>
  # Set as tibble
  as_tibble() |> 
  # Filter to the ten years prior variable
  filter(year >= first_year) |>
  # Use iso3n code conversion on country name to eliminate non-country territories
  mutate(iso3n = countrycode(country_name, "country.name", "iso3n")) |> 
  # Filter out NAs under iso3n column
  filter(!is.na(iso3n)) |> 
  # Select specific variables of interest as mentioned above
  select(country = country_name, iso3c = country_text_id, region = e_regionpol_6C, 
         year, regime = v2x_regime, account_index = v2x_accountability_osp, 
         vertaccount_index = v2x_veracc_osp, diagaccount_index = v2x_diagacc_osp, 
         horizaccount_index = v2x_horacc_osp, ruleoflaw_index = v2x_rule, 
         corecs_index = v2xcs_ccsi, partipcomp_index = v2x_partip, state_territory = v2svstterr, 
         cleanelec_index = v2xel_frefair, women_suffrage = v2fsuffrage, pp_bar = v2psbars_ord, 
         free_exp_altinf = v2x_freexp_altinf, pol_party_inst = v2xps_party) |> 
  # Create regime categorical variable, rename some countries, change region
  # codes to text, add categorical variable for barriers for political parties, 
  # freedom of expression grade, and create time_stamp for data load
  mutate(regime_cat = case_when(regime == 0 ~ "Closed Autocracy",
                                regime == 1 ~ "Electoral Autocracy",
                                regime == 2 ~ "Electoral Democracy",
                                regime == 3 ~ "Liberal Democracy"), 
         country = case_when(str_detect(country, "Palestine") ~ "West Bank/Gaza", 
                             str_detect(country, "Burma") ~ "Burma", 
                             country == "Kyrgyzstan" ~ "Kyrgyz Republic",
                             TRUE ~ country),
         region = case_when(region == 1 ~ "Eurasia",
                            region == 2 ~ "Latin America and the Caribbean",
                            region == 3 ~ "Middle East and North Africa",
                            region == 4 ~ "Africa",
                            region == 5 ~ "Western Europe and North America",
                            region == 6 ~ "Asia"), 
         pp_bar_cat = case_when(pp_bar == 0 ~ "Parties are not allowed", 
                                pp_bar == 1 ~ "It is impossible, or virtually impossible, for parties to form (legally)",
                                pp_bar == 2 ~ "There are significant obstacles",
                                pp_bar == 3 ~ "There are modest barriers", 
                                pp_bar == 4 ~ "There are no substantial barriers"),
         free_exp_grade = case_when(free_exp_altinf < 0.2 ~ "F", 
                                    free_exp_altinf >= 0.2 & free_exp_altinf < 0.4 ~ "D", 
                                    free_exp_altinf >= 0.4 & free_exp_altinf < 0.6 ~ "C", 
                                    free_exp_altinf >= 0.6 & free_exp_altinf < 0.8 ~ "B", 
                                    free_exp_altinf >= 0.8 ~ "A"),
         time_stamp = Sys.Date())

# Clean up environment
rm(first_year)