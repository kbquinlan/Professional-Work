################################################################################
######################### IFES ELECTION GUIDE DATA #############################
################################################################################

# Load necessary libraries
library(tidyverse)
library(dtplyr)
#library(httr)
#library(jsonlite)
library(rvest)
library(vdemdata)
library(countrycode)
#library(fuzzyjoin)

# Set URLs to pull tables from
upcoming_url <- "https://www.electionguide.org/elections/type/upcoming/"
recent_url <- "https://www.electionguide.org/elections/type/past/"

# Scrape tables from URLs
upcoming_elections <- upcoming_url |> 
  read_html() |>
  html_table()

upcoming_elections_df <- upcoming_elections[[2]] |> 
  select(-1) |> 
  mutate(iso3c = countrycode(Country, "country.name", "iso3c"), 
         .after = Country) |> 
  mutate(Date = mdy(Date))

recent_elections <- recent_url |>
  read_html() |>
  html_table()

recent_elections_df <- recent_elections[[2]] |>
  select(-1) |>
  mutate(iso3c = countrycode(Country, "country.name", "iso3c"), 
         .after = Country) |>
  mutate(Date = mdy(Date))

# Combine rows of upcoming and recent elections dfs
all_elections <- bind_rows(upcoming_elections_df |> 
                             select(Country:Status), 
                           recent_elections_df |> 
                             select(Country:Date, Participation)) |> 
  mutate(Status = ifelse(is.na(Status), "Held", Status), 
         status_binary = ifelse(Status %in% c("Confirmed", "Date not confirmed", 
                                              "Tentative", "Postponed"), 
                                "Upcoming", 
                                "Held"),      
         `Election for` = ifelse(`Election for` == "Referendum", 
                                 paste(Country, `Election for`, sep = " "), 
                                 `Election for`), 
         Participation = parse_number(Participation)/100, 
         Year = year(Date)) |> 
  arrange(desc(Date), Country) |> 
  group_by(Country, Year) |> 
  mutate(status_all = toString(unique(Status)), 
         elections_fornames = toString(`Election for`), 
         election_dates = toString(unique(Date)), 
         participation_all = toString(scales::percent(Participation[Status == "Held"]))) |> 
  ungroup()

# Create V-Dem data frame of specific indicators: v2x_regime, v2x_polyarchy, v2xel_frefair, v2xps_party, v2x_partipdem
vdem_indicators <- vdemdata::vdem |> 
  filter(year >= min(all_elections$Year)) |> 
  select(iso3c = country_text_id, 
         region = e_regionpol_6C,
         year, 
         regime = v2x_regime, 
         edi = v2x_polyarchy, 
         cei = v2xel_frefair,
         pii = v2xps_party,
         pdi = v2x_partipdem) |> 
  mutate(regime = case_when(regime == 0 ~ "Closed Autocracy", 
                            regime == 1 ~ "Electoral Autocracy", 
                            regime == 2 ~ "Electoral Democracy",
                            regime == 3 ~ "Liberal Democracy"), 
         region = case_when(region == 1 ~ "Eurasia", 
                            region == 2 ~ "Latin America and the Caribbean", 
                            region == 3 ~ "Middle East and North Africa", 
                            region == 4 ~ "Africa", 
                            region == 5 ~ "Western Europe and North America", 
                            region == 6 ~ "Asia"))

vdem_indicators_2024 <- vdemdata::vdem |> 
  filter(year == max(year)) |> 
  select(iso3c = country_text_id, 
         region = e_regionpol_6C,
         year, 
         regime = v2x_regime, 
         edi = v2x_polyarchy, 
         cei = v2xel_frefair,
         pii = v2xps_party,
         pdi = v2x_partipdem) |> 
  mutate(year = 2024, 
         regime = case_when(regime == 0 ~ "Closed Autocracy", 
                            regime == 1 ~ "Electoral Autocracy", 
                            regime == 2 ~ "Electoral Democracy",
                            regime == 3 ~ "Liberal Democracy"), 
         region = case_when(region == 1 ~ "Eurasia", 
                            region == 2 ~ "Latin America and the Caribbean", 
                            region == 3 ~ "Middle East and North Africa", 
                            region == 4 ~ "Africa", 
                            region == 5 ~ "Western Europe and North America", 
                            region == 6 ~ "Asia"))

# Combine V-Dem data sets
vdem_combo <- bind_rows(vdem_indicators_2024, vdem_indicators)

# Merge vdem_combo with all_elections
elections_final <- all_elections |> 
  left_join(vdem_combo, 
            by = join_by(iso3c, Year == year))

# Clean up environment
rm(upcoming_url, recent_url, upcoming_elections, upcoming_elections_df, 
   recent_elections, recent_elections_df, all_elections, vdem_indicators,
   vdem_indicators_2024, vdem_combo)
