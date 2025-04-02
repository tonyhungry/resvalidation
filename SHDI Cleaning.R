# HDI Data Cleaning for BNL Countries Only
# Downloaded from Global Data Lab

library(tidyverse)
library(stringr)
library(stringi)
library(fuzzyjoin)
# Tidy up what I've managed to download...

countrycodes = c("BEL", "BGR", "CZE", "DNK", "DEU", "EST", "IRL", "GRC", "ESP", "FRA", "HRV", 
                  "ITA", "CYP", "LVA", "LTU", "LUX", "HUN", "MLT", "NLD", "AUT", "POL", "PRT", 
                  "ROU", "SVN", "SVK", "FIN", "SWE")

# Take the nuts2024 dataset and add country names and ISO codes
ccnames <- readxl::read_excel("CCNamesEU27.xlsx")
nuts2024 <- readxl::read_excel("NUTS2024EU27.xlsx")
nuts2024 = nuts2024 %>% full_join(ccnames, by = join_by(`Country code` == `EU Code`))

nuts2024 = nuts2024 %>% 
  mutate(Region = stri_trans_general(`NUTS label`, "Latin-ASCII")) %>% 
  rename(NUTS_ID = `NUTS Code`) %>% 
  filter(`NUTS level` == 2) %>% 
  rename(ISO_Code = `ISO Code`) %>% 
  filter(!str_ends(NUTS_ID, "ZZ"))

shdi = read.csv("~/Documents/Resilience Validation/GDL-Subnational-HDI-data.csv")

shdi = shdi %>% filter(ISO_Code %in% countrycodes) %>% 
  rename(shdi_score = `X2022`)

exact_matches <- inner_join(shdi, nuts2024, by = c("ISO_Code", "Region"))
shdi_unmatched <- anti_join(shdi, exact_matches, by = c("ISO_Code", "Region"))

shdi_unmatched <- shdi_unmatched %>%
  mutate(Region_clean = stri_trans_general(tolower(Region), "Latin-ASCII"))

nuts2024 <- nuts2024 %>%
  mutate(Region_clean = stri_trans_general(tolower(Region), "Latin-ASCII"))

fuzzy_match <- stringdist_inner_join(
  shdi_unmatched,
  nuts2024,
  by = c("ISO_Code", "Region_clean"),
  method = "jw",
  max_dist = 0.1,
  distance_col = ".dist"
)

best_matches <- fuzzy_match %>%
  group_by(ISO_Code.x, Region.x) %>%
  slice_min(order_by = .dist, n = 1) %>%
  ungroup()

combined_matches <- bind_rows(
  exact_matches %>% select(NUTS_ID, shdi_score),  
  best_matches %>% select(NUTS_ID, shdi_score)    
)

nuts2024_with_shdi <- nuts2024 %>%
  left_join(
    combined_matches %>% select(NUTS_ID, shdi_score), 
    by = "NUTS_ID"
  ) %>% 
  select(`Country code`,NUTS_ID,`NUTS label`,`NUTS level`,ISO_Code,Region,shdi_score) %>% 
  filter(ISO_Code %in% c("BEL","NLD","LUX"))

rowsinput <- tibble(
  NUTS_ID = c("LU00", "NL12", "BE10","NL42"),
  shdi_score = c(0.927,0.923,0.960,0.936)
)

nuts2024_with_shdi <- nuts2024_with_shdi %>%
  rows_update(rowsinput, by = "NUTS_ID") %>% select(NUTS_ID,shdi_score)

write_csv(nuts2024_with_shdi, "benelux_hdi_2022.csv")










# Trying the gdldata package ####
# library(gdldata)


# This just doesn't work...
# session <- gdl_session(Sys.getenv('wFg1lDC0v5-60klXWw77wt0f8arOU3TjW0v0Q4QWY3I'))
# gdlindicators <- gdl_indicators(session)
# 
# countrycodes = c("BEL", "BGR", "CZE", "DNK", "DEU", "EST", "IRL", "GRC", "ESP", "FRA", "HRV", 
#                  "ITA", "CYP", "LVA", "LTU", "LUX", "HUN", "MLT", "NLD", "AUT", "POL", "PRT", 
#                  "ROU", "SVN", "SVK", "FIN", "SWE")
# 
# session <- session %>%
#   set_dataset('shdi') %>%
#   set_countries(c("BEL", "BGR", "CZE", "DNK", "DEU", "EST", "IRL", "GRC", "ESP", "FRA", "HRV", 
#                   "ITA", "CYP", "LVA", "LTU", "LUX", "HUN", "MLT", "NLD", "AUT", "POL", "PRT", 
#                   "ROU", "SVN", "SVK", "FIN", "SWE"))
# 
# shdi <- gdl_request(session)