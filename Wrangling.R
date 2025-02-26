# EM-DAT Wrangling

library(tidyverse)
library(stringdist)
library(stringi)

# First try to identify which NUTS region it is.
# Eurostat NUTS table: https://ec.europa.eu/eurostat/documents/345175/629341/NUTS2021-NUTS2024.xlsx/2b35915f-9c14-6841-8197-353408c4522d?t=1717505289640

# EU27 Version NUTS2 ####
geo_hazard <- readxl::read_excel("EM-DAT data.xlsx")
nuts2024 <- readxl::read_excel("NUTS2024EU27.xlsx")
ccnames <- readxl::read_excel("CCNamesEU27.xlsx")

# Take the nuts2024 dataset and add country names and ISO codes
countries = nuts2024 %>% full_join(ccnames, by = join_by(`Country code` == `EU Code`))
countrycodes = unique(countries$`ISO Code`)

# Filter out countries that are not in the EU27 and deselect columns we don't need
fil_geo = geo_hazard %>% filter(ISO %in% countrycodes) %>% select(-Historic,-`Classification Key`,-`Disaster Group`,-`Disaster Subtype`,-`External IDs`,-`Event Name`,-Subregion,-Region,-Origin,-`Associated Types`,-`OFDA/BHA Response`,-Appeal,-Declaration,-`AID Contribution ('000 US$)`,-Latitude,-Longitude,-`River Basin`,-CPI,-`Admin Units`,-`Entry Date`,-`Last Update`) %>% filter(!is.na(Location))

# Pre-processing: transform all letters into regular latin alphabets and to make them lower.
location_data = fil_geo %>% 
  mutate(location = Location %>% stri_trans_general("Latin-ASCII") %>% str_to_lower())
nuts_data <- countries %>% 
  mutate(nuts_region = `NUTS label` %>% stri_trans_general("Latin-ASCII") %>% str_to_lower())

location_data <- location_data %>% 
  mutate(location = stri_trans_general(location, "Latin-ASCII") %>% str_to_lower())

nuts_data <- nuts_data %>% 
  mutate(nuts_region = stri_trans_general(nuts_region, "Latin-ASCII") %>% str_to_lower())

# Function for fuzzy matching within ISO country code
match_nuts <- function(location, country_iso, nuts_df, threshold = 0.3) {
  filtered_nuts <- nuts_df %>%
    filter(`ISO Code` == country_iso)  # Match only within the same ISO
  
  # Extract relevant columns
  nuts_list <- filtered_nuts$nuts_region  
  nuts_codes <- filtered_nuts$`NUTS Code`  # Extract NUTS codes
  nuts_levels <- filtered_nuts$`NUTS level`  # Extract corresponding NUTS levels
  
  # If no matching ISO in NUTS dataset, return "ERROR"
  if (length(nuts_list) == 0) {
    return(tibble(nuts_match = "ERROR", `NUTS level` = "ERROR", `NUTS Code` = "ERROR"))
  }
  
  # Compute string distances
  distances <- map_dbl(nuts_list, ~stringdist(location, .x, method = "jw"))  # Jaro-Winkler distance
  
  # If distances is empty, return "ERROR"
  if (length(distances) == 0) {
    return(tibble(nuts_match = "ERROR", `NUTS level` = "ERROR", `NUTS Code` = "ERROR"))
  }
  
  best_match_index <- which.min(distances)  # Find closest match
  
  # If the best match is too different, return "ERROR"
  if (distances[best_match_index] > threshold) {
    return(tibble(nuts_match = "ERROR", `NUTS level` = "ERROR", `NUTS Code` = "ERROR"))
  } else {
    return(tibble(nuts_match = nuts_list[best_match_index], 
                  `NUTS level` = as.character(nuts_levels[best_match_index]),
                  `NUTS Code` = nuts_codes[best_match_index]))
  }
}

# Applying the function
expanded_location_data <- location_data %>%
  mutate(row_id = row_number()) %>%  # Create a unique identifier before expanding
  mutate(location = str_replace_all(location, "\\(([^)]*),([^)]*)\\)", "(\\1 \\2)"),  # Remove commas within parentheses
         location = str_replace_all(location, "\\((.*?)\\)", " \\1"),  # Remove parentheses but keep text
         location = str_replace_all(location, ",(\\S)", ", \\1"))  %>% # Ensure space after commas 
  separate_rows(location, sep = ", ") %>%  # Split locations into separate rows
  mutate(match_result = map2(location, ISO, ~match_nuts(.x, .y, nuts_data))) %>%  # Match only within the same ISO
  unnest(match_result) %>%  # Unnest the tibble results properly
  arrange(row_id) %>%  # Ensure correct ordering
  select(-row_id)  # Remove temporary identifier


# Geo-location identification ####
library(httr)
library(jsonlite)

# Function to get latitude & longitude from location
geocode_location <- function(location, cache = NULL) {
  # Check cache first
  if (!is.null(cache) && location %in% cache$location) {
    return(cache %>% filter(location == !!location) %>% select(latitude, longitude))
  }
  
  # API call
  base_url <- "https://nominatim.openstreetmap.org/search"
  response <- GET(base_url, query = list(q = location, format = "json", limit = 1))
  data <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON()
  
  # Handle missing results
  if (length(data) == 0) {
    return(tibble(latitude = NA, longitude = NA))
  }
  
  # Extract and return result
  result <- tibble(location = location, 
                   latitude = as.numeric(data$lat[1]), 
                   longitude = as.numeric(data$lon[1]))
  
  # Slow down requests to avoid API blocking
  Sys.sleep(1)
  
  return(result)
}

# Create an empty cache
cache <- tibble(location = character(), latitude = numeric(), longitude = numeric())

# Apply the function
expanded_location_data <- location_data %>%
  mutate(row_id = row_number()) %>%  # Create a unique identifier before expanding
  mutate(location = str_replace_all(location, "\\(([^)]*),([^)]*)\\)", "(\\1 \\2)"),  # Remove commas within parentheses
         location = str_replace_all(location, "\\((.*?)\\)", " \\1"),  # Remove parentheses but keep text
         location = str_replace_all(location, ",(\\S)", ", \\1"))  %>% # Ensure space after commas 
  separate_rows(location, sep = ", ") %>%  # Split locations into separate rows
  mutate(geo_data = map(location, ~geocode_location(.x, cache))) %>%  # Geocode
  mutate(geo_data = map(geo_data, ~rename_with(.x, ~paste0("geo_", .)))) %>%  # Rename columns before unnesting
  unnest(geo_data) %>%  # Unnest the tibble results properly
  arrange(row_id) %>%  # Ensure correct ordering
  select(-row_id)  # Remove temporary identifier

library(sf)
nuts3_polygons <- st_read("NUTS3.geojson") %>% filter(LEVL_CODE == 3)

# Bulk Matching
expanded_location_data = expanded_location_data %>% filter(!is.na(geo_latitude))
# Convert geocoded locations into spatial points
location_points <- st_as_sf(expanded_location_data, coords = c("geo_longitude", "geo_latitude"), crs = 4326)
# Perform spatial join to find NUTS3 region for each coordinate
matched_data <- st_join(location_points, nuts3_polygons, join = st_within)
# View matched results
matched_data %>% select(location, geometry, NUTS_ID, NUTS_NAME)

matched_data %>% summarise(na_count = sum(is.na(NUTS_ID))) #NA count of 178






# Experimental ####
geo_hazard <- readxl::read_excel("EM-DAT data.xlsx")
nuts2024 <- read_csv("NUTS2024.csv")
ccnames <- readxl::read_excel("Country_Codes_and_Names.xlsx")

setdiff(unique(nuts2024$`Country code`),ccnames$`EU Code`)
setdiff(ccnames$`EU Code`,unique(nuts2024$`Country code`))

unique(nuts2024$`Country code`)

# Filter out countries that are not part of Eurostat Regional Data
euro_hazard = geo_hazard %>% 



colnames(geo_hazard) = tolower(colnames(geo_hazard))
colnames(nuts2024) = tolower(colnames(nuts2024))

# Filter out countries that are 

# Create a function to determine the NUTS level

get_nuts_level = function(location,nuts_data) {
  
  match = nuts_data %>% filter(str_detect(tolower(nuts.label),tolower(location)))
  if (nrow(match) > 0) {
    return(paste(unique(match$nuts.level),collapse = ", "))
  } else {
    return(NA)
  }
}

geo_hazard = geo_hazard %>% 
  mutate(nuts_level = sapply(location, get_nuts_level,nuts_data = nuts2024))


# This doesn't work...
# Will probably have to first filter out the unnecessary countries - Done
# And then identify if it is nuts 2 or nuts 3 - Done
# 


# From there then we can decide what's the best... 
# Oooh! and use the eurostat package to source all of the necessary data (should probably identify which tables I want first as well...)
