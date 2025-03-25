# Wrangling 

# Packages that are needed
library(tidyverse)
library(stringdist)
library(stringi)
library(httr)
library(jsonlite)
library(purrr)
library(furrr)
library(progressr)

# Load in EM-DAT dataset
geo_hazard <- readxl::read_excel("EM-DAT data.xlsx")

# EU27 Countries
countrycodes = c("BEL", "BGR", "CZE", "DNK", "DEU", "EST", "IRL", "GRC", "ESP", "FRA", "HRV", 
                 "ITA", "CYP", "LVA", "LTU", "LUX", "HUN", "MLT", "NLD", "AUT", "POL", "PRT", 
                 "ROU", "SVN", "SVK", "FIN", "SWE")

BNL = c("BEL","NLD","LUX")

# Filter out countries that are not in the EU27 and deselect columns we don't need
fil_geo = geo_hazard %>% 
  filter(ISO %in% countrycodes) %>% # Filter for EU27 Countries
  select(-Historic,-`Classification Key`,-`Disaster Group`,-`Disaster Subtype`,-`External IDs`,-`Event Name`,-Subregion,-Region,-Origin,-`Associated Types`,-`OFDA/BHA Response`,-Appeal,-Declaration,-`AID Contribution ('000 US$)`,-Latitude,-Longitude,-`River Basin`,-CPI,-`Admin Units`,-`Entry Date`,-`Last Update`) %>% # Take out columns that are not needed
  filter(!is.na(Location)) %>%  # filter out entries with no location information
  filter(!is.na(value)) # filter out entries with no 

# Make an empty cache
cache <- tibble(location = character(), latitude = numeric(), longitude = numeric())

# Function to geocode location with cache check
geocode_location <- function(location, cache) {
  if (location %in% cache$location) {
    return(cache %>% filter(location == !!location) %>% select(latitude, longitude))
  }
  base_url <- "https://nominatim.openstreetmap.org/search"
  response <- GET(base_url, query = list(q = location, format = "json", limit = 1))
  data <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON()
  if (length(data) == 0) {
    return(tibble(latitude = NA, longitude = NA))
  }
  result <- tibble(
    location = location,
    latitude = as.numeric(data$lat[1]),
    longitude = as.numeric(data$lon[1])
  )
  return(result)
}

# Pre-process the location vector 
unique_locations <- fil_geo %>%
  pull(Location) %>%
  str_replace_all(",\\s*", ", ") %>%                # Ensure consistent spacing after commas
  str_remove_all("\\sprovinces?\\b") %>%            # Remove 'province' or 'provinces' (handles both singular & plural)
  str_remove_all("\\scity\\b") %>%                  # Remove 'city'
  str_remove_all("\\stown\\b") %>%                  # Remove 'town'
  str_remove_all("\\s*\\([^)]*\\)") %>%             # ✅ Remove contents within parentheses (with parentheses)
  str_split(",\\s*(?=[^)]*$)") %>%                  # Split locations at commas NOT inside parentheses
  unlist() %>%                                       # Flatten into a vector
  str_trim() %>%                                     # Remove any trailing spaces
  unique()                                           # Ensure unique entries only

# Pre-cache geocoded locations to avoid repeat calls
start_time <- Sys.time()

progressr::with_progress({
  p <- progressr::progressor(steps = length(unique_locations))  # Progress bar setup
  cache <- future_map_dfr(unique_locations, function(loc) {
    p()  # Increment progress bar
    geocode_location(loc, cache)
  })
})
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)

# Pre-process the 'Location' column in fil_geo
fil_geo_clean <- fil_geo %>%
  mutate(row_id = row_number()) %>%
  mutate(location = str_replace_all(Location, ",\\s*", ", ") %>%
           str_remove_all("\\sprovinces") %>%
           str_replace_all("\\(([^)]+)\\)", " \\1")) %>%
  separate_rows(location, sep = ",\\s*(?=[^)]*$)") %>%  
  mutate(location = str_trim(location))                 

df = cbind.data.frame(unique_locations,cache)
colnames(df)[1] = "location"

# Merge cleaned data with cached coordinates
fil_geo_clean <- fil_geo_clean %>%
  left_join(df, by = "location")

# Identify rows that still need geocoding
locations_to_geocode <- fil_geo_clean %>%
  filter(is.na(latitude) | is.na(longitude)) %>%
  pull(location)

# Geocode only the missing locations
if (length(locations_to_geocode) > 0) {
  new_geocodes <- future_map_dfr(locations_to_geocode, ~ geocode_location(.x, cache))
  cache <- bind_rows(cache, new_geocodes) %>%
    distinct(location, .keep_all = TRUE)
  fil_geo_clean <- fil_geo_clean %>%
    rows_update(new_geocodes, by = "location")
}

# Collapse back to original structure
final_fil_geo <- fil_geo_clean %>%
  group_by(row_id) %>%
  summarise(
    Location = paste(unique(location), collapse = ", "),
    latitude = mean(latitude, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-row_id)

