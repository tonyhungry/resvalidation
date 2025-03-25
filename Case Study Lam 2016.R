# Case Study using RIM (Lam 2015/2016) for Benelux countries 
# Adapted the 2015 study with the data that I have. 

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

# Benelux countries (Belgium, Netherlands, Luxembourg)
BNL = c("BEL","NLD","LUX")

# Filter out countries that are not in the EU27 and deselect columns we don't need
fil_geo = geo_hazard %>% 
  filter(ISO %in% BNL) %>% # Filter for BNL countries
  select(-Historic,-`Classification Key`,-`Disaster Group`,-`Disaster Subtype`,-`External IDs`,-`Event Name`,-Subregion,-Region,-Origin,-`Associated Types`,-`OFDA/BHA Response`,-Appeal,-Declaration,-`AID Contribution ('000 US$)`,-Latitude,-Longitude,-`River Basin`,-CPI,-`Admin Units`,-`Entry Date`,-`Last Update`) %>% # Take out columns that are not needed
  filter(!is.na(Location))  # filter out entries with no location information

# fil_geo = fil_geo %>% filter(!is.na(?)) # filter out entries for specific columns

# Dealing with the location column ####

# Function to geocode location with cache check
geocode_location <- function(location, cache) {
  tryCatch({
    if (location %in% cache$location) {
      return(tibble(
        location = location, 
        latitude = cache$latitude[cache$location == location],
        longitude = cache$longitude[cache$location == location]
      ))
    }
    
    base_url <- "https://nominatim.openstreetmap.org/search"
    response <- GET(base_url, query = list(q = location, format = "json", limit = 1))
    data <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON()
    
    if (length(data) == 0) {
      return(tibble(location = location, latitude = NA, longitude = NA))
    }
    
    result <- tibble(
      location = location,
      latitude = as.numeric(data$lat[1]),
      longitude = as.numeric(data$lon[1])
    )
    
    return(result)
  },
  error = function(e) {
    message(glue::glue("Error geocoding: {location} - {e$message}"))
    return(tibble(location = location, latitude = NA, longitude = NA))
  })
}

# Pre-process the location vector 
unique_locations <- fil_geo %>%
  pull(Location) %>%
  str_replace_all(",\\s*", ", ") %>%                # Ensure consistent spacing after commas
  str_replace_all(";/", ", ") %>%                   # handle separators like ";" and "/"
  str_replace_all(";", ", ") %>%                    # Split entries on ";"
  str_replace_all("/", ", ") %>%                    # Split entries on "/"
  str_replace_all("Zuit-holland", "Zuid-Holland") %>%
  str_replace_all("South Holland", "Zuid-Holland") %>%
  str_replace_all("Hal", "Halle BE") %>%
  str_remove_all("\\sprovinces?\\b") %>%            # Remove 'province' or 'provinces'
  str_remove_all("\\scities\\b") %>%                # Remove 'cities'
  str_remove_all("\\scity\\b") %>%                  # Remove 'city'
  str_remove_all("\\stown\\b") %>%                  # Remove 'town'
  str_remove_all("\\sand\\b") %>%                   # Remove 'and'
  str_remove_all("\\sdistrict\\b") %>%                    # Remove 'district'
  str_remove_all("\\s*\\([^)]*\\)") %>%             # Remove contents inside parentheses
  str_remove_all("\\svillage\\b") %>%
  str_split(",\\s*") %>%                            # Split on all commas (after cleaning)
  unlist() %>% 
  str_trim() %>%
  unique()

# Make an empty cache
cache <- tibble(location = character(), latitude = numeric(), longitude = numeric())

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
  mutate(row_id = row_number()) %>%  # Track original row IDs for later merging
  mutate(location = Location %>%
           str_replace_all(",\\s*", ", ") %>%           # Ensure consistent spacing after commas
           str_replace_all(";/", ", ") %>%                   # Handle separators like ";" and "/"
           str_replace_all(";", ", ") %>%                    # Split entries on ";"
           str_replace_all("/", ", ") %>%                    # Split entries on "/"
           str_replace_all("Zuit-holland", "Zuid-Holland") %>%
           str_replace_all("South Holland", "Zuid-Holland") %>%
           str_replace_all("Hal", "Halle BE") %>%
           str_remove_all("\\sprovinces?\\b") %>%             # Remove 'province' or 'provinces'
           str_remove_all("\\scities\\b") %>%                 # Remove 'cities'
           str_remove_all("\\scity\\b") %>%                   # Remove 'city'
           str_remove_all("\\stown\\b") %>%                   # Remove 'town'
           str_remove_all("\\svillage\\b") %>%            # Remove 'village'
           str_remove_all("\\sand\\b") %>%                    # Remove 'and'
           str_remove_all("\\sdistrict\\b") %>%                    # Remove 'district'
           str_remove_all("\\s*\\([^)]*\\)")) %>%             # Remove text inside parentheses
  separate_rows(location, sep = ",\\s*") %>%                  # Split on all commas (after cleaning)
  mutate(location = str_trim(location))                       # Clean up extra spaces 

# df = cbind.data.frame(unique_locations,cache)
# colnames(df)[1] = "location"

# Merge cleaned data with cached coordinates
fil_geo_clean <- fil_geo_clean %>%
  left_join(cache, by = "location")

# Identify rows that still need geocoding
locations_to_geocode <- fil_geo_clean %>%
  filter(is.na(latitude) | is.na(longitude)) %>%
  distinct(location) %>%
  anti_join(cache, by = "location") %>%
  pull(location)

# Geocode only the missing locations
if (length(locations_to_geocode) > 0) {
  new_geocodes <- future_map_dfr(locations_to_geocode, ~ geocode_location(.x, cache)) %>%
    distinct(location, .keep_all = TRUE)
  cache <- bind_rows(cache, new_geocodes) %>%
    distinct(location, .keep_all = TRUE)
  fil_geo_clean <- fil_geo_clean %>%
    rows_update(new_geocodes, by = "location")
}

library(sf)
nuts3 <- st_read("NUTS3.geojson") %>% filter(LEVL_CODE == 2)
nuts3 <- st_transform(nuts3, crs = 4326)

fil_geo_clean_sf <- fil_geo_clean %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%  # Filter rows with coordinates
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

fil_geo_with_nuts3 <- st_join(fil_geo_clean_sf, nuts3, join = st_within)

# Fixing the Zeeland weirdness
replacement_data = nuts3 %>%
  filter(NUTS_NAME == "Zeeland") %>%
  select(location = NUTS_NAME, NUTS_ID, LEVL_CODE, CNTR_CODE, NAME_LATN) %>% 
  st_drop_geometry()

fil_geo_with_nuts3 = fil_geo_with_nuts3 %>%
  st_drop_geometry() %>% 
  rows_update(replacement_data, by = "location")

# Connecting to Eurostat ####
library(eurostat)

which_nuts = fil_geo_with_nuts3 %>% select(NUTS_ID,NUTS_NAME)

pop_data <- get_eurostat("demo_r_d2jan", time_format = "num") %>%
  filter(geo %in% unique(fil_geo_with_nuts3$NUTS_ID),
         sex == "T",
         age == "TOTAL",
         TIME_PERIOD >= 2002, TIME_PERIOD <= 2024) %>%
  select(NUTS_ID = geo, TIME_PERIOD, population = values)

area_data <- get_eurostat("demo_r_d3area", time_format = "num") %>%
  filter(unit == "KM2", TIME_PERIOD == 2015) %>%  # Latest available year
  select(NUTS_ID = geo, area_km2 = values) %>%
  distinct(NUTS_ID, .keep_all = TRUE)

# Preparing Data for K-Means Cluster####
# I need Exposure, Damage and Population Growth

## Exposure ####
# Calculated as normalized exposure = sum(hazard events) / (population/area)

## Damage ####
# Calculated as 


# Maybe Codes ####
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

