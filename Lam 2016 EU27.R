# Case Study using RIM (Lam 2015/2016) for EU27 countries 
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

# EU27 Countries
EU27 = c("BEL", "BGR", "CZE", "DNK", "DEU", "EST", "IRL", "GRC", "ESP", "FRA", "HRV", 
                 "ITA", "CYP", "LVA", "LTU", "LUX", "HUN", "MLT", "NLD", "AUT", "POL", "PRT", 
                 "ROU", "SVN", "SVK", "FIN", "SWE")

# Filter out countries that are not in the EU27 and deselect columns we don't need
fil_geo = geo_hazard %>% 
  filter(ISO %in% EU27) %>% # Filter for BNL countries
  select(-Historic,-`Classification Key`,-`Disaster Group`,-`Disaster Subtype`,-`External IDs`,-`Event Name`,-Subregion,-Region,-Origin,-`Associated Types`,-`OFDA/BHA Response`,-Appeal,-Declaration,-`AID Contribution ('000 US$)`,-Latitude,-Longitude,-`River Basin`,-CPI,-`Admin Units`,-`Entry Date`,-`Last Update`) %>% # Take out columns that are not needed
  filter(!is.na(Location))  %>% # filter out entries with no location information
  mutate(ISO2 = countrycode::countrycode(ISO, origin = 'iso3c', destination = 'iso2c'))

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
  select(Location, ISO2) %>%
  mutate(
    # Standardize separators
    Location = str_replace_all(Location, ",\\s*", ", "),
    Location = str_replace_all(Location, ";/", ", "),
    Location = str_replace_all(Location, ";", ", "),
    Location = str_replace_all(Location, "/", ", "),
    
    # Common misspellings or local inconsistencies
    Location = str_replace_all(Location, "Zuit-holland", "Zuid-Holland"),
    Location = str_replace_all(Location, "South Holland", "Zuid-Holland"),
    Location = str_replace_all(Location, "\\bGroninge\\b", "Groningen"),
    Location = str_replace_all(Location, "Totterdam", "Rotterdam"),
    Location = str_replace_all(Location, "Westhoerk", "Westhoek"),
    Location = str_replace_all(Location, "\\bHal\\b", "Halle"),
    
    # Remove descriptive or irrelevant suffixes
    Location = str_remove_all(Location, regex("\\sprovinces?\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\sareas?\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\scities\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\scity\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\stowns?\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\svillage[s]?\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\sand\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\sdistricts?\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\sHoofdtedelijk Gewes\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\smunicipalit(?:y|ies)\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\sregions?\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\sdepartments?\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\sIsl\\.?\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, regex("\\sLaen\\b", ignore_case = TRUE)),
    Location = str_remove_all(Location, "\\s*\\([^)]*\\)")  # Remove parentheses and content
  ) %>%
  separate_rows(Location, sep = ",\\s*") %>%
  mutate(
    Location = str_trim(Location),
    Location = paste(Location, ISO2)  # Append country code to location
  ) %>%
  distinct(Location) %>%
  pull(Location)

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

# Check which rows are faulty ####
improper = cache %>% filter(is.na(latitude))

# Pre-process the 'Location' column in fil_geo
fil_geo_clean <- fil_geo %>%
  mutate(row_id = row_number()) %>%  # Track original row IDs for later merging
  mutate(location = Location %>%
           str_replace_all(",\\s*", ", ") %>%           # Ensure consistent spacing after commas
           str_replace_all(";/", ", ") %>%              # Handle separators like ";" and "/"
           str_replace_all(";", ", ") %>%               
           str_replace_all("/", ", ") %>%               
           
           # Common misspellings
           str_replace_all("Zuit-holland", "Zuid-Holland") %>%
           str_replace_all("South Holland", "Zuid-Holland") %>%
           str_replace_all("\\bGroninge\\b", "Groningen") %>%
           str_replace_all("Totterdam", "Rotterdam") %>%
           str_replace_all("Westhoerk", "Westhoek") %>%
           str_replace_all("\\bHal\\b", "Halle") %>%
           
           # Remove suffixes, descriptors, etc.
           str_remove_all(regex("\\sprovinces?\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\sareas?\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\scities\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\scity\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\stowns?\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\svillage[s]?\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\sand\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\sdistricts?\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\smunicipalit(?:y|ies)\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\sregions?\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\sdepartments?\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\sIsl\\.?\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\sLaen\\b", ignore_case = TRUE)) %>%
           str_remove_all(regex("\\sHoofdtedelijk Gewes\\b", ignore_case = TRUE)) %>%
           str_remove_all("\\s*\\([^)]*\\)")  # Remove parentheses and content
  ) %>%
  separate_rows(location, sep = ",\\s*") %>%  # Split on commas
  mutate(
    location = str_trim(location),
    location = paste(location, ISO2)  # Append ISO2
  )

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
  filter(!is.na(latitude) & !is.na(longitude)) %>%  # Filter rows without coordinates
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

fil_geo_with_nuts3 <- st_join(fil_geo_clean_sf, nuts3, join = st_within)

# Fixing the Zeeland weirdness
replacement_data = nuts3 %>%
  filter(NUTS_NAME == "Zeeland") %>%
  mutate(NUTS_NAME = paste(NUTS_NAME, "NL")) %>%
  select(location = NUTS_NAME, NUTS_ID, LEVL_CODE, CNTR_CODE, NAME_LATN) %>%
  st_drop_geometry()

fil_geo_with_nuts3 = fil_geo_with_nuts3 %>%
  st_drop_geometry() %>%
  rows_update(replacement_data, by = "location") %>% 
  filter(CNTR_CODE %in% unique(fil_geo$ISO2))

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
# I need Exposure, Damage and Population Growth data to do the K-Means Cluster

## Exposure ####
# Calculated as normalized exposure = sum(hazard events) / (population/area) = 
# This is equal to sum(hazard events) / population density
# Hazard event per NUTS2 region

# Calculate population density per region per year
pop_density <- pop_data %>%
  left_join(area_data, by = "NUTS_ID") %>%
  mutate(pop_density = population / area_km2) %>%
  filter(!is.na(pop_density))

# Count hazard events per region per year
hazard_counts <- fil_geo_with_nuts3 %>%
  filter(!is.na(NUTS_ID)) %>%
  group_by(NUTS_ID, `Start Year`) %>%
  rename(TIME_PERIOD = `Start Year`) %>% 
  summarise(event_count = n(), .groups = "drop")

# Merge the two datasets
exposure_data <- hazard_counts %>%
  left_join(pop_density, by = c("NUTS_ID", "TIME_PERIOD"))  # TIME_PERIOD = year

# Calculate the normalized index by region 
exposure_data <- exposure_data %>%
  mutate(exposure_index = event_count / pop_density)

final_exposure_index <- exposure_data %>%
  group_by(NUTS_ID) %>%
  summarise(
    exposure_index_avg = mean(exposure_index, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  filter(!is.na(exposure_index_avg))

## Damage ####
# Calculated as the sum of the damage from each event divided by the population.
# Since the data is incomplete, thus I have made an index based on the available data, which are all normalized first. 
# Need to do this per event and then later divide it by

damage_raw <- fil_geo %>%
  select(
    DisNo.,
    deaths = `Total Deaths`,
    affected = `Total Affected`,
    insured_damage = `Insured Damage, Adjusted ('000 US$)`,
    total_damage = `Total Damage, Adjusted ('000 US$)`
  )

damage_norm <- damage_raw %>%
  mutate(across(c(deaths, affected, insured_damage, total_damage), 
                ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)),
                .names = "norm_{.col}"))

damage_index <- damage_norm %>%
  mutate(damage_index = rowMeans(select(., starts_with("norm_")), na.rm = TRUE)) %>%
  select(DisNo., damage_index)

pop_data <- pop_data %>%
  rename(year = TIME_PERIOD)

damagePC <- fil_geo_with_nuts3 %>%
  left_join(damage_index, by = "DisNo.") %>% 
  rename(year = `Start Year`) %>% 
  left_join(pop_data, by = c("NUTS_ID", "year")) %>% 
  st_drop_geometry() %>% 
  select(DisNo., NUTS_ID, NUTS_NAME,damage_index,population,year) %>% 
  mutate(damage_per_capita = damage_index / population)

avg_damage_per_capita <- damagePC %>%
  group_by(NUTS_ID) %>%
  summarise(
    avg_damage_per_capita = mean(damage_per_capita, na.rm = TRUE),
    .groups = "drop"
  )

## Population Growth ####
# Find the population growth rate between 2002 and 2024 (for most regions)
# For some regions, it is then calculated based 

# Get earliest population for each NUTS_ID
pop_start <- pop_data %>%
  group_by(NUTS_ID) %>%
  filter(!is.na(population)) %>%
  slice_min(year, with_ties = FALSE) %>%
  rename(start_year = year, pop_start = population)

# Get population in 2022
pop_end <- pop_data %>%
  filter(year == 2022) %>%
  select(NUTS_ID, pop_end = population)

pop_growth <- pop_start %>%
  left_join(pop_end, by = "NUTS_ID") %>%
  mutate(
    growth_abs = pop_end - pop_start,
    growth_pct = (pop_end - pop_start) / pop_start
  ) %>%
  select(NUTS_ID, start_year, growth_pct)


# K-Means Cluster ####

# Consolidate the data
kmclusdata = final_exposure_index %>% 
  left_join(avg_damage_per_capita, by = "NUTS_ID") %>% 
  left_join(pop_growth, by = "NUTS_ID") %>% 
  filter(!is.na(avg_damage_per_capita), !is.nan(avg_damage_per_capita))

# Scale all of the variables before running K-Means Cluster
km_data <- kmclusdata %>%
  select(exposure_index_avg, avg_damage_per_capita, growth_pct) %>%
  scale()

# Use the elbow method to find the optimal K
library(factoextra)
fviz_nbclust(km_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k")
# It seems like there are 8 clusters

# Use the silhouette method 
fviz_nbclust(km_data, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal k")
# Suggests that there are 8 clusters

set.seed(123) # for reproducibility purposes

kmeans_result <- kmeans(km_data, centers = 4, nstart = 25)

# Add cluster labels back to original data
kmclusdata <- kmclusdata %>%
  mutate(cluster = kmeans_result$cluster)

# Preparing Data for discriminant analysis ####

# Getting data from Eurostat
# Since not every year has the full data, we will have to choose the year that has the most data for each variable and go from there. This would then lead to 

get_best_year_data <- function(table_code, value_name, extra_filter = NULL, target_nuts) {
  raw <- get_eurostat(table_code, time_format = "num") %>%
    rename(NUTS_ID = geo, year = TIME_PERIOD, value = values) %>%
    filter(NUTS_ID %in% target_nuts)
  
  # Ensure filter uses tidy evaluation
  if (!rlang::quo_is_null(enquo(extra_filter))) {
    raw <- raw %>% filter(!!enquo(extra_filter))
  }
  
  # Determine year with the most data
  best_year <- raw %>%
    filter(!is.na(value)) %>%
    group_by(year) %>%
    summarise(n_available = n(), .groups = "drop") %>%
    arrange(desc(n_available)) %>%
    slice(1) %>%
    pull(year)
  
  # Filter to best year and rename the value column
  data_filtered <- raw %>%
    filter(year == best_year) %>%
    select(NUTS_ID, !!value_name := value)
  
  list(data = data_filtered, best_year = best_year)
}

target_nuts <- unique(kmclusdata$NUTS_ID)

pop_density = get_best_year_data(
  table_code = "tgs00024",
  value_name = "pop_density",
  target_nuts = target_nuts
)
pop_density_data = pop_density$data
message("Population density: using year ", pop_density$best_year)

internet_res <- get_best_year_data(
  table_code = "tgs00047",
  value_name = "internet_access_pct",
  target_nuts = target_nuts
)
internet_access_data <- internet_res$data
message("Internet access: using year ", internet_res$best_year)

# At-risk-of-poverty rate
poverty_res <- get_best_year_data(
  table_code = "tgs00103",
  value_name = "poverty_pct",
  target_nuts = target_nuts
)
poverty_data <- poverty_res$data
message("Poverty rate: using year ", poverty_res$best_year)

# Unemployment rate
unemploy_res <- get_best_year_data(
  table_code = "tgs00010",
  value_name = "unemployment_pct",
  extra_filter = (sex == "T" & isced11 == "TOTAL"),
  target_nuts = target_nuts
)
unemploy_data <- unemploy_res$data
message("Unemployment rate: using year ", unemploy_res$best_year)

# Road density with extra filters
road_res <- get_best_year_data(
  table_code = "tran_r_net",
  value_name = "road_density_km",
  extra_filter = (tra_infr == "MWAY" & unit == "KM_TKM2"),
  target_nuts = target_nuts
)
road_data <- road_res$data
message("Road density: using year ", road_res$best_year)

# Gini coefficient
gini_res <- get_best_year_data(
  table_code = "ilc_di11_r",
  value_name = "gini_coeff",
  target_nuts = target_nuts
)
gini <- gini_res$data

# Female employment rate
femalework_res <- get_best_year_data(
  table_code = "lfst_r_lfe2emprt",
  value_name = "womwork_pct",
  extra_filter = age == "Y15-64" & sex == "F",
  target_nuts = target_nuts
)
femalework <- femalework_res$data

# % without high school (ED0-2)
noHS_res <- get_best_year_data(
  table_code = "edat_lfse_04",
  value_name = "noHS_pct",
  extra_filter = age == "Y25-64" & sex == "T" & isced11 == "ED0-2",
  target_nuts = target_nuts
)
noHS <- noHS_res$data

# Working population
workingpop_res <- get_best_year_data(
  table_code = "lfst_r_lfp2act",
  value_name = "workingpop",
  extra_filter = age == "Y15-64" & sex == "T",
  target_nuts = target_nuts
)
workingpopulation <- workingpop_res$data

# Regional GDP
gdp_res <- get_best_year_data(
  table_code = "tgs00005",
  value_name = "gdppps",
  target_nuts = target_nuts
)
gdp <- gdp_res$data

# Household income
income_res <- get_best_year_data(
  table_code = "nama_10r_2hhinc",
  value_name = "hhincome",
  extra_filter = na_item == "B6N" & unit == "PPS_EU27_2020_HAB" & direct == "BAL",
  target_nuts = target_nuts
)
income <- income_res$data

# Cars per 1000
cars_res <- get_best_year_data(
  table_code = "tran_r_vehst",
  value_name = "carsperthou",
  extra_filter = vehicle == "CAR" & unit == "P_THAB",
  target_nuts = target_nuts
)
cars <- cars_res$data

# Agriculture employment
farm_res <- get_best_year_data(
  table_code = "lfst_r_lfe2en2",
  value_name = "farmpeople",
  extra_filter = sex == "T" & age == "Y15-74" & nace_r2 == "A",
  target_nuts = target_nuts
)
farm <- farm_res$data

# Artificial land
artland_res <- get_best_year_data(
  table_code = "lan_lcv_art",
  value_name = "artland_pct",
  extra_filter = landcover == "LCA" & unit == "PC",
  target_nuts = target_nuts
)
artland <- artland_res$data

# Below 5% and Above 75%
pop_raw <- get_eurostat("demo_r_pjangroup", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, value = values) %>%
  filter(NUTS_ID %in% target_nuts, sex == "T")
# Total pop
totalpop <- pop_raw %>%
  filter(age == "TOTAL") %>%
  group_by(NUTS_ID) %>%
  slice_max(year, with_ties = FALSE) %>%
  rename(totalpop = value)
# Under 5
underfive <- pop_raw %>%
  filter(age == "Y_LT5") %>%
  group_by(NUTS_ID) %>%
  slice_max(year, with_ties = FALSE) %>%
  rename(underfivepop = value)
# Over 75
over75 <- pop_raw %>%
  filter(age == "Y_GE75") %>%
  group_by(NUTS_ID) %>%
  slice_max(year, with_ties = FALSE) %>%
  rename(overseventyfive = value)
# Merge and calculate percentages
fiveseventyfive <- underfive %>%
  left_join(totalpop, by = "NUTS_ID") %>%
  left_join(over75, by = "NUTS_ID") %>%
  mutate(
    under5_pct = (underfivepop / totalpop) * 100,
    over75_pct = (overseventyfive / totalpop) * 100
  ) %>%
  select(NUTS_ID, under5_pct, over75_pct)

resilience_vars <- fiveseventyfive %>%
  left_join(pop_density_data,        by = "NUTS_ID") %>%
  left_join(internet_access_data,    by = "NUTS_ID") %>%
  left_join(poverty_data,            by = "NUTS_ID") %>%
  left_join(unemploy_data,           by = "NUTS_ID") %>%
  left_join(road_data,               by = "NUTS_ID") %>%
  left_join(gini,                    by = "NUTS_ID") %>%
  left_join(femalework,              by = "NUTS_ID") %>%
  left_join(noHS,                    by = "NUTS_ID") %>%
  left_join(workingpopulation,       by = "NUTS_ID") %>%
  left_join(gdp,                     by = "NUTS_ID") %>%
  left_join(income,                  by = "NUTS_ID") %>%
  left_join(cars,                    by = "NUTS_ID") %>%
  left_join(farm,                    by = "NUTS_ID") %>%
  left_join(artland,                 by = "NUTS_ID")

# Check how many complete rows that we have
summary(complete.cases(resilience_vars))

# Check which columns have no missing values
resilience_vars %>%
  ungroup() %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ all(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "complete") %>%
  filter(complete == TRUE)

# Check which columns have missing values
resilience_vars %>%
  ungroup() %>%  # 💥 This clears any previous groupings
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  arrange(desc(n_missing))

# Look at which entries are missing per variable (the small ones)
resilience_vars %>%
  select(NUTS_ID,farmpeople,artland_pct,carsperthou) %>% 
  pivot_longer(-NUTS_ID, names_to = "variable", values_to = "value") %>%
  filter(is.na(value)) %>%
  arrange(NUTS_ID, variable)

discdata = kmclusdata %>% 
  select(NUTS_ID,cluster) %>% 
  left_join(resilience_vars,by = "NUTS_ID") %>% 
  mutate(poverty_pct = if_else(NUTS_ID == "LU00", 18.8, poverty_pct)) %>% 
  mutate(farmpeople = if_else(NUTS_ID == "BE10", 0.2, farmpeople)) 

# Discriminant Analysis ####

lda_model <- MASS::lda(cluster ~ under5_pct + over75_pct + pop_density + unemployment_pct + womwork_pct + noHS_pct + workingpop + gdppps + hhincome, data = discdata)
lda_pred <- predict(lda_model)
discdata <- discdata %>% mutate(predicted_cluster = lda_pred$class)
table(Actual = discdata$cluster, Predicted = discdata$predicted_cluster)
mean(discdata$cluster == discdata$predicted_cluster)

diag_mat <- diag(table(discdata$cluster, discdata$predicted_cluster))
row_totals <- rowSums(table(discdata$cluster, discdata$predicted_cluster))
round(diag_mat / row_totals * 100, 1)

# Overall accuracy 67.9%

discdata$LD1 <- lda_pred$x[,1]
ggplot(discdata, aes(x = LD1, fill = factor(cluster))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 15) +
  labs(title = "LDA Separation by Cluster", x = "LD1", fill = "Cluster") +
  theme_minimal()

ggplot(discdata, aes(x = LD1, fill = as.factor(cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Linear Discriminant Function (LD1)",
       x = "LD1", fill = "Actual Cluster")

lda_model$scaling







# Stepwise Discriminant Analysis ####
# library(klaR)

discdata_clean <- discdata %>%
  select(-NUTS_ID,-year,-gini_coeff) %>% 
  filter(if_all(where(is.numeric), ~ !is.na(.))) %>%
  mutate(cluster = as.factor(cluster))

nrow(discdata_clean)        # Number of rows in full dataset
length(discdata_clean$cluster)  # Number of group labels
anyNA(discdata_clean)   
summary(discdata_clean$cluster)

predictors <- discdata_clean %>% select(-cluster)
grouping <- discdata_clean$cluster

step_model <- klaR::stepclass(
  x = discdata_clean %>% select(-cluster),
  grouping = discdata_clean$cluster,
  method = "lda",
  improvement = 0,
  direction = "both",
  verbose = TRUE
)

dim(as.data.frame(discdata_clean))[1] == length(grouping)

selected_vars <- step_model$variables
print(selected_vars)

formula_final <- as.formula(paste("cluster ~", paste(selected_vars, collapse = "+")))

lda_stepwise <- MASS::lda(cluster ~ ., data = discdata_clean)
lda_pred <- predict(lda_stepwise)
discdata <- discdata %>% mutate(predicted_cluster = lda_pred$class)
table(Actual = discdata$cluster, Predicted = discdata$predicted_cluster)
mean(discdata$cluster == discdata$predicted_cluster)

# Stepwise discriminant analysis yielded no improvement 
# Thus I have used all possible variables in the discriminant analysis and now the accuracy is 100%

discdata$LD1 <- lda_pred$x[,1]
ggplot(discdata, aes(x = LD1, fill = factor(cluster))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 15) +
  labs(title = "LDA Separation by Cluster", x = "LD1", fill = "Cluster") +
  theme_minimal()

ggplot(discdata, aes(x = LD1, fill = as.factor(cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Linear Discriminant Function (LD1)",
       x = "LD1", fill = "Actual Cluster")

lda_stepwise$scaling
