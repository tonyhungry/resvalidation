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

# Filter out countries that are not in the BNL and deselect columns we don't need
fil_geo = geo_hazard %>% 
  filter(ISO %in% BNL) %>% # Filter for BNL countries
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
    Location = str_replace_all(Location, "Hal", "Halle"),
    
    # Remove descriptive or irrelevant suffixes
    Location = str_remove_all(Location, "\\sprovinces?\\b"),
    Location = str_remove_all(Location, "\\sarea\\b"),
    Location = str_remove_all(Location, "\\scities\\b"),
    Location = str_remove_all(Location, "\\scity\\b"),
    Location = str_remove_all(Location, "\\stown\\b"),
    Location = str_remove_all(Location, "\\svillage\\b"),
    Location = str_remove_all(Location, "\\sand\\b"),
    Location = str_remove_all(Location, "\\sdistrict\\b"),
    Location = str_remove_all(Location, "\\sHoofdtedelijk Gewes\\b"),
    Location = str_remove_all(Location, "\\s*\\([^)]*\\)")  # remove parentheses and content
  ) %>%
  separate_rows(Location, sep = ",\\s*") %>%
  mutate(
    Location = str_trim(Location),
    Location = paste(Location, ISO2)  # 💥 Append country code to location
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
           str_replace_all("\\bGroninge\\b", "Groningen") %>%
           str_replace_all("Totterdam", "Rotterdam") %>%
           str_replace_all("Westhoerk", "Westhoek") %>%
           str_replace_all("\\bHal\\b", "Halle") %>%
           str_remove_all("\\sprovinces?\\b") %>%
           str_remove_all("\\sarea\\b") %>%
           str_remove_all("\\scities\\b") %>%
           str_remove_all("\\scity\\b") %>%
           str_remove_all("\\stown\\b") %>%
           str_remove_all("\\svillage\\b") %>%
           str_remove_all("\\sand\\b") %>%
           str_remove_all("\\sdistrict\\b") %>%
           str_remove_all("\\sHoofdtedelijk Gewes\\b") %>%
           str_remove_all("\\s*\\([^)]*\\)")) %>% 
  separate_rows(location, sep = ",\\s*") %>%                  # Split on all commas (after cleaning)
  mutate(
    location = str_trim(location),
    location = paste(location, ISO2)  # Final step: append ISO2
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
  filter(!is.na(exposure_index_avg)) %>% 
  filter(substr(NUTS_ID, 1, 2) %in% c("BE", "NL", "LU")) # This means there are some problems with geocoding... should work on that. I think also adding the ISO code into the search would make it better.

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
# Find the population growth rate between 2002 and 2024.
pop_growth <- pop_data %>%
  filter(year %in% c(2002, 2022)) %>%
  select(NUTS_ID, year, population) %>%
  pivot_wider(names_from = year, values_from = population, names_prefix = "pop_") %>%
  mutate(
    growth_abs = pop_2022 - pop_2002,
    growth_pct = (pop_2022 - pop_2002) / pop_2002
  ) %>% 
  select(NUTS_ID, growth_pct)


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
# It seems like there are 2/3/4 clusters

# Use the silhouette method 
fviz_nbclust(km_data, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal k")
# Suggests that there are 2 clusters

set.seed(123) # for reproducibility purposes

kmeans_result <- kmeans(km_data, centers = 2, nstart = 25)

# Add cluster labels back to original data
kmclusdata <- kmclusdata %>%
  mutate(cluster = kmeans_result$cluster)

# Preparing Data for discriminant analysis ####

# Getting data from Eurostat
# Population Density
pop_density_data <- get_eurostat("tgs00024", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, pop_density = values) %>%
  filter(!is.na(pop_density)) %>% 
  filter(year == 2022) %>% 
  select(NUTS_ID,pop_density)
# Population that has internet access
internet_access <- get_eurostat("tgs00047", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, internet_access_pct = values) %>%
  filter(!is.na(internet_access_pct)) %>% 
  filter(year == 2024) %>% 
  select(NUTS_ID,internet_access_pct)
# Poverty rate
poverty_rate <- get_eurostat("tgs00103", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, poverty_pct = values) %>%
  filter(!is.na(poverty_pct)) %>% 
  filter(year == 2021) %>% 
  select(NUTS_ID,poverty_pct)
# Road density in km2
road_density <- get_eurostat("tran_r_net", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, road_density_km = values) %>%
  filter(!is.na(road_density_km)) %>% 
  filter(tra_infr == "MWAY") %>% 
  filter(unit == "KM_TKM2") %>% 
  filter(year == 2022) %>% 
  select(NUTS_ID,road_density_km)
# Gini coefficient
gini <- get_eurostat("ilc_di11_r", time_format = "num") %>%
  filter(TIME_PERIOD == 2021) %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, gini_coeff = values) %>% 
  select(NUTS_ID, year, gini_coeff)
# gini doesn't work for Belgium because of incomplete data.

benelux_hdi_2022 <- read.csv("~/Documents/Resilience Validation/benelux_hdi_2022.csv")

resilience_vars <- pop_density_data %>%
  left_join(internet_access, by = c("NUTS_ID")) %>%
  left_join(poverty_rate, by = c("NUTS_ID")) %>%
  left_join(road_density, by = c("NUTS_ID")) %>% 
  left_join(gini, by = c("NUTS_ID")) %>% 
  left_join(benelux_hdi_2022, by = c("NUTS_ID"))

discdata = kmclusdata %>% 
  select(NUTS_ID,cluster) %>% 
  left_join(resilience_vars,by = "NUTS_ID") %>% 
  mutate(poverty_pct = if_else(NUTS_ID == "LU00", 18.8, poverty_pct))

# Discriminant Analysis ####

# Without gini coefficient
lda_model <- MASS::lda(cluster ~ pop_density + internet_access_pct + poverty_pct + road_density_km  + shdi_score, data = discdata)
lda_pred <- predict(lda_model)
discdata <- discdata %>% mutate(predicted_cluster = lda_pred$class)
table(Actual = discdata$cluster, Predicted = discdata$predicted_cluster)
mean(discdata$cluster == discdata$predicted_cluster)

# Overall accuracy 86.4%

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

# Acquiring more socioeconomic indicators for step-wise linear discriminant ####
# % under 5 years and % over 75 years
totalpop <- get_eurostat("demo_r_pjangroup", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, totalpop = values) %>%
  filter(year == 2024, age == "TOTAL", sex == "T") %>%
  select(NUTS_ID, totalpop)
underfive <- get_eurostat("demo_r_pjangroup", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, underfivepop = values) %>%
  filter(year == 2024, age == "Y_LT5", sex == "T") %>% 
  select(NUTS_ID, underfivepop)
over75 <- get_eurostat("demo_r_pjangroup", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, overseventyfive = values) %>%
  filter(year == 2024, age == "Y_GE75", sex == "T") %>% #65 and over not available
  select(NUTS_ID,overseventyfive)
fiveseventyfive <- underfive %>%
  left_join(totalpop, by = c("NUTS_ID")) %>%
  left_join(over75, by = c("NUTS_ID")) %>% 
  mutate(under5_pct = (underfivepop / totalpop) * 100) %>% 
  mutate(over75_pct = (overseventyfive / totalpop) * 100) %>% 
  select(NUTS_ID, under5_pct, over75_pct)

# Female employment rate (15-64 years)
femalework <- get_eurostat("lfst_r_lfe2emprt", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, womwork_pct = values) %>%
  filter(year == 2023, age == "Y15-64", sex == "F") %>%
  select(NUTS_ID, womwork_pct)

# Percent of population ages 25-64 without high school or higher levels of education
noHS <- get_eurostat("edat_lfse_04", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, noHS_pct = values) %>%
  filter(year == 2023, age == "Y25-64", sex == "T",isced11 == "ED0-2") %>%
  select(NUTS_ID, noHS_pct)

# Economically active population in 1000s
workingpopulation = get_eurostat("lfst_r_lfp2act", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, workingpop = values) %>%
  filter(year == 2023, age == "Y15-64", sex == "T") %>%
  select(NUTS_ID, workingpop)

# regional GDP PPS per inhabitant
gdp = get_eurostat("tgs00005", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, gdppps = values) %>%
  filter(year == 2023) %>%
  select(NUTS_ID, gdppps)

# household income - net disposable income balance
income = get_eurostat("nama_10r_2hhinc", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, hhincome = values) %>%
  filter(year == 2022, na_item == "B6N", unit == "PPS_EU27_2020_HAB", direct == "BAL") %>%
  select(NUTS_ID, hhincome)

# Passenger cars per thousand inhabitants
cars = get_eurostat("tran_r_vehst", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, carsperthou = values) %>%
  filter(year == 2022, vehicle == "CAR", unit == "P_THAB") %>%
  select(NUTS_ID, carsperthou)

# Unemployment rate of ages 15 to 74
unemploy = get_eurostat("tgs00010", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, unemployment = values) %>%
  filter(year == 2023, sex == "T", isced11 == "TOTAL") %>%
  select(NUTS_ID, unemployment)

# Employed in Agriculture, Forestry and Fisheries in 1000 persons
farm = get_eurostat("lfst_r_lfe2en2", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, farmpeople = values) %>%
  filter(year == 2023, sex == "T", age == "Y15-74", nace_r2 == "A") %>%
  select(NUTS_ID, farmpeople)

# % artificial land (corresponds to impervious surfaces)
artland = get_eurostat("lan_lcv_art", time_format = "num") %>%
  rename(NUTS_ID = geo, year = TIME_PERIOD, artland_pct = values) %>%
  filter(year == 2018, landcover == "LCA", unit == "PC") %>%
  select(NUTS_ID, artland_pct)

resilience_vars <- resilience_vars %>%
  left_join(fiveseventyfive, by = c("NUTS_ID")) %>%
  left_join(femalework, by = c("NUTS_ID")) %>%
  left_join(noHS, by = c("NUTS_ID")) %>% 
  left_join(workingpopulation, by = c("NUTS_ID")) %>% 
  left_join(gdp, by = c("NUTS_ID")) %>% 
  left_join(income, by = c("NUTS_ID")) %>% 
  left_join(cars, by = c("NUTS_ID")) %>% 
  left_join(unemploy, by = c("NUTS_ID")) %>% 
  left_join(farm, by = c("NUTS_ID")) %>% 
  left_join(artland, by = c("NUTS_ID")) 

discdata = kmclusdata %>% 
  select(NUTS_ID,cluster) %>% 
  left_join(resilience_vars,by = "NUTS_ID") %>% 
  mutate(poverty_pct = if_else(NUTS_ID == "LU00", 18.8, poverty_pct)) %>% 
  mutate(farmpeople = if_else(NUTS_ID == "BE31", 2.5, farmpeople)) %>% #2010 data
  mutate(farmpeople = if_else(NUTS_ID == "BE10", 0.2, farmpeople)) 

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
