# EM-DAT Wrangling

library(tidyverse)

# First try to identify which NUTS region it is.
# Eurostat NUTS table: https://ec.europa.eu/eurostat/documents/345175/629341/NUTS2021-NUTS2024.xlsx/2b35915f-9c14-6841-8197-353408c4522d?t=1717505289640

geo_hazard <- read_excel("EM-DAT data.xlsx")
nuts2024 = read.csv

colnames(geo_hazard) = tolower(colnames(geo_hazard))
colnames(nuts2024) = tolower(colnames(nuts2024))

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
# Will probably have to first filter out the unnecessary countries
# And then make location column into a list
# And then identify if it is nuts 2 or nuts 3
# From there then we can decide what's the best... 
# Oooh! and use the eurostat package to source all of the necessary data (should probably identify which tables I want first as well...)
