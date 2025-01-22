# Data Wrangling and Exploration

library(tidyverse)
library(readxl)

data <- read_excel("EM-DAT data.xlsx")

# 
# Insured Damage
insured = data %>% filter(!is.na(`Insured Damage, Adjusted ('000 US$)`))

summed = insured %>% group_by(`End Year`) %>% summarize(insureddam = sum(`Insured Damage, Adjusted ('000 US$)`)) %>% rename(year = `End Year`) %>% mutate(logins = log(insureddam))

ggplot(summed, aes(x = factor(year), y = insureddam)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Insured Damage in 1000 USD (adjusted) by Year",
    x = "Year",
    y = "Insured Damage in 1000 USD (adjusted)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(summed, aes(x = factor(year), y = logins)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Insured Damage in 1000 USD (adjusted) by Year",
    x = "Year",
    y = "Log of Insured Damage in 1000 (adjusted) USD") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Insured damage by Subregion
summed = insured %>% group_by(`Subregion`) %>% summarize(insureddam = sum(`Insured Damage, Adjusted ('000 US$)`)) %>% mutate(logins = log(insureddam))

ggplot(summed, aes(x = factor(Subregion), y = insureddam)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Insured Damage in 1000 USD (adjusted) by Subregion",
    x = "Subregion",
    y = "Insured Damage in 1000 USD (adjusted)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(summed, aes(x = factor(Subregion), y = logins)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Insured Damage in 1000 USD (adjusted) by Subregion",
    x = "Subregion",
    y = "Log of Insured Damage in 1000 (adjusted) USD") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Insured damage by Event Type
summed = insured %>% group_by(`Disaster Type`) %>% summarize(insureddam = sum(`Insured Damage, Adjusted ('000 US$)`)) %>% mutate(logins = log(insureddam))

ggplot(summed, aes(x = factor(`Disaster Type`), y = insureddam)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Insured Damage in 1000 USD (adjusted) by Disaster Type",
    x = "Disaster Type",
    y = "Insured Damage in 1000 USD (adjusted)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(summed, aes(x = factor(`Disaster Type`), y = logins)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Insured Damage in 1000 USD (adjusted) by Disaster Type",
    x = "Disaster Type",
    y = "Log of Insured Damage in 1000 (adjusted) USD") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Total Damage
total = data %>% filter(!is.na(`Total Damage, Adjusted ('000 US$)`))
summed = total %>% group_by(`End Year`) %>% summarize(totaldam = sum(`Total Damage, Adjusted ('000 US$)`)) %>% rename(year = `End Year`) %>% mutate(logdam = log(totaldam))

ggplot(summed, aes(x = factor(year), y = totaldam)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Total Damage in 1000 USD (adjusted) by Year",
    x = "Year",
    y = "Total Damage in 1000 USD (adjusted)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(summed, aes(x = factor(year), y = logdam)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Total Damage in 1000 USD (adjusted) by Year",
    x = "Year",
    y = "Log of Total Damage in 1000 (adjusted) USD") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Total Damage by Subregion

summed = total %>% group_by(`Subregion`) %>% summarize(totaldam = sum(`Total Damage, Adjusted ('000 US$)`)) %>% mutate(logdam = log(totaldam))

ggplot(summed, aes(x = factor(Subregion), y = totaldam)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Total Damage in 1000 USD (adjusted) by Subregion",
    x = "Subregion",
    y = "Total Damage in 1000 USD (adjusted)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(summed, aes(x = factor(Subregion), y = logdam)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Total Damage in 1000 USD (adjusted) by Subregion",
    x = "Subregion",
    y = "Log of Total Damage in 1000 (adjusted) USD") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Total Damage by Disaster Type

summed = total %>% group_by(`Disaster Type`) %>% summarize(totaldam = sum(`Total Damage, Adjusted ('000 US$)`)) %>% mutate(logdam = log(totaldam))

ggplot(summed, aes(x = factor(`Disaster Type`), y = totaldam)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Total Damage in 1000 USD (adjusted) by Disaster Type",
    x = "Disaster Type",
    y = "Total Damage in 1000 USD (adjusted)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(summed, aes(x = factor(`Disaster Type`), y = logdam)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Total Damage in 1000 USD (adjusted) by Disaster Type",
    x = "Disaster Type",
    y = "Log of Total Damage in 1000 (adjusted) USD") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Total Affected

affected = data %>% filter(!is.na(`Total Affected`))
summed = affected %>% group_by(`End Year`) %>% summarize(totalaff = sum(`Total Affected`)) %>% rename(year = `End Year`) 

ggplot(summed, aes(x = factor(year), y = totalaff)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Number of Total Affected Persons by Year",
    x = "Year",
    y = "Number of Total Affected Persons") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Total Affected by Region

summed = affected %>% group_by(`Subregion`) %>% summarize(totalaff = sum(`Total Affected`))

ggplot(summed, aes(x = factor(Subregion), y = totalaff)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Number of Total Affected Persons by Subregion",
    x = "Subregion",
    y = "Number of Total Affected Persons") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Total Affected by Disaster Type

summed = affected %>% group_by(`Disaster Type`) %>% summarize(totalaff = sum(`Total Affected`))

ggplot(summed, aes(x = factor(`Disaster Type`), y = totalaff)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Number of Total Affected Persons by Disaster Type",
    x = "Disaster Type",
    y = "Number of Total Affected Persons") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Total deaths

deaths = data %>% filter(!is.na(`Total Deaths`))
summed = deaths %>% group_by(`End Year`) %>% summarize(totaldea = sum(`Total Deaths`)) %>% rename(year = `End Year`) 

ggplot(summed, aes(x = factor(year), y = totaldea)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Number of Total Deaths by Year",
    x = "Year",
    y = "Number of Total Deaths") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Total Deaths by Subregion

summed = deaths %>% group_by(`Subregion`) %>% summarize(totaldea = sum(`Total Deaths`))

ggplot(summed, aes(x = factor(Subregion), y = totaldea)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Number of Total Deaths by Subregion",
    x = "Subregion",
    y = "Number of Total Deaths") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Total Deaths by Disaster Type

summed = deaths %>% group_by(`Disaster Type`) %>% summarize(totaldea = sum(`Total Deaths`))

ggplot(summed, aes(x = factor(`Disaster Type`), y = totaldea)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Number of Total Deaths by Disaster Type",
    x = "Disaster Type",
    y = "Number of Total Deaths") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
