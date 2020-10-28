# Step 1: import libraries ---------------------------------------

# Import libraries
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
options(scipen=999)

# Step 2: produce excess mortality estimates for Italy's comunes, provinces and regions ---------------------------------------

# Import Italy's data
italy_comunes <- read_excel("source-data/italy-excess-deaths/italy_comunes.xlsx")
italy_total_source_latest <- fread("source-data/italy-excess-deaths/italy_total_source_2020_06_30.csv")

# Create list of Italian comunes with reliable data
italy_comunes_reliable <- italy_total_source_latest %>%
  filter(GE <= 630, T_20 != "n.d.") %>% # Filter out any comunes missing data before June 30th
  dplyr::select(COD_PROVCOM) %>%
  distinct() %>%
  pull()

# Group total deaths by month and comune
italy_comunes_monthly_total_deaths <- italy_total_source_latest %>%
  mutate(comune_code = COD_PROVCOM,
         month = as.numeric(round(GE/100))) %>%
  filter(month <= 6, T_20 != "n.d.") %>% # Remove any missing days
  group_by(comune_code,month) %>%
  summarise(`2015` = sum(T_15,na.rm=T),
            `2016` = sum(T_16,na.rm=T),
            `2017` = sum(T_17,na.rm=T),
            `2018` = sum(T_18,na.rm=T),
            `2019` = sum(T_19,na.rm=T),
            `2020` = sum(as.numeric(T_20),na.rm=T)) %>%
  gather("year","total_deaths",
         -c(comune_code,month)) %>%
  mutate(year = as.numeric(year)) %>%
  ungroup() %>%
  left_join(italy_comunes)

# Calculate excess deaths by month and comune
italy_comunes_monthly_excess_deaths <- italy_comunes_monthly_total_deaths %>%
  filter(year == 2020) %>%
  left_join(italy_comunes_monthly_total_deaths %>%
              filter(year <= 2019) %>%
              group_by(comune_code,month) %>%
              summarise(expected_deaths = sum(total_deaths,na.rm=T) / 5)) %>%
  mutate(excess_deaths = total_deaths - expected_deaths,
         excess_deaths_pct = (total_deaths / expected_deaths) - 1,
         excess_deaths_per_100 = excess_deaths / population * 100)

# Calculate overall excess deaths (March to June) by comune
italy_comunes_overall_excess_deaths <- italy_comunes_monthly_excess_deaths %>%
  filter(month >= 3) %>%
  group_by(country,region,region_code,province,province_code,comune,comune_code,population) %>%
  summarise(total_deaths = sum(total_deaths,na.rm=T),
            expected_deaths = sum(expected_deaths,na.rm=T),
            excess_deaths = sum(excess_deaths,na.rm=T)) %>%
  mutate(excess_deaths_pct = (total_deaths / expected_deaths) - 1,
         excess_deaths_per_100 = excess_deaths / population * 100)

# Export excess deaths by comune
fwrite(italy_comunes_overall_excess_deaths,"output-data/italy_comunes_overall_excess_deaths.csv")

# Calculate overall excess deaths (March to June) by province
italy_provinces_overall_excess_deaths <- italy_comunes_overall_excess_deaths %>%
  drop_na() %>%
  group_by(country,region,region_code,province,province_code) %>%
  summarise(population = sum(population,na.rm=T),
            total_deaths = sum(total_deaths,na.rm=T),
            expected_deaths = sum(expected_deaths,na.rm=T),
            excess_deaths = sum(excess_deaths,na.rm=T)) %>%
  mutate(excess_deaths_pct = (total_deaths / expected_deaths) - 1,
         excess_deaths_per_100 = excess_deaths / population * 100)

# Export excess deaths by province
fwrite(italy_provinces_overall_excess_deaths,"output-data/italy_provinces_overall_excess_deaths.csv")

# Calculate overall excess deaths (March to June) by region
italy_regions_overall_excess_deaths <- italy_comunes_overall_excess_deaths %>%
  drop_na() %>%
  group_by(country,region,region_code) %>%
  summarise(population = sum(population,na.rm=T),
            total_deaths = sum(total_deaths,na.rm=T),
            expected_deaths = sum(expected_deaths,na.rm=T),
            excess_deaths = sum(excess_deaths,na.rm=T)) %>%
  mutate(excess_deaths_pct = (total_deaths / expected_deaths) - 1,
         excess_deaths_per_100 = excess_deaths / population * 100)

# Export excess deaths by region
fwrite(italy_regions_overall_excess_deaths,"output-data/italy_regions_overall_excess_deaths.csv")

# Step 3: import case numbers for Italy's comunes and provinces ---------------------------------------

# Import case numbers for Lombardy's comunes
lombardy_comunes_cases_2020_09_01 <- fread("source-data/lombardy-cases/lombardy_comunes_cases_2020_09_01.csv") %>% drop_na(comune_code)
lombardy_comunes_cases_2020_10_25 <- fread("source-data/lombardy-cases/lombardy_comunes_cases_2020_10_25.csv") %>% drop_na(comune_code)

# Calculate number of second wave cases in Lombardy's comunes
lombardy_comunes_second_wave_cases <- lombardy_comunes_cases_2020_10_25 %>%
  mutate(cases_2020_10_25 = cases) %>%
  left_join(lombardy_comunes_cases_2020_09_01 %>%
              mutate(cases_2020_09_01 = cases) %>%
              dplyr::select(comune_code,cases_2020_09_01)) %>%
  mutate(second_wave_cases = cases_2020_10_25 - cases_2020_09_01) %>%
  filter(second_wave_cases >= 0) %>%
  dplyr::select(comune,comune_code,cases_2020_09_01,cases_2020_10_25,second_wave_cases)
  
# Create daily time series of covid deaths by province
italy_provinces_daily_covid_cases <- fread("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv") %>%
  mutate(date = as.Date(data),
         country = "Italy",
         region = denominazione_regione,
         region_code = codice_regione,
         province = denominazione_provincia,
         province_code = codice_provincia,
         cumulative_cases = totale_casi) %>%
  dplyr::select(date, country, region, region_code, province, province_code, cumulative_cases) %>%
  filter(province_code <= 111) %>%
  arrange(province_code,date) %>%
  group_by(province_code) %>%
  mutate(previous_day_cases = lag(cumulative_cases),
         new_cases = cumulative_cases - previous_day_cases) %>%
  ungroup()

# Step 4: compare excess deaths to second wave cases in Italy's comunes and provinces ---------------------------------------

# Compare each comune's excess deaths and second wave cases
lombardy_comunes_excess_deaths_vs_second_wave_cases <- italy_comunes_overall_excess_deaths %>%
  left_join(lombardy_comunes_second_wave_cases) %>%
  drop_na() %>%
  mutate(second_wave_cases_per_100k = second_wave_cases / population * 100000)

# Export dataframe of comunes
fwrite(lombardy_comunes_excess_deaths_vs_second_wave_cases,"output-data/lombardy_comunes_excess_deaths_vs_second_wave_cases.csv")

# Plot comunes' excess deaths vs second wave cases
lombardy_comunes_plot <- ggplot(lombardy_comunes_excess_deaths_vs_second_wave_cases %>% filter(population >= 4000),
                                          aes(x = excess_deaths_per_100,
                                              y = second_wave_cases_per_100k,
                                              size = population)) +
  geom_point() + 
  geom_smooth(weight="population") +
  ylim(0,1000)

# Show plot
lombardy_comunes_plot

# Calculate average rate of second wave cases in comunes, grouped by rate of excess deaths
grouped_lombardy_comunes_excess_deaths_vs_second_wave_cases <- lombardy_comunes_excess_deaths_vs_second_wave_cases %>%
  mutate(grouped_excess_deaths = case_when(excess_deaths_per_100 <= 0 ~ "0.0_or_less",
                                           excess_deaths_per_100 > 0 & excess_deaths_per_100 <= 0.1 ~ "0.0_to_0.1",
                                           excess_deaths_per_100 > 0.1 & excess_deaths_per_100 <= 0.2 ~ "0.1_to_0.2",
                                           excess_deaths_per_100 > 0.2 & excess_deaths_per_100 <= 0.3 ~ "0.2_to_0.3",
                                           excess_deaths_per_100 > 0.3 & excess_deaths_per_100 <= 0.4 ~ "0.3_to_0.4",
                                           excess_deaths_per_100 > 0.4 & excess_deaths_per_100 <= 0.5 ~ "0.4_to_0.5",
                                           excess_deaths_per_100 > 0.5 ~ "0.5_or_more")) %>%
  group_by(country,region,region_code,grouped_excess_deaths) %>%
  summarise(comunes = n(),
            second_wave_cases_per_100k = weighted.mean(second_wave_cases_per_100k,population))

# Export grouped dataframe
fwrite(grouped_lombardy_comunes_excess_deaths_vs_second_wave_cases,"output-data/grouped_lombardy_comunes_excess_deaths_vs_second_wave_cases.csv")

# Compare each province's excess deaths and new cases
italy_provinces_excess_deaths_vs_second_wave_cases <- italy_provinces_overall_excess_deaths %>%
  left_join(italy_provinces_daily_covid_cases %>%
              filter(date >= as.Date("2020-09-01")) %>%
              group_by(province_code) %>%
              summarise(second_wave_cases = sum(new_cases,na.rm=T))) %>%
  mutate(second_wave_cases_per_100k = second_wave_cases / population * 100000)
