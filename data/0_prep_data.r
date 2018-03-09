require(tidyverse)
require(haven)
require(broom)
require(maps)
require(leaflet)

### Define Bins and color palette ----------------------------------------------

bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
pal <- colorBin("Blues", domain = c(0, 1), bins)

### Read and manipulate original data ------------------------------------------

dat_full <- haven::read_dta("data/SVMDI_new.dta")

dat_clean <- dat_full %>% 
  select(country, iso, year, continent, region, 
         csvmdi, csvmdimin, csvmdimax, dsvmdi) %>% 
  mutate(year        = as.integer(year),
         country = recode(
           country,
           "Congo_Democratic Republic of" = "Democratic Republic of the Congo",
           "Congo_Republic of the"        = "Republic of Congo",
           "United Kingdom" = "UK",
           "East Timor"     = "Timor-Leste",
           "United States"  = "USA",
           "Korea_South"    = "South Korea",
           "Korea_North"    = "North Korea",
           "Burma_Myanmar"  = "Myanmar",
           "St. Lucia"      = "Saint Lucia"
         )) %>% 
  filter(!(country %in% c("St. Kitts and Nevis", "Azores", 
                          "St. Vincent and the Grenadines", 
                          "Trinidad and Tobago",
                          "Antigua and Barbuda",
                          "Germany (East)",
                          "Madeira Islands", "Kosovo"))) %>% 
  group_by(year, region) %>% 
  mutate("region_csvmdi" = mean(csvmdi, na.rm = TRUE),
         "region_csvmdimin" = mean(csvmdimin, na.rm = TRUE),
         "region_csvmdimax" = mean(csvmdimax, na.rm = TRUE)) %>% 
  ungroup()

### Country/region polygon data --------------------------------------------------------------------

dat_leaflet <- maps::map(database = "world", regions = iso.expand(unique(dat_clean$iso)), 
                         fill = TRUE, plot = FALSE) %>% 
  broom::tidy() %>% 
  rename(country = region) %>% 
  mutate(country = recode(
    country,
    "Canary Islands"  = "Spain",
    "Azores"          = "Portugal",
    "Madeira Islands" = "Portugal"
  )) %>% 
  split(list(.$group)) %>% 
  purrr::map(add_row) %>% 
  purrr::map(fill, group, country, subregion) %>% 
  bind_rows() %>% 
  group_by(country, group) %>% 
  nest() %>% 
  inner_join(dat_clean, by = "country") %>% 
  select(country, group, year, data, region, csvmdi, region_csvmdi) %>% 
  mutate(color_country = pal(.$csvmdi),
         color_region = pal(.$region_csvmdi))

### Save -------------------------------------------------------------------------------------------
save(list = c("dat_full", "dat_clean", "dat_leaflet", "bins", "pal"), file = "data/dat_csvmdi.RData")
save(list = c("dat_clean"), file = "data/dat_clean.RData")
