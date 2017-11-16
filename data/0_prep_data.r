require(tidyverse)
require(haven)
require(broom)
require(maps)
require(leaflet)

### Define Bins and color palette ------------------------------------------------------------------

bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
pal <- colorBin("Blues", domain = c(0, 1), bins)

### Read and manipulate original data --------------------------------------------------------------

dat_full <- haven::read_dta("data/CSVMDI.dta")

dat_clean <- dat_full %>% 
  select(country_new, iso, year, continent, regioniii, 
         csvmdiq003, csvmdiq005, csvmdiq095, csvmdiq097, csvmdi, dsvmdi) %>% 
  mutate(year        = as.integer(year),
         country_new = recode(
           country_new,
           "Congo Dem Rep" = "Democratic Republic of the Congo",
           "Congo Rep"     = "Republic of Congo",
           "United Kingdom"= "UK",
           "East Timor"    = "Timor-Leste",
           "United States" = "USA",
           "St. Lucia"     = "Saint Lucia"
         )) %>% 
  filter(!(country_new %in% c("St. Kitts and Nevis", "Azores", 
                              "St. Vincent and the Grenadines", "Trinidad and Tobago",
                              "Madeira Islands", "Kosovo"))) %>% 
  group_by(year, regioniii) %>% 
  mutate("regioniii_csvmdi" = mean(csvmdi, na.rm = TRUE),
         "regioniii_csvmdiq003" = mean(csvmdiq003, na.rm = TRUE),
         "regioniii_csvmdiq005" = mean(csvmdiq005, na.rm = TRUE),
         "regioniii_csvmdiq095" = mean(csvmdiq095, na.rm = TRUE),
         "regioniii_csvmdiq097" = mean(csvmdiq097, na.rm = TRUE)) %>% 
  ungroup()

### Country/region polygon data --------------------------------------------------------------------

dat_leaflet <- maps::map(database = "world", regions = iso.expand(unique(dat_clean$iso)), 
                         fill = TRUE, plot = FALSE) %>% 
  broom::tidy() %>% 
  rename(country_new = region) %>% 
  mutate(country_new = recode(
    country_new,
    "Canary Islands"  = "Spain",
    "Azores"          = "Portugal",
    "Madeira Islands" = "Portugal"
  )) %>% 
  split(list(.$group)) %>% 
  purrr::map(add_row) %>% 
  purrr::map(fill, group, country_new, subregion) %>% 
  bind_rows() %>% 
  group_by(country_new, group) %>% 
  nest() %>% 
  inner_join(dat_clean, by = "country_new") %>% 
  select(country_new, group, year, data, regioniii, csvmdi, regioniii_csvmdi) %>% 
  mutate(color_country = pal(.$csvmdi),
         color_regioniii = pal(.$regioniii_csvmdi))

### Save -------------------------------------------------------------------------------------------
save(list = c("dat_full", "dat_clean", "dat_leaflet", "bins", "pal"), file = "data/dat_csvmdi.RData")