# Load DEMIG VISA
# Data from https://www.migrationinstitute.org/data/demig-data/demig-visa-data

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "lubridate","rio", "janitor", "countrycode", "states")

# Load data
### ------------------------------------------------------------------------###
# DEMIG Visa
# Load
demig.df <- import("./data/DEMIG Visa/DEMIG VISA Database_version 1.4.xlsx",
                   skip = 1) %>% 
  clean_names() %>%
  select(destination = country_of_visa_issuance, 
         nationality = `nationality_of_traveller`,
         destination_iso3 = un_3_digit_code_3,
         nationality_iso3 = un_3_digit_code_7,
         policy_measure,
         num_range("x", 1993:2013)) %>%
  filter(policy_measure == "Visa") %>%
  # wrong iso3 codes
  mutate(across(c(destination_iso3, nationality_iso3), 
                ~if_else(. == "ROM", "ROU", 
                         if_else(. == "DMA", "DOM", 
                                 .)))) %>%
  # missing: French West Indies
  filter(
    !is.na(destination_iso3),
    nationality_iso3 %in% unique(destination_iso3),
    destination_iso3 %in% unique(nationality_iso3))

# Limit DEMIG Visa to independent states
### ------------------------------------------------------------------------###
# Gleditsch & Ward (2007) Independent states
# Load
data(gwstates)

# Create iso3 codes that match DEMIG Visa
gwstates.df <- gwstates %>%
  filter(year(end) == 9999 & microstate == FALSE) %>%
  mutate(iso3c = countrycode(country_name, "country.name.en", "iso3c",
                             custom_match = c("Korea, People's Republic of" = "PRK",
                                              "Kosovo" = "XKX",
                                              "Yemen (Arab Republic of Yemen)" = "YEM")))

# Limit DEMIG Visa to independent states (gwstates.df)
demig.df <- demig.df %>%
  filter(destination_iso3 %in% gwstates.df$iso3c,
         nationality_iso3 %in% gwstates.df$iso3c)

# 
### ------------------------------------------------------------------------###
demig_long.df <- demig.df %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "visa_restriction") %>%
  mutate(year = strtoi(str_extract(year, "[:digit:]{4}")),
         visa_restriction = if_else(destination_iso3 == nationality_iso3, 0, 
                                    visa_restriction))
