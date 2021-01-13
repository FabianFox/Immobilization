# Load DEMIG VISA
# Data from https://www.migrationinstitute.org/data/demig-data/demig-visa-data

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "janitor", "countrycode")

# Load data
### ------------------------------------------------------------------------###
demig.df <- import("./data/DEMIG Visa/DEMIG VISA Database_version 1.4.xlsx",
                   skip = 1) %>% 
  clean_names() %>%
  select(destination = country_of_visa_issuance, 
         nationality = `nationality_of_traveller`,
         destination_iso3 = un_3_digit_code_3,
         nationality_iso3 = un_3_digit_code_7,
         num_range("x", 1973:2014))
