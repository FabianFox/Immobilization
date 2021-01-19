# Load DEMIG VISA
# Data from https://www.migrationinstitute.org/data/demig-data/demig-visa-data

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "lubridate","rio", "janitor", "countrycode", "states")

# Issues
# Check country codes; so far multiple erroneous entries

# Load data
### ------------------------------------------------------------------------ ###
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
                                 .))))

# Deal with erroneous entries of Japan and Lesotho (i.e. nationality != nationality_iso3)
# Wrong entries start in row nationality == "Northern Mariana Islands"
# Use donor rows to correct errors (here: AFG)
demig.df[demig.df$destination_iso3 %in% c("JPN", "LSO"),]$nationality_iso3 <- 
  demig.df[demig.df$destination_iso3 %in% "AFG",][
    match(demig.df[demig.df$destination_iso3 %in% c("JPN", "LSO"),]$nationality, 
          demig.df[demig.df$destination_iso3 %in% "AFG",]$nationality), 
  ]$nationality_iso3

# Limit DEMIG Visa to countries that report as sending and receiving country
demig.df <- demig.df %>%
    filter(
    # Vietnam, Rep of (South Vietnam) and Vietnam, Socialist Rep of the same iso3 code
    !destination == "Vietnam, Rep of (South Vietnam)",
    # missing: French West Indies
    !is.na(destination_iso3),
    # both sending and receiving reports
    nationality_iso3 %in% unique(destination_iso3),
    destination_iso3 %in% unique(nationality_iso3))

# Limit DEMIG Visa to independent states
### ------------------------------------------------------------------------ ###
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

# Remove countries with excessive missing values
### ------------------------------------------------------------------------ ###
# Countries with many missing values
missing <- demig.df %>%
  rowwise() %>%
  mutate(row_missing = sum(is.na(c_across(num_range("x", 1993:2013))))) %>%
  filter(row_missing > 0) %>%
  count(destination_iso3) %>%
  filter(n == 171)

# Remove counties with multiple missing values
demig.df <- demig.df %>%
  filter(!destination_iso3 %in% missing$destination_iso3,
         !nationality_iso3 %in% missing$destination_iso3)

# Second stage missing

# Long format 
### ------------------------------------------------------------------------ ###
demig_long.df <- demig.df %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "visa_restriction") %>%
  mutate(year = strtoi(str_extract(year, "[:digit:]{4}")),
         visa_restriction = if_else(destination_iso3 == nationality_iso3, 0, 
                                    visa_restriction))
