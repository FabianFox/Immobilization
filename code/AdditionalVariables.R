# Additional variables

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "lubridate","rio", "janitor", "countrycode", "states", 
            "pwt10", "wbstats", "zoo")

# DEMIG Visa data (from: Load_DEMIG.R)
demig.df <- import("./data/DEMIG Visa/DEMIG_1993-2013.rds")

# Penn World House 10.0
# Load
data("pwt10.0")

# Limit
pwt.df <- pwt10.0 %>%
  filter(between(year, 1993, 2013)) %>%
  select("country_iso3" = isocode, year, "gdp" = rgdpo) %>%
  filter(country_iso3 %in% unique(demig.df$destination_iso3))

# Join to DEMIG Visa
visa.df <- demig.df %>%
  left_join(y = pwt.df, by = c("destination_iso3" = "country_iso3", "year"))

# World Bank Data
wb.info <- wb_data(country = unique(visa.df$destination_iso3),
                   indicator = c("NY.GDP.PCAP.CD", "NY.GDP.PCAP.PP.CD"), 
                   start_date = 1993, end_date = 2013, return_wide = TRUE) %>%
  select("country_iso3" = iso3c, "year" = date, "gdp_cons" = NY.GDP.PCAP.CD, "gdp_ppp" = NY.GDP.PCAP.PP.CD)

# Join to DEMIG Visa
visa.df <- demig %>%
  left_join(y = wb.info, by = c("destination_iso3" = "country_iso3", "year"))

# Interpolate missing values
### ------------------------------------------------------------------------ ###
wb.info <- wb.info %>%
  arrange(country_iso3, year) %>%
  mutate(ip.gdp = na.approx(gdp_ppp, rule = 2)) # maxgap = 4
