# Regional integration

# Load/install packages
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rvest", "rio", "docxtractr", "countrycode")

# All regional integration entities in RIKS
reg_int.df <- read_docx("./data/Regional Integration/AgreementList_August2020_0.docx") %>%
  docx_extract_tbl(., 1)

# Base URL 
base_url <- "https://riks.cris.unu.edu/country-organisation?filter=organisation&organisations="
  
# Create URLs to information on membership
reg_int.df <- reg_int.df %>%
  mutate(url = paste0(base_url, code))

# Create a function that grabs the tables 
riks_scrape_fun <- function(x){
  read_html(x) %>%
    html_table(".result__table > tbody", header = TRUE) %>%
    .[[1]]
}

# Safe function
riks_scrape_fun <- possibly(riks_scrape_fun, otherwise = NA)

# Map over urls
reg_int.df <- reg_int.df %>%
  mutate(tables = imap(.x = url, ~ {
    print(paste("Iterating over", .y))
    Sys.sleep(sample(3:10, 1))
    riks_scrape_fun(.x)
  }))

# Some connections are unstable; repeat for those entries
reg_missing.df <- reg_int.df %>%
  filter(is.na(tables)) %>%
  mutate(tables = imap(.x = url, ~ {
    print(paste("Iterating over", .y))
    Sys.sleep(sample(3:10, 1))
    riks_scrape_fun(.x)
  }))

# Update rows
reg_int.df <- reg_int.df %>%
  rows_update(reg_missing.df %>%
                select(code, tables), by = "code")

# Load data
# export(reg_int.df, "./data/Regional Integration/RegMembership.rds")
reg_int.df <- import("./data/Regional Integration/RegMembership.rds")

# Empty tables
reg_int.df <- reg_int.df %>%
  mutate(empty_table = map(tables, ~dim(.x) %>%
                             .[1] == 0)) %>%
  filter(empty_table == FALSE)

# Clean data
reg_int.df <- reg_int.df %>%
  mutate(
    # lower case variable names
    tables = map(tables, ~clean_names(.x)),
    # clean character strings
         tables = map(tables, ~.x %>%
                        mutate(range = qdap::bracketX(range))),
    # separate joined - left
         tables = map(tables, ~.x %>%
                        separate(range, into = c("joined", "current_status")) %>%
                        # turn character into numeric
                        mutate(current_status = str_replace(current_status, "Present", "2020"),
                               across(c("joined", "current_status"), ~strtoi(.x)),
                               # iso3 code (missing: Sahrawi Arab Dem. Rep. / Serbia-Montenegro / Czechoslovakia / Yugoslavia)
                               country_iso3 = countrycode(country, "country.name.en", "iso3c", 
                                                          custom_match = c("Yemen Arab Rep." = "YEM",
                                                                           "Central African Rep." = "CAF",
                                                                           "Kosovo" = "XKX")))))

# Data from Czaika et al. (2018) "The Global Evolution of Travel Regimes"
reg_select_int.df <- tribble(
  ~code, ~name,
  "AFTA", "ASEAN Free Trade Area", 
  "APTA", "Asia Pacific Trade Agreement", 
  "CACM", "Central American Common Market",
  "CAN", "Andean Community",
  "CARICOM", "Caribbean Community",
  "CEFTA", "Central European Free Trade Agreement",
  "CEMAC", "Economic and Monetary Community of Central Africa",
  "CIS", "Commonwealth of Independent States Free Trade Agreement",
  "COMESA", "Common Market for Eastern and Southern Africa",
  "EAC", "East African Community",
  "ECOWAS", "Economic Community of West African States",
  "EFTA", "European Free Trade Association",
  "EU", "European Union",
  "GCC", "Gulf Cooperation Council",
  "LAIA", "Latin American Integration Association",
  "MERCOSUR", "Mercado Común del Sur — Common market of the South",
  "NAFTA", "North American Free Trade Agreement",
  "PAFTA", "Pan-Arab Free Trade Area",
  "SACU", "Southern African Customs Union",
  "SADC", "Southern African Development Community",
  "SAFTA", "South Asian Free Trade Agreement") %>%
  mutate(code = case_when(code == "AFTA" ~ "ASEAN", 
                          code == "SAFTA" ~ "SAARC",
                          TRUE ~ code))

# Add missing economic blocs included in Czaika et al. (2018)
# APTA, PAFTA, SAFTA
reg_add.df <- reg_select_int.df %>%
  filter(!reg_select_int.df$code %in% reg_int.df$code) %>%
  mutate(
    tables = 
      
    list(
      tibble(
        country = c("Bangladesh", "China", "India", "Korea, Rep.", "Laos", 
                  "Sri Lanka", "Mongolia"),
        joined = c(1975, 2001, 1975, 1975, 1975, 1975, 2013),
        current_status = rep(2020, 7),
        country_iso3 = countrycode(country, "country.name.en", "iso3c")),
      tibble(
        country = c("Algeria", "Bahrain", "Egypt", "Iraq", "Jordan", "Kuwait", 
                  "Lebanon", "Morocco", "Oman", "Palestine", "Qatar", 
                  "Saudi Arabia", "Sudan", "Syria", "Tunisia", 
                  "United Arab Emirates", "Yemen"),
        joined = rep(2005, 17),
        current_status = rep(2020, 17), 
        country_iso3 = countrycode(country, "country.name.en", "iso3c"))),
    
    url = NA,
    empty_table = list(FALSE))

# Join to main data
reg_select_int.df <- reg_int.df %>%
  filter(code %in% reg_select_int.df$code) %>%
  bind_rows(reg_add.df)
  
# Export
