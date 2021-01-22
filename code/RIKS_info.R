# Regional integration

# Load/install packages
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rvest", "docxtractr")

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
    Sys.sleep(sample(3:7, 1))
    riks_scrape_fun(.x)
  }))

# Some connections are unstable; repeat for those entries
reg_int_exp.df <- reg_int.df %>%
  mutate(missing = map_lgl(tables, is_null))

reg_int_exp.df <- reg_int_exp.df %>%
  mutate(tables = if_else(missing == TRUE,
                          imap(.x = url, ~ {
                                 print(paste("Iterating over", .y))
                                 Sys.sleep(sample(3:7, 1))
                                 riks_scrape_fun(.x)
                                 }), 
                          tables))
