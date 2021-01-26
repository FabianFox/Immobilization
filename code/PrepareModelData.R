# Data Preparation

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "lubridate","rio", "janitor", "countrycode", "states")

# Load data
### ------------------------------------------------------------------------ ###
# DEMIG Visa data (from: Load_DEMIG.R)
demig.df <- import("./data/DEMIG Visa/DEMIG_1993-2013.rds")

# Regional integration membership (from: RIKS.info.R)
reg_int.df <- import("./data/Regional Integration/RegMembership_Small.rds") %>%
  unnest(tables)

# Join RIKS data to DEMIG Visa
### ------------------------------------------------------------------------ ###
# Make RIKS longer (one row per membership-year by country)
reg_int_long.df <- reg_int.df %>%
  mutate(year = map2(joined, current_status, seq)) %>%
  unnest(year) %>%
  select(-joined, -current_status) %>%
  # collapse multi-membership countries
  group_by(country_iso3, year) %>%
  summarise(membership = paste0(code, collapse = "|"))

# Join the RIKS data to dyadic DEMIG Visa
visa.df <- demig.df %>%
  left_join(y = reg_int_long.df %>%
              select("membership_dest" = membership, year, country_iso3), by = c("destination_iso3" = "country_iso3", "year")) %>%
  left_join(y = reg_int_long.df %>%
              select("membership_nat" = membership, year, country_iso3), by = c("nationality_iso3" = "country_iso3", "year"))

# Transform to network
### ------------------------------------------------------------------------ ###
# Nest by year
demig_net.df <- visa.df %>%
  select(destination_iso3, nationality_iso3, year, visa) %>%
  group_by(year) %>%
  nest()

# Transform into network format
# Edgelist
demig_net.df <- demig_net.df %>%
  mutate(nodes = map(data, ~.x %>%
                       pull(destination_iso3) %>%
                       unique()),
         edges = map(data, ~.x %>%
                       filter(visa == 1) %>%
                       select(from = destination_iso3, 
                              to = nationality_iso3)))

# Igraph
demig_net.df <- demig_net.df %>%
  mutate(igraph = map2(.x = edges, .y = nodes, ~igraph::graph_from_data_frame(
    d = .x, 
    vertices = .y,
    directed = TRUE
  )))

# Adjacency matrix
demig_net.df <- demig_net.df %>%
  mutate(adj_matrix = map(igraph, ~igraph::get.adjacency(.x, sparse = FALSE)))

# Network
demig_net.df <- demig_net.df %>%
  mutate(network = map(adj_matrix, ~as.network(.x, directed = TRUE)))


# Switching visa regulations
### ------------------------------------------------------------------------ ###
switch.df <- demig_long.df %>%
  group_by(destination_iso3, nationality_iso3) %>%
  mutate(switch = ifelse(visa != lag(visa), 1, 0)) %>%
  summarise(num_switches = sum(switch, na.rm = TRUE)) %>%
  ungroup()
