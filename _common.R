# Install packages

# install.packages(
#   c("HistData", "GGally", "palmerpenguins", "lubridate", 
#     "countdown", "knitr", "cowplot", "tidyverse", "here", 
#     "ggrepel", "RColorBrewer", "scales", "MASS", "viridis", 
#     "ggforce", "jph", "fontawesome", "metathis", "janitor", 
#     "maps", "mapproj", "sf", "rnaturalearth", "remotes",
#     geomtextpath", "gsheet", "hrbrthemes",
#     "magick", "quarto", "readxl", "rmarkdown", "rvest",
#     "waffle", "xaringanExtra")
# )
# remotes::install_github("ropensci/rnaturalearthhires")
# remotes::install_github("ropensci/rnaturalearthdata")

# General packages
library(tidyverse)
library(here)
library(countdown)
library(readxl)
library(kableExtra)

# Plotting packages
library(cowplot)
library(geomtextpath)
library(waffle)
library(lubridate)
library(knitr)
library(ggrepel)
library(viridis)
library(ggforce)
library(RColorBrewer)
library(scales)
library(gganimate)
library(magick)
library(ggridges)
library(hrbrthemes)
library(RColorBrewer)

# Data packages
library(HistData)
library(GGally)
library(palmerpenguins)

# Misc packages
library(fontawesome)
library(metathis)
library(janitor)

# Mapping packages
library(maps)
library(mapproj)
library(sf)

set.seed(42)

# Setting options

theme_set(theme_cowplot(font_size = 20))

options(
    dplyr.print_min = 6,
    dplyr.print_max = 6,
    stringr.view_n = 10,
    pillar.bold = TRUE,
    width = 77 # 80 - 3 for #> comment
)

knitr::opts_chunk$set(
    comment    = "#>",
    fig.retina = 3,
    fig.width  = 6,
    fig.height = 4,
    fig.show = "hold",
    fig.align  = "center",
    fig.path   = "figs/",
    warning = FALSE,
    message = FALSE
)
 
# Load data

internet_regions <- read_csv(here('data', 'internet_users_region.csv'))
college_all_ages <- read_csv(here('data', 'college_all_ages.csv'))
transit_cost <- read_csv(here::here('data', 'transit_cost.csv'))
avengers         <- read_csv(here::here('data', 'avengers.csv'))
bears            <- read_csv(here::here('data', 'north_america_bear_killings.csv'))
msleep           <- read_csv(here::here('data', 'msleep.csv'))
gapminder        <- read_csv(here::here('data', 'gapminder.csv'))
global_temps     <- read_csv(here::here('data', 'nasa_global_temps.csv'))

hotdogs          <- read_csv(here::here('data', 'hot_dog_winners.csv'))
hotdogs_mens <- hotdogs %>%
    filter(Competition == 'Mens') %>%
    rename(dogs = `Dogs eaten`,
           record = `New record`) %>%
    mutate(
        record = if_else(record == 1, 'Yes', 'No'),
        Winner = fct_other(Winner,
            keep = c('Joey Chestnut', 'Takeru Kobayashi')))

internet_region  <- read_csv(here::here('data', 'internet_users_region.csv'))
internet_country <- read_csv(here::here('data', 'internet_users_country.csv'))
internet_users  <- read_csv(here::here('data', 'internet_users_country.csv'))
internet_country_summary <- internet_country %>%
    filter(country %in% c(
        'United States', 'China',
        'Singapore', 'Cuba')) %>%
    mutate(
        label = ifelse(year == max(year), country, NA))

internet_region_summary <- internet_region %>%
    mutate(
        numUsers = numUsers / 10^9,
        label    = ifelse(year == max(year), region, NA))


lotr_words <- read_csv(here('data', 'lotr_words.csv'))
lotr_summary <- lotr_words %>%
    gather(key = 'gender', value = 'wordCount', Female:Male) %>%
    group_by(Race, gender) %>%
    summarise(wordCount = sum(wordCount)) %>%
    ungroup() %>%
    mutate(Race = fct_reorder(Race, desc(wordCount)))
    
us_covid         <- read_csv(here::here('data', 'us_covid.csv'))
us_diseases      <- read_csv(here::here('data', 'us_contagious_diseases.csv'))
us_coffee_shops <- read_csv(here::here('data', 'us_coffee_shops.csv'))
marathon <- read_csv(here::here('data', 'marathon.csv'))

tb_cases <- read_csv(here::here('data', 'tb_cases.csv'))
tb_rates <- table3
tb_cases <- read_csv(here('data', 'tb_cases.csv'))

fed_spend_long <- read_csv(here('data', 'fed_spend_long.csv'))
fed_spend_wide <- read_csv(here('data', 'fed_spend_wide.csv'))
fed_spend <- fed_spend_long
federal_spending <- fed_spend_long
federal_spending_2017 <- federal_spending %>%
    filter(year == 2017) %>%
    mutate(rd_budget_mil = rd_budget_mil / 10^3) %>%
    mutate(department = fct_reorder(
        department, rd_budget_mil))

federal_spending_summary <- federal_spending %>%
    mutate(department = fct_other(
        department, keep = 'DOD')) %>%
    group_by(department, year) %>%
    summarise(rd_budget_mil = sum(rd_budget_mil) / 10^3) %>%
    ungroup() %>%
    mutate(department = fct_relevel(
        department, c('Other', 'DOD')))

daysToShip <- data.frame(
    order = seq(12),
    warehouseA = c(3,3,3,4,4,4,5,5,5,5,5,5),
    warehouseB = c(1,1,1,3,3,4,5,5,5,6,7,10)
)

milk_production  <- read_csv(here::here('data', 'milk_production.csv'))
milk_region <- milk_production %>%
    filter(region %in% c(
        'Pacific', 'Northeast', 'Lake States', 'Mountain')) %>%
    group_by(year, region) %>%
    summarise(milk_produced = sum(milk_produced)) %>%
    ungroup() %>%
    mutate(label = ifelse(year == max(year), region, NA))
milk_2017 <- milk_production %>% 
  filter(year == 2017) %>% 
  dplyr::select(name = state, milk_produced) %>% 
  mutate(milk_produced = milk_produced / 10^9)
milk_summary_2017 <- milk_production %>%
  filter(year == 2017) %>%
  mutate(state = fct_other(state,
    keep = c('California', 'Wisconsin'))) %>%
  group_by(state) %>%
  summarise(milk_produced = sum(milk_produced)) %>%
  mutate(milk_produced = milk_produced / 10^9)
milk_compare <- milk_production %>%
  filter(year %in% c(1970, 2017)) %>%
  mutate(state = fct_other(state,
    keep = c('California', 'Wisconsin'))) %>%
  group_by(year, state) %>%
  summarise(milk_produced = sum(milk_produced) / 10^9)
milk_top10states <- milk_production %>%
  filter(year == 2017) %>%
  arrange(desc(milk_produced)) %>%
  slice(1:10) %>%
  mutate(
    milk_produced = milk_produced / 10^9,
    state = fct_reorder(state, milk_produced))
milk_summary_dumbbell <- milk_production %>%
  filter(
    year %in% c(1970, 2017),
    state %in% milk_top10states$state) %>%
  mutate(
    # Reorder state variables to follow top 10 states
    state = fct_relevel(state, levels(milk_top10states$state)),
    # Convert year to discrete variable
    year = as.factor(year),
    # Modify the units
    milk_produced = milk_produced / 10^9)
milk_ca <- milk_production %>%
    filter(state == 'California')
milk_ca_sparse <- milk_ca %>%
    mutate(yearColor = ifelse(year %in% seq(1970, 2020, 10), 'one', 'two'))
milk_race <- milk_production %>%
    group_by(year) %>%
    mutate(
        rank = rank(-milk_produced),
        Value_rel = milk_produced / milk_produced[rank==1],
        Value_lbl = paste0(' ', round(milk_produced))) %>%
    group_by(state) %>%
    filter(rank <= 10) %>%
    ungroup() %>%
    mutate(year = as.integer(year))

pv_cells <- read_excel(
    here('data', 'pv_cell_production.xlsx'),
    sheet = 'Cell Prod by Country', skip = 2) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year))

us_gdp <- read_csv(here::here('data', 'total_gdp_us_inflation_adjusted.csv'))
us_gdp <- us_gdp %>%
    gather(key = year, value = gdp, `1960`:`2017`) %>%
    filter(country == 'United States') %>%
    mutate(gdp = gdp / 10^12,
           year = as.numeric(year))

wildlife_impacts <- read_csv(here::here('data', 'wildlife_impacts.csv'))
arrivals <- c('approach', 'arrival', 'descent', 'landing roll')
departures <- c('climb', 'departure', 'take-off run')
wildlife_impacts_2016 <- wildlife_impacts %>%
  filter(incident_year == 2016) %>%
  mutate(
    phase_of_flt = str_to_lower(phase_of_flt),
    phase_of_flt = case_when(
      phase_of_flt %in% arrivals ~ 'arrival',
      phase_of_flt %in% departures ~ 'departure',
      TRUE ~ 'other'),
    phase_of_flt = str_to_title(phase_of_flt))
wildlife_costs <- wildlife_impacts %>%
    rename(cost = cost_repairs_infl_adj) %>%
    filter(! is.na(cost)) %>%
    mutate(cost = cost / 10^6,
           incident_date = as.Date(incident_date))

measles <- us_diseases %>%
  filter(
    disease == 'Measles',
    !state %in% c("Hawaii", "Alaska")) %>%
  mutate(
    rate = (count / population) * 10000,
    state = fct_reorder(state, rate)) %>%
  # Compute annual mean rate across all states
  group_by(year) %>%
  mutate(
    mean_rate = sum(count) / sum(population) * 10000)

measles_ca <- measles %>%
    filter(state == "California")

measles_us <- measles %>%
    group_by(year) %>%
    summarize(rate = sum(count) / sum(population) * 10000)

# Load state_abbs data frame, containing state abbreviations 
source(here::here('data', 'state_abbs.R'))