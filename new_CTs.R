# libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(magrittr)

# read xlsx ---------------------------------------------------------------

# file provided by Christian Fellner. Contains EIA, NEEDS, and NESHAP post-2018
path_new_CT <- "input/New Combustion Turbine Lists.xlsx"

ls_sheets_new_CT <- excel_sheets(path_new_CT)
ls_sheets_new_CT

tb_EIA_raw <- read_excel(path_new_CT, ls_sheets_new_CT[[1]])
tb_EIA_proposed_raw <- read_excel(path_new_CT, ls_sheets_new_CT[[2]])
tb_NEEDS_raw <- read_excel(path_new_CT, ls_sheets_new_CT[[3]])
tb_NESHAP_raw <- read_excel(path_new_CT, ls_sheets_new_CT[[4]])

# wrangle xlsx ---------------------------------------------------------------

tb_states <- 
  tibble(state = state.name,
         state_abb = state.abb)

tb_EIA <- tb_EIA_raw %>% 
  select(`Plant Name`, State, County) %>% 
  rename(plant_name = `Plant Name`, state_abb = State) %>% 
  rename_with(str_to_lower) %>% 
  mutate(EIA = T) %>% 
  left_join(tb_states) %>% 
  select(-state_abb) %>% 
  relocate(state, county) %>% 
  group_by(state, county, plant_name) %>% 
  mutate(unit = seq(n()), .after = plant_name) %>% 
  ungroup()

tb_EIA_proposed <- tb_EIA_proposed_raw  %>% 
  select(`Plant Name`, State, County) %>% 
  rename(plant_name = `Plant Name`, state_abb = State) %>% 
  rename_with(str_to_lower) %>% 
  mutate(EIA_proposed = T) %>% 
  left_join(tb_states) %>% 
  select(-state_abb) %>% 
  relocate(state, county) %>% 
  group_by(state, county, plant_name) %>% 
  mutate(unit = seq(n()), .after = plant_name) %>% 
  ungroup()


tb_NEEDS <- tb_NEEDS_raw %>% 
  select(`Plant Name`, `State Name`, County, `Owner Name`) %>% 
  rename(plant_name = `Plant Name`, state = `State Name`, 
         owner_name = `Owner Name`) %>% 
  rename_with(str_to_lower) %>%  
  mutate(NEEDS = T) %>% 
  relocate(state, county) %>% 
  group_by(state, county, plant_name) %>% 
  mutate(unit = seq(n()), .after = owner_name) %>% 
  ungroup()

tb_NESHAP <- tb_NESHAP_raw %>% 
  select(`OWNER/OPERATOR (Needs > FRS > Facility Name)`,
         STATE_ABBR, COUNTY_NAME
         ) %>% 
  rename(owner_name = `OWNER/OPERATOR (Needs > FRS > Facility Name)`,
         state_abb = STATE_ABBR, county = COUNTY_NAME
         ) %>% 
  mutate(NESHAP = T) %>% 
  left_join(tb_states) %>% 
  select(-state_abb) %>% 
  relocate(state, county) %>% 
  group_by(state, county, owner_name) %>% 
  mutate(unit = seq(n()), .after = owner_name) %>% 
  ungroup()

# join  --------------------------------------------------------------

tb_join <- 
  reduce(list(tb_EIA, tb_EIA_proposed, tb_NEEDS, tb_NESHAP),
         full_join
         ) %>%
  relocate(owner_name, .before = unit) %>% 
  mutate(across(EIA:NESHAP, \(col) coalesce(col, F))) %>% 
  arrange(state, county, plant_name)
  

# write csv ---------------------------------------------------------------

tb_join %>% write_csv("output/new_CTs.csv")

# summary

