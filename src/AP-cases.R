# Summarize cases and blocks completed by pathologists

# load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(gt)

# import raw data for all cases
files <- list.files(path = "data/", pattern = "2019-0.*.xls", full.names = T)

ap_raw <- sapply(files, read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id") %>% 
  select(-id) 

ap_clean <- 
  ap_raw %>% 
  mutate(
    `Create Date` = mdy(`Create Date`),
    yr = year(`Create Date`),
    mth = month(`Create Date`, label = TRUE, abbr = TRUE)
  ) %>% 
  filter(
    `Create Date` < "2020-10-01",
    status != "Deleted"
  ) %>% 
  group_by(yr, mth, `Primary Pathologist`) %>% 
  summarize(
    Cases = as.numeric(sum(n())),
    Blocks = as.numeric(sum(Blocks))
  ) %>% 
  ungroup()

ap_cases <- 
  ap_clean %>% 
  select(-Blocks) %>% 
  pivot_wider(names_from = mth, values_from = Cases)

ap_blocks <- 
  ap_clean %>% 
  select(-Cases) %>% 
  pivot_wider(names_from = mth, values_from = Blocks)

# define summary row functions
fns_labels <- 
  list(
    Avg = ~mean(., na.rm = TRUE),
    Total = ~sum(., na.rm = TRUE)
  )

ap_blocks %>% 
  rename(rowname = `Primary Pathologist`) %>% 
  gt(groupname_col = "yr") %>% 
  tab_header(
    title = md("Monthly Blocks for AP Cases by Pathologist")
  ) %>% 
  fmt_missing(everything()) %>% 
  summary_rows(
    groups = TRUE,
    columns = vars(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
    fns = fns_labels,
    formatter = fmt_number,
    decimals = 0
  )

# Monthly case counts by pathologist
ap_cases %>% 
  rename(rowname = `Primary Pathologist`) %>% 
  gt(groupname_col = "yr") %>% 
  tab_header(
    title = md("Monthly AP Cases by Pathologist")
  ) %>% 
  fmt_missing(everything()) %>% 
  summary_rows(
    groups = TRUE,
    columns = vars(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
    fns = fns_labels,
    formatter = fmt_number,
    decimals = 0
  )