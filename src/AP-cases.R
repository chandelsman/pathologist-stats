# Summarize cases and blocks completed by pathologists

# load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(gt)

# import raw data for all cases
files <- list.files(path = "data/", pattern = "2019-0.*.xls", full.names = T)

cases_raw <- sapply(files, read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id") %>% 
  select(-id) 

cases_clean <- 
  cases_raw %>% filter(status != "Deleted") %>% 
  mutate(
    `Create Date` = mdy(`Create Date`),
    yr = year(`Create Date`),
    mth = month(`Create Date`, label = TRUE, abbr = TRUE)
  ) %>% 
  group_by(yr, mth, `Primary Pathologist`) %>% 
  summarize(
    n = sum(n())
  ) %>% 
  ungroup()

cases_wide <- 
  cases_clean %>% 
  pivot_wider(names_from = mth, values_from = n)

cases_wide %>% 
  gt(
    rowname_col = "Primary Patholgist",
    groupname_col = "yr"
  ) %>% 
  fmt_missing(
    everything()
  ) %>% 
  summary_rows(
    groups = TRUE,
    columns = vars(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
    fns = list(
      Average = ~mean(., na.rm = TRUE),
      Sum = ~sum(., na.rm = TRUE),
      `Std dev` = ~sd(., na.rm = TRUE)
    )
  ) %>% 
  tab_options(column_labels.font.weight = "bold",
              table.width = pct(100))


