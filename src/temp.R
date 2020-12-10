# Bone marrow cases
# Summaries are per case, not per specimen
# Bone marrow interpretations are the sum of unique combinations of 
# Result ID and Pathologist

marrow_raw %>% 
  select(PATHOLOGIST, `RESULT ID`, Create) %>% 
  rename(
    Pathologist = PATHOLOGIST, 
    `Result ID` = `RESULT ID`
  ) %>% 
  mutate(
    create_date = as_date(mdy_hm(Create))
  ) %>% 
  filter(
    create_date >= "2020-07-01" & 
      create_date <= "2020-09-30"
  ) %>% 
  group_by(Pathologist, `Result ID`) %>% 
  distinct() %>% 
  ungroup() %>% 
  select(Pathologist) %>% 
  mutate(n = sum(n())) %>% 
  arrange(desc(n))
