iop_raw %>% 
  select(`IntraOp Path`, `IntraOp Type`) %>% 
  group_by(`IntraOp Type`, `IntraOp Path`) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = `IntraOp Type`, values_from = n) %>% 
  rowwise() %>% 
  mutate(
    Total = sum(`FNA Adequacy`, Frozen, Gross, `Touch Prep`, na.rm = TRUE)
  ) %>% 
  arrange(desc(Total)) %>% 
  gt(
    rowname_col = "IntraOp Path",
    groupname_col = "IntraOp Type"
  ) %>% 
  fmt_missing(everything()) %>% 
  summary_rows(
    groups = NULL,
    columns = vars(`FNA Adequacy`, Frozen, Gross, `Touch Prep`,Total),
    fns = fns_labels,
    formatter = fmt_number,
    decimals = 0
  ) %>% 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      rows = Total >= median(Total))
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "indianred3"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      rows = median(Total) > Total)
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
    table.font.size = px(11), 
    table.width = pct(50)
  )
