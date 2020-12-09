iop_raw %>% 
  group_by(`IntraOp Type`, `IntraOp Path`) %>% 
  summarize(n = n()) %>% 
  arrange(`IntraOp Type`, desc(n)) %>% 
  ungroup() %>% 
  # mutate(rowname = `IntraOp Path`) %>% 
  gt(
    rowname_col = "IntraOp Path",
    groupname_col = "IntraOp Type"
  ) %>% 
  fmt_missing(everything()) %>% 
  summary_rows(
    groups = TRUE,
    columns = vars(n),
    fns = fns_labels,
    formatter = fmt_number,
    decimals = 0
  ) %>% 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      rows = n >= median(n))
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "indianred3"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      rows = median(n) > n)
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
    table.font.size = px(11), 
    table.width = pct(50)
  )
  
  