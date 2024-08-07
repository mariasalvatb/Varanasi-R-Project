table(data_filtered$`Ever participated in a needle exchange program`)
table(data_filtered$`Ever participated in an OST program`)

# TABLE 1
tbl1data <- data_filtered %>% select(c(`Age`, `Marital status`, Education, Employment, `Housing status`, `First non-medical drug injection age`, `History of needle sharing`, `Alcohol use`, `History of incarceration`, `Sex work involvement`, `Lifetime sexual partners`, Depression, `Ever participated in a needle exchange program`, `Ever participated in an OST program`, `HCV status`, `HIV status`)) 

tbl1data %>% 
  tbl_summary(by = `HIV status`) %>% 
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**HIV Status**") %>%
  modify_caption("**Respondent Characteristics**") %>%
  bold_labels()