#Table 1 by quarter
tbl1data <- data_filtered %>% 
  select(c(Age, `Marital status`, Education, Employment, `Housing status`, `First non-medical drug injection age`, `History of needle sharing`, `Alcohol use`, `History of incarceration`, `Sex work involvement`, `Lifetime sexual partners`, Depression, `Ever tested for HIV`, `Ever tested for HCV`, `Ever participated in a needle exchange program`, `Ever participated in an OST program`, `HCV status`, `HIV status`, enrollquarter))
tbl1data %>% 
  tbl_summary(by = enrollquarter) %>% 
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Enrollment Quarter**") %>%
  modify_caption("**Respondent Characteristics**") %>%
  bold_labels()


#Table 1 by RDS wave group
tbl1data <- data_filtered %>% 
  select(c(Age, `Marital status`, Education, Employment, `Housing status`, `First non-medical drug injection age`, `History of needle sharing`, `Alcohol use`, `History of incarceration`, `Sex work involvement`, `Lifetime sexual partners`, Depression, `Ever tested for HIV`, `Ever tested for HCV`, `Ever participated in a needle exchange program`, `Ever participated in an OST program`, `HCV status`, `HIV status`, wave_group))
tbl1data %>% 
  tbl_summary(by = wave_group) %>% 
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Wave Group**") %>%
  modify_caption("**Respondent Characteristics by Wave Group**") %>%
  bold_labels()
