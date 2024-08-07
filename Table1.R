
#Table 1 by HIV status
tbl1data <- data_filtered %>% select(c(AgeGroup, `Marital status`, Education, Employment, `Housing status`, `First non-medical drug injection age`, `History of needle sharing`, `Alcohol use`, `History of incarceration`, `Sex work involvement`, `Lifetime sexual partners`, Depression, `Ever tested for HIV`, `Ever tested for HCV`, `Ever participated in a needle exchange program`, `Ever participated in an OST program`, `HCV status`, `HIV status`)) 
tbl1data %>% 
  tbl_summary(by = `HIV status`) %>% 
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**HIV Status**") %>%
  modify_caption("**Respondent Characteristics**") %>%
  bold_labels()


tbl1data <- data_filtered %>% select(c(AgeGroup, `Marital status`, Education, Employment, `Housing status`, 
                                       `First non-medical drug injection age`, `History of needle sharing`, 
                                       `Alcohol use`, `History of incarceration`, `Sex work involvement`, 
                                       `Lifetime sexual partners`, Depression, `Ever tested for HIV`, 
                                       `Ever tested for HCV`, `Ever participated in a needle exchange program`, 
                                       `Ever participated in an OST program`, `HCV status`, `HIV status`))
tbl1 <- tbl1data %>%
  tbl_summary(by = `HIV status`) %>%
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**HIV Status**") %>%
  modify_caption("**Respondent Characteristics**") %>%
  bold_labels()
