#HCV CASCADE

##HCV antibody testing positive (155)
hcv_positive_data <- data_filtered[data_filtered$HCV status == "HCV positive", ]

##HCV RNA testing positive (153: 75 HCV only, and 78 coinfected - 2 as not infected)
table(hcv_positive_data$`coinfection_status`)

##HCV viremic (112 >30, 41 <30)
table(hcv_positive_data$hcvundet)

##Aware of HCV-positive status (4 aware)
###Replace with 1 where the conditions match (if hcv6 or hcv7 are yes)
hcv_positive_data$hcvaware <- NA
hcv_positive_data$hcvaware <- ifelse(
  hcv_positive_data$hcv6 == 1 | hcv_positive_data$hcv7 == 1, 
  1, 
  hcv_positive_data$hcvaware
)

##Saw a clinician for HCV infection (3)
sum(hcv_positive_data$hcv9 == 1, na.rm = TRUE)

##Initiated treatment (3)
hcv_positive_data$hcv11_binary <- ifelse(hcv_positive_data$hcv11 %in% c(2, 3), 1, hcv_positive_data$hcv11)
table(hcv_positive_data$hcv11_binary)

##Cured (3)
table(hcv_positive_data$hcv13k)


