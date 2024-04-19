library("here")
library("tidyverse")
library("forcats")
library("dplyr")
library("table1")
library("aod")
library("readxl")
library(gtsummary)

setwd("C:/Users/msalvat4/Documents/Varanasi/Data/Dataset")

#LOADING DATA
Varanasi <- read_xlsx("Varanasi_ORIGINAL.xlsx")
data1 <- Varanasi

#DATA MANIPULATION

##filtering out one missing entry for HCV and HIV status (now the name of the data of the dataset is data_filtered)
data_filtered <- data1 %>% filter(!is.na(hcvrapid))
data_filtered <- data1 %>% filter(!is.na(hiv1rapid))

##filtering out site variable (since they are all Varanasi) 
data_filtered <- subset(data_filtered, select = -site)

##filtering out all females from the dataset
data_filtered <- data_filtered[data_filtered$gender != "Female", ]

##labeling some variables
names(data_filtered)[names(data_filtered) == "hiv1rapid"] <- "HIV status"
names(data_filtered)[names(data_filtered) == "hcvrapid"] <- "HCV status"

##making variables categorical type
data_filtered$`HCV status` <- as.factor(data_filtered$`HCV status`)
data_filtered$`HIV status` <- as.factor(data_filtered$`HIV status`)
data_filtered$su50a <- as.factor(data_filtered$su50a)
data_filtered$su35 <- as.factor(data_filtered$su35)
data_filtered$su36 <- as.factor(data_filtered$su36)


##rename some of the categorical variables into more descriptive names
data_filtered$`HIV status` <- factor(data_filtered$`HIV status`, levels = c("0", "1"), labels = c("HIV negative", "HIV positive"))
data_filtered$`HCV status` <- factor(data_filtered$`HCV status`, levels = c("0", "1"), labels = c("HCV negative", "HCV positive"))


##DEMOGRAPHIC VARIABLES

#Variable: AGE (Age) - grouping in age ranges
names(data_filtered)[names(data_filtered) == "cm_age"] <- "Age"
data_filtered$Age <- as.numeric(as.character(data_filtered$Age))
data_filtered$Age = cut(data_filtered$Age,
                        breaks = c(-Inf, 25, 35, 45, 55, Inf),
                        labels = c("16-25", "26-35", "36-45", "46-55", "56+"),
                        right = FALSE)


#Variable: MARITAL STATUS (`Marital status`)
names(data_filtered)[names(data_filtered) == "dg5"] <- "Marital status"
data_filtered$`Marital status` <- as.factor(data_filtered$`Marital status`)
data_filtered$`Marital status` <- fct_collapse(data_filtered$`Marital status`,
                                               Unmarried = c("2", "3", "7"),
                                               Married = c("1", "4", "5", "6"),
)


#Variable: EDUCATION (Education)
names(data_filtered)[names(data_filtered) == "dg6"] <- "Education"
data_filtered$Education <- as.factor(data_filtered$Education)
data_filtered$Education <- fct_collapse(data_filtered$Education,
                                        `Primary school or lower` = c("1", "2"),
                                        `Secondary school or higher` = c("3", "4", "5", "6", "7", "8"),
)


#Variable: EMPLOYMENT (Employment)
names(data_filtered)[names(data_filtered) == "dg7"] <- "Employment"
data_filtered$Employment <- as.factor(data_filtered$Employment)
data_filtered$Employment <- fct_collapse(data_filtered$Employment,
                                       `Monthly or weekly wages` = c("1", "2"),
                                       `Daily or seasonal wages`= c("3", "4"),
                                       `Unemployed` = c("5", "6", "7", "8", "9", "10", "11"),
)
data_filtered$Employment <- relevel(data_filtered$Employment, ref = "Unemployed")


#Variable: HOUSING (Housing status)
names(data_filtered)[names(data_filtered) == "dg10"] <- "Housing status"
data_filtered$`Housing status` <- as.factor(data_filtered$`Housing status`)
data_filtered$`Housing status` <- fct_collapse(data_filtered$`Housing status`,
                                               `Not experiencing homelessness` = c("1", "2", "3", "4", "5", "6", "7", "10"),
                                               `Experiencing homelessness` = c("8"),
)




##SUBSTANCE USE VARIABLES

#Variable: FIRST NON-MEDICAL DRUG INJECTION AGE (su1a)
names(data_filtered)[names(data_filtered) == "su1a"] <- "First non-medical drug injection age"
data_filtered <- subset(data_filtered, `First non-medical drug injection age` != 997)
data_filtered$`First non-medical drug injection age` <- as.numeric(as.character(data_filtered$`First non-medical drug injection age`)) 
data_filtered$`First non-medical drug injection age` <- as.numeric(as.character(data_filtered$`First non-medical drug injection age`))


#Variable: YEARS INJECTING NON-MEDICAL DRUGS (by subtracting "Age" and "First non-medical drug injection age")
data_filtered$`Years injecting non-medical drugs` <- data_filtered$Age - data_filtered$`First non-medical drug injection age`


#Variable: HISTORY OF NEEDLE SHARING (History of needle sharing), Su35 and su36: Yes (1) if there is a Yes in either su35 or su36
data_filtered$`History of needle sharing` <- ifelse(data_filtered$su35 == 1 | data_filtered$su36 == 1, 1, 0)
data_filtered$`History of needle sharing` <- as.factor(data_filtered$`History of needle sharing`)
data_filtered$`History of needle sharing` <- factor(data_filtered$`History of needle sharing`, levels = c("0", "1"), labels = c("No", "Yes"))


#Variable: ALCOHOL USE (Alcohol use). Created a composite AUDIT score, anything more than 8 is considered hazardous consumption
calculate_audit_score <- function(data_filtered) {
  audit_columns <- c("su25", "su26", "su27", "su28", "su29", "su30", "su31", "su32", "su33", "su34")
  audit_score <- data_filtered[, audit_columns]
  data_filtered$audit_score <- rowSums(audit_score, na.rm = TRUE)
  return(data_filtered)
}
data_filtered <- calculate_audit_score(data_filtered)

data_filtered$`Alcohol use` <- ifelse(data_filtered$audit_score >= 8, 1, 0)
data_filtered$`Alcohol use` <- as.factor(data_filtered$`Alcohol use`)
data_filtered$`Alcohol use` <- factor(data_filtered$`Alcohol use`, levels = c("0", "1"), labels = c("Not hazardous", "Hazardous"))




##PSYCHOSOCIAL RISKS VARIABLES

#Variable: HISTORY IF INCARCERATION (History of incarceration)
names(data_filtered)[names(data_filtered) == "screen10"] <- "History of incarceration"
data_filtered$`History of incarceration` <- as.factor(data_filtered$`History of incarceration`)
data_filtered$`History of incarceration` <- factor(data_filtered$`History of incarceration`, levels = c("0", "1"), labels = c("No", "Yes"))


#Variable: SEX WORK INVOLVEMENT (Sex work involvement)
names(data_filtered)[names(data_filtered) == "su58"] <- "Sex work involvement"
data_filtered$`Sex work involvement`<- as.factor(data_filtered$`Sex work involvement`)
data_filtered <- data_filtered %>% filter(`Sex work involvement` !=997)
data_filtered$`Sex work involvement` <- factor(data_filtered$`Sex work involvement`, levels = c("0", "1"), labels = c("No", "Yes"))


#Variable: LIFETIME SEXUAL PARTNERS
##Lifetime women sex partners, including 0 from su49 and responses from su50a combined into categories
data_filtered$su50a_numeric <- as.numeric(as.character(data_filtered$su50a))
data_filtered$wsexnum <- with(data_filtered, ifelse(su49 == 0, "Less than 2",
                                                    ifelse(su50a_numeric == 997, "Don't know",
                                                           ifelse(su50a_numeric >= 2, "2 or more", "Less than 2"))))
data_filtered$wsexnum <- factor(data_filtered$wsexnum, 
                                levels = c("Less than 2", "2 or more", "Don't know"))
##Lifetime men sex partners, including 0 from su40aa and responses from su40c combined into categories
data_filtered$su40c_numeric <- as.numeric(as.character(data_filtered$su40c))
data_filtered$msexnum <- with(data_filtered, ifelse(su40aa == 0, "Less than 2",
                                                    ifelse(su40c_numeric == 997, "Don't know",
                                                           ifelse(su40c_numeric >= 2, "2 or more", "Less than 2"))))
data_filtered$msexnum <- factor(data_filtered$msexnum, 
                                levels = c("Less than 2", "2 or more", "Don't know"))

data_filtered$`Lifetime sexual partners` <- with(data_filtered, ifelse(wsexnum == "Don't know" | msexnum == "Don't know", "Don't know",
                                                                                 ifelse(wsexnum == "2 or more" | msexnum == "2 or more", "2 or more",
                                                                                        ifelse(wsexnum == "Less than 2" & msexnum == "Less than 2", "Less than 2", NA))))
data_filtered$`Lifetime sexual partners` <- factor(data_filtered$`Lifetime sexual partners`, 
                                                             levels = c("Less than 2", "2 or more", "Don't know"))


#Variable: DEPRESSION (Depression). Created a composite PHQ9 score, anything more than 10 is considered moderate, and 15 severe depression
calculate_phq9_score <- function(data_filtered, exclude_column) {
  phq9_items <- data_filtered[, grep("phq", names(data_filtered))]
  phq9_items1 <- phq9_items[, !names(phq9_items) %in% exclude_column]
  data_filtered$phq9_score <- rowSums(phq9_items1, na.rm = TRUE)
  return(data_filtered)
}
data_filtered <- calculate_phq9_score(data_filtered, exclude_column = "phq10")

data_filtered$Depression <- ifelse(data_filtered$phq9_score >= 10, 1, 0)
data_filtered$Depression <- as.factor(data_filtered$Depression)
data_filtered$Depression <- factor(data_filtered$Depression, levels = c("0", "1"), labels = c("No", "Yes"))




##SERVICES VARIABLES

#Variable: EVER TESTED FOR HIV (hiv1)
names(data_filtered)[names(data_filtered) == "hiv1"] <- "Ever tested for HIV"
data_filtered$`Ever tested for HIV` <- as.factor(data_filtered$`Ever tested for HIV`)
data_filtered$`Ever tested for HIV` <- factor(data_filtered$`Ever tested for HIV`, levels = c("0", "1"), labels = c("No", "Yes"))


#Variable: Ever tested for HCV (hcv1)
names(data_filtered)[names(data_filtered) == "hcv1"] <- "Ever tested for HCV"
data_filtered$`Ever tested for HCV` <- as.factor(data_filtered$`Ever tested for HCV`)
data_filtered$`Ever tested for HCV` <- factor(data_filtered$`Ever tested for HCV`, levels = c("0", "1", "997"), labels = c("No", "Yes", "Don't know"))


#Variable: NEEDLE EXCHANGE PROGRAM (sv1)
data_filtered$`Ever participated in a needle exchange program` = ifelse(data_filtered$sv1 == 0, "No", "Yes")


#Variable: OPIATE SUBSTITUTION PROGRAM (sv7)
data_filtered$`Ever participated in an OST program` = ifelse(data_filtered$sv7 == 0, "No", "Yes")


#Variable: REASON FOR NOT ATTENDING AN OST PROGRAM (sv8a1)
names(data_filtered)[names(data_filtered) == "sv8a1"] <- "Reason for not attending an OST program"
data_filtered$`Reason for not attending an OST program` <- as.factor(data_filtered$`Reason for not attending an OST program`)
data_filtered$`Reason for not attending an OST program` <- factor(data_filtered$`Reason for not attending an OST program`, levels = c("1", "2", "3", "5"), labels = c("I inject very infrequently", "I do not need OST", "I do not know where to find an OST program", "I do not have time to go"))
  


table1(~ Age + `Marital status` + Education + Employment + `Housing status` + `First non-medical drug injection age` + `Years injecting non-medical drugs` + `History of needle sharing` + `Alcohol use` + `History of incarceration` + `Sex work involvement` + `Lifetime sexual partners` + Depression + `Ever tested for HIV` + `Ever tested for HCV` + `Ever participated in a needle exchange program` + `Ever participated in an OST program` + `Reason for not attending an OST program` | `HIV status`, data = data_filtered)

table1(~ Age + `Marital status` + Education + Employment + `Housing status` + 
         `First non-medical drug injection age` + `Years injecting non-medical drugs` + 
         `History of needle sharing` + `Alcohol use` + `History of incarceration` + 
         `Sex work involvement` + `Lifetime sexual partners` + Depression + 
         `Ever tested for HIV` + `Ever tested for HCV` + 
         `Ever participated in a needle exchange program` + 
         `Ever participated in an OST program` + `Reason for not attending an OST program` |
         `HIV status` + `HCV status`, 
       data = data_filtered)



#Creating new enroll month variable
data_filtered2$enrollmonth <- format(data_filtered2$enrolldt, "%Y-%m")

## label months
data_filtered2 <- data_filtered2 %>% 
  mutate(enrollmonth = case_when(
    enrollmonth == "2017-12" ~ "Dec 2017",
    enrollmonth == "2018-01" ~ "Jan 2018",
    enrollmonth == "2018-02" ~ "Feb 2018",
    enrollmonth == "2018-03" ~ "Mar 2018",
    enrollmonth == "2018-04" ~ "Apr 2018",
    enrollmonth == "2018-05" ~ "May 2018",
    enrollmonth == "2018-06" ~ "Jun 2018",
    enrollmonth == "2018-07" ~ "Jul 2018",
    enrollmonth == "2018-08" ~ "Aug 2018",
    enrollmonth == "2018-09" ~ "Sep 2018",
    enrollmonth == "2018-10" ~ "Oct 2018",
    enrollmonth == "2018-11" ~ "Nov 2018",
    enrollmonth == "2018-12" ~ "Dec 2018",
  ))

## make month factor variable
month_levels <- c("Dec 2017", "Jan 2018", "Feb 2018", "Mar 2018", "Apr 2018", "May 2018", "Jun 2018", "Jul 2018", "Aug 2018", "Sep 2018", "Oct 2018", "Nov 2018", "Dec 2018")  # Define the desired order
data_filtered2$enrollmonth<- factor(data_filtered2$enrollmonth, levels = month_levels)

## Ever been tested for HIV by enroll month
table(data_filtered2$enrollmonth, data_filtered$hiv1)

## HIV pos by enroll month
table(data_filtered2$enrollmonth, data_filtered$hiv)



#RDS POPULATION ESTIMATES

## 1.Creating a new variable "recruited_by"
outcoupon_to_studyid <- setNames(data_filtered2$studyid, data_filtered2$outcoupon1)

outcoupon_to_studyid <- c(outcoupon_to_studyid, setNames(data_filtered2$studyid, data_filtered2$outcoupon2))

data_filtered2$recruited_by <- outcoupon_to_studyid[as.character(data_filtered2$incoupon)]

data_filtered2$recruited_by[is.na(data_filtered2$recruited_by)] <- "No recruiter"

###To see the new variable
head(data_filtered2[, c("studyid", "incoupon", "recruited_by")], n = 15)

##Saving the new dataset for uploading it into RDS package
write.xlsx(data_filtered2, file = "Updated_Dataset.xlsx")



## 2.Creating an RDS object
rds.object <- rds.data_filtered2.frame(id = data_filtered2$studyid,
                             network.size = data_filtered2$nw2,
                             recruit = data_filtered2$recruited_by)


