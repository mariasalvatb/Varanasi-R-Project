#UNIVARIABLE MODELS BY HIV STATUS

##AGE
logitu <- glm(`HIV status` ~ Age, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##MARITAL
logitu <- glm(`HIV status` ~ `Marital status`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##EDUCATION
logitu <- glm(`HIV status` ~ Education, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##EMPLOYMENT
logitu <- glm(`HIV status` ~ Employment, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##HOUSING STATUS
logitu <- glm(`HIV status` ~ `Housing status`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##NON-MEDICAL DRUG INJECTION AGE
logitu <- glm(`HIV status` ~ `First non-medical drug injection age`, data=data_filtered, family = "binomial")
exp(coef(logitu))
summary(logitu, exp = TRUE)

##NEEDLE SHARING
logitu <- glm(`HIV status` ~ `History of needle sharing`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##ALCOHOL
logitu <- glm(`HIV status` ~ `Alcohol use`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##INCARCERATION
logitu <- glm(`HIV status` ~ `History of incarceration`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##SEX WORK
logitu <- glm(`HIV status` ~ `Sex work involvement`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##SEXUAL PARTNERS
logitu <- glm(`HIV status` ~ `Lifetime sexual partners`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##DEPRESSION
logitu <- glm(`HIV status` ~ `Depression`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##EVER TESTED FOR HIV
logitu <- glm(`HIV status` ~ `Ever tested for HIV`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##EVER TESTED FOR HCV
logitu <- glm(`HIV status` ~ `Ever tested for HCV`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))

##EVER PARTICIPATED IN NEEDLE EXCHANGE
logitu <- glm(`HIV status` ~ `Ever participated in a needle exchange program`, data=data_filtered, family = "binomial")
summary(logitu, exp = TRUE)
exp(coef(logitu))