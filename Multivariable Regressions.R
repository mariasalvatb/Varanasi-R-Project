#MULTIVARIABLE REGRESSION
logitm <- glm(`HIV status` ~ Age + `Marital status` + Education + Employment + `Housing status` + `History of needle sharing` + `History of incarceration` + `Lifetime sexual partners` + `HCV status`, data=data_filtered, family = "binomial")
summary(logitm, exp = TRUE)
exp(coef(logitm, scientific=FALSE))