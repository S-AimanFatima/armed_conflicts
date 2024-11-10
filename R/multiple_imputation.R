library(here)
library(tidyverse)
library(kableExtra)
library(texreg)
library(plm)
library(mice)

##read in data

data <- read.csv(here("Data", "finaldata.csv"), header = TRUE)

#Log transformation of GDP

data$log_GDP <- log(data$gdp1000)


#convert ISO to numeric

midata <- data |>
  mutate(ISOnum = as.numeric(as.factor(data$ISO))) |>
  select(-country_name, -ISO)

#Dry run to get meth and pred

mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "Maternal_Mortality", "Infant_Mortality","Neonatal_Mortality", "Under5_Mortality", "log_GDP", "popdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "Maternal_Mortality", "Infant_Mortality", "Neonatal_Mortality", "Under5_Mortality", "log_GDP", "popdens"), "ISOnum"] <- -2

#Perform MI with m=10 imputations

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)

#fit 4 models

fit.mi.matmor <- with(mice.multi.out, 
                      lm(Maternal_Mortality ~ -1 + armconf1 + log_GDP + OECD + popdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(Year)))

fit.mi.infmor <- with(mice.multi.out, 
                      lm(Infant_Mortality ~ -1 + armconf1 + log_GDP + OECD + popdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(Year)))

fit.mi.neomor <- with(mice.multi.out, 
                      lm(Neonatal_Mortality ~ -1 + armconf1 + log_GDP + OECD + popdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(Year)))

fit.mi.un5mor <- with(mice.multi.out, 
                      lm(Under5_Mortality ~ -1 + armconf1 + log_GDP + OECD + popdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISOnum) + as.factor(Year)))

out.matmor <- pool(fit.mi.matmor)
out.infmor <- pool(fit.mi.infmor)
out.neomor<- pool(fit.mi.neomor)
out.un5mor <- pool(fit.mi.un5mor)


### CC analysis

preds <- as.formula(" ~ -1 + armconf1 + log_GDP + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  as.factor(ISO) + as.factor(Year)")

matmormod <- lm(update.formula(preds, Maternal_Mortality ~ .), data = data)
un5mormod <- lm(update.formula(preds, Under5_Mortality ~ .), data = data)
infmormod <- lm(update.formula(preds, Infant_Mortality ~ .), data = data)
neomormod <- lm(update.formula(preds, Neonatal_Mortality ~ .), data = data)

tosave <- list(out.matmor, out.infmor, out.neomor, out.un5mor, 
               matmormod, un5mormod, infmormod, neomormod)

keepvars <- list("armconf1" = "Armed conflict",
                 "log_GDP" = "log(GDP)",
                 "OECD" = "OECD",
                 "popdens" = "Population density",
                 "urban" = "Urban",
                 "agedep" = "Age dependency",
                 "male_edu" = "Male education",
                 "temp" = "Average temperature",
                 "rainfall" = "Average rainfall",
                 "earthquake" = "Earthquake",
                 "drought" = "Drought")
screenreg(tosave, 
          ci.force = TRUE,
          custom.coef.map = keepvars,
          custom.model.names = c("Mat CC", "Mat MI", "Un5 CC", "Un5 MI", "Inf CC", "Inf MI", "Neo CC", "Neo MI"))

save(tosave, file = here("output", "mi_output.Rdata"))
