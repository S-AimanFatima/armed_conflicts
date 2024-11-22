library(here)
library(tidyverse)

finaldata <- read.csv(here("Data", "finaldata.csv"), header = TRUE)

#####INFANT MORTALITY####

#Infant mortality in 2017 by armed conflict
data2017 <- finaldata |>
  dplyr::filter(Year == 2017) |>
  dplyr::filter(!is.na(Infant_Mortality))

#Summary statistics by armed conflict
data2017 |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.infmor = median(Infant_Mortality, na.rm = T))


obs.med.diff <- median(data2017[data2017$armconf1 == 1,]$Infant_Mortality) -
  median(data2017[data2017$armconf1 == 0,]$Infant_Mortality)
obs.med.diff


#Use stratified bootstrap
infmor.arm1 <- finaldata |>
  dplyr::filter(Year == 2017 & !is.na(Infant_Mortality) & armconf1 == 1) |>
  dplyr::select(ISO, Infant_Mortality)

infmor.arm0 <- finaldata |>
  dplyr::filter(Year == 2017 & !is.na(Infant_Mortality) & armconf1 == 0) |>
  dplyr::select(ISO, Infant_Mortality)

set.seed(2024)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- infmor.arm1[sample(nrow(infmor.arm1), size = nrow(infmor.arm1), replace = TRUE),]
  resamp.arm0 <- infmor.arm0[sample(nrow(infmor.arm0), size = nrow(infmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$Infant_Mortality) - median(resamp.arm0$Infant_Mortality)
}

head(resamp.arm1, 12)


#Histogram of the 1000 bootstrap medians
hist(med.diff, main = "Distribution of bootstrap statistic")


#Use the boot package/function to see if we get the same answer

library(boot)

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Infant_Mortality, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
bootout

bootout$t0
head(bootout$t)
sd(bootout$t)

#Calculate confidence intervals#

#percentile bootstrap CI
quantile(bootout$t, probs = c(0.025, 0.975))

#basic bootstrap CI
2 * bootout$t0 - quantile(bootout$t, probs = 0.975)
2 * bootout$t0 - quantile(bootout$t, probs = 0.025)

#bias corrected & accelerated bootstrap CI
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))



#####UNDER-5 MORTALITY####



#Under5 mortality in 2017 by armed conflict
data2017.und5 <- finaldata |>
  dplyr::filter(Year == 2017) |>
  dplyr::filter(!is.na(Under5_Mortality))

#Summary statistics by armed conflict
data2017.und5 |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.infmor = median(Under5_Mortality, na.rm = T))


obs.med.diff <- median(data2017.und5[data2017.und5$armconf1 == 1,]$Under5_Mortality) -
  median(data2017.und5[data2017.und5$armconf1 == 0,]$Under5_Mortality)
obs.med.diff

#Use the boot package/function

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Under5_Mortality, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017.und5, statistic = getmeddiff, strata = data2017.und5$armconf1, R = 1000)
bootout

bootout$t0
head(bootout$t)
sd(bootout$t)

#bias corrected & accelerated bootstrap CI
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))



#####UNDER-5 MORTALITY####



#Under5 mortality in 2017 by armed conflict
data2017.und5 <- finaldata |>
  dplyr::filter(Year == 2017) |>
  dplyr::filter(!is.na(Under5_Mortality))

#Summary statistics by armed conflict
data2017.und5 |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.infmor = median(Under5_Mortality, na.rm = T))


obs.med.diff <- median(data2017.und5[data2017.und5$armconf1 == 1,]$Under5_Mortality) -
  median(data2017.und5[data2017.und5$armconf1 == 0,]$Under5_Mortality)
obs.med.diff

#Use the boot package/function

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Under5_Mortality, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017.und5, statistic = getmeddiff, strata = data2017.und5$armconf1, R = 1000)
bootout

bootout$t0
head(bootout$t)
sd(bootout$t)

#bias corrected & accelerated bootstrap CI
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))


#####UNDER-5 MORTALITY####



#Under5 mortality in 2017 by armed conflict
data2017.und5 <- finaldata |>
  dplyr::filter(Year == 2017) |>
  dplyr::filter(!is.na(Under5_Mortality))

#Summary statistics by armed conflict
data2017.und5 |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.infmor = median(Under5_Mortality, na.rm = T))


obs.med.diff <- median(data2017.und5[data2017.und5$armconf1 == 1,]$Under5_Mortality) -
  median(data2017.und5[data2017.und5$armconf1 == 0,]$Under5_Mortality)
obs.med.diff

#Use the boot package/function

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Under5_Mortality, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017.und5, statistic = getmeddiff, strata = data2017.und5$armconf1, R = 1000)
bootout

bootout$t0
head(bootout$t)
sd(bootout$t)

#bias corrected & accelerated bootstrap CI
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))


#####UNDER-5 MORTALITY####



#Under5 mortality in 2017 by armed conflict
data2017.und5 <- finaldata |>
  dplyr::filter(Year == 2017) |>
  dplyr::filter(!is.na(Under5_Mortality))

#Summary statistics by armed conflict
data2017.und5 |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.und5 = median(Under5_Mortality, na.rm = T))


obs.med.diff <- median(data2017.und5[data2017.und5$armconf1 == 1,]$Under5_Mortality) -
  median(data2017.und5[data2017.und5$armconf1 == 0,]$Under5_Mortality)
obs.med.diff

#Use the boot package/function

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Under5_Mortality, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017.und5, statistic = getmeddiff, strata = data2017.und5$armconf1, R = 1000)
bootout

bootout$t0
head(bootout$t)
sd(bootout$t)

#bias corrected & accelerated bootstrap CI
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))




#####NEONATAL MORTALITY####



#Neonatal mortality in 2017 by armed conflict
data2017.neo <- finaldata |>
  dplyr::filter(Year == 2017) |>
  dplyr::filter(!is.na(Neonatal_Mortality))

#Summary statistics by armed conflict
data2017.neo |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.neomor = median(Neonatal_Mortality, na.rm = T))


obs.med.diff <- median(data2017.neo[data2017.neo$armconf1 == 1,]$Neonatal_Mortality) -
  median(data2017.neo[data2017.neo$armconf1 == 0,]$Neonatal_Mortality)
obs.med.diff

#Use the boot package/function

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Neonatal_Mortality, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017.neo, statistic = getmeddiff, strata = data2017.neo$armconf1, R = 1000)
bootout

bootout$t0
head(bootout$t)
sd(bootout$t)

#bias corrected & accelerated bootstrap CI
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))
