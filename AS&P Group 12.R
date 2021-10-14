#####################################################
### Advanced Statistics & Programming             ###
### Group Assignment                              ###
#####################################################
setwd()

#------------------------------------------------------------------------------------
### Clean global environment and console ### 
#------------------------------------------------------------------------------------
remove(list=ls())
cat("\f")

#------------------------------------------------------------------------------------
### Load Libraries ### 
#------------------------------------------------------------------------------------
#install.packages("maps")
library(maps)
#install.packages("ggmap")
library(ggmap)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(tidyverse)
#install.packages("nortest")
library(nortest)
#install.packages("car")
library(car)
#install.packages("sandwich")
library(sandwich)

#------------------------------------------------------------------------------------
### Importing Data ### 
#------------------------------------------------------------------------------------
# set up path names
#------------------------------------------------------------------------------------
dir <- "~/Desktop/Advanced Stats & Programming/AS&P Group Project/"
dirData <- paste0(dir,"Data/")
dirResults <- paste0(dir,"Results/")

# read data
#------------------------------------------------------------------------------------
covidCasesAndDeaths <- read.csv(paste0(dirData,
        "COVID19 cases & deaths all-states.csv"))

hospitalPatientAndCapacity <- read.csv(paste0(dirData,
        "COVID_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries .csv"))

genderPerState <- read.csv(paste0(dirData,
        "Gender distribution.csv"),sep = ";")

gdpPerState <- read.csv(paste0(dirData,
        "GDP per state.csv"),sep = ";")

agePerState <- read.csv(paste0(dirData,
        "Age distribution.csv"),sep = ";")

populationPerState <- read.csv(paste0(dirData,
        "pop_per_state_cleaned.csv"),sep = ";")

stateAbbreviations <- data.frame(state.name, state.abb)

stateMapData <- read.csv(paste0(dirData,
        "state_map_data.csv"),sep = ",")

ICUPhysiciansPerState <- read.csv(paste0(dirData,
        "Number of Doctors.csv"),sep = ";")

USElection2020Result <- read.csv(paste0(dirData,
        "Election_US_2020.csv"),sep = ";")

#------------------------------------------------------------------------------------
### Data Cleaning & Prepartion ### 
#------------------------------------------------------------------------------------

# stateAbbreviations data frame clean
#------------------------------------------------------------------------------------
colnames(stateAbbreviations)[colnames(stateAbbreviations) ==
                               "state.abb"] <- "State_ID"
colnames(stateAbbreviations)[colnames(stateAbbreviations) == 
                               "state.name"] <- "State"
stateAbbreviations$region <- tolower(stateAbbreviations$State)

# stateMapData data frame clean

# agePerState data frame clean
#------------------------------------------------------------------------------------
# remove last rows
agePerState <- agePerState[-c(54,55,56,57,58,59,60,61,62,63,64,65),]
# row total and footnotes column
agePerState$Total <- NULL
agePerState$Footnotes <- NULL
colnames(agePerState)[colnames(agePerState) == "Location"] <- "State"
colnames(agePerState)[colnames(agePerState) == "X65."] <- "Age_65plus"

agePerState$Children.0.18 <- NULL
agePerState$Adults.19.25 <- NULL
agePerState$Adults.26.34 <- NULL
agePerState$Adults.35.54 <- NULL
agePerState$Adults.55.64 <- NULL

# genderPerState data frame clean
#------------------------------------------------------------------------------------
genderPerState <- genderPerState[-c(54,55,56,57,58,59,60,61,62,63,64,65),]
genderPerState$Total <- NULL
colnames(genderPerState)[colnames(genderPerState) == "Location"] <- "State"
genderPerState[,'Male'] <- as.numeric(as.character(genderPerState[,'Male']))

genderPerState$Female <- NULL

#gdpPerState data frame clean
#------------------------------------------------------------------------------------
gdpPerState <- subset(gdpPerState,
          Description == "Current-dollar GDP (millions of current dollars)")
colnames(gdpPerState)[2] <- "Location"
# remove columns 
gdpPerState$GeoFIPS <- NULL
gdpPerState$Description <- NULL
gdpPerState$X2020 <- NULL
# rename columns
colnames(gdpPerState)[colnames(gdpPerState) == "X2019"] <- "GDP_2019"
colnames(gdpPerState)[colnames(gdpPerState) == "Location"] <- "State"

# populationPerState data frame clean
#------------------------------------------------------------------------------------
# change column name to State
colnames(populationPerState)[colnames(populationPerState) ==
                  "Geographic.Area..pop.estimate.as.of.July.1."] <- "State"
colnames(populationPerState)[colnames(populationPerState) ==
                  "X2019"] <- "Population_2019"
# remove full stop from state names
populationPerState$State <- sub(".","",populationPerState$State)
# remove empty last rows
populationPerState <- populationPerState[-c(52,53,54,55,56,57,58),]
# make data row with only neccesary columns
populationPerState <- populationPerState[,c("State", "Population_2019")]

# ICUPhysiciansPerState data frame clean
#------------------------------------------------------------------------------------
colnames(ICUPhysiciansPerState)[colnames(ICUPhysiciansPerState) == 
  "Intensivist..Critical.Care..Physicians.per.10.000.Adults"] <- "ICUDoctors"

ICUPhysiciansPerState$Critical.Care.Nurses.and.CRNAs.per.10.000.Adults <- NULL
ICUPhysiciansPerState$Total.2nd.Line.Critical.Care.Physicians..per.10.000.Adults <- NULL

## hospitalPatientAndCapacity data frame clean
#------------------------------------------------------------------------------------
# change date format
hospitalPatientAndCapacity$date <- 
  format(as.Date(hospitalPatientAndCapacity$date),"%Y/%m/%d")
colnames(hospitalPatientAndCapacity)[colnames(hospitalPatientAndCapacity) == 
  "state"] <- "State_ID"

## covidCasesAndDeaths data frame clean
#------------------------------------------------------------------------------------
# change date format
covidCasesAndDeaths$date <- 
  format(as.Date(covidCasesAndDeaths$date),"%Y/%m/%d")
colnames(covidCasesAndDeaths)[colnames(covidCasesAndDeaths) == 
  "state"] <- "State_ID"
colnames(covidCasesAndDeaths)[colnames(covidCasesAndDeaths) == 
  "totalTestResultsIncrease"] <- "totalTestResultsDaily"

## USElection2020Result data frame clean
#------------------------------------------------------------------------------------
colnames(USElection2020Result)[colnames(USElection2020Result) ==
  "Number.of.electoral.per.State"] <- "numberElectoralSeatsPerState"
colnames(USElection2020Result)[colnames(USElection2020Result) == 
  "For.President.Joseph.R..Biden.Jr..of.Delaware"] <- "SeatsBiden"
colnames(USElection2020Result)[colnames(USElection2020Result) == 
  "Donald.J..Trump.of.Florida"] <- "SeatsTrump"

# add Republican dummy variable column
USElection2020Result$Republican <- 
  ifelse(USElection2020Result$SeatsTrump > 0, 1, 0)

# remove unnecessary columns
USElection2020Result$numberElectoralSeatsPerState <- NULL
USElection2020Result$SeatsBiden <- NULL
USElection2020Result$SeatsTrump <- NULL


#------------------------------------------------------------------------------------
### Aggregate CovidCasesDeaths Data per State ### 
#------------------------------------------------------------------------------------
# creating crossSectionalCovidCasesDeaths data frame
#------------------------------------------------------------------------------------
crossSectionalCovidCasesDeaths <-
  covidCasesAndDeaths %>%
  group_by(State_ID) %>%
  summarise_at(vars(deathIncrease,positiveIncrease,totalTestResultsDaily),
               list(~sum(.,na.rm = TRUE)))

colnames(crossSectionalCovidCasesDeaths)[colnames(crossSectionalCovidCasesDeaths) == 
      "totalTestResultsDaily"] <- "totalTestResults"

#------------------------------------------------------------------------------------
### Merging Data ### 
#------------------------------------------------------------------------------------

# create timeIndepDF by merging
#time-independent data frames containing 
#------------------------------------------------------------------------------------
# merge stateAbbreviations & agePerState
timeIndepDF <- merge(stateAbbreviations, agePerState, by = "State")
# merge timeIndepDF & genderPerState
timeIndepDF <- merge(timeIndepDF, genderPerState, by = "State")
# merge timeIndepDF & gdpPerState
timeIndepDF <- merge(timeIndepDF, gdpPerState, by = "State")
# merge timeIndepDF & populationPerState
timeIndepDF <- merge(timeIndepDF, populationPerState, by = "State")
# merge timeIndepDF & USElection2020Result
timeIndepDF <- merge(timeIndepDF, USElection2020Result, by = "State")
# merge timeIndepDF & ICUPhysiciansPerState
timeIndepDF <- merge(timeIndepDF, ICUPhysiciansPerState, by = "State")

# create crossSectionalDF by merging:
#timeIndepDF & crossSectionalCovidCasesDeaths
#------------------------------------------------------------------------------------
crossSectionalDF <- merge(crossSectionalCovidCasesDeaths,
                          timeIndepDF,
                          by = "State_ID")

# Shorten total test results metric column name
colnames(crossSectionalDF)[colnames(crossSectionalDF) == 
                        "totalTestResults"] <- "TestResults"



# create allPanelDataDF by merging:
#covidCasesAndDeaths & hospitalPatientAndCapacity & timeIndepDF
#------------------------------------------------------------------------------------
# merge covidCasesAndDeaths & hospitalPatientAndCapacity
allPanelDataDF <- merge(covidCasesAndDeaths,
                        hospitalPatientAndCapacity,
                        by = c("date", "State_ID"))

# select only the neccessary columns from allPanelDataDF
PanelDataDF <- allPanelDataDF[,c("date",
                                 "State_ID",
                                 "inpatient_beds",
                                 "deathIncrease",
                                 "positiveIncrease",
                                 "totalTestResultsDaily")]

# remove rows were there are missing data
PanelDataDF <- na.omit(PanelDataDF)

# delete duplicate couples
PanelDataDF <- unique(PanelDataDF,
                      by = c("State", "date"))

# Add timeIndepDF to PanelDataDF
PanelDataDF <- merge(PanelDataDF,
                     timeIndepDF,
                     by = "State_ID")

# Make total test results metric column name consistent with crossSectionalDF
colnames(PanelDataDF)[colnames(PanelDataDF) == 
                    "totalTestResultsDaily"] <- "TestResults"



#------------------------------------------------------------------------------------
### Remove unneccesary data frames from Global Environment ### 
#------------------------------------------------------------------------------------
remove(agePerState)
remove(gdpPerState)
remove(genderPerState)
remove(ICUPhysiciansPerState)
remove(populationPerState)
remove(stateAbbreviations)
remove(USElection2020Result)
remove(timeIndepDF)
remove(covidCasesAndDeaths)
remove(crossSectionalCovidCasesDeaths)
remove(hospitalPatientAndCapacity)
remove(allPanelDataDF)

#------------------------------------------------------------------------------------
### Add metrics to data frames ### 
#------------------------------------------------------------------------------------

# add metrics to crossSectionalDF
#------------------------------------------------------------------------------------
crossSectionalDF$covidMortality <- 
  crossSectionalDF$deathIncrease/crossSectionalDF$positiveIncrease

crossSectionalDF$GDPcap <- 
  (crossSectionalDF$GDP_2019*1000000)/crossSectionalDF$Population_2019


# add metrics to PanelDataDF
#------------------------------------------------------------------------------------
PanelDataDF$covidMortality <- 
  ifelse(PanelDataDF$positiveIncrease == 0,
         NA,
         PanelDataDF$deathIncrease/PanelDataDF$positiveIncrease)

PanelDataDF$GDPcap <- 
  (PanelDataDF$GDP_2019*1000000)/PanelDataDF$Population_2019

PanelDataDF$HospitalBeds <- 
  PanelDataDF$inpatient_beds/(PanelDataDF$Population_2019/10000)

PanelDataDF$month <- 
  as.numeric(str_sub(PanelDataDF$date,6,7))

PanelDataDF$season <- 
  ifelse(PanelDataDF$month ==12 | PanelDataDF$month == 1 | PanelDataDF$month == 2,
         "Winter",
  (ifelse(PanelDataDF$month ==3 | PanelDataDF$month == 4 | PanelDataDF$month == 5,
          "Spring",
  (ifelse(PanelDataDF$month ==6 | PanelDataDF$month == 7 | PanelDataDF$month == 8,
          "Summer","Autumn")))))


#write_csv(PanelDataDF,paste0(dirResults,"PanelDataDF.csv"))
#write_csv(crossSectionalDF,paste0(dirResults,"crossSectionalDF.csv"))


#####################################################
### Testing OLS Assumptions ###
#####################################################

#------------------------------------------------------------------------------------
### Testing for Normality (Shapiro Test) ### 
#------------------------------------------------------------------------------------
# crossSectionalDF
shapiro.test(crossSectionalDF$covidMortality)
shapiro.test(crossSectionalDF$ICUDoctors)

# PanelDataDF
# sample size is greater than 18k thus testing for normality is not of great concern


#####################################################
### Descriptive Statistics ###
#####################################################
#------------------------------------------------------------------------------------
### Summary Stats ### 
#------------------------------------------------------------------------------------

temp_PanelDataDF <- PanelDataDF[complete.cases(PanelDataDF),]

stargazer(temp_PanelDataDF,
          omit = c("deathIncrease",
                     "positiveIncrease",
                     "inpatient_beds",
                     "inpatient_beds_used",
                     "Age_65plus",
                     "Male",
                     "GDP_2019",
                     "Population_2019",
                     "ICUDoctors",
                     "GDPcap",
                     "month",
                     "bedsPer1k",
                      "Republican"))


stargazer(crossSectionalDF,
          omit = c("deathIncrease",
                    "positiveIncrease",
                    "Population_2019",
                    "GDP_2019"))


#------------------------------------------------------------------------------------
### Maps ### 
#------------------------------------------------------------------------------------

# data set for map plots
#------------------------------------------------------------------------------------
crossDataForMap <- merge(crossSectionalDF, stateMapData, by = "region")

crossDataForMap <- crossDataForMap[with(crossDataForMap,order(group,order)),]

# ICUDoctors per State
ggplot(crossDataForMap, aes(long, lat, group = group, fill = ICUDoctors)) +
  geom_polygon(color = "white") +
  scale_fill_continuous(low = "white", high = "darkred")


#ggsave(paste0(dirResults,"ICUDoctors_per_state_map.png"))

# covidMortality per State
ggplot(crossDataForMap, aes(long, lat, group = group, fill = covidMortality)) +
  geom_polygon(color = "white") +
  scale_fill_continuous(low = "white", high = "darkred")

#ggsave(paste0(dirResults,"Mortality_per_state_map.png"))



#####################################################
### Analysis: Cross Sectional Data ###
#####################################################
# Model formulation
#------------------------------------------------------------------------------------
cs.mdlA <- covidMortality ~ ICUDoctors

cs.mdlB <- covidMortality ~ ICUDoctors +
  TestResults + Male + Age_65plus + Republican + GDPcap

# Model estimation
#------------------------------------------------------------------------------------
cs.mdlA_rslt <- lm(cs.mdlA, data = crossSectionalDF)

cs.mdlB_rslt <- lm(cs.mdlB, data = crossSectionalDF)

# Display Results
#------------------------------------------------------------------------------------
stargazer(cs.mdlA_rslt, cs.mdlB_rslt)


# Testing homoscedasticity assumption with Breusch–Pagan test
#------------------------------------------------------------------------------------
# cs.mdlA
lmtest::bptest(cs.mdlA_rslt) # homoscedastic, p-value not statistically significant
# Assign results to object
cs.mdlA_bptest <- lmtest::bptest(cs.mdlA_rslt)

# cs.mdlB
lmtest::bptest(cs.mdlB_rslt)
# homoscedastic as p-value was not statistically significant
cs.mdlB_bptest <- lmtest::bptest(cs.mdlB_rslt)
  
  
# Testing multicollinearity among explanatory variables with VIF test
#------------------------------------------------------------------------------------
cs.mdlB_vif <- vif(cs.mdlB_rslt)
# not multicollinear, VIF is less than 5

stargazer(cs.mdlB_vif,
          summary = FALSE,
          type = "text")

#####################################################
### Analysis: Fixed Effects Models ###
#####################################################

# Model formulation
#------------------------------------------------------------------------------------
pd.mdlA <- covidMortality ~ HospitalBeds +
  as.factor(State)

pd.mdlB <- covidMortality ~ HospitalBeds +
  TestResults + Male + Age_65plus + Republican + GDPcap + season +
  as.factor(State)


# Model estimation
#------------------------------------------------------------------------------------
pd.mdlA_rslt <- lm(pd.mdlA, data = PanelDataDF)

pd.mdlB_rslt <- lm(pd.mdlB, na.rm = TRUE, data = PanelDataDF)

# Display Results
#------------------------------------------------------------------------------------
stargazer(pd.mdlA_rslt, pd.mdlB_rslt,
          omit = "State",
          omit.labels = "State FE",
          type = "text")

# Testing homoscedasticity assumption with Breusch–Pagan test
#------------------------------------------------------------------------------------
# pd.mdlA
lmtest::bptest(pd.mdlA_rslt)
# heteroskedasticity as p-value was significant
# Assign results to object
pd.mdlA_bptest <- lmtest::bptest(pd.mdlA_rslt)


# pd.mdlB
lmtest::bptest(pd.mdlB_rslt)
# heteroskedasticity as p-value was significant
# Assign results to object
pd.mdlB_bptest <- lmtest::bptest(pd.mdlB_rslt)

# Robust standard errors
#------------------------------------------------------------------------------------

pd.mdlA_robustSE <- sqrt(diag(vcovHC(pd.mdlA_rslt, type = "HC0")))

pd.mdlB_robustSE <- sqrt(diag(vcovHC(pd.mdlB_rslt, type = "HC0")))


# Display Results with Robust SE
#------------------------------------------------------------------------------------
stargazer(pd.mdlA_rslt, pd.mdlB_rslt,
          omit = "State",
          se = list(pd.mdlA_robustSE,pd.mdlB_robustSE),
          omit.labels = "State FE",
          type = "text")


#####################################################
### Analysis: Consolidated Fixed Effects Model ###
#####################################################

# Model formulation
#------------------------------------------------------------------------------------
pd.mdlC <- covidMortality ~ HospitalBeds + ICUDoctors +
  as.factor(State)


pd.mdlD <- covidMortality ~ HospitalBeds + ICUDoctors + 
  TestResults + Male + Age_65plus + Republican + GDPcap + season +
  as.factor(State)


# Model estimation
#------------------------------------------------------------------------------------
pd.mdlC_rslt <- lm(pd.mdlC, na.rm = TRUE, data = PanelDataDF)

pd.mdlD_rslt <- lm(pd.mdlD, na.rm = TRUE, data = PanelDataDF)

# Display Results
#------------------------------------------------------------------------------------
stargazer(pd.mdlC_rslt, pd.mdlD_rslt,
          omit = "State",
          omit.labels = "State FE")

# Testing homoscedasticity assumption with Breusch–Pagan test
#------------------------------------------------------------------------------------
# pd.mdlC
lmtest::bptest(pd.mdlC_rslt)
# heteroskedasticity as p-value was significant

# assign results to object
pd.mdlC_bptest <- lmtest::bptest(pd.mdlC_rslt)



# pd.mdlD
lmtest::bptest(pd.mdlD_rslt)
# heteroskedasticity as p-value was significant

# assign results to object
pd.mdlD_bptest <- lmtest::bptest(pd.mdlD_rslt)


# Robust standard errors
#------------------------------------------------------------------------------------

pd.mdlC_robustSE <- sqrt(diag(vcovHC(pd.mdlC_rslt, type = "HC0")))

pd.mdlD_robustSE <- sqrt(diag(vcovHC(pd.mdlD_rslt, type = "HC0")))

# Display Results with Robust SE
#------------------------------------------------------------------------------------
stargazer(pd.mdlC_rslt,pd.mdlD_rslt,
          omit = "State",
          omit.labels = "State FE",
          se = list(pd.mdlC_robustSE,pd.mdlD_robustSE))


# Display all panel data results together
#------------------------------------------------------------------------------------
stargazer(pd.mdlA_rslt,
          pd.mdlB_rslt,
          pd.mdlC_rslt,
          pd.mdlD_rslt,
          omit = "State",
          se = list(pd.mdlA_robustSE,
                    pd.mdlB_robustSE,
                    pd.mdlC_robustSE,
                    pd.mdlD_robustSE),
          omit.labels = "State FE",
          type = "text")

# Display all Breusch-Pagan test results together
#------------------------------------------------------------------------------------
# Create vector with BP values
BP <- c(cs.mdlA_bptest$statistic,
        cs.mdlB_bptest$statistic,
        pd.mdlA_bptest$statistic,
        pd.mdlB_bptest$statistic,
        pd.mdlC_bptest$statistic,
        pd.mdlD_bptest$statistic)
# Create vector with df values
df <- c(cs.mdlA_bptest$parameter,
        cs.mdlB_bptest$parameter,
        pd.mdlA_bptest$parameter,
        pd.mdlB_bptest$parameter,
        pd.mdlC_bptest$parameter,
        pd.mdlD_bptest$parameter)
# Create vector with p-values
p_value <- c(cs.mdlA_bptest$p.value,
        cs.mdlB_bptest$p.value,
        pd.mdlA_bptest$p.value,
        pd.mdlB_bptest$p.value,
        pd.mdlC_bptest$p.value,
        pd.mdlD_bptest$p.value)

# Create data frame combining all vectors
df.bptest_rslt <- data.frame(cbind(round(BP,2),
      round(df,0),
      round(p_value,3)))

# Change column names
colnames(df.bptest_rslt) <- c("BP",
                              "df",
                              "p-value")
# Change row names
rownames(df.bptest_rslt) <- c("cs.mdlA",
                              "cs.mdlB",
                              "pd.mdlA",
                              "pd.mdlB",
                              "pd.mdlC",
                              "pd.mdlD")

# Display all bptest resutls
stargazer(df.bptest_rslt,
          summary = FALSE)


