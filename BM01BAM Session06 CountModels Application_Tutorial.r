#-------------------------------------------------------------------------
# BM01BAM session 6: Count Data Models
#-------------------------------------------------------------------------

remove(list=ls())

#-------------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------------

# Package plyr for group statistics
# install.packages("plyr", dependencies = TRUE)
library(plyr)

# Package stargazer for appropriately formatted tables
# install.packages("stargazer", dependencies = TRUE)
library(stargazer)

# Package ggplot2 for graphics
# install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Package aer for the data 
# install.packages("AER", dependencies = TRUE)
library(AER)

# Package pscl for the prussian data 
# install.packages("pscl", dependencies = TRUE)
library(pscl)

# Package MASS for the negative binomial regression 
# install.packages("MASS", dependencies = TRUE)
library(MASS)

# PAckage sandwich to determine heteroskedaticity
# robust standard errors
# install.packages("sandwich", dependencies = TRUE)
library(sandwich)  

# Package extraDistr for random sampling from a
# conditional Poisson distribution
# install.packages("extraDistr")
library(extraDistr)

#-------------------------------------------------------------------------
# Define paths
#-------------------------------------------------------------------------

dir <- "C:/Users/Gutt/Dropbox (Erasmus Universiteit Rotterdam)/Advanced Statistics & Programming/2021/Tutorials/Session 6/"

dirData <- paste0(dir, "Session00Data/")
dirProg <- paste0(dir, "Session06/Programs/")
dirRslt <- paste0(dir, "Session06/Slides/Figures/")

#-------------------------------------------------------------------------
# Import data about doctor's visits
#-------------------------------------------------------------------------

# The applications are based on the example of doctor's 
# visits. This example is also in Greene's book, though 
# in a different form. The data are available in package 
# AER

# Data have been applied in:
# John Mullahy, "Heterogeneity, Excess Zeros, and the Structure of Count
# Data Models", Journal of Applied Econometrics, Vol. 12, No. 3, 1997,
# pp. 337-350.

# The sample consists of 5,190 observations and is from the 1977-78
# Australian Health Survey and contains information on health service
# utilization and covariates describing factors that affect health care
# utilization propensities.  The sample, whose distribution in entirety is
# limited by agreement with its Australian source, were kindly provided to
# me by Prof. P.K. Trivedi.  The dataset posted here contains only those
# variables used in the analysis.  A detailed description is provided in: 
#   
# Cameron, A.C. and P.K. Trivedi. 1986. "Econometric Models Based on 
# Count Data: Comparisons and Applications of Some Estimators and 
# Tests." Journal of Applied Econometrics 1: 29-53.

# Make available the data about doctor's visits
# http://qed.econ.queensu.ca/jae/1997-v12.3/mullahy/
data("DoctorVisits")
help(DoctorVisits)

# Variables
# visits    Number of doctor visits in past 2 weeks.
# gender    Factor indicating gender.
# age       Age in years divided by 100.
# income    Annual income in tens of thousands of dollars.
# illness   Number of illnesses in past 2 weeks.
# reduced   Number of days of reduced activity in past 2 weeks due to illness or injury.
# health    General health questionnaire score using Goldberg's method.
# private   Factor. Does the individual have private health insurance?
# freepoor  Factor. Does the individual have free government health insurance due to low income?
# freerepat Factor. Does the individual have free government health insurance due to old age, disability or veteran status?
# nchronic  Factor. Is there a chronic condition not limiting activity?
# lchronic  Factor. Is there a chronic condition limiting activity?

#-------------------------------------------------------------------------
# Distribution of the doctor's visits
#-------------------------------------------------------------------------

ggplot(DoctorVisits, aes(x=visits)) +
   geom_bar(fill="Blue3", colour="black") +
   xlab("Doctor's visits past two weeks") + 
   theme(axis.title = element_text(size=rel(1.15)),
         axis.text  = element_text(size=rel(1.15)))
ggsave(paste0(dirRslt, "Session06CountsDoctorVisits01.pdf"))


#-------------------------------------------------------------------------
# Basic model specification and estimation
#-------------------------------------------------------------------------

# Model specification
mdlA <- visits ~ age + income + private

# Model estimation
rsltPoisson <- glm(mdlA, data=DoctorVisits, family=c("poisson"))

# Ship results to a latex file
stargazer(rsltPoisson, 
          align=TRUE, no.space = TRUE, intercept.bottom = FALSE, type="text")


#-------------------------------------------------------------------------
# Find predicted values
#-------------------------------------------------------------------------

# # Test area
# dfSub         <- DoctorVisits[c("age", "income", "private")]
# dfSub$private <-as.numeric(as.character(dfSub$private)=="yes")
# est <- coef(rsltPoisson)
# tmp <- as.matrix(cbind(1,dfSub))
# head(tmp %*% est)

# Type 'link' gives the predicted link function x'beta,
head(predict.glm(rsltPoisson, type="link"))

# Find parameter estimates 
estBeta <- coef(rsltPoisson)

# Calculate the average partial effects, APE
APE <- mean(exp(predict.glm(rsltPoisson, type="link")))*estBeta
round(APE, 3)

# Calculate the average partial effects for dummy, APE
tmp <- DoctorVisits

tmp$private[tmp$private == "no"] <- "yes"
tmpAPE.1 <- mean(exp(predict.glm(rsltPoisson, newdata=tmp, type="link")))
round(tmpAPE.1, 3)

tmp$private[tmp$private == "yes"] <- "no"
tmpAPE.0 <- mean(exp(predict.glm(rsltPoisson, newdata=tmp, type="link")))
round(tmpAPE.0, 3)

APE.private <- tmpAPE.1 - tmpAPE.0
round(cbind(tmpAPE.1, tmpAPE.0, APE.private), 3)


#-------------------------------------------------------------------------
# Assess model fit
#-------------------------------------------------------------------------

str(rsltPoisson)

# Make a data frame
dsSub <- 
   data.frame(y = DoctorVisits$visits,
              mu.hat = predict(rsltPoisson, 
                               type="response"))
dsSub$lnL.ML   <- log(dpois(dsSub$y, dsSub$mu.hat))
dsSub$lnL.null <- log(dpois(dsSub$y, mean(dsSub$y)))
dsSub$lnL.sat  <- log(dpois(dsSub$y, dsSub$y))
dsSub$R2Num    <- ((dsSub$y - dsSub$mu.hat)/sqrt(dsSub$mu.hat))^2
dsSub$R2Den    <- ((dsSub$y - mean(dsSub$y))/sqrt(mean(dsSub$y)))^2

# Show some of the calculations
round(head(dsSub), 3)

# Calculate the column sums
tmp <- colSums(dsSub)
round(tmp, 3)

# Determine the goodness of fit measures
R2.p  <- 1 - tmp["R2Num"]/tmp["R2Den"]
R2.d  <- 1 - (tmp["lnL.sat"] - tmp["lnL.ML"])/
             (tmp["lnL.sat"] - tmp["lnL.null"])
R2.LRI<- 1- tmp["lnL.ML"]/tmp["lnL.null"]

round(cbind(R2.p, R2.d, R2.LRI), 3)



#-------------------------------------------------------------------------
# Examine under/over-dispersion
#-------------------------------------------------------------------------

# Estimate the quasi-Poisson and negative binomial models
# (function glm.nb is from the MASS package)
rsltQuasi   <- glm(mdlA, data=DoctorVisits, family=c("quasipoisson"))
rsltNegBin  <- glm.nb(mdlA, data=DoctorVisits)

summary(rsltPoisson)
summary(rsltQuasi)
summary(rsltNegBin)

# Make table of the results
seWhite <- sqrt(diag(vcovHC(rsltPoisson, type="HC0")))

stargazer(rsltPoisson, rsltPoisson, rsltQuasi, rsltNegBin, type="text",
          align=TRUE, no.space = TRUE, intercept.bottom = FALSE,
          se = list(NULL, seWhite, NULL, NULL))

# Perform Likehood Ratio test
lrtest(rsltPoisson, rsltNegBin)



#-------------------------------------------------------------------------
# Zero-inflated and hurdle models
#-------------------------------------------------------------------------

#------------------
# Simulation
#------------------
# A simulation of these models is performed in order
# to demonstrate their difference

# In a zero-inflated data generating process, two processes
# are at work. Example from book: one process z that indi-
# cates if production (or something else) is in control
# (z=0) or not (z=1); and second process y that generates
# a number of defectives which can be 0 or positive
nObs  <- 100   # Sample size
prob.z <- 0.6  # Probability z = 1
lambda <- 2    # Poisson parameter

# ... generate the data
dfTmp <- data.frame(z = rbinom(nObs, 1, prob.z),
                    y.latent = rpois(nObs, 2))
dfTmp$y.obs <- dfTmp$z*dfTmp$y.latent

# ... make the plots
ggplot(dfTmp, aes(x=y.obs)) +
   geom_bar(color="black", fill="Blue4")+ 
   theme(axis.title = element_text(size=rel(1.15)),
         axis.text  = element_text(size=rel(1.15)))
ggsave(paste0(dirRslt, "Session06CountsZeroInflated01.pdf"))
ggplot(dfTmp, aes(x=y.obs, fill=as.factor(z))) +
   geom_bar(color="black") +
   scale_fill_manual("z", values = c("Red3", "Blue4")) +
   theme(axis.title = element_text(size=rel(1.15)),
         axis.text  = element_text(size=rel(1.15)))
ggsave(paste0(dirRslt, "Session06CountsZeroInflated02.pdf"))


# In a hurdle data generating process, likewise two pro-
# cesses are at work, where the outcomes of the second
# are conditional on the first. Example from book: a
# first process z decides if someone decides to make use
# of health care (z=1) or not (z=0), while the second
# process y, is conditional on the former, describes how
# often use is made of health care services
nObs  <- 100   # Sample size
prob.z <- 0.6  # Probability z = 1
lambda <- 2    # Poisson parameter

# ... generate the data; function rtpois takes random 
# ... draw from conditional Poisson distribution, i.e.
# ... conditional on y >= 1, if z=1
dfTmp <- data.frame(z = rbinom(nObs, 1, prob.z))
dfTmp$y.obs <- (dfTmp$z > 0)*rtpois(nObs, lambda, 0)


# ... make the plots
ggplot(dfTmp, aes(x=y.obs)) +
   geom_bar(color="black", fill="Blue4")+ 
   theme(axis.title = element_text(size=rel(1.15)),
         axis.text  = element_text(size=rel(1.15)))
ggsave(paste0(dirRslt, "Session06CountsHurdle01.pdf"))
ggplot(dfTmp, aes(x=y.obs, fill=as.factor(z))) +
   geom_bar(color="black") +
   scale_fill_manual("z", values = c("Red3", "Blue4")) +
   theme(axis.title = element_text(size=rel(1.15)),
         axis.text  = element_text(size=rel(1.15)))
ggsave(paste0(dirRslt, "Session06CountsHurdle02.pdf"))


#------------------
# Application to doctor's visits
#------------------

# In R, zero-inflated and hurdle models can be estimated 
# with respectively functions zeroinfl and hurdle from the
# pscl package which needs to be installed and load first)

# Estimate the zero-inflated and hurdle models 
# (functions zeroinfl and hurdle are from package pscl)

# The formula mdlA needs to be updated with `| 1`,
# where `1` refers to a constant term (can also be 
# a model). As the regular update function did not 
# work as expected, a brute force workaround was applied 
mdlAplus <- mdlA

mdlAplus[[3]][[1]] <- quote(`|`)
mdlAplus[[3]][[2]] <- mdlA[[3]]
mdlAplus[[3]][[3]] <- quote(1)

rsltZIP  <- zeroinfl(mdlAplus, data=DoctorVisits, dist = "poisson")
rsltZINB <- zeroinfl(mdlAplus, data=DoctorVisits, dist = "negbin")
rsltHRP  <- hurdle(mdlAplus,   data=DoctorVisits, 
                   zero.dist="binomial", dist="poisson")
rsltHRNB <- hurdle(mdlAplus,   data=DoctorVisits, 
                   zero.dist="binomial", dist = "negbin")


summary(rsltZIP)
summary(rsltZINB)
summary(rsltHRP)
summary(rsltHRNB)

str(summary(rsltZIP))
str(summary(rsltZINB))


# Again specific results need to be added to the table by
# using add.lines instructions
est.ZIP  <- summary(rsltZIP)$coefficients$zero
est.ZINB <- summary(rsltZINB)$coefficients$zero
est.HRP  <- summary(rsltHRP)$coefficients$zero
est.HRNB <- summary(rsltHRNB)$coefficients$zero

addZero.est <- c("zero const",
                 "", "",
                 round(est.ZIP[1, "Estimate"], 3),
                 round(est.HRP[1, "Estimate"], 3),
                 "",
                 round(est.ZINB[1, "Estimate"], 3),
                 round(est.HRNB[1, "Estimate"], 3))
addZero.std <- c("",
                 "", "",
                 round(est.ZIP[1, "Std. Error"], 3),
                 round(est.HRP[1, "Std. Error"], 3),
                 "",
                 round(est.ZINB[1, "Std. Error"], 3),
                 round(est.HRNB[1, "Std. Error"], 3))

# same for logLik and AIC
add.lnL <- c("lnL", 
             round(logLik(rsltPoisson),3), 
             round(logLik(rsltPoisson),3),
             round(logLik(rsltZIP),3), 
             round(logLik(rsltHRP),3), 
             round(logLik(rsltNegBin),3), 
             round(logLik(rsltZINB),3), 
             round(logLik(rsltHRNB),3)
)

add.Aic <- c("AIC", 
             round(AIC(rsltPoisson),3), 
             round(AIC(rsltPoisson),3),
             round(AIC(rsltZIP),3), 
             round(AIC(rsltHRP),3), 
             round(AIC(rsltNegBin),3), 
             round(AIC(rsltZINB),3), 
             round(AIC(rsltHRNB),3)
)

# Make table of the results
seWhite <- sqrt(diag(vcovHC(rsltPoisson, type="HC0")))

stargazer(rsltPoisson, rsltPoisson, rsltZIP, rsltHRP,
          rsltNegBin, rsltZINB, rsltHRNB,
          align=TRUE, no.space = TRUE, intercept.bottom = FALSE,
          se = list(NULL, seWhite, NULL, NULL, NULL, NULL, NULL),
          add.lines = list(addZero.est,addZero.std,
                           add.lnL, add.Aic))


