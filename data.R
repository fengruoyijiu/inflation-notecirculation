library(readxl)
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)

#load the dataset
#replace the string "DATA PATH" with your data path
dat <- read_excel("DATA PATH/data.xlsx", sheet = 5, skip = 0)
head(dat)

#declare time serious variables
inflation <- ts(dat$Inflation, start = c(1944), frequency = 1)
note <- ts(dat$GRNC, start = c(1944), frequency = 1)

#plot the series - line plot
autoplot(cbind(inflation, note))

#the philosophy of the VAR is that we are not supposed to be imposing structures
# it's not right for us to impose a definite structure of how data are related
#better to let the data speak for itself for the chain of causality, 
# i.e. dependent and independent variables

#Find the optimal lag
dat.bv <- cbind(inflation, note)
colnames(dat.bv) <- cbind("inflation", "note.circ")

lagselect <- VARselect(dat.bv, lag.max = 10, type = "const")
lagselect$selection
  #since most indicators say 6 lags for our VAR, we use a lag of 6


#Section 1  VAR(6)
###############################################################
###############################################################

#Building VAR
model1 <- VAR(dat.bv, p = 6, type = "const", 
              season = NULL, exogen = NULL)
summary(model1)

###########################################
#diagnosing the VAR

  #Serial correlation
Serial1 <- serial.test(model1, lags.pt = 12, type = "PT.asymptotic")
Serial1  
# p = 0.07 > 0.05 means no serial correlation

  #heteroscedasticity
Arch1 <- arch.test(model1, lags.multi = 12, multivariate.only = TRUE)
Arch1
# p =0.8476 > 0.05 means no heteroscedasticity

  #Normal distribution of the residuals
Norm1 <- normality.test(model1, multivariate.only = TRUE)
Norm1
# all p values > 0.05, suggesting normality

  #testing for structural breaks in the residuals
Stability1 <- stability(model1, type = "OLS-CUSUM")
plot(Stability1)
# no points exceed the red line, meaning the system is stable

################################
# Granger causality
Granger.inflation <- causality(model1, cause ="inflation")
Granger.inflation
# p < 0.05, we can reject the null hypothesis 
#"H0: inflation does not Granger-cause note.cric"
# we conclude that: inflation Granger causes note circulation

Granger.note <- causality(model1, cause ="note.circ")
Granger.note
# p = 0.1621 > 0.05, we fail to reject the null hypothesis
# H0: note.circ do not Granger-cause inflation
# note doesn't seem to causes inflation

#Impulse response functions
inflation.irf <- irf(model1, impulse = "note.circ",
                     response = "inflation",
                     n.ahead = 20, boot = TRUE)
plot(inflation.irf, ylab = "Inflation", main = "Shock from note circulation")


note.irf <- irf(model1, impulse = "inflation",
                     response = "note.circ",
                     n.ahead = 20, boot = TRUE)
plot(note.irf, ylab = "Note circulation", main = "Shock from inflation")

#Variance Decomposition
FEVD1 <- fevd(model1, n.head = 10)
win.graph(width=15,height=8)
plot(FEVD1, xlab = "Number of Years Ahead")


#Section 2  VAR(1)
###############################################################
###############################################################
#Building VAR
model2 <- VAR(dat.bv, p = 1, type = "const", 
              season = NULL, exogen = NULL)
summary(model2)


###########################################
#diagnosing the VAR

#Serial correlation
Serial2 <- serial.test(model2, lags.pt = 12, type = "PT.asymptotic")
Serial2  
# p = 0.3025 > 0.05 means no serial correlation

#heteroscedasticity
Arch2 <- arch.test(model2, lags.multi = 12, multivariate.only = TRUE)
Arch2
# p =0.4819 > 0.05 means no heteroscedasticity

#Normal distribution of the residuals
Norm2 <- normality.test(model2, multivariate.only = TRUE)
Norm2
# all p values < 0.05, suggesting non-normality!!!!

#testing for structural breaks in the residuals
Stability2 <- stability(model2, type = "OLS-CUSUM")
plot(Stability2)
# no points exceed the red line, meaning the system is stable

################################
# Granger causality
Granger.inflation <- causality(model2, cause ="inflation")
Granger.inflation
# p = 0.1303 > 0.05, we fail to reject the null hypothesis 
#"H0: inflation does not Granger-cause note.circ"

Granger.note <- causality(model2, cause ="note.circ")
Granger.note
# p = 0.02338 < 0.05, we reject the null hypothesis
# H0: note.cric does not Granger-cause inflation
# note.cric seems to Granger cause inflation


#Impulse response functions
inflation.irf <- irf(model2, impulse = "note.circ",
                     response = "inflation",
                     n.ahead = 20, boot = TRUE)
plot(inflation.irf, ylab = "Inflation", main = "Shock from note circulation")


note.irf <- irf(model2, impulse = "inflation",
                response = "note.circ",
                n.ahead = 20, boot = TRUE)
plot(note.irf, ylab = "Note circulation", main = "Shock from inflation")

#Variance Decomposition
FEVD1 <- fevd(model2, n.head = 10)
win.graph(width=15,height=8)
plot(FEVD1, xlab = "Number of Years Ahead")
