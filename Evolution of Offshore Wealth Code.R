# INSTALLING PACKAGES
# install.packages("plm")
# install.packages('olsrr')
# install.packages('fmsb')
# install.packages('readxl')
# install.packages('dplyr')
# install.packages("sandwich")
# install.packages('lmtest')
# install.packages("VIF")
# install.packages("dummies")
# install.packages("qcc")
# install.packages("Smisc")
# install.packages("strucchange")

#LOADING PACKAGES
library('ggplot2')
library('foreign')
library('car')
library('gplots')
library('plm')
library('olsrr')
library('fmsb')
library("plm")
library('olsrr')
library('fmsb')
library('readxl')
library('dplyr')
library('sandwich')
library('lmtest')
library('VIF')
library('dummy')
library('dummies')
library('qcc')
library('Smisc')
library('strucchange')


#LOADING DATA FOR MAIN ANALYSIS, DEFINING VARIABLES
Panel <- read_excel("Desktop/Regression.xlsx")
View(Panel)
Panel$...14 <- NULL
Panel$...15 <- NULL
Panelupload <- na.exclude(Panel)
View(Panelupload)
wealth <- Panelupload$wealth
time <- Panelupload$time
country <- Panelupload$country
debt <- Panelupload$debt
unemployment <- Panelupload$unemployment
politics <- Panelupload$politics
resources <- Panelupload$resources 
GDP <- Panelupload$GDP 
income <- Panelupload$income 
law <- Panelupload$law

#T-TESTS NOT-SCALED + SCALED BY GDP GROWTH
#t-test without GDP adjustment
offshore <- read_excel("Desktop/offshorewealth.xlsx")
View(offshore)
before = offshore$`36981`
after = offshore$`42369`
t.test(before, after, paired = TRUE, alternative = "less")
#t-test with GDP adjustment
offshore2 <- read_excel("Desktop/offi.xlsx")
View(offshore2)
before = offshore2$`2001`
after = offshore2$`2015`
t.test(before, after, paired = TRUE, alternative = "less")
# without GDP adjustment, significant at the 5% significance level
# with the GDP adjustment, significant at 1% significance level


#GRAPHING THE EVOLUTION OF OFFSHORE WEALTH (1-TO-1 PROXIED BY OFFSHORE DEPOSITS)
#Graph labels
region <- factor(country)
region
levels(region) = c('Hungary','Slovakia','Czech Republic', 'Poland')
region
#Graph for Dataset Overview
x <- ggplot(data = Panelupload, mapping = aes(x=time, y=wealth, group = region)) +
  geom_line(aes(color = region), na.rm = TRUE) + 
  title("Evolution of Offshore Deposits in V4 Countries") +
  xlab(NULL) +
  ylab("offshore deposits") +
  theme(panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()) +
  theme_minimal() +
  labs(fill = NULL) 
x
#Graph for Appendix
a <- ggplot(data = Panelupload, mapping = aes(x=time, y=wealth, group = region)) +
  geom_line(aes(color = region), na.rm = TRUE) + 
  title("Evolution of Offshore Wealth in V4 Countries") +
  xlab(NULL) +
  ylab("offshore wealth") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_minimal()
a

#PANEL DATA GRAPHS
summary(country)
coplot(wealth ~ time|country, type="l", data=Panelupload) # Lines
coplot(wealth ~ time|country, type="b", data=Panelupload) # Points and lines
scatterplot(wealth ~ time|labelcountry, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panelupload, ylab = "offshore wealth")
plotmeans(wealth ~ labelcountry, main="Heterogeneity across countries", data=Panelupload, xlab = "country", ylab = "offshore wealth")
plotmeans(wealth ~ time, main="Heterogeneity across years", data=Panelupload, ylab = "offshore wealth")

#HETEROSKEDASTICITY
bptest(wealth ~ debt + unemployment + politics + resources + GDP + income + law + factor(country)-1, data = Panelupload, studentize=F)
# We discover a heteroskedasticity in the series at the time-series level, to account for it, we will apply heteroskedasticity-robust standard errors. 
#Based on Torres-Reyna (2010), it really is a time-series heteroskedasticity accounting for country specific heterogeneity/fixed effect!!!

#SERIAL CORRELATION
pdwtest(wealth ~ debt + unemployment + politics + resources + GDP + income + law + factor(country)-1, data = Panelupload, studentize=F)
#We discover serial correlation AR(1) in our errors

#TIME FIXED EFFECTS
fixed.time <- plm(wealth ~ debt + unemployment + politics + resources + GDP + income + law + factor(time), data=Panelupload, index=c("country",
                                                             "time"), model="within")
summary(fixed.time)
coeftest(fixed.time)

#HAUSMAN TEST
phtest(wealth ~ debt + politics, data=Panelupload)
#Random effects not consistent, only fixed effects consistent
#Why can't we test Hausman with more IVs

#RANDOM EFFECTS
pool <- plm(wealth ~ debt + unemployment + politics + resources + GDP + income + law, data=Panelupload, index=c("country", "time"), model="pooling")
summary(pool)
plmtest(pool, type=c("bp"))
#Random effects are inappropriate

#FIXED EFFECTS WITH SE ADJUSTED FOR HETEROSKEDASTICITY AND SERIAL CORRELATION
#just political stability
fixed1 <- plm(wealth ~ politics, data = Panelupload, index=c("country", "time"), model="within")
summary(fixed1)
coeftest(fixed1)
coeftest(fixed1, vcovHC(fixed1, method = "arellano"))
#just economic stability
fixed2 <- plm(wealth ~ debt + unemployment + GDP + income, data=Panelupload, index=c("country", "time"), model="within")
summary(fixed2)
coeftest(fixed2)
coeftest(fixed2, vcovHC(fixed2, method = "arellano"))
#political and economic stability
fixed3 <- plm(wealth ~ debt + unemployment + politics + GDP + income, data=Panelupload, index=c("country", "time"), model="within")
summary(fixed3)
coeftest(fixed3)
coeftest(fixed3, vcovHC(fixed3, method = "arellano"))
#everything you can dream of
fixed4 <- plm(wealth ~ debt + unemployment + politics + resources + GDP + income + law, data=Panelupload, index=c("country", "time"), model="within")
summary(fixed4)
coeftest(fixed4)
coeftest(fixed4, vcovHC(fixed4, method = "arellano"))

#VIF
fixed.DV <- lm(wealth ~ debt + unemployment + politics + resources + GDP + income + law + factor(country) - 1, data=Panelupload)
ols_vif_tol(fixed.DV)
VIF(fixed.DV)
VIF(fixed4)
# pooled1 <- lm(wealth ~ politics + , data=Panel, )
# VIF(fixed.dum3)
# vif_func(in_frame=rand.vars,thresh=5,trace=T)

#STRUCTUAL BREAK CZECH REP
#Testing for Structural break for Czech Rep Data in Q1 of 2010 or later
Czech <- read_excel("Desktop/Czech Republic.xlsx")
View(Czech)
Czech$...14 <- NULL
Czech$...15 <- NULL
Czechupload <- na.exclude(Czech)
View(Czechupload)
Cwealth <- Czechupload$wealth

## test the model null hypothesis that the average wealth remains
## constant over the years
## compute Rec-CUSUM fluctuation process
wealth.cus <- efp(Cwealth ~ 1)
## plot the process
plot(wealth.cus, alpha = 0.01)
## and calculate the test statistic
sctest(wealth.cus)
## compute (recursive estimates) fluctuation process
## with an additional linear trend regressor
lin.trend <- 1:60
wealth.me <- efp(Cwealth ~ lin.trend, type = "fluctuation")
## plot the bivariate process
plot(wealth.me, functional = NULL)
## and perform the corresponding test
sctest(wealth.me)
## test the model null hypothesis that the average wealth remains
## constant over the years for potential break points between Q1 2010
## (corresponds to from = 0.55) and 2015 (corresponds to to = 1)
## compute F statistics
fs <- Fstats(Cwealth ~ 1, from = 0.55, to = 0.90)
## plot the F statistics
plot(fs, alpha = 0.01)
## and the corresponding p values
plot(fs, pval = TRUE, alpha = 0.01)
## perform the aveF test
sctest(fs, type = "aveF")

## Czech wealth data with one breakpoint
Cwealth37 <- Cwealth[37:60]
plot(Cwealth37)
## F statistics indicate one breakpoint
fs.wealth <- Fstats(Cwealth37 ~ 1)
plot(fs.wealth)
breakpoints(fs.wealth)
lines(breakpoints(fs.wealth))
## or
bp.wealth <- breakpoints(Cwealth37 ~ 1)
summary(bp.wealth)
## the BIC also chooses one breakpoint
plot(bp.wealth)
breakpoints(bp.wealth)
## fit null hypothesis model and model with 1 breakpoint
fm0 <- lm(Cwealth37 ~ 1)
fm1 <- lm(Cwealth37 ~ breakfactor(Cwealth37, breaks = 1))
plot(Cwealth37)
lines(ts(fitted(fm0), start = 1), col = 3)
lines(ts(fitted(fm1), start = 1), col = 4)
lines(bp.wealth)
## confidence interval
ci.wealth <- confint(bp.wealth)
ci.wealth
lines(ci.wealth)

Cwealth <- Czechupload$wealth
Cdebt <- Czechupload$debt
Cunemployment <- Czechupload$unemployment
C1 <- lm(Czechupload$wealth ~ Czechupload$debt + Czechupload$unemployment + Czechupload$politics + Czechupload$resources + Czechupload$GDP + Czechupload$income + Czechupload$law, data=Czechupload)
summary(C1)
w <- Czech$wealth
t <- Czech$time
w1 <- na.omit(w)
View(w1[33:60])
q <- cusum(w1[33:60])
sctest(Czechupload$wealth ~ Czechupload$debt + Czechupload$unemployment + Czechupload$politics + Czechupload$resources + Czechupload$GDP + Czechupload$income + Czechupload$law, data=Czechupload,
       type = "Rec-CUSUM", from = 0.15, to = NULL)
help(cusum)
plot(q)
q1 <- ewma(w1,  decision.interval = 10, limit)
plot(q1)
head(q)
plot(q)
signal(q)
attributes(q)
V4less <- dummy.data.frame(Panel, names = c("Slovakia, Poland") , sep = ".")
