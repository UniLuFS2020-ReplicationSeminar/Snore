#Data modeling
install.packages("BBmisc")
install.packages("stargazer")
library(BBmisc)
library(stargazer)

slsc <- normalize(x = sl1[,3:7])
summary(slsc)

# Get a first look at data
head(slsc, n = 5)
str(slsc)

model1<-lm(slsc$Sleep.efficiency~slsc$Caffeine.consumption+slsc$Alcohol.consumption+slsc$Smoking.status)
summary(model1)
summary(model1)$coefficient
confint(model1)
stargazer(model1, type='text')
