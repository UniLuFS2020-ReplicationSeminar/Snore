#Data modeling  -----------------------------------------------------------------
install.packages("BBmisc")
install.packages("stargazer")
library(BBmisc)
library(stargazer)

slsc <- normalize(x = sl1[,3:7])
summary(slsc)

# Get a first look at data ------------------------------------------------------
head(slsc, n = 5)
str(slsc)

# Check for missing values ------------------------------------------------------

cat("Number of missing values:\n")
cat("by column:\n")
nan_val_count <- colSums(is.na(slsc))
print(nan_val_count)

som <- sum(nan_val_count)
cat("Total:", som, " ~ ", round(som / (nrow(slsc) * ncol(slsc)) * 100), "% of the dataset\n")


#--------------------------------------------------------------------------------
model1<-lm(slsc$Sleep.efficiency~slsc$Caffeine.consumption+slsc$Alcohol.consumption+slsc$Smoking.status)
summary(model1)
summary(model1)$coefficient
confint(model1)
stargazer(model1, type='text')
