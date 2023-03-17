#Data modeling  -----------------------------------------------------------------
library(BBmisc)
library(stargazer)

slsc <- normalize(x = sl1[,2:7])
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

# replace null values with mean-------------------------------------------------

library(dplyr) # load the dplyr package

slsc$Awakenings[is.na(slsc$Awakenings)] <- mean(slsc$Awakenings, na.rm = TRUE)
slsc$`Caffeine consumption`[is.na(slsc$`Caffeine.consumption`)] <- mean(slsc$`Caffeine.consumption`, na.rm = TRUE)
slsc$`Alcohol consumption`[is.na(slsc$`Alcohol.consumption`)] <- mean(slsc$`Alcohol.consumption`, na.rm = TRUE)
slsc$`Exercise frequency`[is.na(slsc$`Exercise.frequency`)] <- mean(slsc$`Exercise.frequency`, na.rm = TRUE)
slsc$`Sleep efficiency`[is.na(slsc$`Sleep.efficiency`)] <- mean(slsc$`Sleep.efficiency`, na.rm = TRUE)
#--------------------------------------------------------------------------------
model1<-lm(sleep_data$`Sleep efficiency`~sleep_data$`Caffeine consumption`+sleep_data$`Alcohol consumption`+sleep_data$`Smoking status`+sleep_data$`Exercise frequency`+sleep_data$Gender)
summary(model1)
summary(model1)$coefficient
confint(model1)
stargazer(model1, type='text')
