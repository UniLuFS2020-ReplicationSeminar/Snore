library(rio)
library(dplyr)
library(tidyverse)
library(ggplot2)

# ------- 1. Importing and Cleaning
# Import data
sleep_data <- import("C:\\Users\\amand\\OneDrive\\Bureau\\Snore\\Sleep_Efficiency.csv")

# ------- 2. Data Visualisation 1: Impact of caffeine consumption on sleep efficiency

# Histogram of age distribution
histogram_caffeine <- sleep_data %>% 
  drop_na(`Caffeine consumption`) %>% 
  ggplot(aes(`Caffeine consumption`))+
  geom_histogram(stat="count")

# Divide caffeine consumption into groups:
# None=0
# Low+ 25
# Medium=50
# High >=75

sleep_data_caffeine_groups <- sleep_data %>% 
  drop_na(`Caffeine consumption`) %>% 
  mutate(caffeine_group=case_when(`Caffeine consumption`==0 ~ "None",
                                  `Caffeine consumption` ==25 ~ "Low (25mg)",
                                  `Caffeine consumption` ==50 ~ "Medium (50mg)",
                                  TRUE ~ "High (75mg-200mg)"))

# Convert caffeine consumption groups column to factor variable 
sleep_data_factor <- sleep_data_caffeine_groups %>% 
  mutate(caffeine_group= factor(caffeine_group,
                                ordered=TRUE,
                                levels=c("None","Low (25mg)","Medium (50mg)","High (75mg-200mg)")))


# Box plots indicating how caffeine consumption affects sleep efficiency
caffeine_sleep_efficiency <- ggplot(sleep_data_factor, aes(x=caffeine_group, y=`Sleep efficiency`)) + 
  geom_boxplot()+
  xlab(" Amount of Caffeine Consumed")+
  ylab("Sleep Efficiency")+
  ggtitle("Effect of Caffeine Consumption on Sleep Efficiency")+
  theme_minimal()

# Counter intuitively appears that drinking more coffee improves sleep efficiency i.e 
# amount of time in bed spent asleep, but is this difference statistically significant? The median
# sleep efficiency in the high group is larger than the others, but all groups overlap.
# Why could this be? Do people who drink more coffee, exercise more which could lead to more efficient sleep? 
# Do they drink more/less alcohol? Is gender an important factor? Could caffeine consumption
# affect the type or duration of sleep?

# ------- 2. Data Visualisation 2: Impact of gender and caffeine consumption on sleep efficiency

# Plot using the same code as above but split by gender
caffeine_gender_sleep_efficiency <- ggplot(sleep_data_factor, aes(x=caffeine_group, y=`Sleep efficiency`, fill=Gender)) + 
  geom_boxplot()+
  xlab(" Amount of Caffeine Consumed")+
  ylab("Sleep Efficiency")+
  ggtitle("Effect of Caffeine Consumption and Gender on Sleep Efficiency")+
  theme_minimal()

# Seems to be a difference in the median sleep efficiency between the genders for each level of caffeine 
# consumption, but is it significant? For women consuming no or high levels of caffeine seems
# to increase sleep efficiency, while low to medium amounts decrease it.(Quadratic shape?) 
# Mens' sleep efficiency seems to increase with the amount of caffeine consumed. Seems unlikely caused
# by caffeine affecting men and women in different ways, instead likely to have different lifestyles.

# -------  Data Visualisation 3. Comparison of lifestyle choices between genders
# Histogram of exercise frequency by gender
histogram_exercise <- sleep_data %>% 
  drop_na(`Exercise frequency`) %>% 
  ggplot(aes(`Exercise frequency`, fill=Gender))+
  geom_histogram(stat="count", position="dodge")+
  ggtitle("Difference in exercise frequency between men and women")

#Men seem to exercise considerably more than women
# Histogram of alcohol consumption by gender
histogram_alcohol <- sleep_data %>% 
  drop_na(`Alcohol consumption`) %>% 
  ggplot(aes(`Alcohol consumption`, fill=Gender))+
  geom_histogram(stat="count", position="dodge")+
  ggtitle("Difference in alcohol consumption between men and women")
# Not big difference in alcohol consumption between genders, most
# participants in sleep study don't rink at all






