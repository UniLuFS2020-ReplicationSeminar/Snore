library(rio)
library(dplyr)
library(tidyverse)
library(ggplot2)

# ------- 1. Importing and Cleaning
# Import data
sleep_data <- import("C:\\Users\\amand\\OneDrive\\Bureau\\Snore\\Sleep_Efficiency.csv")

# Check fraction of NAs in dataset
NA_distribution <- sleep_data %>%
  is.na %>%
  colSums/nrow(sleep_data)

# Most columns have no missing data, caffeine consumption(6%) and awakenings(4%) are highest

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
# amount of time in bed spent asleep, but is this difference statistically significant?
# Why could this be? Do people who drink more coffee, exercise more which could lead to more efficient sleep? 
# Do they drink more/less alcohol? Is gender an important factor? Could caffeine consumption
# affect the type or duration of sleep?
