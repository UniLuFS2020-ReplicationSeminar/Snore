#setup
install.packages("tidyverse")
library(tidyverse)

#import dataset
sleep <- read.csv("Sleep_Efficiency.csv")


#select dependent and independent variables
sl1 <- sleep %>% 
  select(c(ID, Age,Sleep.efficiency, Caffeine.consumption, Alcohol.consumption, Smoking.status, 
           Exercise.frequency))

# Get a first look at data
head(sleep, n = 5)
str(sleep)