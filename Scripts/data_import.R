#setup
library(tidyverse)

#import dataset
sleep <- read.csv("Sleep_Efficiency.csv")


#select dependent and independent variables
sl1 <- sleep %>% 
  select(c(ID, Sleep.efficiency, Caffeine.consumption, Alcohol.consumption, Smoking.status, 
           Exercise.frequency))


