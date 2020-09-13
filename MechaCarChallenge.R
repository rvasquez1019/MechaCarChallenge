# Load libraries
install.packages('tidyverse')
library(ggplot2)
library(tidyverse)
library(dplyr)

# Csv datasets
mechaCar_mpg <- read.csv('MechaCar_mpg.csv')
suspension_coil <- read.csv('Suspension_Coil.csv')

# Quantitative Test of normality 
shapiro.test(mechaCar_mpg$mpg)
# Quality test of normality
ggplot(mechaCar_mpg,aes(mpg))+geom_density()

# Statistical matrix test
matrix <- as.matrix(mechaCar_mpg[, c('mpg','vehicle.length', 'vehicle.weight','spoiler.angle','ground.clearance' )])

# Linear Regression Model
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance, data =mechaCar_mpg))

# Statistic Summary
summarize_sus_coil <- suspension_coil  %>%
  summarise(Mean_PSI = mean(PSI), Median_PSI = median(PSI),Variance_PSI = var(PSI),Stdev_PSI =sd(PSI))

# Statistic summary group by different Lot 
group_summarize_sus_coil <- suspension_coil %>% group_by(Manufacturing_Lot) %>%
  summarise(Mean_PSI = mean(PSI), Median_PSI = median(PSI),Variance_PSI = var(PSI),Stdev_PSI =sd(PSI))

# Sample t-test
t.test(suspension_coil$PSI, mu = 1500)

