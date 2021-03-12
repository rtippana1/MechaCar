#Deliverable 1: Linear Regression to Predict MPG
#Laod necessary packages
library(dplyr)
library(tidyverse)
#Import and Read the CSV File as DataFrame
Car_Data <- read.csv('Resources/MechaCar_mpg.csv')
head(Car_Data)
#generate multiple linear regression model
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data =Car_Data)
#generate summary statistics
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data =Car_Data)) 





