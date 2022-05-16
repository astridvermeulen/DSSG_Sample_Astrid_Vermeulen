#load packages
if(!require(pacman, quietly = TRUE)) install.packages('pacman') ; require(pacman, quietly = TRUE)
p_load(tidyverse, skimr, psych,correlation, lubridate, Hmisc)
p_load(imputeMissings)
p_load("readxl")

#Load in data
riders <- read_excel('./RiderData.xlsx')
races <- read.csv('./Races_WITHindex.csv')



