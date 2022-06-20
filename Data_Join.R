rm(list = ls())

#install packages
if (!require("pacman")) install.packages("pacman")
require("pacman", character.only = TRUE, quietly = TRUE)
p_load(tidyverse)
library("readxl")
library('stringr')
library('dplyr')
p_load(fuzzyjoin)
library(ggplot2)

first_cycling_data <- read.csv("Races_WITHindex.csv", header = TRUE)
pro_cycling_data <- read_excel("RiderData.xlsx")

#check for missing values
colSums(is.na(first_cycling_data))
colSums(is.na(pro_cycling_data))

#impute missing UCIs with 0 as missing values means that the driver finished outside of the points and thus did not earn any
first_cycling_data$UCI[is.na(first_cycling_data$UCI)] <- 0

#drop races with missing values for Team_ID and/or Rider_ID (signify different racing formats) 
first_cycling_data <- first_cycling_data %>% drop_na(Team_ID, Rider_ID)

#merge on name and team (because what if a rider switches teams?)
#merge Given Name and Family name to one column: Rider (necessary for merge)
pro_cycling_data$Rider <- str_c(pro_cycling_data$`Family Name`, ' ', pro_cycling_data$`Given Name`)

# get distinct riders from first both datasets
riders_FC <- first_cycling_data %>% distinct(Rider)
riders_PC <- pro_cycling_data %>% distinct(Rider)

# match all riders in First Cycling data with the closest corresponding name in Pro Cycling data
riders_match_table <- stringdist_join(riders_FC, riders_PC,
                                      by="Rider",
                                      method = 'jw',
                                      max_dist=0.99,
                                      mode='left',
                                      distance_col='dist') %>%
  group_by(Rider.x) %>%
  slice_min(order_by = dist, n=1) 

# show ties (i.e. names matched with 2 or more names that show similar deviations)
riders_match_table[duplicated(riders_match_table$Rider.x),]

# manual inspection shows that there are no correct matches
# cut-off should be below minimum value: 0,1325
min(riders_match_table[duplicated(riders_match_table$Rider.x),]$dist) 

# make an elbow plot to determine cut-off
# first filter duplicates resulting from ties
riders_match_table <- riders_match_table %>% distinct(Rider.x, .keep_all = TRUE)

plot_data <- data.frame(cut_off=numeric(), riders_matched=integer())

for (i in seq(0, 0.99, by=0.01)) {
  plot_data <- plot_data %>% add_row(cut_off=i, riders_matched=nrow(riders_match_table %>%
                                                                      filter(dist < i)))
}

ggplot(data=plot_data, aes(x=cut_off, y=riders_matched, group=1)) +
  geom_line()+
  geom_point()+
  geom_vline(xintercept = 0.1, linetype="dotted", 
             color = "red")+
  ggtitle("Approximate String Matching")+
  ylab("# riders matched")+ 
  xlab("cut-off (JW score)")+
  ylim(0,1800) +
  xlim(0.001,0.5)

# based on manual inspection of the database and the plot, we take a cut-off value of 0.1
basetable <- stringdist_join(first_cycling_data, pro_cycling_data, 
                             by="Rider",
                             method = 'jw',
                             max_dist=0.1,
                             mode='inner',
                             distance_col='dist') %>%
  group_by(Rider.x) %>%
  slice_min(order_by = dist, n=1) 

# check some of the matched names that are not identical
head(basetable %>% select(Rider.x, Rider.y, dist) %>% distinct() %>% filter(dist>0))

# drop columns used for fuzzy string merge
basetable <- basetable %>% select(-c(dist, Rider.y)) %>% rename(Rider = Rider.x)

# save basetable
save(basetable, file = "basetable_merge_phase.RData")
