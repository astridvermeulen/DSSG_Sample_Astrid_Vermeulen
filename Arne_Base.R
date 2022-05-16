#load packages
if(!require(pacman, quietly = TRUE)) install.packages('pacman') ; require(pacman, quietly = TRUE)
p_load(tidyverse, skimr, psych,correlation, lubridate, Hmisc)
p_load(imputeMissings)
p_load("readxl")
p_load(randomForest)

#Load in data
pro_cycling_data <- read_excel('./RiderData.xlsx')
first_cycling_data <- read.csv('./Races_WITHOUTindex.csv')

#impute missing UCIs with 0 as missing values means that the driver finished outside of the points and thus did not earn any
first_cycling_data$UCI[is.na(first_cycling_data$UCI)] <- 0

#merge on name and team (because what if a rider switches teams?)
#merge Given Name and Family name to one column: Rider (necessary for merge)
pro_cycling_data$Rider <- str_c(pro_cycling_data$`Given Name`, ' ', pro_cycling_data$`Family Name`)
# merge two data frames by ID
basetable <- merge(first_cycling_data, pro_cycling_data,by="Rider")

#define succes on team basis: annually more (or equal) points than the previous year = succes?
#Pos is character column, so this will not work
#basetable$success <- ifelse(basetable$Pos <= 60, 1, 0)
#basetable$podium <- ifelse(basetable$Pos <= 3, 1, 0)

#check missing values
colSums(is.na(basetable))
#Var_9 has a high amount of missing values, as well as VAR_16, so we will drop those

#drop columns that are not of any use
drop <- c("X.x", "Rider_ID", "Team_ID", "ID", "Family Name", "Given Name", "Name", "Team.y", "Region", "ID2", "VAR10", "VAR9", "VAR16")
basetable = basetable[,!(names(basetable) %in% drop)]

#get age of rider
basetable$Birthday <- as.Date(basetable$Birthday)
basetable$age <- as.numeric(year(Sys.Date()) - year(basetable$Birthday))
#drop birthday
drop <- c("Birthday")
basetable = basetable[,!(names(basetable) %in% drop)]

basetable %>%
  glimpse()

#As a tryout give riders points based on the F1 system
#If he rider comes first, he gets 10 points. If he is 2nd, 9 points
#All the way down to 1 point for place 10
#If you finish outside of top 10, 0 points
#Can be altered to be more forgiving
basetable$points <- ifelse(basetable$Pos == '01', 10, 
                           ifelse(basetable$Pos == '02', 9,
                              ifelse(basetable$Pos == '03', 8,
                                ifelse(basetable$Pos == '04', 7,
                                  ifelse(basetable$Pos == '05', 6,
                                    ifelse(basetable$Pos == '06', 5,
                                           ifelse(basetable$Pos == '07', 4,
                                                  ifelse(basetable$Pos == '08', 3,
                                                         ifelse(basetable$Pos == '09', 2,
                                  ifelse(basetable$Pos == '10', 1, 0)
                           )
))))))))


#Define that a driver has a specialty in a specific category if he has a score in that category above 75
#Exact cutoff also up for discussion, most categories have a range between +-50 and +-83
basetable$flat_driver <- ifelse(basetable$FLAT >= 75,1,0)
basetable$mountain_driver <- ifelse(basetable$MOUNTAIN >= 75,1,0)
basetable$downhill_driver <- ifelse(basetable$DOWNHILL >= 75,1,0)
basetable$cobbles_driver <- ifelse(basetable$COBBLES >= 75,1,0)
basetable$tt_driver <- ifelse(basetable$TT >= 75,1,0)
basetable$prologue_driver <- ifelse(basetable$PROLOGUE >= 75,1,0)
basetable$sprint_driver <- ifelse(basetable$SPRINT >= 75,1,0)
basetable$acceleration_driver <- ifelse(basetable$ACCELERATION >= 75,1,0)
basetable$resistance_driver <- ifelse(basetable$RESISTANCE >= 75,1,0)
basetable$endurance_driver <- ifelse(basetable$ENDURANCE >= 75,1,0)
basetable$recup_driver <- ifelse(basetable$RECUP >= 75,1,0)
basetable$hill_driver <- ifelse(basetable$HILL >= 75,1,0)
basetable$attack_driver <- ifelse(basetable$ATTACK >= 75,1,0)


#Compute team scores on different aspects per race (quantifying team selection)
#Like this we get per row the specifics of a team, per race, per year
team_scores_per_race <-basetable %>%
  group_by(Year, Race_ID, Team.x) %>%
  summarise(team_points = sum(points), team_popularity = sum(Popularity), team_potential = sum(Potential),
            team_score_flat = sum(FLAT), team_score_mountain = sum(MOUNTAIN), team_score_downhill = sum(DOWNHILL),
            team_score_cobbles = sum(COBBLES), team_score_tt = sum(TT), team_score_prologue = sum(PROLOGUE),
            team_score_sprint = sum(SPRINT), team_score_acceleration = sum(ACCELERATION), team_score_endurance = sum(ENDURANCE),
            team_score_resistance = sum(RESISTANCE), team_score_recup = sum(RECUP), team_score_hill = sum(HILL),
            team_score_attack = sum(ATTACK), 
            avg_team_size = mean(Size), avg_team_weight = mean(Weight),
            nr_drivers = n_distinct(Rider),
            nr_flat_drivers = sum(flat_driver), nr_mountain_drivers = sum(mountain_driver), nr_downhill_drivers = sum(downhill_driver),
            nr_cobbles_drivers = sum(cobbles_driver), nr_tt_drivers = sum(tt_driver), nr_prologue_drivers = sum(prologue_driver),
            nr_sprint_drivers = sum(sprint_driver), nr_acceleration_drivers = sum(acceleration_driver), nr_resistance_drivers = sum(resistance_driver),
            nr_endurance_drivers = sum(endurance_driver), nr_recup_drivers = sum(recup_driver), nr_hill_drivers = sum(hill_driver), nr_attack_drivers = sum(attack_driver))


#Something with avg price of team, but every rider has FREE
length(unique(basetable[["Price"]]))


## 50% of the sample size
smp_size <- floor(0.50 * nrow(team_scores_per_race))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(team_scores_per_race)), size = smp_size)

train <- team_scores_per_race[train_ind, ]
test <- team_scores_per_race[-train_ind, ]


ytrain <- as.factor(train$team_points)
train <- train %>% dplyr :: select(-c("team_points"))
ytest <- as.factor(test$team_points)
test <- test %>% dplyr :: select(-c("team_points"))

# create a first random forest model on train and val to see how it competes against others
rFmodel <- randomForest(x = train, y = ytrain,
                        ntree = 500, importance = TRUE)

predrF <- predict(rFmodel, test)

AUC::auc(AUC::roc(predrF,ytest))


