rm(list = ls())

#install packages
if (!require("pacman")) install.packages("pacman")
require("pacman", character.only = TRUE, quietly = TRUE)
p_load(tidyverse)
library("readxl")
p_load(lubridate)
library('stringr')
set.seed(123)
p_load(dummy)

first_cycling_data <- read.csv("Races_WITHindex.csv", header = TRUE)
pro_cycling_data <- read_excel("RiderData.xlsx")

#impute missing UCIs with 0 as missing values means that the driver finished outside of the points and thus did not earn any
first_cycling_data$UCI[is.na(first_cycling_data$UCI)] <- 0

#merge on name and team (because what if a rider switches teams?)
#merge Given Name and Family name to one column: Rider (necessary for merge)
pro_cycling_data$Rider <- str_c(pro_cycling_data$`Family Name`, ' ', pro_cycling_data$`Given Name`)

# merge two data frames by ID
basetable <- merge(first_cycling_data, pro_cycling_data,by="Rider")

#transform position column to numbers
basetable$Pos <- as.integer(basetable$Pos)
basetable$Pos[is.na(basetable$Pos)] <- 100 #give the empty values a random number that does NOT indicate success (NA means DNF or DNS)

#check missing values
colSums(is.na(basetable))
#Var_9 has a high amount of missing values, as well as VAR_16, so we will drop those

#drop columns that are not of any use
drop <- c("X.x", "Rider_ID", "Team_ID", "ID", "Family Name", "Given Name", "Name", "Team.y", "Region", "ID2", "VAR10", "VAR9", "VAR16",
          "VAR25", "VAR26", "VAR23", "VAR24", "VAR29", "VAR27", "VAR28", "VAR21", "VAR22", "VAR20", "X.y", "VAR2", "VAR3",
          "VAR5", "VAR6", "VAR14", "VAR15", "Type", "VAR12", "VAR13", "VAR18", "Price", "VAR19", "VAR17", "VAR8", "VAR11", "VAR30",
          "VAR31", "Unnamed..1", "Icon")
basetable = basetable[,!(names(basetable) %in% drop)]

#get age of rider
basetable$Birthday <- ymd(basetable$Birthday)
basetable$age <- as.integer(floor(as.numeric(basetable$Year) - (as.numeric(format(basetable$Birthday, format = "%Y")))))
basetable$age[basetable$age > 100] <- NA
#drop birthday
drop <- c("Birthday")
basetable = basetable[,!(names(basetable) %in% drop)]

#As a tryout give riders points based on the F1 system
#If he rider comes first, he gets 10 points. If he is 2nd, 9 points
#All the way down to 1 point for place 10 till 60
#If you finish outside of top 60, 0 points
#Can be altered to be more forgiving
basetable$points <- ifelse(basetable$Pos == 1, 10, 
                           ifelse(basetable$Pos == 2, 9,
                                  ifelse(basetable$Pos == 3, 8,
                                         ifelse(basetable$Pos == 4, 7,
                                                ifelse(basetable$Pos == 5, 6,
                                                       ifelse(basetable$Pos == 6, 5,
                                                              ifelse(basetable$Pos == 7, 4,
                                                                     ifelse(basetable$Pos == 8, 3,
                                                                            ifelse(basetable$Pos == 9, 2,
                                                                                   ifelse(basetable$Pos >= 10 && basetable$Pos <= 60, 1, 0)
                                                                            )
                                                                     ))))))))

#Define that a driver has a specialty in a specific category if he has a score in that category above 75
#Exact cutoff also up for discussion, mcategories have a range between 50 and 85
#https://web.cyanide-studio.com/games/cycling/2021/pcm/guide/basics-specialisations/
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

#Everything gets grouped by Race_ID here, but a Race_ID can sometimes have multiple stages
#If a Race_ID is just a one day race, stage column is 0
#Make a new Race_ID_Stage , indicating the race id and stage in one column
basetable <- basetable %>%
  unite(Race_ID_Stage, Race_ID, Stage,
        remove = FALSE,
        sep = '_') %>%
  # Replace '_' with '' if it is at the end of the line
  mutate(Race_ID_Stage = gsub(', $', '', Race_ID_Stage))

#Aggregate everything so that one row equals one team per race
#Variables inlcude team scores on different attributes, number of drivers with certain attributes
#Team points in the race and amount of top_X finishes per race
team_scores_per_race <-basetable %>%
  group_by(Year, Race_ID_Stage, Team.x) %>%
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
            nr_sprint_drivers = sum(sprint_driver), nr_acceleration_drivers = sum(acceleration_driver), nr_resistance_drivers =
              sum(resistance_driver), nr_endurance_drivers = sum(endurance_driver), nr_recup_drivers = sum(recup_driver), 
            nr_hill_drivers = sum(hill_driver), nr_attack_drivers = sum(attack_driver), avg_age_drivers = mean(age),
            top20_finishes = sum(Pos <= 20), top10_finishes = sum(Pos <= 10), top5_finishes = sum(Pos <= 5), top3_finishes = sum(Pos <= 3)
            )

#Assign a category to each race, in line with the rider categories (done with help of stage_profiles in first cycling api and the stage profile icons on the site of firstcycling)
#In the html code you can see the name for the icon, that indicated the race category
#For the single day races we assign the category by hand
ids_single <- c("4_0", "7_0", "5_0", "8_0", "9_0", "11_0", "59_0", "58_0", "24_0", "47_0", "10_0", "1988_0", "20_0", "22_0", "1172_0", "53_0", "54_0", "75_0", "35_0", "40_0")
cats_single <- c("Flat", "Cobble", "Cobble", "Cobble", "Hill", "Hill", "Hill_UHF", "Hill", "Hill", "Cobble", "Hill_UHF", "Flat", "Flat", "Hill", "Hill", "Cobble", "Hill_UHF", "Cobble", "Hill", "Cobble")
race_cats_single <- data.frame(ids_single, cats_single)
race_cats_single <- race_cats_single %>%
  rename(Race_ID_Stage = ids_single, Race_Category = cats_single)

#For the races with multiple stages we extracted the profiles from the html response, with the help of python (see Cycling API.ipynb)
race_categories <- read.csv("race_categories.csv", header = TRUE)
unique(race_categories$Category)
#Translate the danish names we extracted from FC API
#UHF = up hill finish
race_categories <- race_categories %>% 
                        mutate(Race_Category = case_when(Category == "Flatt"   ~ "Flat", Category == "Smaakupert-MF" ~ "Hill_UHF",
                                  Category == "Smaakupert"   ~ "Hill", Category == "Fjell" ~ "Mountain",
                                  Category == "Fjell-MF"   ~ "Mountain_UHF", Category == "Tempo" ~ "TimeTrial",
                                  Category == "Bakketempo"   ~ "TimeTrial", Category == "Brosten" ~ "Cobble",TRUE ~ "TimeTrial"))
#Make the same Race_ID_Stage as before
race_categories <- race_categories %>%
  unite(Race_ID_Stage, Race_ID, Stage,
        remove = FALSE,
        sep = '_') %>%
  # Replace ', ' with '' if it is at the end of the line
  mutate(Race_ID_Stage = gsub(', $', '', Race_ID_Stage))

drop <- c("Race_ID", "Stage", "Category")
race_categories = race_categories[,!(names(race_categories) %in% drop)]

#Join the categories to the team_scores df
team_scores_1 <- merge(team_scores_per_race, race_cats_single,by="Race_ID_Stage")
team_scores_2 <- merge(team_scores_per_race, race_categories,by= c("Year", "Race_ID_Stage"))
team_scores_per_race <- rbind(team_scores_1, team_scores_2)

hist(team_scores_per_race$team_points)

#success is defined as when the team scores at least 10 points
team_scores_per_race$success <- ifelse(team_scores_per_race$team_points >= 10, 1, 0)
team_scores_per_race$success <- as.factor(team_scores_per_race$success)

#drop columns that are not of any use
drop <- c("Team.x", "Year", "team_points", "Race_ID", "Race_ID_Stage", "Stage")
team_scores_per_race = team_scores_per_race[,!(names(team_scores_per_race) %in% drop)]

#Make dummies from Race_category
dummies <- dummy(team_scores_per_race["Race_Category"])
dummies <- dummies %>%
  mutate(across(everything(), as.factor))
team_scores_per_race <- team_scores_per_race %>%
  dplyr::select(!Race_Category) %>%
  bind_cols(dummies)

# Inspect class imbalance
table(team_scores_per_race$success)

# create indicators randomize order of indicators
allind <- sample(x = 1:nrow(team_scores_per_race), size = nrow(team_scores_per_race))

# split in two equal parts
trainind <- allind[1:round(length(allind) * 0.5)]
testind <- allind[(round(length(allind) * (0.5)) + 1):length(allind)]

# actual subsetting
train <- team_scores_per_race[trainind, ]
test <- team_scores_per_race[testind, ]

# Variable Selection
p_load(varSelRF, Boruta)

# varSelRF ?varSelRF vars.drop.frac = 0.20, the fraction of
# variables dropped in each iteration, the higher this
# number, the more aggressive the feature selection Can be
# compared to a learning rate parameter in neural nets
# ntree = the number of trees in the first forest (like in
# randomForest) ntreeIterat = the number of trees to use in
# the subsequent forests It has been shown that more
# iterations do not lead to a better performance
rf_vsrf <- varSelRF(train[, !names(train) %in% c("churn")], train$churn, vars.drop.frac = 0.2,
                    ntree = 500, ntreeIterat = 100)
(varsel_vsrf <- rf_vsrf$selected.vars)

# # As an exercise you can play around with the parameters and see what happens
# # Boruta ?Boruta doTrace = 0 means no tracing, 1 means
# # reporting decision about each attribute as soon as it is
# # justified, 2 means the same as 1, plus reporting each
# # importance source run, 3 means the same as 2, plus
# # reporting of hits assigned to yet undecided attributes.
(rf_boruta <- Boruta(train_final[, !names(train_final) %in% c("churn")], train_final$churn, doTrace = 0))
plot(rf_boruta)

# # Let's just select the confirmed variables As an exercise,
# # you can try the rough fix or increase the maxRuns or the
# # significance level and see what happens
(varsel_boruta <- names(rf_boruta$finalDecision[rf_boruta$finalDecision =="Confirmed"]))

# Inspect class imbalance
table(train$success)

##OVERSAMPLING##

# Get indices of each group
success <- which(train$success == "1")
fail <- which(train$success == "0")

# See how many instances we would want to have equal number
# as majority_class
n_desired <- length(fail)

resampled_success <- sample(x = success, size = n_desired,
                            replace = TRUE)

print(resampled_success)

# Combine into one large data set
train <- train[c(resampled_success, fail),]
table(train$success)

#Model building:
p_load(h2o)
h2o.init(max_mem_size = "7G")

train_h2o<-as.h2o(train)
test_h2o<-as.h2o(test)

y <- "success"
x <- setdiff(names(train_h2o), y)

#xgboost, h2o does not support xgboost on windows
# my_xgb <- h2o.xgboost(x = x,
#                            y = y,
#                            training_frame = train_h2o,
#                            validation_frame = test_h2o,
#                            booster = "dart",
#                            normalize_type = "tree",
#                            nfolds = 5,
#                            #fold_column = "group",
#                            keep_cross_validation_predictions = TRUE,
#                            seed = 5)

#naive bayes
my_nb <- h2o.naiveBayes(x = x,
                        y = y,
                        training_frame = train_h2o,
                        laplace = 0,
                        nfolds = 5,
                        #fold_column = "group",
                        keep_cross_validation_predictions = TRUE,
                        seed = 5)

# Train & Cross-validate a RF (REACHES CAPACITY EVERY TIME)
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train_h2o,
                          nfolds = 5,
                          #fold_column = "group",
                          keep_cross_validation_predictions = TRUE,
                          seed = 5)

# Train & Cross-validate a LR
my_lr <- h2o.glm(x = x,
                 y = y,
                 training_frame = train_h2o,
                 family = c("binomial"),
                 nfolds = 5,
                 #fold_column = "group",
                 keep_cross_validation_predictions = TRUE,
                 seed = 5)

# Train a stacked ensemble using the models above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                metalearner_algorithm="naivebayes",
                                training_frame = train_h2o,
                                base_models = list(#my_xgb, 
                                  my_nb, my_rf, my_lr))

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test_h2o)

# Compare to base learner performance on the test set
#perf_gbm_test <- h2o.performance(my_xgb, newdata = test_h2o)
perf_nb_test <- h2o.performance(my_nb, newdata = test_h2o)
perf_lr_test <- h2o.performance(my_lr, newdata = test_h2o)
perf_rf_test <- h2o.performance(my_rf, newdata = test_h2o)
baselearner_best_auc_test <- max(#h2o.auc(perf_gbm_test), 
  h2o.auc(perf_nb_test), h2o.auc(perf_lr_test), h2o.auc(perf_rf_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

# predict
preds <- h2o.predict(ensemble, test_h2o)
mean(preds)
preds <- preds[,3]
preds = as.data.frame(preds)
colnames(preds) = c("prob_success")
preds <- cbind(preds)
preds[,1]
