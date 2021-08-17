###############################################################
# Random Forest Binary Classification
###############################################################

library(mice)
library(dplyr)
library(tidyr)
library(readxl)
pacman::p_load(pacman, rio, tidyverse)
library(splitstackshape)
library(caret)
library(h2o)
h2o.init()

# Link to download PIRUS dataset https://www.start.umd.edu/data-tools/profiles-individual-radicalization-united-states-pirus

#Import PIRUS Data
pirus_classify <- read_excel("INSERT PATH TO DOWNLOAD FILE")

# Remove dates prior to 1995
pirus_classify$Date_Exposure <- as.Date(pirus_classify$Date_Exposure)
pirus_classify <- pirus_classify %>% filter(Date_Exposure >= as.Date("1995-01-01"))

# Remove missing group membership variables
pirus_classify <- pirus_classify %>%
  filter(Group_Membership != -99) %>% 
  filter(Age != -99)

# Select vars
vars <- c('Standing','Radical_Beliefs','Work_History','Angry_US','Platonic_Troubles','Group_Membership','Gender', 'Age', 'Previous_Criminal_Activity', 'Psychological', 'Internet_Radicalization', 'Attack_Preparation', 'Rad_duration', 'Plot_Target1', 'Anticp_Fatals_Targ', 'Op_Security', 'Military', 'Marital_Status')
pirus_classify <- pirus_classify %>% select(all_of(vars))

# Convert missing and unknown values to NA
pirus_classify[pirus_classify < 0] <- NA

# View proportion of missing values
pirus_classify_na <- pirus_classify %>% summarise_all(~ sum(is.na(.)))
pirus_classify_na <- (pirus_classify_na/1509)*100
view(pirus_classify_na)
total_na <- sum(pirus_classify_na)/(1509*18)
total_na

# Convert Group Membership variable to lone-actor & group-actor class
pirus_classify$Group_Membership <- ifelse(pirus_classify$Group_Membership == 0, 1, 0)
pirus_classify$Group_Membership <- factor(pirus_classify$Group_Membership, levels = 0:1, labels = c('group', 'loner'))

write_sav(pirus_classify, 'whole_pirus_classify.sav')

# Group and non-group missingness
pirus_classify_group <- pirus_classify %>% filter(Group_Membership == 'group')
pirus_classify_lone <- pirus_classify %>% filter(Group_Membership == 'loner')
group_na <- pirus_classify_group %>% summarise_all(~ sum(is.na(.)))
lone_na <- pirus_classify_lone %>% summarise_all(~ sum(is.na(.)))
total_group_na <- sum(group_na)/(1060*18)
total_lone_na <- sum(lone_na)/(449*18)

# Build Train and Test Set
set.seed(500)
train.index <- createDataPartition(pirus_classify$Group_Membership, p = .8, list = FALSE)
train <- pirus_classify[ train.index,]
test  <- pirus_classify[-train.index,]

# Convert train and test splits to H2O class
train_h2o <- as.h2o(train)
test_h2o <- as.h2o(test)

# DRF Params
drf_params <- list(ntrees = c(50, 75, 100, 125, 150),
                   max_depth = c(5, 7, 10, 12, 15))

# Parameter Grid Search
drf <- h2o.grid("randomForest",
                y = "Group_Membership",
                training_frame = train_h2o,
                grid_id = "drf",
                hyper_params = drf_params,
                categorical_encoding = 'enum',
                min_rows = 1,
                binomial_double_trees = T,
                balance_classes = T, 
                nfolds = 5,
                fold_assignment = "Stratified",
                seed = 500)

# Sort models
f1_model <- h2o.getGrid(grid_id = "drf",
                        sort_by = "f1")

# Choose best model
best_f1 <- h2o.getModel(f1_model@model_ids[[1]])

best_f1@model[['model_summary']]

# Model Performance
perf <- h2o.performance(best_f1, test_h2o)
perf

# Confusion Matrix
conf_f1 <- h2o.confusionMatrix(best_f1, test_h2o)
conf_f1

# Var Imp
imp_f1 <- h2o.varimp_plot(best_f1, num_of_features = 17)

# PD Plot
h2o.pd_plot(best_f1, test_h2o, column = 'Platonic_Troubles')
h2o.pd_plot(best_f1, test_h2o, column = 'Psychological')
h2o.pd_plot(best_f1, test_h2o, column = 'Radical_Beliefs')
h2o.pd_plot(best_f1, test_h2o, column = 'Age')
h2o.pd_plot(best_f1, test_h2o, column = 'Plot_Target1')

