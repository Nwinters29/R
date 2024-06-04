# Packages needed for data manipulation, visualization, model building, and model evaluation
library(tidyverse)
library(tidymodels)
library(workflows)
library(themis)
library(tune)
library(ranger)
library(xgboost)
library(kknn)


# Import the data
#######################
# set working directory
getwd()
setwd("/Users/nickwinters/desktop/DS Projects")

# assign csv file to an object
strk <- read.csv("healthcare-dataset-stroke-data.csv")


# Data Exploration
######################
#Taking a peak at the data

# display the first five rows
head(strk, 5)

# size and type of data
dim(strk)
summary(strk)

# lets specifically look at class of each column
sapply(strk, class)

# Lets check for any missing data
sum(is.na(strk))

# Data Manipulation
###########################
# address missing values ("N/A" was used for NA values)
strk[strk == 'N/A'] <- NA
strk[strk == 'Unknown'] <- NA
strk[strk == "formerly smoked"] <- "has smoked"
strk[strk == "smokes"] <- "has smoked"

#validate the above change
sum(is.na(strk))

# Change datatypes
strk <- transform(strk,
                  bmi = as.double(bmi),
                  gender = as.factor(gender),
                  ever_married = as.factor(ever_married),
                  work_type = as.factor(work_type),
                  Residence_type = as.factor(Residence_type),
                  smoking_status = as.factor(smoking_status),
                  hypertension = as.factor(hypertension),
                  heart_disease = as.factor(heart_disease),
                  stroke = as.factor(stroke))

# validate the above change
sapply(strk, class)

## Binning Data
 strk <- strk |> 
   mutate(age_group = cut(age, 
                          breaks = c(0, 6, 19, 46, 65, 76, 100),
                          labels = c('0-5y', '6-18y', '19-45y', '46-64y', '65-75y', '76+'),
                          include.lowest = T, 
                          right = F,
                          ordered_result = T),
          bmi_group = cut(bmi,
                          breaks = c(0, 19, 25, 30, 35, 40, 100),
                          labels = c("underweight", "normal", "overweight", "class I obesity", "class II obesity", "class III obesity"),
                          include.lowest = T,
                          right = F,
                          ordered_result = T),
          glucose_group = cut(avg_glucose_level,
                              breaks = c(0, 140, 200, 300),
                              labels = c("Normal", "Prediabetic", "Diabetic"),
                              include.lowest = T,
                              right = F,
                              ordered_result = T)
   )
 
# validate the bins
strk |> 
  select(age, age_group, bmi, bmi_group, avg_glucose_level, glucose_group) |> 
  head()

# check order
is.ordered(strk$age_group)
levels(strk$age_group)

is.ordered(strk$bmi_group)
levels(strk$bmi_group)

is.ordered(strk$glucose_group)
levels(strk$glucose_group)

# Data Visualization

## stroke
strk |> 
  ggplot(aes(x = stroke, fill = stroke)) +
  geom_bar()

## gender
strk |> 
  ggplot(aes(x = gender, fill = stroke)) +
  geom_bar(position = "fill")

## age
strk |> 
  ggplot(aes(x = age_group, fill = stroke)) +
  geom_bar(position = "fill")

## hypertension
strk |> 
  ggplot(aes(x = hypertension, fill = stroke)) +
  geom_bar(position = "fill")

## heart_disease
strk |> 
  ggplot(aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = "fill")

## ever_married
strk |> 
  ggplot(aes(x = ever_married, fill = stroke)) +
  geom_bar(position = "fill")

strk |> 
  ggplot(aes(x = ever_married, fill = age_group)) +
  geom_bar(position = "fill") +
  facet_wrap(~stroke)

## work_type
strk |> 
  ggplot(aes(x = work_type, fill = stroke)) +
  geom_bar(position = "fill")

## Residence_type
strk |> 
  ggplot(aes(x = Residence_type, fill = stroke)) +
  geom_bar(position = "fill")

## avg_glucose_level
strk |> 
  ggplot(aes(x = glucose_group, fill = stroke)) +
  geom_bar(position = "fill")

## bmi
strk |> 
  filter(!is.na(bmi_group)) |> 
  ggplot(aes(x = bmi_group, fill = stroke)) +
    geom_bar(position = "fill")

strk |> 
  ggplot(aes(x = bmi_group, fill = stroke)) +
    geom_bar(position = "fill")

## smoking_status
strk |> 
  ggplot(aes(x = smoking_status, fill = stroke)) +
  geom_bar(position = "fill")

# Feature Selection
clean_strk <- strk |> 
  select(age_group, 
         bmi_group,
         glucose_group,
         hypertension, 
         heart_disease,
         stroke)

# Building the prediction model

## Splitting the data

# set random seed for replication
set.seed(42)

# split the data into training(80%) and testing (20%)
strk_split <- initial_split(clean_strk, 
                            prop = 4/5)
strk_split

# extracting training and test sets
train <- training(strk_split)
test <- testing(strk_split)

## Cross fold analysis
# set random seed for reproducability
set.seed(42)

# create a CV object
stroke_folds <- vfold_cv(train, strata = stroke)

## Recipe
# recipe creation
stroke_recipe <- recipe(stroke ~ 
                          age_group + 
                          bmi_group + 
                          glucose_group + 
                          hypertension + 
                          heart_disease, 
                        data = clean_strk) |> 
  step_impute_knn(all_predictors()) |> 
  step_dummy(all_unordered_predictors()) |> 
  step_ordinalscore(all_ordered_predictors()) |> 
  step_smote(stroke)

# Specify desired metrics
stroke_metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)


## Workflow
# add recipe to workflow
stroke_workflow <- workflow() |> 
  add_recipe(stroke_recipe)

### Random forest

#### Model Creation
rf_grid <- expand.grid(mtry = c(3, 4, 5))

### Random Forest Model with flexible tuning
rf_model_2 <- rand_forest() |> 
  set_args(mtry = tune()) |> 
  set_engine("ranger") |> 
  set_mode("classification")


### Workflow
rf_workflow_2 <- workflow() |> 
  add_recipe(stroke_recipe) |> 
  add_model(rf_model_2) 

### Grid Search
rf_tune <- rf_workflow_2 |> 
  tune_grid(
    resamples = stroke_folds,
    grid = rf_grid,
    metrics = stroke_metrics,
    control = control_resamples(save_pred = TRUE))

# evaluation

rf_tune |> 
  collect_metrics()

# selection
param_final <- rf_tune |> 
  select_best(metric = "sensitivity")
param_final


## Finalized Workflow
rf_workflow_tuned <- finalize_workflow(rf_workflow_2, param_final)
rf_workflow_tuned

## Fit to testing
rf_fit <- rf_workflow_tuned |> 
  last_fit(split = strk_split,
           metrics = stroke_metrics)

## Evaluate Testing
rf_fit_metric <- collect_metrics(rf_fit)
rf_fit_metric

collect_predictions(rf_fit) |> 
  conf_mat(stroke, .pred_class)

# Final Model
final_model <- fit(rf_workflow_tuned, clean_strk)
final_model


# Prediction based on new data
# new data for fictional patient
new_patient <- tribble(~age_group, ~bmi_group, ~glucose_group, ~hypertension, ~heart_disease, 
                     "76+", "overweight", "Prediabetic", "1", "0")

# stroke prediction
prediction <- predict(final_model, new_data = new_patient)
prediction
