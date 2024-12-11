library(tidymodels)
library(vroom)
library(embed)
library(ranger)

source("item_recipe.R")

item_train <- vroom("train.csv")
item_test <- vroom("test.csv")

item_rec <- item_recipe(item_train)

item_forest <- rand_forest(mtry = tune(),
                          trees = 250) |> 
  set_mode("regression") |> 
  set_engine("ranger")

forest_grid <- grid_regular(
  mtry(range = c(1, ncol(bake(prep(item_rec), new_data=item_train)))),
  min_n(),
  levels = 5)

forest_wf <- workflow() |> 
  add_model(item_forest) |> 
  add_recipe(item_rec)

folds <- vfold_cv(item_train, v = 5, repeats = 1)

CV_results <- run_cv(forest_wf, folds, forest_grid, metric = metric_set(smape),
                     parallel = F)

bestTune <- CV_results |> 
  select_best(metric = "smape")

CV_results |> 
  show_best(metric = "smape", n = 1)

bestTune$mtry
# bestTune$min_n
bestTune$collect_metrics

final_wf <- forest_wf |> 
  finalize_workflow(bestTune) |> 
  fit(data=item_train)

forest_preds <- final_wf |> 
  predict(new_data = item_test,
          type = "numeric")

kaggle_submission <- forest_preds |> 
  bind_cols(item_test) |> 
  select(id, .pred) |> 
  rename(sales = .pred)

vroom_write(x=kaggle_submission, file="./submission.csv", delim = ",")
