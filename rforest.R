library(tidymodels)
library(vroom)
library(embed)
library(ranger)

source("item_recipe.R")

item_train <- vroom("train.csv")
item_test <- vroom("test.csv")
s10_i48 <- item_train |> 
  filter(store == 10,
         item == 48)

item_rec <- item_recipe(s10_i48)

item_forest <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees = 500) |> 
  set_mode("regression") |> 
  set_engine("ranger")

forest_grid <- grid_regular(
  mtry(range = c(1, ncol(bake(prep(item_rec), new_data=s10_i48)))),
  min_n(),
  levels = 5)

forest_wf <- workflow() |> 
  add_model(item_forest) |> 
  add_recipe(item_rec)

folds <- vfold_cv(s10_i48, v = 10, repeats = 1)

CV_results <- run_cv(forest_wf, folds, forest_grid, metric = metric_set(smape),
                     cores = 6)

bestTune <- CV_results |> 
  select_best(metric = "smape")

CV_results |> 
  show_best(metric = "smape", n = 1) # std_err .128

bestTune$mtry
bestTune$min_n
bestTune$collect_metrics

final_wf <- forest_wf |> 
  finalize_workflow(bestTune) |> 
  fit(data=s10_i48)

forest_preds <- final_wf |> 
  predict(new_data = item_test,
          type = "prob")

kaggle_submission <- forest_preds |> 
  bind_cols(item_test) |> 
  select(id, .pred_1) |> 
  rename(Id = id,
         sales = .pred_1)

vroom_write(x=kaggle_submission, file="./rforest2.csv", delim = ",")
