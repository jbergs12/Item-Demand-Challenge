library(tidymodels)
library(vroom)
library(embed)
library(modeltime)
library(timetk)

item_train <- vroom("train.csv")
item_test <- vroom("test.csv")

# Store and Item 1
SI_train <- item_train |> 
  filter(store == 10,
         item == 48)
SI_test <- item_test |> 
  filter(store == 10,
         item == 48)

# Store and Item 2
SI_train <- item_train |>
  filter(store == 3,
         item == 5)
SI_test <- item_test |>
  filter(store == 3,
         item == 5)

CV_split <- time_series_split(SI_train,
                               assess="3 months",
                               cumulative = T)

CV_split |> 
  tk_time_series_cv_plan() |> 
  plot_time_series_cv_plan(date,
                           sales,
                           .interactive = F)

item_prophet <- prophet_reg() |> 
  set_engine(engine = "prophet") |> 
  fit(sales ~ date, data = training(CV_split))

CV_results <- modeltime_calibrate(item_prophet,
                                   new_data = testing(CV_split))

CV_plot2 <- CV_results |> 
  modeltime_forecast(
    new_data = testing(CV_split),
    actual_data = training(CV_split)
  ) |> 
  plot_modeltime_forecast(.interactive=F)

fullfit <- CV_results |> 
  modeltime_refit(data=SI_train)

fullfit_plot2 <- fullfit |> 
  modeltime_forecast(
    new_data = SI_test,
    actual_data = SI_train
  ) |> 
  plot_modeltime_forecast(.interactive=T)


plotly::subplot(CV_plot1, CV_plot2, fullfit_plot1, fullfit_plot2, nrows = 2)

# CAN PROBABLY DELETE:
# item_rec <- item_recipe(SI_train)
# 
# item_forest <- rand_forest(mtry = tune(),
#                            min_n = tune(),
#                            trees = 500) |> 
#   set_mode("regression") |> 
#   set_engine("ranger")
# 
# forest_grid <- grid_regular(
#   mtry(range = c(1, ncol(bake(prep(item_rec), new_data=SI_train)))),
#   min_n(),
#   levels = 5)
# 
# forest_wf <- workflow() |> 
#   add_model(item_forest) |> 
#   add_recipe(item_rec)
# 
# folds <- vfold_cv(SI_train, v = 10, repeats = 1)
# 
# CV_results <- run_cv(forest_wf, folds, forest_grid, metric = metric_set(smape),
#                      cores = 6)
# 
# bestTune <- CV_results |> 
#   select_best(metric = "smape")
# 
# CV_results |> 
#   show_best(metric = "smape", n = 1) # std_err .128
# 
# bestTune$mtry
# bestTune$min_n
# bestTune$collect_metrics
# 
# final_wf <- forest_wf |> 
#   finalize_workflow(bestTune) |> 
#   fit(data=SI_train)
# 
# forest_preds <- final_wf |> 
#   predict(new_data = item_test,
#           type = "prob")
# 
# kaggle_submission <- forest_preds |> 
#   bind_cols(item_test) |> 
#   select(id, .pred_1) |> 
#   rename(Id = id,
#          sales = .pred_1)
# 
# vroom_write(x=kaggle_submission, file="./rforest2.csv", delim = ",")
