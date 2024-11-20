library(tidymodels)
library(vroom)
library(embed)
library(modeltime)
library(timetk)

source("item_recipe.R")

item_train <- vroom("train.csv")
item_test <- vroom("test.csv")

# Store and Item 1
s10_i48_train <- item_train |> 
  filter(store == 10,
         item == 48)
s10_i48_test <- item_test |> 
  filter(store == 10,
         item == 48)

# Store and Item 2
s3_i5_train <- item_train |> 
  filter(store == 3,
         item == 5)
s3_i5_test <- item_test |> 
  filter(store == 3,
         item == 5)

CV_split1 <- time_series_split(s10_i48_train,
                              assess="3 months",
                              cumulative = T)

CV_split2 <- time_series_split(s3_i5_train,
                               assess="3 months",
                               cumulative = T)

CV_split1 |> 
  tk_time_series_cv_plan() |> 
  plot_time_series_cv_plan(date,
                           sales,
                           .interactive = F)

CV_split2 |> 
  tk_time_series_cv_plan() |> 
  plot_time_series_cv_plan(date,
                           sales,
                           .interactive = F)

item_rec1 <- item_recipe(s10_i48_train)

item_rec2 <- item_recipe(s3_i5_train)

item_arima <- arima_reg(seasonal_period=365,
                        non_seasonal_ar=7,
                        non_seasonal_ma=5,
                        seasonal_ar=2,
                        seasonal_ma=2,
                        non_seasonal_differences=2,
                        seasonal_differences=2
                        ) |> 
  set_engine("auto_arima")

arima_wf1 <- workflow() |> 
  add_model(item_arima) |> 
  add_recipe(item_rec1) |> 
  fit(data=training(CV_split1))

arima_wf2 <- workflow() |> 
  add_model(item_arima) |> 
  add_recipe(item_rec2) |> 
  fit(data=training(CV_split2))

CV_results1 <- modeltime_calibrate(arima_wf1,
                                  new_data = testing(CV_split1))

CV_results2 <- modeltime_calibrate(arima_wf2,
                                   new_data = testing(CV_split2))

CV_plot1 <- CV_results1 |> 
  modeltime_forecast(
    new_data = testing(CV_split1),
    actual_data = training(CV_split1)
  ) |> 
  plot_modeltime_forecast(.interactive=F)

CV_plot2 <- CV_results2 |> 
  modeltime_forecast(
    new_data = testing(CV_split2),
    actual_data = training(CV_split2)
  ) |> 
  plot_modeltime_forecast(.interactive=F)

fullfit1 <- CV_results1 |> 
  modeltime_refit(data=s10_i48_train)

fullfit2 <- CV_results2 |> 
  modeltime_refit(data=s3_i5_train)

fullfit_plot1 <- fullfit1 |> 
  modeltime_forecast(
    new_data = s10_i48_test,
    actual_data = s10_i48_train
  ) |> 
  plot_modeltime_forecast(.interactive=F)

fullfit_plot2 <- fullfit2 |> 
  modeltime_forecast(
    new_data = s3_i5_test,
    actual_data = s3_i5_train
  ) |> 
  plot_modeltime_forecast(.interactive=F)

plotly::subplot(CV_plot1, CV_plot2, fullfit_plot1, fullfit_plot2, nrows = 2)

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
# final_wf <- arima_wf |> 
#   finalize_workflow(bestTune) |> 
#   fit(data=s10_i48_train)
# 
# arima_preds <- final_wf |> 
#   predict(new_data = item_test,
#           type = "prob")
# 
# kaggle_submission <- arima_preds |> 
#   bind_cols(item_test) |> 
#   select(id, .pred_1) |> 
#   rename(Id = id,
#          sales = .pred_1)
# 
# vroom_write(x=kaggle_submission, file="./rarima2.csv", delim = ",")
