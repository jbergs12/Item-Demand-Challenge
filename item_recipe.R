item_recipe <- function(traindata){
  recipe(sales~., data=traindata) |> 
    step_date(date, features = c("month", "dow", "year", "doy", "decimal")) |> 
    step_range(date_doy, min=0, max = pi) |> 
    step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy),
                date_dow=as.factor(date_dow)) |> 
    step_rm(c("store", "item"))
}

run_cv <- function(wf, folds, grid, metric=metric_set(smape), cores=8, parallel = TRUE){
  if(parallel == TRUE){
    library(doParallel)
    
    cl <- makePSOCKcluster(cores)
    registerDoParallel(cl)
  }
  
  results <- wf |>
    tune_grid(resamples=folds,
              grid=grid,
              metrics=metric)
  if(parallel == TRUE){
    stopCluster(cl)
  }
  return(results)
}

# itemsmote_recipe <- function(traindata, neighbors){
#   library(themis)
#   recipe(sales~., data=traindata) |> 
#     step_mutate_at(all_nominal_predictors(), fn = factor) |>
#     step_lencode_mixed(all_nominal_predictors(), outcome = vars(type)) |>
#     step_normalize(all_numeric_predictors()) |>
#     step_pca(all_predictors(), threshold = .85) |> 
#     step_smote(all_outcomes(), neighbors=neighbors)
# }