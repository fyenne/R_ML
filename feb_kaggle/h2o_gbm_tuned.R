library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

#--------------------------------------------

library(h2o)
h2o.init(nthreads = -1, #Number of threads -1 means use all cores on your machine
         max_mem_size = "16G")  #max mem size is the maximum memory to allocate to H2O

splits <- h2o.splitFrame(data = x_train, .8, 
                         destination_frames = c("trainSplit","validSplit"),
                         # ratios = ,  #partition data into 70%, 15%, 15% chunks
                         seed = 12)  #setting a seed will guarantee reproducibility
atrain <- splits[[1]]
valid <- splits[[2]]

# Construct a large Cartesian hyper-parameter space
ntrees_opts = c(10000)       # early stopping will stop earlier
max_depth_opts = seq(4,20)
min_rows_opts = c(5,10,20,50)
learn_rate_opts = seq(0.01, 0.1, .25)
sample_rate_opts = seq(0.3, 1, 0.05)
col_sample_rate_opts = seq(0.3, 1, 0.05)
col_sample_rate_per_tree_opts = seq(0.3, 1, 0.05)
nbins_cats_opts = seq(100,10000,100) # categorical features


hyper_params = list( ntrees = ntrees_opts,
                     max_depth = max_depth_opts,
                     min_rows = min_rows_opts,
                     learn_rate = learn_rate_opts,
                     sample_rate = sample_rate_opts,
                     col_sample_rate = col_sample_rate_opts,
                     col_sample_rate_per_tree = col_sample_rate_per_tree_opts,
                     nbins_cats = nbins_cats_opts,
                     # learn_rate_annealing = .995,
                     stopping_rounds = 30,
                     stopping_metric = "RMSE",
                     # nfolds = folds,
                     seed = 2,
                     categorical_encoding = catEncoding
                     # fold_assignment = "Modulo",
                     # keep_cross_validation_predictions = TRUE
)
# Search a random subset of these hyper-parmameters. Max runtime 
# and max models are enforced, and the search will stop after we 
# don't improve much over the best 5 random models.
search_criteria = list(strategy = "RandomDiscrete",
                       max_runtime_secs = 1000,
                       max_models = 1999,
                       stopping_metric = "AUTO",
                       stopping_tolerance = 0.0001,
                       stopping_rounds = 25,
                       seed = 12)


gbm_grid <- h2o.grid("gbm",
                     grid_id = "mygrid",
                     # x = x,
                     y = "target",
                     # faster to use a 80/20 split
                     training_frame = atrain,
                     validation_frame = valid,
                     nfolds = 0,
                     # alternatively, use N-fold cross-validation:
                     # Gaussian is best for MSE loss, but can try 
                     # other distributions ("laplace", "quantile"):
                     # distribution="gaussian",
                     # stop as soon as mse doesn't improve by 
                     # more than 0.1% on the validation set, 
                     # for 2 consecutive scoring events:
                     # stopping_rounds = 10,
                     # stopping_tolerance = 1e-3,
                     # stopping_metric = "MAE",
                     # how often to score (affects early stopping):
                     # score_tree_interval = 100,
                     ## seed to control the sampling of the 
                     ## Cartesian hyper-parameter space:
                     seed = 12,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria
                     )
#--------------------------------------------
# result part"

# h2o.getGrid(grid_id = "mygrid", sort_by = "mse")
# summary(gbm_grid, show_stack_traces = TRUE)
gbm_sorted_grid <- h2o.getGrid(grid_id = "mygrid", sort_by = "mse")

# gbm_sorted_grid@model_ids[[1]]
best_model <- h2o.getModel("mygrid_model_5")
# summary(best_model)

# h2o.saveModel(best_model, "./tuned_h2o_gbm.H2OModel")
# best_model = h2o.loadModel("./tuned_h2o_gbm/mygrid_model_5")
#--------------------------------------------
# predict

tune_h2o_gbm = h2o.predict(best_model, newdata = x_test)
tune_h2o_gbm = tune_h2o_gbm %>% as.data.frame()
sub$target = tune_h2o_gbm$predict
# write.csv(sub, "h2o_tuned_gbm.csv", row.names = F) # final grade .84460
