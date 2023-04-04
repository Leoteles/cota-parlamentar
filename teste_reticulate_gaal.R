rm(list=ls())
library(reticulate)
#py_install('pyod')



mogaal <- import('pyod.models.mo_gaal')


pyod_data <- import('pyod.utils.data')#from pyod.utils.data import generate_data
pyod_example <- import('pyod.utils.example')

contamination <- 0.1  # percentage of outliers
n_train <- 200  # number of training points
n_test <- 100  # number of testing points

#X_train, y_train, X_test, y_test = 
res <- pyod_data$generate_data(n_train=n_train, n_test=n_test, contamination=contamination)
X_train <- res[[1]]
y_train <- res[[2]]
X_test <- res[[3]]
y_test <- res[[4]]


clf_name <- 'mo_gaal'
clf <- mogaal$MO_GAAL(stop_epochs = as.integer(1000),contamination = 0.5)
clf$fit(X_train)

# get the prediction labels and outlier scores of the training data
y_train_pred <- clf$labels_  # binary labels (0: inliers, 1: outliers)
y_train_scores <- clf$decision_scores_  # raw outlier scores

# get the prediction on the test data
y_test_pred <- clf$predict(X_test)  # outlier labels (0 or 1)
y_test_scores <- clf$decision_function(X_test)  # outlier scores



# evaluate and print the results
print("Training Data:")
pyod_data$evaluate_print(clf_name, y_train, y_train_scores)
print("On Test Data:")
pyod_data$evaluate_print(clf_name, y_test, y_test_scores)
pyod_example$visualize(clf_name,
                             X_train, y_train,
                             X_test, y_test,
                             y_train_pred,y_test_pred,
                             show_figure=T,
                             save_figure=F)
