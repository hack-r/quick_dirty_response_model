##########################################
# File:            ml1.R                 #
# Context:         Run by Master file    #
# Contact author:  Miller, J.            #
# Initial Date:    2019-04-04            #
# Version:         0.0.1                 #
# Description:     NN Metalearning       #
##########################################

# The h2o.stack function is an alternative to the h2o.ensemble function, which
# allows the user to specify H2O models individually and then stack them together
# at a later time.  Saved models, re-loaded from disk, can also be stacked.

# The base models must use identical cv folds; this can be achieved in two ways:
# 1. they be specified explicitly by using the fold_column argument, or
# 2. use same value for `nfolds` and set `fold_assignment = "Modulo"`


h2o.init()

train <- readRDS("train_checkpoint1.RDS")
test  <- readRDS("test_checkpoint1.RDS")

# Number of folds for CV
nfolds <- 5  

# Identify predictors and response
y <- "Y"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train.h2o <- as.h2o(train[1:(nrow(train)-5000),])
test.h2o  <- as.h2o(train[(nrow(train)-5000+1):nrow(train),])

train.h2o[,y] <- as.factor(train.h2o$Y)
test.h2o[,y]  <- as.factor(test.h2o$Y)


# Base Learning
glm1 <- h2o.glm(x = x, y = y, family = "binomial", 
                training_frame = train.h2o,
                nfolds = nfolds,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)
h2o.saveModel(glm1, "h2o_glm1")
# [1] "C:\\Users\\jmiller\\Desktop\\response_modeling\\data\\h2o_glm1\\GLM_model_R_1554476314148_1"

gbm1 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = train.h2o,
                seed = 1,
                nfolds = nfolds,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

rf1 <- h2o.randomForest(x = x, y = y, # distribution not used for RF
                        training_frame = train.h2o,
                        seed = 1,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)

dl1 <- h2o.deeplearning(x = x, y = y, distribution = "bernoulli",
                        training_frame = train.h2o,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)

models <- list(glm1, gbm1, rf1, dl1)
metalearner <- "h2o.glm.wrapper"

stack <- h2o.stack(models = models,
                   response_frame = train.h2o[,y],
                   metalearner = metalearner, 
                   seed = 1,
                   keep_levelone_data = TRUE)


# Compute test set performance on OOF holdout:
perf <- h2o.ensemble_performance(stack, newdata = test.h2o)
print(perf)

h2o.saveModel(rf1,"h2o_rf1")
h2o.saveModel(dl1,"h2o_dl1")
h2o.saveModel(stack, "h2o_stack")

h2o.shutdown()