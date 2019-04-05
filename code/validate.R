##########################################
# File:            validate.R            #
# Context:         Run by Master file    #
# Contact author:  Miller, J.            #
# Initial Date:    2019-04-04            #
# Version:         0.0.1                 #
# Description:     OOF/OOS Holdout       #
##########################################

test.h2o <- as.h2o(test)

# Compute test set performance on OOF holdout:
perf <- h2o.performance(ensemble, newdata = test.h2o)
print(perf)
