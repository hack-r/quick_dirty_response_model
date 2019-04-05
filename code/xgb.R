##########################################
# File:            xgb.R                 #
# Context:         Run by Master file    #
# Contact author:  Miller, J.            #
# Initial Date:    2019-04-04            #
# Version:         0.0.1                 #
##########################################

setdiff2(colnames(train),
         colnames(test))

cat("training a XGBoost classifier\n")
clf <- xgboost(data        = data.matrix(train[,!colnames(train) %in% c("Y", "target", "ID")]),
               label       = train$Y,
               nrounds     = 20,
               objective   = "binary:logistic",
               eval_metric = "auc")
saveRDS(clf, "clf.RDS")

cat("making predictions in batches due to 8GB memory limitation\n")
submission <- data.frame(ID=test$ID)
submission$target <- NA 

# predict in chunks
for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
  submission[rows, "target"] <- predict(clf, data.matrix(test[rows, colnames(test) %in% clf$feature_names]))
}


cat("saving the output file... \n")
write_csv(submission, "xgboost_submission.csv")
