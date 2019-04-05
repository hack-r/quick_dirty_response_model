##########################################
# File:            pull_data.R           #
# Context:         Run by Master file    #
# Contact author:  Miller, J.            #
# Initial Date:    2019-04-04            #
# Version:         0.0.1                 #
##########################################

# Extract Raw Data ---------------------------------------------------------

cat("reading the train and test data\n")
train <- read_csv("train.csv")
test  <- read_csv("test.csv")

saveRDS(train, "train.RDS")
saveRDS(test, "test.RDS")

# Sample for development only
if(dev){
  cat("Sample for development only \n\n")
  train <- train[sample(nrow(train), 20000),]
  test <- test[sample(nrow(test), 20000),]
  gc()
}

Y        <- train$target
id.train <- train$ID
id.test  <- test$ID

saveRDS(Y,"Y.RDS")
saveRDS(train,"train.RDS")
saveRDS(test,"test.RDS")
