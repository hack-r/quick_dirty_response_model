##########################################
# File:            feature_engineering.R #
# Context:         Run by Master file    #
# Contact author:  Miller, J.            #
# Initial Date:    2019-04-04            #
# Version:         0.0.1                 #
##########################################


# Set up ------------------------------------------------------------------
feature.names <- names(train)[2:ncol(train)-1]

cat("Re-encode factor to numeric for XGB... \n\n")
cat("!!REMINDER TO JASON!! \n")
cat("Explain to C2G that in prod I handle factors better, but this and other simplifications were used due to the *extremely* limited time available to work on this presentation.\n\n")


# Factors -----------------------------------------------------------------
res <- factorFix.xgb(train, test)

train <- res[[1]]
test  <- res[[2]]
rm(res); gc()


# Missingness -------------------------------------------------------------
cat("Check and handle missingness (quick and dirty approach)... \n\n")

sum(is.na(train))
train <- as.data.frame(apply(train,2,function(x) na.roughfix(x)))
test  <- as.data.frame(apply(test,2,function(x) na.roughfix(x)))

sum(is.na(train)) # any remaining are ALL NA
sum(is.na(test))

bad <- unique(c(names(which(apply(train, 2, function(x) sum(is.na(x))) > 0)),
                names(which(apply(test, 2, function(x) sum(is.na(x))) > 0))))

 
train <- train[,!colnames(train) %in% bad] # discard all NA
test  <- test[,!colnames(test) %in% bad]


# Check for Constant + Identical Feats ------------------------------------
cat("Checking for constant and duplicated features...\n\n")

train <- rm_cons(train)
test  <- test[,colnames(test) %in% colnames(train)]

rm_ident(train, test)

cat("We are now down to 1874 original features... \n\n")


# Unsupervised Learning ---------------------------------------------------

cat("Unsupervised learning is being conducted for both dimensionality reduction + new feature generation... \n\n")


# PCA ---------------------------------------------------------------------
setdiff2(colnames(train),colnames(test))

train$ID     <- NULL
train$target <- NULL

xx <- sample_n(train[,!grepl("PC",colnames(train))], 20000) # PCA is slow here
                             # faster versions rely on rJava

# We will not re-calc this on bigger data for
#   discussion purposes but we would for competition
x.pca <- prcomp(xx[,colnames(xx) %in% colnames(test)],
                center = TRUE,
                scale. = TRUE,
                tol = .25)
saveRDS(x.pca, "x.pca.RDS")

x.pca <- readRDS("x.pca.RDS")

print(summary(x.pca))
cat("\n")
cat("We could probably get away with a model of only about 6 - 15 PC's with no base features for parsimony, but I elected not to take that approach to maximize accuracy... \n\n")

x.pca.pred <- predict(x.pca, newdata = train)
dim(x.pca.pred)

x.pca.pred <- x.pca.pred[,1:6]
train      <- cbind(train, x.pca.pred)

x.pca.pred <- predict(x.pca, newdata = test)
dim(x.pca.pred)
x.pca.pred <- x.pca.pred[,1:6]
test       <- cbind(test, x.pca.pred)

# Cluster Analysis -------------------------------------------------------
# train$location.num <- gsub("location ","",train$location)
# test$location.num  <- gsub("location ","",test$location)
# train$location.num <- as.numeric(train$location.num)
# test$location.num  <- as.numeric(test$location.num)
# 
# nums <- sapply(train, is.numeric)
# colnames(train[!nums])
# 
# train.n <- train
# test.n  <- test
# train.n$location <- NULL
# test.n$location  <- NULL
# 
# train.n[!is.finite(train.n),] <- NA
# test.n[!is.finite(test.n)]    <- NA
# require(RRF)
# train.n <- na.roughfix(train.n)
# test.n  <- na.roughfix(test.n)
# 
# train.n <- as.data.frame(train.n)
# test.n  <- as.data.frame(test.n)
# train.n$location <- NULL
# test.n$location  <- NULL
# 
# # Determine number of clusters
# wss <- (nrow(train.n)-1)*sum(apply(train.n[,2:74],2,var))
# for (i in 2:74) wss[i] <- sum(kmeans(train.n[,2:74],
#                                      centers=i)$withinss)
# plot(1:74, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")
# 
# # K-Means Cluster Analysis
# # append cluster assignment
# fit3    <- kmeans(train.n[2:74], 3)
# train.n <- data.frame(train.n, fit3$cluster)
# fit3    <- kmeans(test.n[1:73], 3)
# test.n  <- data.frame(test.n, fit3$cluster)
# 
# fit20   <- kmeans(train.n[2:74], 20)
# train.n <- data.frame(train.n, fit20$cluster)
# fit20   <- kmeans(test.n[1:73], 20)
# test.n  <- data.frame(test.n, fit20$cluster)
# 
# fit2a    <- kmeans(train.n[2:400], 2)
# train.n  <- data.frame(train.n, fit2a$cluster)
# fit2a    <- kmeans(test.n[1:399], 2)
# test.n   <- data.frame(test.n, fit2a$cluster)
# 
# fit3a    <- kmeans(train.n[2:400], 3)
# train.n  <- data.frame(train.n, fit3a$cluster)
# fit3a    <- kmeans(test.n[1:399], 3)
# test.n   <- data.frame(test.n, fit3a$cluster)
# 
# fit5a    <- kmeans(train.n[2:400], 5)
# train.n  <- data.frame(train.n, fit5a$cluster)
# fit5a    <- kmeans(test.n[1:399], 5)
# test.n   <- data.frame(test.n, fit5a$cluster)



# Save --------------------------------------------------------------------
train$Y <- Y

saveRDS(train, "train_checkpoint1.RDS")
saveRDS(test, "test_checkpoint1.RDS")

train <- readRDS("train_checkpoint1.RDS")
test  <- readRDS("test_checkpoint1.RDS")
