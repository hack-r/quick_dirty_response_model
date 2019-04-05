################################################
# File:            library_options_functions.R #
# Context:         Run by Master file          #
# Contact author:  Miller, J.                  #
# Initial Date:    2019-04-04                  #
# Version:         0.0.1                       #
################################################

# Set working directory
setwd("data")

# Libraries
if(!require(pacman)) install.packages("pacman"); require(pacman)
p_load(adabag, 
       bit64, 
       cvAUC,
       caret,
       corrplot,
       data.table,
       doParallel,
       ggplot2,
       gridExtra,
       h2o, 
       #h2oEnsemble,
       Hmisc,
       lubridate,
       maps,
       maptools,
       Matrix,
       Metrics,
       mlbench,
       nnet,
       pls,
       ROCR,
       R.utils,
       RRF,
       sp,
       tidyverse, 
       xgboost)


# Options
set.seed(2019)



# Custom Functions --------------------------------------------------------

setdiff2 <- function (x, y, cn = F) {
  # author: Miller, J.
  # note: I'm only putting my name here so that
  #       the good people at C2G won't think
  #       I'm just copying and pasting others' work
  if (cn) {
    d0 <- setdiff(colnames(x), colnames(y))
    d1 <- setdiff(colnames(y), colnames(x))
    cat("Columns in X but not in Y: \n\n")
    cat(d0, "\n\n")
    cat("Columns in Y but not in X: \n\n")
    cat(d1)
  }
  else {
    d0 <- setdiff(x, y)
    d1 <- setdiff(y, x)
    cat("Values in X but not in Y: \n\n")
    cat(d0, "\n\n")
    cat("Values in Y but not in X: \n\n")
    cat(d1)
  }
}

rm_cons <- function (x) {
  # author: Miller, J. (just for the record!)
  cat("\n## Removing the constant features.\n")
  for (f in names(x)) {
    if (length(unique(x[[f]])) == 1) {
      cat(f, "is constant. It is has been deleted.\n")
      x[[f]] <- NULL
    }
  }
  return(x)
}

rm_ident <- function (train, test = NULL) {
  # author: Miller, J.
  features_pair <- combn(names(train), 2, simplify = F)
  toRemove <- c()
  for (pair in features_pair) {
    f1 <- pair[1]
    f2 <- pair[2]
    if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
      if (all(train[[f1]] == train[[f2]])) {
        cat(f1, "and", f2, "are equals.\n")
        toRemove <- c(toRemove, f2)
      }
    }
  }
  train <- train[, !colnames(train) %in% toRemove]
  if (!missing(test)) {
    test <- test[, !colnames(test) %in% toRemove]
  }
}

target_encode <- function (all, CATEGORICAL_VAR, Y = Y, train = 0, valid = 0) {
  # target_encode
  # author: Miller, J.
  require(dplyr)
  require(RRF)
  all$Y <- na.roughfix(all$Y)
  target_encoded <- paste0("target_enc_", CATEGORICAL_VAR)
  if (!valid == 0) {
    tmp <- all %>% group_by_(CATEGORICAL_VAR) %>% summarize(target_encoded = mean(Y))
    train <- merge(train, tmp, by = CATEGORICAL_VAR, all.x = TRUE)
    valid <- merge(valid, tmp, by = CATEGORICAL_VAR, all.x = TRUE)
    train$flag_training <- 1
    valid$flag_training <- 0
    res <- rbind(train, valid)
  }
  else {
    tmp <- all %>% group_by_(CATEGORICAL_VAR) %>% summarize(target_encoded = mean(Y))
    all <- merge(all, tmp, by = CATEGORICAL_VAR, all.x = TRUE)
    res <- all
  }
  return(res)
}

factorFix <- function (train, test, exclude) {
  if (missing(exclude)) {
    exclude <- c("next_term_retention", "group", "oos_Date", 
                 "Term_Start_Date")
  }
  else {
    if (!class(exclude) == "character") {
      cat("exclude must be a character or character vector")
    }
  }
  for (i in 1:length(colnames(train))) {
    if (class(train[, i]) %in% c("Date", "POSIXct", "POSIXt")) {
      next
    }
    if ("integer" %in% class(train[, i])) {
      train[, i] <- as.numeric(train[, i])
    }
    if ("numeric" %in% class(train[, i])) {
      train[, i] <- RRF::na.roughfix(train[, i])
    }
    if (("factor" %in% class(train[, i])) | ("character" %in% 
                                             class(train[, i]))) {
      if (colnames(train)[i] %in% exclude) {
        next
      }
      else {
        train[, i] <- as.character(train[, i])
        train[1, i] <- NA
        train[, i][is.na(train[, i])] <- "Missing"
        train[, i] <- as.factor(train[, i])
      }
      if (length(levels(train[, i])) > 30) {
        print("TOO MANY LEVELS!")
        print(colnames(train)[i])
        print(length(levels(train[, i])))
      }
    }
  }
  test <- test[, colnames(test) %in% colnames(train)]
  cn <- colnames(test)
  for (i in 1:length(cn)) {
    if ("integer" %in% class(test[, i])) {
      test[, i] <- as.numeric(test[, i])
    }
    if ("numeric" %in% class(test[, i])) {
      test[, i] <- RRF::na.roughfix(test[, i])
    }
    if (("factor" %in% class(test[, i])) | ("character" %in% 
                                            class(test[, i]))) {
      if (colnames(test)[i] %in% exclude) {
        next
      }
      else {
        test[, i] <- as.character(test[, i])
        test[1, i] <- NA
        test[, i][is.na(test[, i])] <- "Missing"
        test[, i] <- as.factor(test[, i])
      }
      if (!identical(levels(test[, i]), levels(train[, 
                                                     colnames(test)[i]]))) {
        l1 <- levels(train[, colnames(test)[i]])
        l2 <- levels(test[, i])
        test[, i] <- as.character(test[, i])
        test[, i] <- ifelse(test[, i] %in% l1, as.character(test[, 
                                                                 i]), "Missing")
        if (length(l2) < length(l1) | (length(l2) == 
                                       length(l1) & !(identical(levels(as.factor(test[, 
                                                                                      i])), levels(train[, colnames(test)[i]]))))) {
          for (n in 1:length(l1[!l1 %in% l2])) {
            test[10 + n, i] <- l1[!l1 %in% l2][n]
          }
        }
        test[, i] <- as.factor(test[, i])
        if (!identical(levels(test[, i]), levels(train[, 
                                                       colnames(test)[i]]))) {
          print("There is a problem with factor levels!")
          print(i)
        }
      }
      if (length(levels(test[, i])) > 30) {
        print(colnames(test)[i])
        print(length(levels(test[, i])))
      }
    }
  }
  for (i in 1:ncol(train)) {
    if (!(identical(colnames(train)[i], colnames(test)[i]))) {
      print(colnames(train)[i])
      print(i)
    }
    if (!(identical(class(train)[i], class(test)[i]))) {
      print("There is a problem with:")
      print(colnames(train)[i])
      print("which is col number:")
      print(i)
    }
    if (!(identical(levels(train)[i], levels(test)[i]))) {
      print("There is a problem with:")
      print(colnames(train)[i])
      print(i)
    }
  }
  if (sum(is.na(test)) > 0) {
    print("!!!WARNING!!! THERE IS A PROBLEM WITH MISSING DATA IN test")
  }
  result <- list(train, test)
  return(result)
}

factorFix.xgb <- function(train, test){
  for (f in feature.names) {
    if (class(train[[f]]) %in% c("character","factor")) {
      levels     <- unique(c(train[[f]], test[[f]]))
      train[[f]] <- as.integer(factor(train[[f]], levels=levels))
      test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    }
  }
  result <- list(train, test)
  return(result)
  
}


