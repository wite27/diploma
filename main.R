library(sjmisc)

targets <- c("NASDAQ", "KOSPI", "MICEX", "SANDP")
periods <- c("5MIN", "1HOUR", "DAILY", "WEEKLY")

files <- character(length(targets) * length(periods))
for (j in 1:length(periods))
for (i in 1:length(targets))
{
  index <- length(targets) * (j - 1) + i
  files[index] <- paste("input/", targets[i], "_", periods[j], ".CSV", sep = "")
}

for (file in files)
{
  data <- read.csv(file, stringsAsFactors = FALSE, sep = ";", dec = ".", colClasses = c(DATE = "character"))
  data$DATE <- as.Date.character(data$DATE, "%Y%m%d")
  data <- na.omit(data[c("DATE", "CLOSE", "OPEN", "LOW", "HIGH")])
  #count_weekdays(data$DATE, file)
  cat(file, sum(diff(data$CLOSE, lag=1, differences=1) == 0), "\n", sep="\t")
}

need.remove.saturdays <- c("SANDP_1HOUR", "SANDP_5MIN",
                           "MICEX_DAILY",
                           "NASDAQ_1HOUR", "NASDAQ_5MIN")

sink(file = paste("output/ga+ica_2.txt", sep=""),
     append = TRUE, type = c("output", "message"),
     split = FALSE)
for (file in files)
{
  data <- read.csv(file, stringsAsFactors = FALSE, sep = ";", dec = ".", colClasses = c(DATE = "character"))
  data$DATE <- as.Date.character(data$DATE, "%Y%m%d")
  data <- na.omit(data[c("DATE", "CLOSE", "OPEN", "LOW", "HIGH")])
  
  set <- data
  
  set_marked <- set
  
  if (need.to.remove.saturdays(file))
  {
    set_marked <- remove_weekdays(set, "Сб")
  }
  
  set_marked <- enrich_TI(set_marked, n = 10, offset = 1, "VOL", need_ema_50 = TRUE)
  set_marked <- na.omit(mark_sign(set_marked))
  
  set_marked$SIGN <- factor(set_marked$SIGN, levels = up_down_levels)
  set_marked <- na.omit(set_marked)
  
  set_marked_save <- set_marked # save set before features selection
  
  set_marked_len <- dim(set_marked)[1]
  train.l <- 0.6 * set_marked_len
  valid.l <- 0.2 * set_marked_len
  test.l <- 0.2 * set_marked_len
  
  train_indexes <- 1:train.l
  valid_indexes <- (train.l+1):(train.l+valid.l)
  test_indexes <- (train.l+valid.l+1):(train.l+valid.l+test.l)
  
  features <- c("SIGN", "EMA10", "EMA50", "stochK", "stochD", "ROC", "WMR",
                "MACD", "DISPARITY5", "DISPARITY10", "ADX", "RSI")
  
  features.cols <- names(set_marked) %in% features
  set_marked <- set_marked[, features.cols]
  
  train_set <- set_marked[train_indexes, ]
  valid_set <- set_marked[valid_indexes, ]
  test_set <- set_marked[test_indexes, ]
  
  #cat("Testing ", file, " with grid search:\n")
  #test.gridsearch(train_set, valid_set, test_set)
  #cat("\n")
  
  #cat(count_signs(train_set), count_signs(valid_set), count_signs(test_set), sep="\t")
  #cat("\n")
  
  #test.rw2(train_set, valid_set, test_set)
  #cat("\n")
  
  #cat(file, "\n")
  #cat("NCOMP", "VALID", "COST", "GAMMA", "TEST")
  #cat("\n")
  #test.gs.ica(train_set, valid_set, test_set)
  #cat("\n")
  # 
  # cat(file, "\n")
  # cat("VALID", "COST", "GAMMA", "TEST", "\n")
  # cat(test.ga(train_set, valid_set, test_set))
  # cat("\n")
  
  cat(file, "\n")
  cat("NCOMP", "VALID", "COST", "GAMMA", "TEST")
  cat("\n")
  cat(test.ga.ica(train_set, valid_set, test_set))
  cat("\n")
  
}
sink()

count_signs <- function(set)
{
  len <- dim(set)[1]
  ups <- sum(set$SIGN == 1)
  downs <- sum(set$SIGN == -1)
  return(paste(len, ups, downs, sep="\t"))
}

test.gridsearch <- function(train, valid, test)
{
  best.c <- NA
  best.g <- NA
  best.perf <- -Inf
  for (c in 10^(-5:1))
  for (g in 10^(-5:1))
  {
    ssvm <- svm(SIGN ~ ., train, cost = c, gamma = g)
    pred <- predict(ssvm, valid)
    perf <- get_right_signs(valid$SIGN, pred)
    if (perf > best.perf)
    {
      best.c <- c
      best.g <- g
      best.perf <- perf
    }
  }
  ssvm <- svm(SIGN ~ ., train, cost = best.c, gamma = best.g)
  pred <- predict(ssvm, test)
  perf <- get_right_signs(test$SIGN, pred)
  
  return(paste(best.perf, best.c, best.g, perf, sep = "\t"))
}

test.rw <- function(train, valid, test)
{
  last.valid <- valid$SIGN[length(valid$SIGN)]
  res <- shift_right(test$SIGN, 1)
  res[1] <- last.valid
  
  cat(get_right_signs(test$SIGN, res))
}

test.rw2 <- function(train, valid, test)
{
  len <- length(test$SIGN)
  res <- sample(c(-1, 1), len, replace = TRUE)
  cat(get_right_signs(test$SIGN, res))
}

test.gs.ica <- function(train, valid, test)
{
  train.save <- train
  valid.save <- valid
  test.save <- test
  for (i in 2:9)
  {
    train <- train.save
    valid <- valid.save
    test <- test.save
    
    train.sign <- train$SIGN
    valid.sign <- valid$SIGN
    test.sign <- test$SIGN
    
    train <- train[,-which(names(train) == "SIGN")]
    valid <- valid[,-which(names(valid) == "SIGN")]
    test <- test[,-which(names(test) == "SIGN")]
    
    ica_res <- fastICA(test, n.comp = i, tol=1e-10)
    
    ica_demixed_train <- as.data.frame((as.matrix(train) %*% ica_res$K) %*% ica_res$W)
    ica_demixed_valid <- as.data.frame((as.matrix(valid) %*% ica_res$K) %*% ica_res$W)
    ica_demixed_test <- as.data.frame((as.matrix(test) %*% ica_res$K) %*% ica_res$W)
    
    ica_demixed_train$SIGN <- train.sign
    ica_demixed_valid$SIGN <- valid.sign
    ica_demixed_test$SIGN <- test.sign
    
    best <- test.gridsearch(ica_demixed_train, ica_demixed_valid, ica_demixed_test)
    cat(i, best, sep="\t")
    cat("\n")
  }
}

test.ga.ica <- function(train, valid, test)
{
  train.save <- train
  valid.save <- valid
  test.save <- test
  for (i in 2:9)
  {
    train <- train.save
    valid <- valid.save
    test <- test.save
    
    train.sign <- train$SIGN
    valid.sign <- valid$SIGN
    test.sign <- test$SIGN
    
    train <- train[,-which(names(train) == "SIGN")]
    valid <- valid[,-which(names(valid) == "SIGN")]
    test <- test[,-which(names(test) == "SIGN")]
    
    ica_res <- fastICA(test, n.comp = i, tol=1e-10)
    
    ica_demixed_train <- as.data.frame((as.matrix(train) %*% ica_res$K) %*% ica_res$W)
    ica_demixed_valid <- as.data.frame((as.matrix(valid) %*% ica_res$K) %*% ica_res$W)
    ica_demixed_test <- as.data.frame((as.matrix(test) %*% ica_res$K) %*% ica_res$W)
    
    ica_demixed_train$SIGN <- train.sign
    ica_demixed_valid$SIGN <- valid.sign
    ica_demixed_test$SIGN <- test.sign
    
    best <- test.ga(ica_demixed_train, ica_demixed_valid, ica_demixed_test)
    cat(i, best, sep="\t")
    cat("\n")
  }
}

test.ga <- function(train, valid, test)
{
  ica_ga_fun <- function(x)
  {
    ssvm <- svm(formula("SIGN ~ ."),
                train, type = "C-classification", cost = x[1], gamma = x[2])
    ssvm.pred <- predict(ssvm, valid)
    
    performance <- get_right_signs(valid$SIGN, ssvm.pred)
    return(performance)
  }
  
  maxiter <- 50
  ica_ga <- ga(type = "real-valued",
               fitness = ica_ga_fun,
               min = c(1e-05, 1e-05),
               max = c(10, 10),
               monitor = FALSE,
               names = c("cost", "gamma"),
               pmutation = 0.6,
               parallel = 6,
               popSize = 25,
               maxiter = maxiter,
               keepBest = TRUE)
  
  ssvm <- svm(SIGN ~ ., train, cost = ica_ga@solution[1,1], gamma = ica_ga@solution[1,2])
  pred <- predict(ssvm, test)
  perf <- get_right_signs(test$SIGN, pred)
  
  return(paste(ica_ga@fitnessValue,
               ica_ga@solution[1,1], ica_ga@solution[1,2],
               perf, sep = "\t"))
}

need.to.remove.saturdays <- function(file)
{
  for (pattern in need.remove.saturdays)
  {
    if (str_contains(file, pattern, ignore.case = TRUE))
      return(TRUE)
  }
  return(FALSE)
}

# for NATURAL GAS
data <- remove_weekdays(data)
data <- data[1200:10000,]
count_weekdays(data$DATE[1200:10000])

count_weekdays(data$DATE)
data <- na.omit(data[c("DATE", "CLOSE", "OPEN", "LOW", "HIGH")])

set <- data

up_down_levels <- c(1, -1)

# if data contain only Monday to Friday
set_marked <- enrich_TI(set, n = 10, offset = 5, "VOL")
set_marked <- na.omit(get_fridays2(set_marked))

# if data contain only Monday to Friday
set_marked <- enrich_TI(set, n = 10, offset = 5, "VOL")
set_marked <- na.omit(get_fridays(set_marked))

# if data contains sundays
set_marked <- enrich_TI(set, n = 14, offset = 7, "VOL")
set_marked <- na.omit(get_sundays2(set_marked))

#set_marked <- na.omit(set_marked[2900:(dim(set_marked)[1]), ])
set_marked <- na.omit(mark_sign(set_marked))

#calculate metrics only after getting weekly prices
set_marked <- na.omit(get_sundays2(set))
set_marked <- set
set_marked <- remove_weekdays(set, "Сб")
set_marked <- enrich_TI(set_marked, n = 10, offset = 1, "VOL")
set_marked <- add_volatility(set_marked, offset = 1, volat.back = 36)
set_marked <- set_marked[1000:10000, ]
set_marked <- na.omit(mark_sign(set_marked))
set_marked <- na.omit(set_marked)
count_weekdays(set_marked$DATE)

#set_marked$PREVCLOSE <- shift_right(set_marked$CLOSE, 1)
set_marked$PREVSIGN <- shift_right(set_marked$SIGN, 1)
set_marked$SIGN <- as.factor(set_marked$SIGN)
set_marked$PREVSIGN <- factor(as.character(set_marked$PREVSIGN),
                              levels = levels(set_marked$SIGN))
set_marked <- na.omit(set_marked)
set_marked <- na.omit(set_marked[500:9999, ])

set_marked_save <- set_marked # save set before features selection


set_marked_len <- dim(set_marked)[1]
train.l <- 0.6 * set_marked_len
valid.l <- 0.2 * set_marked_len
test.l <- 0.2 * set_marked_len

train_indexes <- 1:train.l
valid_indexes <- (train.l+1):(train.l+valid.l)
test_indexes <- (train.l+valid.l+1):(train.l+valid.l+test.l)

features <- c("SIGN",
              "ATR", "EMA", "CCI", "stochK", "stochD", "WMR", "CLV", "chaikinVolatility",
              "SMA", "ROC", "RSI", "TDI", "ADX", "MOMENTUM", "WMA", "MACD")
features <- c("SIGN",
              "ATR5",
              "ATR10",
              "DISPARITY5",
              "DISPARITY10",
              "MOMENTUM1", "MOMENTUM5",
              "MOMENTUM10",
              "OSCP",
              "CCI", "chaikinVolatility",
              "stochK", "stochD", "WMR",
              "CLV", "ROC", "RSI"
              , "TDI", "ADX", "MACD"
)
features <- c("SIGN", "stochK", "stochD", "MOMENTUM5", "ROC", "WMR", "CLV",
              "DISPARITY5", "DISPARITY10", "OSCP", "CCI", "RSI")
features <- c("SIGN", "PREVSIGN", "MOMENTUM5", "ROC", "WMR", "CLV",
              "DISPARITY5", "DISPARITY10", "OSCP", "CCI", "RSI")

features.cols <- names(set_marked) %in% features
set_marked <- set_marked[, features.cols]