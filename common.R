rmse <- function(error)
{
  sqrt(mse(error))
}
mse <- function(error)
{
  mean(error^2)
}

mae <- function(error)
{
  mean(abs(error))
}
mape <- function(error, actual)
{
  actual <- tail(actual, n = length(error))
  mean(abs(na.omit(error/actual*is.finite(error/actual))))
}

predict.factory <- function(name) {
  if (name == "SVM" || name == "LINEAR" || name == "SVM_LOOKS_BACK") {
    return(function(model, set){
      return(predict(model, set))
    })
  } else if (name == "ARFIMA" || name == "ARFIMA_LOOKS_BACK") {
    return(function(model, set){
      n.ahead <- ifelse(is.numeric(set), set, length(set$DATE))
      return(predict(model, n.ahead = n.ahead, do.plot = FALSE)[[1]]$Forecast)
    })
  } else if (name == "REPEAT_LAST") {
    return(function(model, set){
      return(repeat_last(model, set))
    })
  } else if (name == "RANDOM_WALK") {
    return(function(model, set){
      return(random_walk(model, set))
    })
  } else if (name == "SVM_AND_ARFIMA") {
    return(function(model, set){
      return(svm_and_arfima(model, set))
    })
  }
}

repeat_last <- function(model, set) {
  return(rep(model$CLOSE[length(model$CLOSE)], times = length(set$DATE)))
}

svm_and_arfima <- function(model, set) {
  svm_model <- model$SVM
  arfima_model <- model$ARFIMA
  we <- model$we
  
  svm_predict <- predict.factory("SVM")(svm_model, set)
  arfima_predict <- predict.factory("ARFIMA")(arfima_model, set)
  
  svm_weight <- (sum(we) - we[2]) / sum(we)
  arfima_weight <- (sum(we) - we[1]) / sum(we)
  
  predicted <- svm_predict * svm_weight + arfima_predict * arfima_weight
  return(predicted)
}

model.factory <- function(name) {
  if (name == "SVM") {
    return(function(train, params){
      if (length(unique(train$CLOSE)) == 1)
      {
        train$CLOSE[length(train$CLOSE)] <- train$CLOSE[length(train$CLOSE)] + 1e-07
        cat("[WARN] All train data are same: ", train$CLOSE[length(train$CLOSE)], "\n")
      }
      return(svm(CLOSE ~ DATE, train, epsilon = params$eps, cost = params$cost, gamma = params$gamma))
    })
  } else if (name == "LINEAR") {
    return(function(train, params){
      return(lm(CLOSE ~ DATE, train))
    })
  } else if (name == "ARFIMA") {
    return(function(train, params){
      if (length(unique(train$CLOSE)) == 1)
      {
        train$CLOSE[length(train$CLOSE)] <- train$CLOSE[length(train$CLOSE)] + 1e-07
        cat("[WARN] All train data are same: ", train$CLOSE[length(train$CLOSE)], "\n")
      }
      return(arfima::arfima(as.ts(train$CLOSE), order=c(params$p, params$d, params$q), quiet = TRUE))
    })
  } else if (name == "REPEAT_LAST") {
    return(function(train, params){
      return(train)
    })
  } else if (name == "RANDOM_WALK") {
    return(function(train, params){
      return(train)
    })
  } else if (name == "SVM_AND_ARFIMA") {
    return(function(train, params){
      return(list(
        SVM = model.factory("SVM")(train, params),
        ARFIMA = model.factory("ARFIMA")(train, params),
        we = params$we
      ))
    })
  } else if (name == "SVM_LOOKS_BACK") {
    return(function(train, params){
      return(looks_back_model("SVM", train, params))
    })
  } else if (name == "ARFIMA_LOOKS_BACK") {
    return(function(train, params){
      return(looks_back_model("ARFIMA", train, params))
    })
  }
}

looks_back_model <- function(name, train, params)
{
  best.err <- Inf
  best.window <- NULL
  
  train_end <- length(train$CLOSE)
  
  for (window in params$windows)
  {
    train_start <- train_end - window - params$depth + 1
    train2 <- train[c(train_start:train_end), ]
    roll <- rolling_abstract(name, train2, params, window.size = window)
    
    metrics <- roll$metrics
    if (metrics$weighted_mean_false_sign < best.err){
      best.err <- metrics$weighted_mean_false_sign
      best.window <- window
    }
  }
  .GlobalEnv$global <- c(.GlobalEnv$global, best.window)
  .GlobalEnv$global2 <- c(.GlobalEnv$global2, best.err)
  train_start <- train_end - best.window
  train2 <- train[c(train_start:train_end), ]
  
  return(model.factory(name)(train2, params))
}

optimize_rolling_abstract <- function(name, train, params,
                                      error.target = "rmse", window.size = 10, prediction.depth = 1)
{
  # value to minimize
  max_error <- Inf
  # roll on which error was minimal
  best_roll <- NULL
  # best params
  params.best <- NULL
  
  # params to grid search
  params.all <- list.decart(params)
  
  model_factory <- model.factory(name)
  
  for(index in 1:length(params.all)){
    params.current <- params.all[[index]]
    # perform a roll through the training set
    
    result <- NULL
    try(result <- rolling_abstract(name, train, params.current, window.size, prediction.depth))
    
    if (!is.null(result))
    {
      #remember the result
      params.all[[index]]$result <- result
    
      #update best result if need
      current_error <- result$metrics[[error.target]]
      if (current_error < max_error){
        max_error <- current_error
        best_roll <- result
        params.best <- params.current
      }
    }
  }
  
  return(list(best.params = params.best,
              best.roll = best_roll,
              best.error = max_error,
              all = params.all))
}

optimize_rolling_abstract_v2 <- function(name, set, training.coef = 0.8, params,
                                      error.target = "rmse", window.size = 10, prediction.depth = 1)
{
  # value to minimize
  max_error <- Inf
  # roll on which error was minimal
  best_roll <- NULL
  # best params
  params.best <- NULL
  
  # params to grid search
  params.all <- list.decart(params)
  
  model_factory <- model.factory(name)
  
  if (!is.null(params$window.size))
  {
    max_window.size <- max(params$window.size)
  } else {
    max_window.size <- window.size
  }
  
  for(index in 1:length(params.all)){
    params.current <- params.all[[index]]
    # perform a roll through the training set
    if (!is.null(params.current$window.size))
    {
      current_window.size <- params.current$window.size
    } else {
      current_window.size <- window.size
    }
    
    train <- first_part(training.coef, set, max_window.size - current_window.size)
    
    result <- NULL
    try(result <- rolling_abstract(name, train, params.current, window.size, prediction.depth))
    
    if (!is.null(result))
    {
      #remember the result
      params.all[[index]]$result <- result
      
      #update best result if need
      current_error <- result$metrics[[error.target]]
      if (current_error < max_error){
        max_error <- current_error
        best_roll <- result
        params.best <- params.current
      }
    }
  }
  
  return(list(best.params = params.best,
              best.roll = best_roll,
              best.error = max_error,
              all = params.all))
}

rolling_abstract <- function(name, set, params, window.size = 10, prediction.depth = 1)
{
  if (!is.null(params$window.size))
  {
    window.size <- params$window.size
  }
  max_len <- length(set$CLOSE) - prediction.depth + 1
  err_sum <- 0
  count <- 0
  res <- list()
  for (i in ((window.size+1):max_len)){
    predicted <- NULL
    train_start <- i - window.size
    train_end <- i - 1
    # inclusive ranges
    train <- set[c(train_start:train_end), ]
    
    test_index <- i + prediction.depth - 1
    test <- set[test_index,]
    
    model <- model.factory(name)(train, params)
    predicted <- make_prediction_abstract(name, model, test)
      if (!is.null(predicted)) {
        #is_increased_actual <- test$CLOSE - set[train_end, ]$CLOSE
        #is_increased_predicted <- test$CLOSE - predicted[1]
        #err <- predicted$mean_false_direction
        res$CLOSE[test_index] <- (predicted$pred)[1]
        res$DATE[test_index] <- test_index
        #err_sum <- err_sum + err
        count <- count + 1
      }
  }
  
  definedIndexes <- which(!is.na(res$CLOSE))
  actual <- (set$CLOSE)[definedIndexes]
  predicted <- (res$CLOSE)[definedIndexes]
  
  res$metrics <- get_metrics(actual, predicted)
  
  res$mean_error <- err_sum / count
  ###print(params)
  #message(sprintf("Err sum: %f",err_sum))
  ###message(sprintf("Count: %f", count))
  ###print(res$metrics$rmse)
  ###print(res$metrics$mean_false_sign)
  #message(sprintf("Mean error: %f", res$mean_error))
  return(res)
}

rolling_abstract_with_retrain <- function(name, name.retrain, set, params.start, params.range,
                                          window.size = 10, prediction.depth = 1, retrain.every = 20)
{
  if (!is.null(params.start$window.size))
  {
    window.size <- params.start$window.size
  }
  windows <- params.start$windows
  depth <- params.start$depth
  
  max_len <- length(set$CLOSE) - prediction.depth + 1
  err_sum <- 0
  count <- 0
  res <- list()
  for (i in ((window.size+1):max_len)){
    predicted <- NULL
    train_start <- i - window.size
    train_end <- i - 1
    # inclusive ranges
    train <- set[c(train_start:train_end), ]
    
    test_index <- i + prediction.depth - 1
    test <- set[test_index,]
    
    model <- model.factory(name)(train, params.start)
    predicted <- make_prediction_abstract(name, model, test)
    if (!is.null(predicted)) {
      #is_increased_actual <- test$CLOSE - set[train_end, ]$CLOSE
      #is_increased_predicted <- test$CLOSE - predicted[1]
      #err <- predicted$mean_false_direction
      res$CLOSE[test_index] <- (predicted$pred)[1]
      res$DATE[test_index] <- test_index
      #err_sum <- err_sum + err
      count <- count + 1
    }
    
    if (count %% retrain.every == 0)
    {
      retrain_set <- set[1:train_end, ]
      last_best_window_size <- .GlobalEnv$global[length(.GlobalEnv$global)]
      print(count / (max_len - (window.size+1)) * 100)
      print("Best window on retrain:")
      print(last_best_window_size)
      optim <- optimize_rolling_abstract(name.retrain, retrain_set, params.range,
                                         window.size = last_best_window_size,
                                         error.target = "mean_false_sign")
      params.start <- optim$best.params
      params.start$windows <- windows
      params.start$depth <- depth
      print("New params:")
      print(params.start)
    }
  }
  
  definedIndexes <- which(!is.na(res$CLOSE))
  actual <- (set$CLOSE)[definedIndexes]
  predicted <- (res$CLOSE)[definedIndexes]
  
  res$metrics <- get_metrics(actual, predicted)
  
  res$mean_error <- err_sum / count
  #print(params)
  #message(sprintf("Err sum: %f",err_sum))
  message(sprintf("Count: %f", count))
  print(res$metrics$rmse)
  print(res$metrics$mean_false_sign)
  #message(sprintf("Mean error: %f", res$mean_error))
  return(res)
}

make_prediction_abstract <- function(name, model, set) {
  predicted <- predict.factory(name)(model, set)
  actual <- set$CLOSE
  
  error <- set$CLOSE - predicted
  result <- list()
  result$mean_false_sign <- (sum(xor((set$CLOSE > 0), predicted > 0)) / length(set$CLOSE))
  result$pred <- predicted
  result$error <- error
  result$mse   <- mse(error)
  result$rmse  <- rmse(error)
  
  actual_diffs <- diff(x = actual, lag = 1, differences = 1)
  predicted_diffs <- diff(x = predicted, lag = 1, differences = 1)
  
  result$mean_false_direction <- (sum(xor((actual_diffs > 0), predicted_diffs > 0)) / length(actual_diffs))
  
  return(result)
}

plot_prediction <- function(name, set, result, window_coef = 0, need_points = FALSE, error.target = "rmse", mfs = 0){
  
  ymax <- max(set$CLOSE)
  ymin <- min(set$CLOSE)
  window <- (ymax - ymin) * window_coef
  ylow <- ymin - window
  yhigh <- ymax + window
  
  colors = c("blue", "red", "green")
  
  # plot true values
  plot(x = set$DATE, y = set$CLOSE, main=paste(name, " Model"), sub=paste("RMSE = ",
                                                  format(result[[1]]$metrics[[error.target]], digits = 10),
                                                  sep = ""),
       
       xlab = "Day",
       ylab = "Close",
       pch = 16, type="l", ylim=c(ylow, yhigh))
  if (need_points)
  {
    points(set, pch=16)
  }
  
  # plot all results with different colors
  for (i in 1:length(result))
  {
    predicted <- result[[i]]$CLOSE
    
    lines(set$DATE, predicted, col = colors[i], pch=4)
    if (need_points)
    {
      points(set$DATE, predicted, col = colors[i], pch=4)
    }
  }
}

plot_sign <- function(actual, expected, main = "")
{
  hits <- which(actual == expected)
  values <- as.numeric(as.character(actual[hits]))
  rate <- length(hits) / length(expected) * 100
  
  plot(as.numeric(as.character(actual)), ylim = c(-1.5, 1.5), type = "p",
       main = paste(main, "Hit Ratio: ", format(rate, digits = 5), "%", sep=""),
       xlab = "Day",
       ylab = "Direction")
  points(as.numeric(as.character(expected)), col="red")
  
  points(hits, values, pch = "|", col="blue")
}

plot_heatmap <- function(actual, expected, main = "")
{
  confusion_matrix <- as.data.frame(table(expected, actual))
  
  ggplot(data = confusion_matrix,
         mapping = aes(x = actual,
                       y = Var2)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_gradient(low = "blue",
                        high = "red",
                        trans = "log")
}

get_right_signs <- function(actual, predicted)
{
  hits <- which(actual == predicted)
  return((length(hits) / length(actual)) * 100)
}

plot_sets <- function(set, train.coef = 0.6, valid.coef = 0.2, test.coef = 0.2, main = "")
{
  plot(set, type = "l", col = "black", main = main)
  len <- length(set)
  train.st <- floor(len * train.coef)
  valid.st <- floor(train.st + len*valid.coef)
  test.st <- floor(valid.st + len*test.coef)
  abline(v = train.st, col = "red")
  abline(v = valid.st, col = "red")
  abline(v = test.st, col = "red")
}

make_prediction <- function(model, set, name, need_plot = TRUE, need_points = FALSE, window_coef = 0)
{
  predicted <- predict(model, set)
  error <- set$CLOSE - predicted
  result <- list()
  result$mean_false_sign <- (sum(xor((set$CLOSE > 0), predicted > 0)) / length(set$CLOSE))
  result$pred <- predicted
  result$error <- error
  result$mse   <- mse(error)
  result$rmse  <- rmse(error)
  
  actual_diffs <- diff(x = set$CLOSE, lag = 1, differences = 1)
  predicted_diffs <- diff(x = predicted, lag = 1, differences = 1)
  
  result$mean_false_direction <- (sum(xor((actual_diffs > 0), predicted_diffs > 0)) / length(actual_diffs))
  result$rsqared <- 1 - (sum(error^2)) / (sum((set$CLOSE - mean(set$CLOSE))^2))
  result$cor <- cor(set$CLOSE, predicted)
  result$cor2 <- result$cor^2
  
  if (need_plot){
    ymax <- max(set$CLOSE)
    ymin <- min(set$CLOSE)
    window <- (ymax - ymin) * window_coef
    ylow <- ymin - window
    yhigh <- ymax + window
    
    plot(set$DATE, set$CLOSE, main=paste(name, " Model"), sub=paste("RMSE = ", result$rmse), pch = 16, type="l", ylim=c(ylow, yhigh))
    lines(set$DATE, predicted, col = "blue", pch=4)
    if (need_points)
    {
      points(set, pch=16)
      points(set$DATE, predicted, col = "blue", pch=4)
    }
  }
  return(result)
}

plot_correlation <- function(actual, predicted, main = "Correlation")
{
  defined_indexes <- which(!is.na(predicted))
  if (length(defined_indexes) != length(predicted))
  {
    warning(length(predicted) - length(defined_indexes) ,
            " NA values")
  }
  actual <- actual[defined_indexes]
  predicted <- predicted[defined_indexes]
  
  maxvalue = max(max(actual),
                 max(predicted))
  minvalue = min(min(actual),
                 min(predicted))
  minmax = c(minvalue, maxvalue)
  
  plot(x = actual, y = predicted, xlim = minmax, ylim = minmax,
       xlab = "actual", ylab = "predicted", main = main)
  lines(x = minmax, y = minmax, col = "red")
  
  cat(cor(actual, predicted) * 100, "\n")
}

plot_prediction_simple <- function(actual, predicted)
{
  plot(x = 1:length(actual), y = actual, pch = 16, type="l")
  lines(x = 1:length(predicted), y = predicted, col = "blue", pch=4)
}

plot_errors <- function(actual, predicted, name)
{
  error <- predicted - actual
  dadadata <- data.frame(x = 1:length(error), y = error)
  ggplot(dadadata, aes(x, y)) + geom_area(aes(y = error)) +
    ggtitle(paste(name, " Errors")) +
    xlab("Day") +
    ylab("Error") +
    theme(plot.title = element_text(hjust = 0.5))
}
ggplot_pred <- function(actual, predicted, main = "SVM")
{
  actual.df <- data.frame(x = 1:length(actual), y = actual)
  predicted.df <- data.frame(x = 1:length(predicted), y = predicted)
  ggplot(actual.df, aes(x,y)) +
    geom_line(data = actual.df, mapping = aes(x,y), col = "black") +
    geom_line(data = predicted.df, mapping = aes(x,y), col = "blue") +
    xlab("Day") +
    ylab("Close") +
    title()
}

rolling_with_retrain_svm <- function(set, retrain.size, retrain.every, eps, cost, gamma)
{
  len <- dim(set)[1]
  pred <- numeric(len - retrain.size)
  
  current.model <- svm(CLOSE ~ ., set[1:retrain.size, ], epsilon = eps, cost = cost, gamma = gamma)
  for (i in (retrain.size+1):len)
  {
    if (i %% retrain.every == 0)
    {
      current.model <- svm(CLOSE ~ ., set[(i-retrain.size):(i-1), ], epsilon = eps, cost = cost, gamma = gamma)
    }
    current.pred <- predict(current.model, set[i,])
    pred[i - retrain.size] <- current.pred
  }
  
  return(pred)
}

rolling_with_retrain_svm2 <- function(set, retrain.size, retrain.every, eps, cost, gamma)
{
  len <- dim(set)[1]
  pred <- factor()
  levels(pred) <- up_down_levels
  
  current.model <- svm(SIGN ~ ., set[1:retrain.size, ], epsilon = eps, cost = cost, gamma = gamma)
  for (i in (retrain.size+1):len)
  {
    if (i %% retrain.every == 0)
    {
      current.model <- svm(SIGN ~ ., set[(i-retrain.size):(i-1), ], epsilon = eps, cost = cost, gamma = gamma)
    }
    current.pred <- predict(current.model, set[i,])
    pred[i - retrain.size] <- current.pred
  }
  
  return(pred)
}

make_arfima_prediction <- function(model, n.ahead, actual, name)
{
  predicted <- predict(model, n.ahead = n.ahead, do.plot = FALSE)$pred
  error <- actual - predicted
  result <- list()
  result$pred <- predicted
  result$error <- error
  result$mse   <- mse(error)
  result$rmse  <- rmse(error)
  result$mean_false_sign <- (sum(xor((actual > 0), predicted > 0)) / length(actual))
  
  return(result)
}

great_plot <- function(data, lag.max = 10, need_points = FALSE)
{
  plot(data, pch=16, type="l")
  if (need_points)
  {
    points(data, pch=16)
  }
  acf(data$CLOSE, lag.max = lag.max,
      type = c("correlation", "covariance", "partial"),
      plot = TRUE, na.action = na.fail, demean = TRUE)
  pacf(data$CLOSE, lag.max = lag.max,
      plot = TRUE, na.action = na.fail)
}

rolling <- function(set, model.name, window.size = 10, prediction.depth = 1,
                    eps = 0.1, cost = 10, gamma = 2,
                    p = 1, q = 1, d = 0)
{
  max_len <- length(set$CLOSE) - prediction.depth + 1
  err_sum <- 0
  count <- 0
  res <- list()
  for (i in ((window.size+1):max_len)){
    predicted <- NULL
    train_start <- i - window.size
    train_end <- i - 1
    # inclusive ranges
    train <- set[c(train_start:train_end), ]
    
    test_index <- i + prediction.depth - 1
    test <- set[test_index,]
    
    if (model.name == "SVM") {
      model <- svm(CLOSE ~ DATE, train, epsilon = eps, cost = cost, gamma = gamma)
      predicted <- make_prediction(model, test, model.name, need_plot = FALSE)
    } else if (model.name == "LINEAR") {
      model <- lm(CLOSE ~ DATE, train)
      predicted <- make_prediction(model, test, model.name, need_plot = FALSE)
    } else if (model.name == "ARFIMA") {
      model <- arima(as.ts(train$CLOSE), order=c(p, d, q), method = "CSS")
      predicted <- make_arfima_prediction(model, prediction.depth, test$CLOSE, model.name)
    }
    if (!is.null(predicted)) {
      err <- predicted$rmse
      res$CLOSE[test_index] <- (predicted$pred)[1]
      res$DATE[test_index] <- test_index
      err_sum <- err_sum + err
      count <- count + 1
    }
  }
  
  definedIndexes <- which(!is.na(res$CLOSE))
  actual <- (set$CLOSE)[definedIndexes]
  predicted <- (res$CLOSE)[definedIndexes]
  
  res$metrics <- get_metrics(actual, predicted)
  
  res$mean_error <- err_sum / count
  message(sprintf("Err sum: %f",err_sum))
  message(sprintf("Count: %f", count))
  message(sprintf("Mean error: %f", res$mean_error))
  return(res)
}

get_metrics <- function(actual, predicted){
  result <- list()
  error <- actual - predicted
  result$error <- error
  result$mse   <- mse(error)
  result$rmse  <- rmse(error)
  result$mean_false_sign <- (sum(xor((actual > 0), predicted > 0)) / length(actual))
  result$weighted_mean_false_sign <- c(length(result$mean_false_sign):1)^-1*(result$mean_false_sign)
  
  actual_diffs <- diff(x = actual, lag = 1, differences = 1)
  predicted_diffs <- diff(x = predicted, lag = 1, differences = 1)
  
  result$mean_false_direction <- (sum(xor((actual_diffs > 0), predicted_diffs > 0)) / length(actual_diffs))
  
  result$cor <- cor(actual, predicted)
  result$cor2 <- result$cor^2
  result$rsquared <- 1 - (sum(error^2)) / (sum((actual - mean(actual))^2))
  
  return(result)
}

make_roll <- function(set, model.name, window.size = 10, prediction.depth = 1,
                      eps = 0.1, cost = 10, gamma = 2,
                      p = 1, q = 1, d = 0){
  roll <- rolling(set, model.name, window.size, prediction.depth, eps, cost, gamma, p, q, d)
  
  plot(set, pch = 16, type = "l", main = model.name,
       sub = paste(sep = "",
                   "RMSE: ", round(roll$metrics$rmse, 5),
                   ". Mean true sign: ", round(100 - roll$metrics$mean_false_sign * 100, 2),
                   "%. Mean true direct: ", round(100 - roll$metrics$mean_false_direction * 100, 2),
                   "%."))
  points(set, pch=16)
  lines(set$DATE, roll$CLOSE, col="blue", pch=4)
  points(set$DATE, roll$CLOSE, col="blue", pch=16)
  
  return (roll)
}

optimize_rolling <- function(train, test, window.size = 10, prediction.depth = 1, epsilon = 10^(-4:-1), cost = c(10^(-1:4), 15000, 20000), gamma = 2*(1:10))
{
  max_err <- Inf
  best_roll <- NULL
  best_eps <- NULL
  best_cost <- NULL
  best_gamma <- NULL
  
  for(eps in epsilon)
    for(c in cost)
      for(g in gamma)
      {
        roll <- make_roll(train, "SVM", window.size, prediction.depth, eps, c, g)
        err <- roll$mean_error
        message(sprintf("Epsilon: %f; Cost: %f; Gamma: %f; Error: %f.", eps, c, g, err))
        if (err < max_err)
        {
          max_err <- err
          best_roll <- roll
          best_eps <- eps
          best_cost <- c
          best_gamma <- g
        }
      }
  return(list(best_roll = best_roll, best_eps = best_eps, best_cost = best_cost, best_gamma = best_gamma))
}

optimize_rolling_arfima <- function(train, test, window.size = 10, prediction.depth = 1, ps = c(2:5), qs = c(2:5))
{
  max_err <- Inf
  best_roll <- NULL
  best_p <- NULL
  best_q <- NULL
  
    for(p in ps)
      for(q in qs)
      {
        roll <- make_roll(train, "ARFIMA", window.size, prediction.depth, p = p, q = q)
        err <- roll$mean_error
        message(sprintf("P: %f; Q: %f; Error: %f.", p, q, err))
        if (err < max_err)
        {
          max_err <- err
          best_roll <- roll
          best_p <- p
          best_q <- q
        }
      }
  return(list(best_roll = best_roll, best_p = p, best_q = q))
}
