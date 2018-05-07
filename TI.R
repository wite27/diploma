library(caTools) #runm(in/ax)

# shifts vector to rigth and adds NAs to the left
shift_right <- function(v, offset)
{
  len <- length(v)
  v <- c(rep(NA, times = offset), v)
  v <- v[1:len]
  return(v)
}

enrich_TI <- function(set, n = 5, offset = 5, vol_name = "VOLUME", need_ema_50 = FALSE)
{
  hlc <- matrix(c(set$HIGH,
                  set$LOW,
                  set$CLOSE),
                ncol = 3)
  hl <- matrix(c(set$HIGH,
                 set$LOW),
               ncol = 2)
  #v <- set[vol_name]
  cloz <- set$CLOSE
  
  big_atr <- TTR::ATR(hlc, n = n, maType = "SMA")[,2]
  big_atr5 <- TTR::ATR(hlc, n = 5, maType = "SMA")[,2]
  big_atr10 <- TTR::ATR(hlc, n = 10, maType = "SMA")[,2]
  big_ema <- TTR::EMA(hlc[,3], n = n)
  big_ema5 <- TTR::EMA(hlc[,3], n = 5)
  big_ema10 <- TTR::EMA(hlc[,3], n = 10)
  big_cci <- TTR::CCI(hlc, n = n)
  stochKD <- TTR::stoch(hlc, nFastK = n, nFaskD = n)
  big_stochK <- stochKD[,1]
  big_stochD <- stochKD[,2]
  big_willamsR <- TTR::WPR(hlc, n = n)
  #big_emv <- TTR::EMV(hl, v, n = n)[,1]
  big_clv <- TTR::CLV(hlc)
  big_chaikonVolatility <- TTR::chaikinVolatility(hl, n = n)
  big_sma <- TTR::SMA(cloz, n = n)
  big_sma5 <- TTR::SMA(cloz, n = 5)
  big_sma10 <- TTR::SMA(cloz, n = 10)
  big_roc <- TTR::ROC(cloz, n = n)
  big_rsi <- TTR::RSI(cloz, n = n)
  big_tdi <- TTR::TDI(cloz, n = n)[,1]
  big_adx <- TTR::ADX(hlc, n = n)[,4]
  big_momentum <- TTR::momentum(x = cloz, n = n)
  big_momentum1 <- TTR::momentum(x = cloz, n = 1)
  big_momentum5 <- TTR::momentum(x = cloz, n = 5)
  big_momentum10 <- TTR::momentum(x = cloz, n = 10)
  big_wma <- TTR::WMA(x = cloz, n = n)
  big_macd <- TTR::MACD(x = cloz)
  big_disparity5 <- cloz / big_sma5 * 100
  big_disparity10 <- cloz / big_sma10 * 100
  big_oscp <- (big_sma5 - big_sma10) / big_sma5
  big_volatility <- TTR::volatility(cloz, n = n)
  if (need_ema_50)
  {
    big_ema50 <- TTR::EMA(hlc[,3], n = 50)
  }
  
  set$ATR <- shift_right(big_atr, offset)
  set$ATR5 <- shift_right(big_atr5, offset)
  set$ATR10 <- shift_right(big_atr10, offset)
  set$EMA <- shift_right(big_ema, offset)
  set$EMA5 <- shift_right(big_ema5, offset)
  set$EMA10 <- shift_right(big_ema5, offset)
  set$CCI <- shift_right(big_cci, offset)
  set$stochK <- shift_right(big_stochK, offset)
  set$stochD <- shift_right(big_stochD, offset)
  set$WMR <- shift_right(big_willamsR, offset)
  #set$EMV <- shift_right(big_emv, offset)
  set$CLV <- shift_right(big_clv, offset)
  set$chaikinVolatility <- shift_right(big_chaikonVolatility, offset)
  set$SMA <- shift_right(big_sma, offset)
  set$SMA5 <- shift_right(big_sma5, offset)
  set$SMA10 <- shift_right(big_sma10, offset)
  set$ROC <- shift_right(big_roc, offset)
  set$RSI <- shift_right(big_rsi, offset)
  set$TDI <- shift_right(big_tdi, offset)
  set$ADX <- shift_right(big_adx, offset)
  set$MOMENTUM <- shift_right(big_momentum, offset)
  set$MOMENTUM1 <- shift_right(big_momentum1, offset)
  set$MOMENTUM5 <- shift_right(big_momentum5, offset)
  set$MOMENTUM10 <- shift_right(big_momentum10, offset)
  set$WMA <- shift_right(big_wma, offset)
  set$MACD <- shift_right(big_macd[,1], offset)
  set$DISPARITY5 <- shift_right(big_disparity5, offset)
  set$DISPARITY10 <- shift_right(big_disparity10, offset)
  set$OSCP <- shift_right(big_oscp, offset)
  set$VOLATILITY <- shift_right(big_volatility, offset)
  if (need_ema_50)
  {
    big_ema50 <- TTR::EMA(hlc[,3], n = 50)
  }
  
  return(set)
}

add_volatility <- function(set, offset = 1, volat.back = 365)
{
  len <- length(set$CLOSE)
  parts.count <- 5 # number of levels
  
  volat <- TTR::volatility(set$CLOSE)
  rmin <- runmin(volat, k = volat.back, endrule = "NA")
  rmax <- runmax(volat, k = volat.back, endrule = "NA")
  
  diff <- rmax - rmin
  part.size <- diff / parts.count
  
  res <- numeric(len)
  for (i in (1:len))
  {
    if (is.na(diff[i]))
    {
      res[i] <- NA
      next
    }
    current.vol <- volat[i] # todays volatility
    current.min <- rmin[i] # min volatility in past year
    current.max <- rmax[i] # max volatility in past year
    current.part <- 1
    while (current.min + current.part * part.size[i] < (current.vol - 1e-10)) # comparing doubles
    {
      current.part <- current.part + 1
    }
    res[i] <- current.part
  }
  set$VOLATILITY_LEVELS <- shift_right(res, offset)
  return(set)
}

get_discreted_TI <- function(set, n = 5, offset = 5, vol_name = "VOLUME")
{
  set <- enrich_TI(set, n = n, offset = 0, vol_name = vol_name)
  
  set$D_SMA <- factor(shift_right(ifelse(set$CLOSE > set$SMA, 1, -1), offset), levels = up_down_levels)
  set$D_EMA <- factor(shift_right(ifelse(set$CLOSE > set$EMA, 1, -1), offset), levels = up_down_levels)
  set$D_MOMENTUM <- factor(shift_right(ifelse(set$MOMENTUM > 0, 1, -1), offset), levels = up_down_levels)
  
  set$D_stochK <- factor(shift_right(c(NA, ifelse(diff(set$stochK, lag = 1) > 0, 1, -1)), offset), levels = up_down_levels)
  set$D_stochD <- factor(shift_right(c(NA, ifelse(diff(set$stochD, lag = 1) > 0, 1, -1)), offset), levels = up_down_levels)
  set$D_WMR <- factor(shift_right(c(NA, ifelse(diff(set$WMR, lag = 1) > 0, 1, -1)), offset), levels = up_down_levels)
  
  set$D_MACD <- factor(shift_right(c(NA, ifelse(diff(set$MACD, lag = 1) > 0, 1, -1)), offset), levels = up_down_levels)
  
  set$D_RSI <- factor(shift_right(discretize_RSI(set$RSI), offset), levels = up_down_levels)
  set$D_CCI <- factor(shift_right(discretize_CCI(set$CCI), offset), levels = up_down_levels)

  return(set)
}

discretize_RSI <- function(rsi)
{
  lagged <- c(NA, diff(rsi, lag = 1))
  result <- numeric(length(rsi))
  for (i in 1:length(rsi))
  {
    if (is.na(rsi[i]))
    {
      result[i] = NA
      next
    }
    if (rsi[i] > 70)
    {
      result[i] = -1
      next
    }
    if (rsi[i] < 30)
    {
      result[i] = 1
      next
    }
    if (is.na(lagged[i]))
    {
      result[i] = NA
      next
    }
    result[i] <- ifelse(lagged[i] > 0, 1, -1)
  }
  return(result)
}

discretize_CCI <- function(cci)
{
  lagged <- c(NA, diff(cci, lag = 1))
  result <- numeric(length(cci))
  for (i in 1:length(cci))
  { 
    if (is.na(cci[i]))
    {
      result[i] = NA
      next
    }
    if (cci[i] > 200)
    {
      result[i] = -1
      next
    }
    if (cci[i] < -200)
    {
      result[i] = 1
      next
    }
    if (is.na(lagged[i]))
    {
      result[i] = NA
      next
    }
    result[i] <- ifelse(lagged[i] > 0, 1, -1)
  }
  return(result)
}

get_TI_infos <- function(set)
{
  titles <- colnames(set)
  res <- data.frame()
  for (title in titles)
  {
    values <- set[,title]
    if (!is.numeric(values))
      next
    values <- na.omit(values)
    my_count <- length(values)
    
    my_min <- min(values)
    my_max <- max(values)
    my_mean <- mean(values)
    my_sd <- sd(values)
    res <- rbind(res, list(title, my_min, my_max, my_mean, my_sd, my_count), stringsAsFactors = FALSE)
  }
  colnames(res) <- c("Variable", "Min", "Max", "Mean", "SD", "Count")
  return(res)
}


mark_sign <- function(set)
{
  len <- length(set$CLOSE)
  sign <- c(NA, ifelse(diff(set$CLOSE, lag = 1, diffrence = 1) > 0, 1, -1))
  set$SIGN <- sign
  return(set)
}