read_yahoo <- function(file) {
  red_data <- read.csv(file, stringsAsFactors = FALSE, sep = ",", dec = ".",
                       colClasses = c(Date="Date",Close="character"))
  red_data <- red_data[c("Date", "Close")]
  red_data$Close <- as.numeric(red_data$Close)
  names(red_data) <- c("DATE", "CLOSE")
  return(na.omit(red_data))
}

read_yahoo_full <- function(file) {
  red_data <- read.csv(file, stringsAsFactors = FALSE, sep = ",", dec = ".",
                       colClasses = c(Date="Date",Close="character",Open="character"))
  red_data <- red_data[c("Date", "Close", "Open")]
  red_data$Close <- as.numeric(red_data$Close)
  red_data$Open <- as.numeric(red_data$Open)
  names(red_data) <- c("DATE", "CLOSE", "OPEN")
  return(na.omit(red_data))
}

read_yahoo_full2 <- function(file)
{
  red_data <- read.csv(file, stringsAsFactors = FALSE, sep = ",", dec = ".",
                       colClasses = c(Date="Date",Close="numeric",Open="numeric",Low="numeric",High="numeric"))
  names(red_data) <- toupper(names(red_data))
  return(na.omit(red_data))
}

read_rbc <- function(file){
  data <- read.csv(file, stringsAsFactors = FALSE, sep = ",", dec = ".")
  data$DATE <- as.Date(data$DATE)
  
  return(na.omit(data[c("DATE", "CLOSE")]))
}