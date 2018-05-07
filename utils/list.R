lappend <- function(list, element){
  list[[length(list) + 1]] <- element
  return(list)
}

list.decart <- function(params)
{
  len <- length(params)
  
  if (len == 0) {
    return(list())
  } else if (len == 1) {
    res <- lapply(params[[1]], function(param) {
      param.as.list <- list(param)
      names(param.as.list) <- names(params[1])
      return(param.as.list)
    })
    return(res)
  }
  
  accum <- list()
  current.params <- params[[1]]
  current.name <- names(params[1])
  rests <- list.decart(params[-1])
  for (param.value in current.params){
    for (rest in rests){
      rest[[current.name]] <- param.value
      accum <- lappend(accum, rest)
    }
  }
  return(accum)
}