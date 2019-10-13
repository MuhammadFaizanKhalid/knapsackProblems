#'@title greedy_knapsack
#'@param x A DataFrame with two columns (weight(w) and value(v))
#'@param w Sack max weight capacity
#'
#'@return returns item with maximized value within knapsack weight limit
#'
#'@references
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm}
#'
#'@export

greedy_knapsack <- function(x,w){
  if (class(x) != "data.frame") {
    stop("x is not data frame")
  }
  else if (!(is.numeric(x$w) && is.numeric(x$v) && is.numeric(w))) {
    stop("Weight, Value and max weight must be numbers")
  }
  x$wtbyval <- round(x$v/x$w)
  x <- x[order(x$wtbyval,decreasing = TRUE),]
  weight <- 0
  items <- 0
  i <- 1
  totvals <- 0
  while(weight+x$w[i] <= w){
    items[i] <- list(c(x$w[i],x$v[i]))
    totvals <- totvals+ x$v[i]
    weight <- weight + x$w[i]
    i <- i+1
  }
  return(list("value"= totvals,"elements"= items))
}
