brute_force_knapsack <- function(x , w) {
  if (class(x) != "data.frame") {
    stop("x is not data frame")
  }
  else if (!(is.numeric(x$w) && is.numeric(x$v) && is.numeric(w))) {
    stopifnot(x$w < 0 && x$v < 0 && w < 0)
    stop("Weight, Value and max weight must be numbers")
  }
  comb <- unlist(lapply(1:nrow(x), function(iterations1) {
    combinat::combn(rownames(x),
                    m = iterations1,
                    simplify = FALSE,
                    fun = as.numeric)
  }),
  use.names = FALSE,
  recursive = FALSE)
  set.values <-  lapply(comb, function(comb) {
    if (sum(x[comb, 1]) <= w) {
      sum(x[comb, 2])
    }else{ 0 }
  })
 largestIndex <- which.max(set.values)
 itemsList <- comb[[largestIndex]]
 return(list("value" = c(set.values[[largestIndex]]),"elements" = c(itemsList)))
}
