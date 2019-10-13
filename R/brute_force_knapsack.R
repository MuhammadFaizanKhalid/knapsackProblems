#'@title brute_force_knapsack
#'@param x A DataFrame with two columns (weight(w) and value(v))
#'@param w Sack max weight capacity
#'@param parallel default = FALSE
#'@return returns item with maximized value within knapsack weight limit
#'
#'@references
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm}
#'
#'@import parallel
#'@import combinat
#'@import microbenchmark
#'@export
brute_force_knapsack <- function(x , w, parallel = FALSE) {
  if (class(x) != "data.frame") {
    stop("x is not data frame")
  }
  else if (!(is.numeric(x$w) && is.numeric(x$v) && is.numeric(w))) {
    stopifnot(x$w < 0 && x$v < 0 && w < 0)
    stop("Weight, Value and max weight must be numbers")
  }
  itemsList <- c()
  systems <- c("Windows","linux","Darwin")
  if( parallel == TRUE ){
    if(Sys.info()["sysname"] %in% systems){
      cores = parallel::detectCores()
      wv.comb <- unlist(parallel::mclapply(1:nrow(x),
                                           function(k)
                                           {
                                             combinat::combn(rownames(x), m = k, simplify = FALSE, fun = as.numeric)
                                           },
                                           mc.cores = cores),
                        recursive = FALSE, use.names = FALSE)

      #sum the item result of each core
      set.values <- parallel::mclapply(comb, function(comb){
        ifelse(sum(x[comb,1]) <= W,
               sum(x[comb,2]),0
        )
      },
      mc.cores = cores)
    }
    else{
      stop("System info is not available")
    }
  }
  else{
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
  }
      largestIndex <- which.max(set.values)
      itemsList <- comb[[largestIndex]]
      return(list("value" = c(set.values[[largestIndex]]),"elements" = c(itemsList)))
}
