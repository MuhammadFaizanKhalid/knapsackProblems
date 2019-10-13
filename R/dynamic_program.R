#'@title Dynamic_program
#'@param x A DataFrame with two columns (weight(w) and value(v))
#'@param w Sack maximum weight capacity
#'
#'@return rreturns item with maximized value within knapsack weight limit
#'
#'@references
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm}
#'
#'@export

dynamic_program <- function(x, w){
  if (class(x) != "data.frame") {
    stop("x is not data frame")
  }
  else if (!(is.numeric(x$w) && is.numeric(x$v) && is.numeric(w))) {
    stop("Weight, Value and max weight must be numbers")
  }
  valmatrix <- matrix(0, nrow = (nrow(x) + 1), ncol =  (w + 1)  )
  numrows = nrow(x) + 1
  wtnums = w+1
  for(i in 2: numrows)
  {
    for(j in 1: wtnums )
    {
      if( x[i-1,1] >= j )
        valmatrix[i,j] = valmatrix[i-1, j]
      else
        valmatrix[i,j] = max(valmatrix[i - 1 ,j] , valmatrix[i - 1 , j - x[i-1,1]] + x[i-1,2] )
    }
  }

  getMax <- valmatrix[numrows,wtnums]
  i <- wtnums
  j <- numrows
  selectedvals <- rep(FALSE,(numrows))
  while( numrows > 1)
  {
    if(getMax > valmatrix[j,i])
    {
      values <- (i - x[j,1])
      if (values > 0) {
        if( (valmatrix[j,values] + x[j,2]) == valmatrix[j+1,i])
        {
          selectedvals[j] <- TRUE
          i <- values
        }
      }
      else
        break
    }
    j <- j - 1
  }

  items <- which(selectedvals)
  return(list( "values" = round(max(valmatrix)) , "elements" = items))
}
