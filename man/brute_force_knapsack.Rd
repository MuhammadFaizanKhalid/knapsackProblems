% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brute_force_knapsack.R
\name{brute_force_knapsack}
\alias{brute_force_knapsack}
\title{brute_force_knapsack}
\usage{
brute_force_knapsack(x, w, parallel = FALSE)
}
\arguments{
\item{x}{A DataFrame with two columns (weight(w) and value(v))}

\item{w}{Sack max weight capacity}

\item{parallel}{default = FALSE}
}
\value{
returns item with maximized value within knapsack weight limit
}
\description{
brute_force_knapsack
}
\references{
\url{https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm}
}
