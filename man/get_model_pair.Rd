% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear.tools.R
\name{get_model_pair}
\alias{get_model_pair}
\title{get a list of model vars with their corresponding coeff vars or raw vars.}
\usage{
get_model_pair(model, data = NULL, pair_with = c("coeff", "raw"))
}
\arguments{
\item{model}{a lm or glm output}

\item{data}{NULL (default) or data.frame, a new dataset to evaluate the categorical variables.
If NULL, then use the data used in model itself.}

\item{pair_with}{either 'raw' (default) or 'coeff', to decide the elements of list are raw vars or coeff vars.
See \code{\link{get_x}} for the meaning of model var, coeff var and raw var.}
}
\value{
a list with names as model vars and elements as their corresponding coeff/raw vars
}
\description{
get a list of model vars with their corresponding coeff vars or raw vars.
}
\details{
get a list of model vars with their corresponding coeff vars or raw vars.
See \code{\link{get_x}} for the meaning of model var, coeff var and raw var.
}
\examples{

# return coeff
get_model_pair(model = price~  I(carat^2) + cut  + carat*table, data = ggplot2::diamonds)
# return raw vars
get_model_pair(price~  I(carat^2) + cut  + carat*table, data= ggplot2::diamonds, pair_with = 'raw')

# correctly deal with irregular formulas
model_dirty = lm(price~  I(carat^   2) + cut  - carat:table - cut ,ggplot2::diamonds)
get_model_pair(model_dirty,pair_with = 'raw')

}

