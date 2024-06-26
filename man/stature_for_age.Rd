% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stature_for_age.R
\name{p_height_for_age}
\alias{p_height_for_age}
\alias{height_for_age}
\alias{length_for_age}
\alias{q_height_for_age}
\alias{z_height_for_age}
\alias{p_length_for_age}
\alias{q_length_for_age}
\alias{z_length_for_age}
\title{Stature for Age - Pediatric Growth Standard}
\usage{
p_height_for_age(
  q,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

q_height_for_age(
  p,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

z_height_for_age(
  q,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

p_length_for_age(
  q,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

q_length_for_age(
  p,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

z_length_for_age(
  q,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)
}
\arguments{
\item{q}{a vector of quantiles}

\item{male}{integer value, 1 = male, 0 = female}

\item{age}{numeric age, in months}

\item{source}{a character string denoting the data source providing the
parameters needed for the estimate.  Valid values are "CDC" and "WHO".  This
can be set explicitly, or by using the \code{pedbp_pgs_source} option.}

\item{...}{pass through}

\item{p}{a vector of probabilities}
}
\value{
\code{p_} method return values from the estimated distribution function.

\code{q_} methods return values from the estimated quantile function.

\code{z_} methods return standard scores.
}
\description{
Stature for age quantile, distribution, and zscore function based on LMS data
from the CDC and WHO.
}
\section{Notes}{
 CDC Recommends using WHO growth charts for infants and
children ages 0 to 2 years of age in the U.S. and CDC growth charts to
monitor growth for children age 2 years and older in the U.S.
}

\examples{

# Find the first quartile height for a 66 month old female.
# The quantile based on CDC data is slightly less than the quantile based on
# the data from the WHO
q_height_for_age(p = 0.25, age = 66, male = 0, source = c("CDC", "WHO"))

# The 90th quantile length/height for a 24 month female: note that these
# values are similar, but not identical
q_length_for_age(p = 0.9, age = 24, male = 0, source = c("CDC"))
q_height_for_age(p = 0.9, age = 24, male = 0, source = c("CDC"))

# Find the percentile for a 28 month old male with a stature (height/length)
# of 88 cm
p_height_for_age(q = 88, male = 1, age = 28, source = "CDC")
p_height_for_age(q = 88, male = 1, age = 28, source = "WHO")
p_length_for_age(q = 88, male = 1, age = 28, source = "CDC")
p_length_for_age(q = 88, male = 1, age = 28, source = "WHO")

# correseponding standard scores
z_height_for_age(q = 88, male = 1, age = 28, source = "CDC")
z_height_for_age(q = 88, male = 1, age = 28, source = "WHO")
z_length_for_age(q = 88, male = 1, age = 28, source = "CDC")
z_length_for_age(q = 88, male = 1, age = 28, source = "WHO")

}
\references{
\url{https://www.cdc.gov/growthcharts/percentile_data_files.htm},
\url{https://www.who.int/tools/child-growth-standards/standards}
}
\seealso{
\itemize{
\item Distribution functions:
  \itemize{
    \item \code{\link{bmi_for_age}}
    \item \code{\link{head_circumference_for_age}}
    \item \code{\link{height_for_age}}
    \item \code{\link{length_for_age}}
    \item \code{\link{weight_for_age}}
    \item \code{\link{weight_for_length}}
    \item \code{\link{weight_for_height}}
  }
\item Plotting functions:
  \itemize{
    \item \code{\link{gs_chart}}
    \item \code{\link{gs_cdf}}
  }
\item Vignette:
  \itemize{
    \item \code{vignette(topic = "growth-standards", package = "pedbp")}
  }
}
}
