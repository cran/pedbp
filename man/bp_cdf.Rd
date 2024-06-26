% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bp_cdf_plot.R
\name{bp_cdf}
\alias{bp_cdf}
\alias{bp_cdf.pedbp_bp}
\alias{bp_cdf.pedbp_p_bp}
\alias{bp_cdf.pedbp_q_bp}
\alias{bp_cdf.default}
\title{Plot the CDF for Blood Pressure}
\usage{
bp_cdf(...)

\method{bp_cdf}{pedbp_bp}(x, ...)

\method{bp_cdf}{pedbp_p_bp}(x, ...)

\method{bp_cdf}{pedbp_q_bp}(x, ...)

\method{bp_cdf}{default}(
  age,
  male,
  height = NA,
  height_percentile = 0.5,
  source = getOption("pedbp_bp_source", "martin2022"),
  sbp = NA,
  dbp = NA,
  ...
)
}
\arguments{
\item{...}{not currently used}

\item{x}{a \code{pedbp_bp} object created by \code{\link{q_bp}} or
\code{\link{p_bp}}.}

\item{age}{numeric age, in months}

\item{male}{integer value, 1 = male, 0 = female}

\item{height}{numeric, in centimeters, can be missing. See Details.}

\item{height_percentile}{height percentile to use; range from [0, 100]. See Details.}

\item{source}{the method, or data set, to use as the reference.  See Details.}

\item{sbp}{the observed systolic blood pressure}

\item{dbp}{the observed diastolic blood pressure}
}
\value{
When passing in a \code{pedbp_bp} object, the return is a list of
\code{ggplot} objects.  The length of the list is equal to the length of the
number of quantiles or percentiles within the \code{pedbp_bp} object.

When using the default method the return is just
a \code{ggplot} object showing the CDF for diastolic and systolic
blood pressures with vertical and horizontal lines highlight the percentile
for the given inputs.
}
\description{
Plot the CDF for blood pressure given age, sex, and height.
}
\examples{

# Explicity defining the inputs
bp_cdf(age = 96, male = 1, sbp = 103, dbp = 55)

# Plotting two cdfs from a call to p_bp
x <- p_bp(q_sbp = rep(100, 2),
          q_dbp = rep( 60, 2),
          age   = rep(35.75, 2),
          male  = c(0, 0),
          height = c(NA, 100))

bp_cdf(x)

# Plotting a cdf from a call to q_bp
x <- q_bp(p_sbp = 0.85, p_dbp = 0.95,
          age = 29.2, male = 0, height_percentile = 0.95,
          source = "flynn2017")

bp_cdf(x)

}
