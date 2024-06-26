% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{cppPGSF}
\alias{cppPGSF}
\title{Pediatric Growth Standards}
\usage{
cppPGSF(metric, source, male, x, qp, type)
}
\arguments{
\item{metric}{string, for example bmi_for_age}

\item{source}{string, CDC or WHO}

\item{male}{integer, 0 = female; 1 = male}

\item{x}{is the age (in months), length (cm) or height (cm) as needed for
the metric.}

\item{qp}{the quantile or percentile, whichever is relevant for the type}

\item{type}{quantile, distribution, or zscore}
}
\description{
Pediatric growth standard based on LMS data from the CDC and WHO.
}
\details{
expect to call this from R after checking some functional
arguments within R.
}
