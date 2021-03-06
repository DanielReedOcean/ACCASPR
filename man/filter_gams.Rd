% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_models.R
\name{filter_gams}
\alias{filter_gams}
\title{Filter out GAMs with excessive concurvity}
\usage{
filter_gams(gam_list, max_conc)
}
\arguments{
\item{gam_list}{List of \link[mgcv]{gam}s}

\item{max_conc}{Maximum concurvity. All \link[mgcv]{gam}s with \link[mgcv]{concurvity} above this are removed}
}
\value{
List of \link[mgcv]{gam}s
}
\description{
A list of \link[mgcv]{gam}s is filtered to remove those with a concurvity greater than \code{max_conc}.
Concurvity is estimated using \code{estimate} values calculated by \link[mgcv]{concurvity}. If any terms
exhibit concurvity greater than the threshold, the model is rejected.
}
