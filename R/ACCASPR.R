#' Link pelagic habitat quality to fish stock metrics using generalised additive models
#'
#' This package provides several helper functions for linking fish stock metrics (e.g., condition, abundance, recruitment) to
#' indices of pelagic habitat quality using \link[mgcv]{gam}s. The basic workflow for using these functions is outlined below.
#'
#' @section Workflow:
#' \enumerate{
#'  \item{Tidy data sets then determine predictors & response variable}
#'  \item{Run all possible GAMs with \code{\link{run_gams}}}
#'  \item{Remove models with excessive concurvity with \code{\link{filter_gams}}}
#'  \item{Rank models according to AIC with \code{\link{rank_gams}}}
#'  \item{View top models in table format with \code{\link{tabulate_gams}}}
#'  \item{Plot effects of each predictor on a model with \code{\link{plot_gam_effect}}}
#'  \item{Plot time series data, model fit, confidence intervals, & contribution of each predictor with \code{\link{plot_gam_ts}}}
#'  \item{Examine residuals, autocorrelation, & quantiles with \code{\link{examine_gam_res}}}
#' }
#'
#'
"_PACKAGE"
#> [1] "_PACKAGE"
