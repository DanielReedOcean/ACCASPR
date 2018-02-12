####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Fit GAMs to fish stock metrics
# Date: Wed Feb 07 13:37:04 2018
####################################################################

#' Determine all possible combinations of terms for a GAM up to 3 terms
#' @keywords internal
get_all_RHS <- function(vars, max_n){
  lapply(1:min(max_n, length(vars)), vars = vars, FUN = function(x, vars) t(combn(vars, x)))
}


#' Create right-hand side of GAM formula by combining terms
#' @keywords internal
combine_terms <- function(vars, kval){
  ifelse(length(vars)>1,
         paste(paste("s(", vars,", k = ", kval, ")", sep = ""), collapse = " + "),
         paste("s(", vars,", k = ", kval, ")", sep = ""))
}


#' Function to create GAM formulas
#' Gets all possible combinations of predictors, then combines them with the response variable
#' Next, these are converted into \code{formulas}.
#' @keywords internal
create_formulas <- function(response, vars, kval, max_n) {
  # Get all possible combinations of predictors
  all_combs <- get_all_RHS(vars, max_n)

  # Combine predictors into a single string
  all_RHS <- unlist(lapply(all_combs, FUN = function(mat) apply(mat, MAR = 1, FUN = combine_terms, kval = kval)))

  # Combine left- and right-hand sides into formula
  lapply(all_RHS, function(x) reformulate(response = response, termlabels = as.character(x)))
}


#' Run GAM with given predictors & response variable on given data
#'
#' Generalised additive models describing the response variable, \code{response}, are constructed using all
#' possible combinations of the given predictors, \code{preds}. These models are fit using the given data frame,
#' \code{data}, and contain up to and including \code{max_n} terms. These are smoothed terms with \code{k} knots.
#'
#' @param preds Names of the predictors (vector of strings)
#' @param response Name of response variable (string)
#' @param kval Number of knots in the smoothers (numeric)
#' @param max_n Maximum number of terms in each GAM (numeric)
#' @param data Data set that includes the predictors and response variable (data frame)
#' @return List of \link[mgcv]{gam}s
#' @export
run_gams <- function(preds, response, kval, max_n, data){
  # List of formulas
  formula_list <- create_formulas(response, preds, kval, max_n)

  # Run GAM
  lapply(formula_list, function(x) mgcv::gam(x, data = data))
}


#' Filter out GAMs with excessive concurvity
#'
#' A list of \link[mgcv]{gam}s is filtered to remove those with a concurvity greater than \code{max_conc}.
#' Concurvity is estimated using \code{estimate} values calculated by \link[mgcv]{concurvity}. If any terms
#' exhibit concurvity greater than the threshold, the model is rejected.
#'
#' @param gam_list List of \link[mgcv]{gam}s
#' @param max_conc Maximum concurvity. All \link[mgcv]{gam}s with \link[mgcv]{concurvity} above this are removed
#' @return List of \link[mgcv]{gam}s
#' @export
filter_gams <- function(gam_list, max_conc){
  gam_list[max_conc >= lapply(gam_list, function(x) max(mgcv::concurvity(x)[3,]))]
}


#' Extract top \code{n} GAMs from list according to AIC
#'
#' A list of \link[mgcv]{gam}s are sorted according to \link[stats]{AIC} value.
#' \code{n} models with the lowest AIC are returned.
#'
#' @param gam_list List of \link[mgcv]{gam}s
#' @param n Number of \link[mgcv]{gam}s to return
#' @return List of \link[mgcv]{gam}s
#' @export
rank_gams <- function(gam_list, n){
  gam_list[order(unlist(lapply(gam_list, AIC)))][1:n]
}
