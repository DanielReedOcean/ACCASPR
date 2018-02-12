####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Utility functions for ACCASPR project
# Date: Wed Feb 07 13:25:36 2018
####################################################################


#' Expand labels for variable groupings
#' @keywords internal
expand_var_names <- function(x){
  # Get first word of variable name
  word_1 <- sub("(^.*)[.](.*)", "\\1", x)

  # Get expanded version
  tmp <- c(zoo = "Zooplankton", phe = "Phenology", phy = "Physical")[word_1]

  # Add PC numbers
  tmp[!is.na(tmp)] <- paste(tmp[!is.na(tmp)],
                            sub("(^.*)[.](.*)", "\\2", x)[!is.na(tmp)])

  # Replace NAs with variable name with periods omitted
  tmp[is.na(tmp)] <- sub("[.]", " ", x)[is.na(tmp)]

  return(tmp)
}


#' Return correct symbol for given p-value
#' @keywords internal
get_pvalue_symbol <- function(p){
  ifelse(p > 0.05, "#",
         ifelse(p > 0.01, "*",
                ifelse(p > 0.001, "**",
                       ifelse(p <= 0.001, "***", NA))))
}


#' Create table of GAM data from given list
#' @keywords internal
get_table_formula <- function(x){
  # Get formula from GAM
  f <- deparse(summary(x)$formula, width.cutoff = 500L)

  # Get p-values from GAM
  p <- summary(x)$s.table[,4]

  # Choose symbol
  c(sub("(^[^)]+[)])$",
        paste("\\1",
              get_pvalue_symbol(p[1]), sep = ""), f),
    sub("(^[^)]+[)])([^)]+[)])$",
        paste("\\1",
              get_pvalue_symbol(p[1]),"\\2",
              get_pvalue_symbol(p[2]), sep = ""), f),
    sub("(^[^)]+[)])([^)]+[)])([^)]+[)])$",
        paste("\\1",
              get_pvalue_symbol(p[1]),"\\2",
              get_pvalue_symbol(p[2]),"\\3",
              get_pvalue_symbol(p[3]), sep = ""), f))[length(p)]
}
