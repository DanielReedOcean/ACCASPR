
<!-- README.md is generated from README.Rmd. Please edit that file -->
ACCASPR
=======

ACCASPR provides several helper functions for linking fish stock metrics (e.g., condition, abundance, recruitment) to indices of pelagic habitat quality using gams. The basic workflow for using these functions is outlined below.

Installation
------------

You can install ACCASPR from github with:

``` r
# install.packages("devtools")
devtools::install_github("DanielReedOcean/ACCASPR")
```

Workflow
--------

The basic workflow for using ACCASPR is as follows:

1.  Tidy data sets then determine predictors & response variable
2.  Run all possible GAMs with `run_gams`
3.  Remove models with excessive concurvity with `filter_gams`
4.  Rank models according to AIC with `rank_gams`
5.  View top models in table format with `tabulate_gams`
6.  Plot effects of each predictor on a model with `plot_gam_effect`
7.  Plot time series data, model fit, confidence intervals, & contribution of each predictor with `plot_gam_ts`
8.  Examine residuals, autocorrelation, & quantiles with `examine_gam_res`
