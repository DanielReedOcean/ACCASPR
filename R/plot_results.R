####################################################################
# Author: Dan Reed (Daniel.Reed@dfo-mpo.gc.ca)
# Purpose: Plotting & tabulating GAM results
# Date: Wed Feb 07 14:50:36 2018
####################################################################

#' Create a data frame for a table
#'
#' Output a list of \link[mgcv]{gam}s in a \link[knitr]{kable} format showing model formula,
#' AIC, percent deviance explained, and GCV.
#'
#' @param gam_list List of \link[mgcv]{gam}s to be tabulated
#' @return \link[knitr]{kable} table of \link[mgcv]{gam}s
#' @export
tabulate_gams <- function(gam_list){
  # Repackage GAM data
  df <- data.frame(formula = unlist(lapply(gam_list, get_table_formula)),
                   AIC = unlist(lapply(gam_list, AIC)),
                   GCV = unlist(lapply(gam_list, function(x)x$gcv.ubre)),
                   dev.expl = round(unlist(lapply(gam_list, function(x)summary(x)$dev.expl)) * 100, 1))

  # Create a table
  print(knitr::kable(df, col.names = c("Formula", "AIC", "GCV", "Dev. explained (%)")))
}


#' Plot effect of predictor for a GAM
#'
#' Produce a partial residual plot for each predictor of a \link[mgcv]{gam}. Plots are
#' on a 2\eqn{\times}{x}2 grid. Ribbons show the 95\% confidence intervals.
#'
#' @param g A \link[mgcv]{gam}
#' @param ylabel Label for the y axis (i.e., the name of the reponse variable)
#' @return NULL
#' @export
plot_gam_effect <- function(g, ylabel){
  # Loop counter
  loop_n <- 1

  # Get fit and SEs
  gam_df <- mgcv::plot.gam(g, select = 0, seWithMean = TRUE)

  # Reshuffle list depending on p value
  gam_df <- gam_df[order(summary(g)$s.table[,4])]

  # Container for plots
  p <- vector("list", length = length(gam_df))

  # Loop through predictors
  for(i in gam_df){
    # Expand predictor name for plotting
    plot_lab_x <- expand_var_names(i$xlab) # Just remove period

    # Data frame for partial residuals
    res_df <- data.frame(i$raw, mgcv::residuals.gam(g, type = "working") +
                           mgcv::predict.gam(g, type = "terms")[,order(summary(g)$s.table[,4])[loop_n]])
    colnames(res_df) <- c("pc", "res")

    # Data frame for fit and SEs
    fit_df <- data.frame(pc = i$x, fit = i$fit, se = i$se)

    # Create figure
    p[[loop_n]] <- ggplot2::ggplot(data = fit_df)
    p[[loop_n]] <- p[[loop_n]] + ggplot2::geom_ribbon(ggplot2::aes(x = pc, ymin = fit - se, ymax = fit + se), fill = "darkgray", alpha = 0.25)
    p[[loop_n]] <- p[[loop_n]] + ggplot2::geom_line(ggplot2::aes(y = fit, x = pc), colour = "salmon", size = 1)
    p[[loop_n]] <- p[[loop_n]] + ggplot2::geom_point(data = res_df, ggplot2::aes(x = pc, y = res), size = 1, colour = "black", shape = 3)
    p[[loop_n]] <- p[[loop_n]] + ggplot2::xlab(plot_lab_x)
    p[[loop_n]] <- p[[loop_n]] + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                panel.grid.minor = ggplot2::element_blank(),
                                                panel.background = ggplot2::element_blank(),
                                                axis.line = ggplot2::element_line(colour = "black", size = 1),
                                                axis.text = ggplot2::element_text(size = 18, color = "black"),
                                                axis.title = ggplot2::element_text(size = 18, color = "black"),
                                                axis.ticks = ggplot2::element_line(size = 1, color = "black"),
                                                text = ggplot2::element_text(family = "serif"))
    p[[loop_n]] <- p[[loop_n]] + ggplot2::ylab(ifelse(loop_n == 1 | loop_n == 3, ylabel, ""))
    p[[loop_n]] <- p[[loop_n]] + ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
    p[[loop_n]] <- p[[loop_n]] + ggplot2::scale_x_continuous(minor_breaks = res_df$pc)

    # Increment loop counter
    loop_n <- loop_n + 1
  }

  # Adjust y limits
  y_min <- y_max <- 0
  for(i in 1:length(gam_df)) {
    y_min <- min(y_min, ggplot2::ggplot_build(p[[i]])$layout$panel_scales_y[[1]]$get_limits()[1])
    y_max <- max(y_max, ggplot2::ggplot_build(p[[i]])$layout$panel_scales_y[[1]]$get_limits()[2])
  }
  p <- lapply(p, function(x) x + ggplot2::ylim(y_min, y_max))

  # Plot figures and title
  cowplot::plot_grid(plotlist = p, ncol=2, nrow = 2,
                     labels = paste("(", letters[1:4], ")", sep = ""),
                     label_x = 0.9, label_y = 0.225)
}


#' Plot time series of a GAM
#'
#' Plots figures to examine model results and observations over time, as well as the contribution of each predictor to the model.
#' Two figures are produced:
#' \enumerate{
#'   \item{Contributions of each predictor to the model over time}
#'   \item{Time series of observations & model fit with 95\% confidence interval}
#' }
#' @param g A \link[mgcv]{gam}
#' @param data Data frame used to produce \code{g}
#' @param response Name of the response variable
#' @export
plot_gam_ts <- function(g, data, response){
  # Create data frame of years, predictions (+ standard errors), and observations contribution of each PC, and overall model behaviour
  time.series <- data.frame(year = dplyr::right_join(data, g$model)$year,
                            mgcv::predict.gam(g, type = "terms")[, order(summary(g)$s.table[,4])],
                            obs = g$model[,1],
                            model = mgcv::predict.gam(g, type = "response"),
                            SE = mgcv::predict.gam(g, type = "response", se.fit = TRUE)$se.fit)

  # Get predictor variables and reorder according to significance
  i <- attributes(g$terms)$term.labels[order(summary(g)$s.table[, 4])]

  # Expand predictor name for plotting
  leg.labs <- expand_var_names(i)

  # Change name of predictors
  colnames(time.series)[2:(1+length(leg.labs))] <- leg.labs

  # Data frame for plotting
  df <- time.series %>%
    dplyr::select(-((ncol(time.series)-2):ncol(time.series))) %>%
    tidyr::gather(variable, value, -year) %>%
    dplyr::mutate(variable = factor(variable, levels = leg.labs))

  # Create figure
  p1 <- ggplot2::ggplot(data = df)
  p1 <- p1 + ggplot2::geom_bar(ggplot2::aes(x =year, y = value, fill = variable), stat = "identity")
  p1 <- p1 + ggplot2::ylab("Contribution of each predictor") + ggplot2::xlim(range(time.series$year) + c(-1,1))
  p1 <- p1 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                            panel.grid.minor = ggplot2::element_blank(),
                            panel.background = ggplot2::element_blank(),
                            axis.line = ggplot2::element_line(colour = "black", size = 1),
                            axis.text = ggplot2::element_text(size = 18, color = "black"),
                            axis.title = ggplot2::element_text(size = 20, color = "black"),
                            axis.ticks = ggplot2::element_line(size = 1, color = "black"),
                            text = ggplot2::element_text(family = "serif"),
                            legend.position = "top",
                            legend.box = "horizontal",
                            legend.text = ggplot2::element_text(size = 18, color = "black"))
  p1 <- p1 + ggplot2::xlab("") + ggplot2::guides(fill = ggplot2::guide_legend(title = ""))
  p1 <- p1 + ggplot2::scale_fill_manual(values = c("salmon","gray","steelblue3"))

  # Create time series figure
  p2 <- ggplot2::ggplot(data = time.series, ggplot2::aes(x = year))
  p2 <- p2 + ggplot2::geom_ribbon(ggplot2::aes(ymin = model - (1.96 * SE), ymax =  model + (1.96 * SE)), fill = "darkgray", alpha = 0.25)
  p2 <- p2 + ggplot2::geom_line(ggplot2::aes(y = model), colour = "salmon", size = 1)
  p2 <- p2 + ggplot2::geom_point(ggplot2::aes(y = obs), size = 1, colour = "black", shape = 3)
  p2 <- p2 + ggplot2::xlab("Year") + ggplot2::xlim(range(time.series$year)+c(-1,1))
  p2 <- p2 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                            panel.grid.minor = ggplot2::element_blank(),
                            panel.background = ggplot2::element_blank(),
                            axis.line = ggplot2::element_line(colour = "black", size = 1),
                            axis.text = ggplot2::element_text(size = 18, color = "black"),
                            axis.title = ggplot2::element_text(size = 20, color = "black"),
                            axis.ticks = ggplot2::element_line(size = 1, color = "black"),
                            text = ggplot2::element_text(family = "serif"))
  p2 <- p2 + ggplot2::ylab(response)

  # Plot figures
  cowplot::plot_grid(p1, p2, ncol = 1, align = "v",
                     labels = paste("(", letters[1:2], ")" , sep = ""),
                     label_x = 0.9, label_y = 0.275)
}


#' Plot diagnostics
#'
#' \code{examine_gam_res} produces diagnostic plots used to assess the validity of model assumptions.
#' Three figures are produced that show the following:
#' \enumerate{
#'   \item{Theoretical & observed quantiles}
#'   \item{Autocorrelation}
#'   \item{Residuals over time}
#' }
#' @param g A \link[mgcv]{gam}
#' @param data Data frame used to produce \code{g}
#' @export
examine_gam_res <- function(g, data){
  # Produce Q-Q data frame, but suppress plot
  res <- as.data.frame(stats::qqnorm(mgcv::residuals.gam(g), plot.it = FALSE))

  # Calculate Q-Q line in accordance with method used by qqline of the stats package
  y <- stats::quantile(mgcv::residuals.gam(g), c(0.25, 0.75))
  x <- stats::qnorm(c(0.25,0.75))
  slope <- diff(y)/diff(x)
  int <- y[1] - slope * x[1]

  # Create Q-Q plot with QQ line
  p <- ggplot2::ggplot(data = res, ggplot2::aes(x = x, y =y))
  p <- p + ggplot2::geom_abline(intercept = int, slope = slope, size = 1.25, colour = "black")
  p <- p + ggplot2::geom_point(size = 5, colour = "darkgray")
  p <- p + ggplot2::xlab("Theoretical quantiles") + ggplot2::ylab("Sample quantiles")
  p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.background = ggplot2::element_blank(),
                          axis.line = ggplot2::element_line(colour = "black", size = 1),
                          axis.text = ggplot2::element_text(size = 18, color = "black"),
                          axis.title = ggplot2::element_text(size = 20, color = "black"),
                          axis.ticks = ggplot2::element_line(size = 1, color = "black"),
                          text = ggplot2::element_text(family = "serif"))

  # Create data frame of years and residuals
  year.res <- data.frame(year = dplyr::right_join(data, g$model)$year, res = mgcv::residuals.gam(g))

  # Fit a linear regression with an autocorrelation structure to quantify autocorrelation
  gls.fit <- nlme::gls(res~year, data = year.res, correlation = nlme::corAR1())

  # Get correlation coefficient
  phi <- paste(c("[", paste(round(nlme::intervals(gls.fit)$corStruct[c(1, 3)], 2), collapse = ", "), "]"), collapse = "")

  # Fit linear regression to quantify trend in residuals
  lm.fit <- stats::lm(res~year, data = year.res)

  #Get confidence interval
  lm.CI <- paste(c("[", paste(round(confint(lm.fit)[2,], 2), collapse = ", "), "]"), collapse = "")

  # Plot time series of residuals
  p1 <- ggplot2::ggplot(year.res) + ggplot2::geom_point(ggplot2::aes(x = year, y = res), size = 5, colour = "darkgray")
  p1 <- p1 + ggplot2::geom_smooth(ggplot2::aes(x = year, y = res), method = "lm", colour  =  "salmon", alpha = 0.3)
  p1 <- p1 + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 1.25)
  p1 <- p1 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                            panel.grid.minor = ggplot2::element_blank(),
                            panel.background = ggplot2::element_blank(),
                            axis.line = ggplot2::element_line(colour = "black", size = 1),
                            axis.text = ggplot2::element_text(size = 18, color = "black"),
                            axis.title = ggplot2::element_text(size = 20, color = "black"),
                            axis.ticks = ggplot2::element_line(size = 1, color = "black"),
                            text = ggplot2::element_text(family = "serif"))
  p1 <- p1 + ggplot2::xlab("Year") + ggplot2::ylab("Residuals")

  # Use ACF to look for temporal autocorrelation
  acf.res <- stats::acf(residuals(g), plot = FALSE)

  # Repackage results for plotting
  acf.res <- data.frame(lag = acf.res$lag, acf = acf.res$acf)

  # Plot time series of residuals
  p2 <- ggplot2::ggplot(acf.res) + ggplot2::geom_bar(ggplot2::aes(x = lag, y = acf), stat = 'identity', width = 0.3, fill = "darkgray")
  p2 <- p2 + ggplot2::geom_hline(yintercept = 0)
  p2 <- p2 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                            panel.grid.minor = ggplot2::element_blank(),
                            panel.background = ggplot2::element_blank(),
                            axis.line = ggplot2::element_line(colour = "black", size = 1),
                            axis.text = ggplot2::element_text(size = 18, color = "black"),
                            axis.title = ggplot2::element_text(size = 20, color = "black"),
                            axis.ticks = ggplot2::element_line(size = 1, color = "black"),
                            text = ggplot2::element_text(family = "serif"))
  p2 <- p2 + ggplot2::xlab("Lag") + ggplot2::ylab("Autocorrelation")

  # Plot correlation coefficient
  p3 <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::annotate("text", x=0.5, y = 0.6,
                                                                      label = paste("95% CI for \u03D5 = ", phi), size = 8)
  p3 <- p3 + ggplot2::annotate("text", x=0.5, y = 0.4, label = paste("95% CI for slope = ", lm.CI), size = 8)
  p3 <- p3 + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::xlim(c(0,1)) + ggplot2::ylim(c(0,1))
  p3 <- p3 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                            panel.grid.minor = ggplot2::element_blank(),
                             panel.background = ggplot2::element_blank(),
                             axis.line.x = ggplot2::element_blank(),
                             axis.text.x = ggplot2::element_blank(),
                             axis.title.x = ggplot2::element_blank(),
                             axis.ticks.x = ggplot2::element_blank(),
                             axis.line.y = ggplot2::element_blank(),
                             axis.text.y = ggplot2::element_blank(),
                             axis.title.y = ggplot2::element_blank(),
                             axis.ticks.y = ggplot2::element_blank())

  # Plot results
  cowplot::plot_grid(p, p1, p2, p3, ncol = 2, align = "hv",
            label_x = 0.9, label_y = 0.225,
            labels = paste("(", letters[1:3], ")" , sep = ""))
}
