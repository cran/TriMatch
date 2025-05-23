% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.balance.R
\name{balance.plot}
\alias{balance.plot}
\title{Balance plot for the given covariate.}
\usage{
balance.plot(
  x,
  covar,
  model,
  nstrata = attr(attr(tmatch, "triangle.psa"), "nstrata"),
  label = "Covariate",
  ylab = "",
  xlab = NULL,
  se.ratio = 2,
  print = TRUE,
  legend.position = "top",
  x.axis.labels,
  x.axis.angle = -45,
  ...
)
}
\arguments{
\item{x}{results from [trimatch()].}

\item{covar}{vector of the covariate to check balance of.}

\item{model}{an integer between 1 and 3 indicating from which model the 
propensity scores will be used.}

\item{nstrata}{number of strata to use.}

\item{label}{label for the legend.}

\item{ylab}{label of the y-axis.}

\item{xlab}{label of the x-axis.}

\item{se.ratio}{a multiplier for how large standard error bars will be.}

\item{print}{print the output if the Freidman Rank Sum Test and repeated
measures ANOVA (for continuous variables).}

\item{legend.position}{the position of the legend.}

\item{x.axis.labels}{labels for the x-axis.}

\item{x.axis.angle}{angle for x-axis labels.}

\item{...}{parameters passed to [plot.balance.plots()].}
}
\value{
a \code{ggplot2} figure or a list of \code{ggplot2} figures if \code{covar}
       is a data frame.
}
\description{
If the covariate is numeric, boxplots will be drawn with red points for the mean
and green error bars for the standard error. For non-numeric covariates a barplot
will be drawn.
}
\details{
A Friedman rank sum test will be performed for all covariate types, printed,
and stored as an attribute to the returned object named \code{friedman}. If
a continuous covariate a repeated measures ANOVA will also be performed, printed,
and returned as an attribute named \code{rmanova}.
}
