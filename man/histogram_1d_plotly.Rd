% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_aggregates.R
\name{histogram_1d_plotly}
\alias{histogram_1d_plotly}
\title{Plot multi-trace 1-d histograms with plotly.}
\usage{
histogram_1d_plotly(
  df,
  var,
  groupby_var = NULL,
  plot_type = c("histogram"),
  breaks = "FD",
  density = TRUE,
  orientation = c("v", "h"),
  margin = 0.02,
  return_type = c("subplot", "list")
)
}
\arguments{
\item{df}{A data.frame.}

\item{var}{A character or (named) charactor vector. Character of the
columns to be used for plotting histograms.}

\item{groupby_var}{A character, or character vector. Character of the
column(s) to be used for grouping.}

\item{plot_type}{for future use, currently "histogram".}

\item{breaks}{breaks from graphics::hist.}

\item{density}{TRUE for density, FALSE for counts.}

\item{orientation}{"v" or "h", for "vertical" and "horizontal". v (vertical)
is the common x/y arrangement e.g. top edge, h (horizontal) is the uncommon
 x/y arrangement e.g. right edge. Not currently used.}

\item{margin}{float. margin argument in subplot call.}

\item{return_type}{"subplot" or "list". return_type should be subplot or list
of plots.}
}
\value{
A plotly (subplot) object or a list of plotly objects depending on
  return_type. The subplot is ncol = length(in_var), nrow = length(de_var)
}
\description{
\code{histogram_1d_plolty} plots multi-trace 1-d histograms with plotly.
}
\details{
This is a function to plot multi-trace 1-d histograms using plotly package.
Multiple variables can be plot, and organized as subplots. Groupby
variable(s) can be controlled. The plots can be both vertical
(default) and horizontal orientations. It can return either subplots or a
list of plotly objects.
}
\examples{
\dontrun{
histogram_1d_plotly(df,
  var = c("column1", "column2"),
  groupby_var = c("group_column1", "group_column2"),
  breaks = "FD",
  density = TRUE,
  margin = 0.02,
  orientation = "v", return_type = "subplot")
}

}
