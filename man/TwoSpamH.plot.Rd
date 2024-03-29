% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TwoSpamH.plot.R
\name{TwoSpamH.plot}
\alias{TwoSpamH.plot}
\title{Visualize the 2SpamH algorithm}
\usage{
TwoSpamH.plot(
  TwoSpamH.output,
  to.plot = c(1, 2),
  xlab = "PC 1",
  ylab = "PC 2",
  title = "Validity Distribition in the Constructed Feature Space",
  variable = "Filtered Variable"
)
}
\arguments{
\item{TwoSpamH.output}{The output from the function TwoSpamH when the \code{plot.data} is set to TRUE}

\item{to.plot}{A length 2 vector that indicates which principle components are to be plotted}

\item{xlab}{The label for x axis}

\item{ylab}{The label for y axis}

\item{title}{The plot title}
}
\value{
A ggplot object that visualize the algorithm's reduced feature space.
}
\description{
This function visualizes the 2SpamH algorithm.
}
