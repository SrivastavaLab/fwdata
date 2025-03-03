% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fw_data_transformed.R
\name{fw_version_current}
\alias{fw_version_current}
\title{Find Current Version of Data}
\usage{
fw_version_current(local = TRUE, path = NULL, biomass = FALSE)
}
\arguments{
\item{local}{Logical indicating if local or GitHub versions should be polled. Defaults to TRUE.}

\item{path}{Optional path to save the data.}

\item{biomass}{Logical indicating if the biomass dataset should be accessed. Defaults to FALSE.}
}
\value{
The current version of the data.
}
\description{
This function retrieves the current version of data, either from local storage or GitHub.
}
