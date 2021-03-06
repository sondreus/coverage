% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coverage.R
\name{coverage}
\alias{coverage}
\title{Coverage - Unit and Time}
\usage{
coverage(fit, timevar = "year", unitvar = "country", data,
  variable.names, special.NA, output = c("visual", "data.frame",
  "latex.table")[1], data.frequency = 1, ...)
}
\arguments{
\item{fit}{A fitted object.}

\item{timevar}{Time variable. Defaults to "year".}

\item{unitvar}{Unit variable. Defaults to "country".}

\item{data}{Data to be investigated. If none is supplied, attempts to use same data as "\code{fit}", first by considering its model matrix, and - if variables are missing (such as timevar and unitvar) - by looking for the source data in the global environment. (Optional)}

\item{variable.names}{Variables to be checked for coverage. If none is supplied, defaults to variables used in \code{fit}, or if fit not provided, all variables in data. (Optional)}

\item{special.NA}{Variable that if missing will indicate "special" missingness. Can be used to distinguish observations with missing data from time-unit combinations which did not exist or were not considered. (Optional)}

\item{output}{Desired extra output: "visual" (default), "data.frame", or "latex.table". First depends on \code{ggplot2}, last \code{stargazer}, both available on CRAN.}

\item{data.frequency}{Integer specifying the increments between observations. Defaults to 1.}

\item{...}{Additional arguments passed to ggplot2's \code{theme} function, or \code{stargazer}, depending on output selected. (Optional)}
}
\description{
The coverage package and associated function provides you with a visual, data frame or latex table summary of your time and unit coverage.
}
\examples{
library(WDI)
wdi.sample <- WDI(indicator=c("GDPPC" = "NY.GDP.PCAP.KD",
                             "services_gdp" = "BG.GSR.NFSV.GD.ZS",
                             "agriculture_gdp" = "NV.AGR.TOTL.ZS",
                             "telephones" = "IT.TEL.TOTL.P3"),
                             start=1970, end=2012,
                             country="all")

lm.fit <- lm(GDPPC ~ ., data = wdi.sample)

coverage(lm.fit)

# One may also specify variables explicitly:
coverage(timevar = "year",
         unitvar = "country",
         variable.names = c("GDPPC",
                  "services_gdp",
                  "agriculture_gdp",
                  "telephones"),
         data = wdi.sample)

# Or request data.frame or latex.table output:

# For data.frame, use:
coverage(fit = lm.fit, output = "data.frame")

# For latex table, use:
coverage(fit = lm.fit, output = "latex.table")


}
\keyword{NA}
\keyword{coverage}
\keyword{lm}
\keyword{missing}
\keyword{missingness}
