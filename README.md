# coverage

The coverage package and associated function provides you with a summary of your time and unit coverage for analysis conducted with row-wise deletion in the presence of missing data. Examples of such analysis include standard regression analysis and most implementations of maximum likelihood. By default provides a data frame of unit and time coverage ("coverage.df"), can also additionally supply a latex table or a visualization of coverage. Supports 3-dimensional data by providing total number of observations in tables, and "heatmap" of observations in visual.

## Example: 

* fit A fitted object. Currently supports base R lm().
* **timevar** Your time variable
* **unitvar** Your unit variable
* **data** Data to be investigated. If none is supplied, defaults to data used in "\code{fit}" if fit is in supported format.
* **variable.names** Variables to be checked for coverage. If none is supplied, defaults to variables used in \code{fit}.
* **output** Desired output: "visual" or "latex.table". Former depends on requires the package \code{ggplot2}, latter requires the package \code{stargazer}, both available on CRAN.

   ```R
library(WDI)
wdi.sample <- WDI(indicator=c('NY.GDP.PCAP.KD',
                              'BG.GSR.NFSV.GD.ZS',
                              'SE.ADT.LITR.FE.ZS',
                              'NV.AGR.TOTL.ZS',
                              'IT.TEL.TOTL.P3'),

                              country=c('MX','CA',
                              'US','TZ'),

                              start=1950, end=2012)

 colnames(wdi.sample)[4:8] <- c("GDPPC",
                                "services_gdp",
                                "literacy_women",
                                "agriculture_gdp",
                                "telephones")

lm.fit <- lm(GDPPC ~ services_gdp, data = wdi.sample)

 coverage(fit = lm.fit, timevar = "year",
          unitvar = "country")

 head(coverage.df)
 head(coverage.summary)

 # For visuals, use:
 coverage(fit = lm.fit, timevar = "year",
          unitvar = "country", output = "visual")

 # For latex tables, use:
 coverage(fit = lm.fit, timevar = "year",
          unitvar = "country", output = "latex.table")

 # Supplying a fit is not required, and it may be easier
 # to compare the consequences of different model specifications
 # by simply providing the variable names:
 coverage(timevar = "year", unitvar = "country",
          data = wdi.sample,
          variable.names = c("GDPPC",
                             "agriculture_gdp"),
          output = "visual")

 # vs:
 coverage(timevar = "year", unitvar = "country",
          data = wdi.sample,
          variable.names = c("GDPPC",
          "services_gdp"),
          output = "visual")

   ```
