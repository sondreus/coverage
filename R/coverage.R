#' Coverage - Unit and Time
#'
#' This function provides you with a summary of your time and unit coverage for analysis conducted with row-wise deletion in the presence of missing data. Examples of such analysis include standard regression analysis and most implementations of maximum likelihood. By default provides a data frame of unit and time coverage ("coverage.df") and a summary ("coverage.summary"), can also additionally supply a latex table or a visualization of coverage. Supports 3-dimensional data by providing total number of observations in tables, and "heatmap" of observations in visual.
#' @param fit A fitted object. Currently supports base R lm().
#' @param timevar Your time variable
#' @param unitvar Your unit variable
#' @param data Data to be investigated. If none is supplied, defaults to data used in "\code{fit}" if fit is in supported format.
#' @param variable.names Variables to be checked for coverage. If none is supplied, defaults to variables used in \code{fit}.
#' @param output Desired output: "visual" or "latex.table". Former depends on requires the package \code{ggplot2}, latter requires the package \code{stargazer}, both available on CRAN.
#' @param visual.source If TRUE, prints ggplot2 code used to create visual.
#' @keywords coverage lm
#' @export
#' @examples
#' library(WDI)
#' wdi.sample <- WDI(indicator=c('NY.GDP.PCAP.KD',
#'                              'BG.GSR.NFSV.GD.ZS',
#'                              'SE.ADT.LITR.FE.ZS',
#'                              'NV.AGR.TOTL.ZS',
#'                              'IT.TEL.TOTL.P3'),
#'
#'                              country=c('MX','CA',
#'                              'US','TZ'),
#'
#'                              start=1950, end=2012)
#'
#' colnames(wdi.sample)[4:8] <- c("GDPPC",
#'                                "services_gdp",
#'                                "literacy_women",
#'                                "agriculture_gdp",
#'                                "telephones")
#'
#' lm.fit <- lm(GDPPC ~ services_gdp, data = wdi.sample)
#'
#' coverage(fit = lm.fit, timevar = "year",
#'          unitvar = "country")
#'
#' head(coverage.df)
#'
#' # For visuals, use:
#' coverage(fit = lm.fit, timevar = "year",
#'          unitvar = "country", output = "visual")
#'
#' # For latex tables, use:
#' coverage(fit = lm.fit, timevar = "year",
#'          unitvar = "country", output = "latex.table")
#'
#' # Supplying a fit is not required, and it may be easier
#' # to compare the consequences of different model specifications
#' # by simply providing the variable names:
#' coverage(timevar = "year", unitvar = "country",
#'          data = wdi.sample,
#'          variable.names = c("GDPPC",
#'                             "agriculture_gdp"),
#'          output = "visual")
#'
#' # vs:
#' coverage(timevar = "year", unitvar = "country",
#'          data = wdi.sample,
#'          variable.names = c("GDPPC",
#'          "services_gdp"),
#'          output = "visual")
#'
#'

coverage <- function(fit, timevar, unitvar, data, variable.names, output, visual.source){

  # Making sure the data supplied is a data frame, else assume same as fit data.
  if(missing("data") == TRUE){
    data <- as.data.frame(eval(fit$call[[3]], envir= .GlobalEnv))
  } else {
    data <- as.data.frame(data)
  }

  ### Acquiring the names of variables used in model

  # Checking if supplied variable names manually
  if(missing("variable.names") == TRUE) {

    # If not, using variable names from fit
    var.names <- as.vector(all.vars(formula(fit)))
  } else {

    # If did, using manually supplied variable names
    var.names <- variable.names
  }

  # Generating a new data set with the relevant variables
  coverage.frame <- data[, c(var.names, timevar, unitvar)]

  # Generating an alternative data set with complete cases only
  observations.in.model <- coverage.frame[complete.cases(coverage.frame[, c(var.names)]),]

  # Outputting a "coverage" data frame detailing the unit and time combinations present in the data (and their number, if data is 3-dimensional).
  coverage <- as.data.frame(table(observations.in.model[, unitvar], observations.in.model[, timevar]))
  colnames(coverage) <- c("Unit", "Time", "N")
  coverage.df <<- coverage

  # Generating a data frame of all unique unit-time combinations represented among complete cases:
  coverage.list <- coverage[coverage$N != 0,]
  coverage.list[, 1] <- as.character(coverage.list[, 1])
  coverage.list[, 2] <- as.character(coverage.list[, 2])
  coverage.list[, 3] <- as.numeric(coverage.list[, 3])

  coverage.unit.time <- cbind.data.frame(rep(NA, length(unique(coverage.list$Unit))), rep(NA, length(unique(coverage.list$Unit))))
  colnames(coverage.unit.time) <- c("Unit", "Time")

  for (i in 1:length(unique(coverage.list$Unit))){
    coverage.unit.time[i, 1] <- as.character(unique(coverage.list$Unit)[i])
    coverage.unit.time[i, 2] <- paste(unique(coverage.list$Time[coverage.list$Unit == unique(coverage.list$Unit)[i]]), collapse=", ")
  }

  ## Generting a compact list of unit-time coverage by converting the separate times into intervals:
  intervals <- cbind.data.frame(rep(unique(coverage.list$Unit), 200), rep(NA, 200))
  colnames(intervals) <- c("Unit", "intervals")

  intervals <- intervals[order(intervals$Unit),]
  unit.time.included <- cbind.data.frame(unique(coverage.list$Unit), rep(NA, length(unique(coverage.list$Unit))))
  colnames(unit.time.included) <- c("Unit", "coverage")

  for (j in unique(coverage.unit.time$Unit)){
    index <- 1
    beginning_time <- "not this time"

    for (i in (min(as.numeric(as.character(coverage$Time)))-1):(max(as.numeric(as.character(coverage$Time))))+1){

      current_time <- i
      if(beginning_time == "not this time"){

        beginning_time <- ifelse(length(grep(i, coverage.unit.time$Time[coverage.unit.time$Unit == j]))>0, current_time, "not this time")
      } else {

        if(length(grep(i, coverage.unit.time$Time[coverage.unit.time$Unit == j]))>0){
          current_interval <- paste(beginning_time, current_time, sep="-")
        } else {

          if(beginning_time == i - 1 & length(grep(i-1, coverage.unit.time$Time[coverage.unit.time$Unit == j]))>0){
            current_interval <- beginning_time
            intervals[intervals$Unit == j, ][index, 2] <- current_interval
            index <- index + 1
            beginning_time <- "not this time"

          } else {
            intervals[intervals$Unit == j, ][index, 2] <- current_interval
            index <- index + 1
            beginning_time <- "not this time"
          }
        }
      }
    }

    unit.time.included[unit.time.included$Unit == j, 2] <- paste0(unique(intervals[is.na(intervals$intervals) == FALSE & intervals$Unit == j, 2]) , collapse = ", ")


    unit.time.included$n.times[unit.time.included$Unit == j] <- length(unique(coverage.list[coverage.list$Unit == j & is.na(coverage.list$Time) == FALSE, 2]))

    unit.time.included$n[unit.time.included$Unit == j] <- sum(coverage.list[as.character(coverage.list$Unit) == j, 3], na.rm = TRUE)
  }

  # Changing column names
  colnames(unit.time.included) <- c("Unit", "Covered time ", "Total time coverage", "Total Observations")

  # Reordering the columns
  unit.time.included <- unit.time.included[,c(1,4,3,2)]

  # Reordering the rows (alphabetically)
  coverage.summary <<- unit.time.included[order(as.character(unit.time.included$Unit)),]

  ## Printing code for visualization if requested:
  if(missing(visual.source) == FALSE){
    if(visual.source == TRUE){
      print("library(ggplot2)")
      print("base_size <- 9") 
      print("p <- ggplot(coverage, aes(Time, Unit)) + geom_tile(aes(fill = N), colour = 'white') + scale_fill_gradient(low = 'white', high = 'darkblue') + theme_grey(base_size = base_size) + labs(x = '', y = '') + scale_x_discrete(expand = c(0, 0), breaks=pretty(as.numeric(as.character(coverage$Time)), n=20)) + scale_y_discrete(expand = c(0, 0)) + theme(legend.position = 'none', axis.text.x = element_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = 'grey50'), plot.margin = unit(c(5, 15, 5, 5), 'pt'))")  
    }
  }
  
  ## Outputting latex table if requested
  if(missing(output) == FALSE){
    if(output == "latex.table"){

      # If smaller than 75 rows, then one table
      if(nrow(unit.time.included) < 75){
        return(stargazer(unit.time.included, summary=FALSE, rownames=FALSE, font.size = "tiny"))}

      # If larger than 75 rows but smaller than 150, then two tables.
      if(nrow(unit.time.included) >= 75 & nrow(unit.time.included) < 150){
        return(output <- c(stargazer(unit.time.included[1:round(nrow(unit.time.included)/2), ], summary=FALSE, rownames=FALSE, font.size = "tiny"),

                           stargazer(unit.time.included[(1+round(nrow(unit.time.included)/2)):nrow(unit.time.included), ], summary=FALSE, rownames=FALSE, font.size = "tiny")))}

      # If larger than 150, then three tables.
      if(nrow(unit.time.included) >= 150){
        return(output <- c(stargazer(unit.time.included[1:round(nrow(unit.time.included)/3), ], summary=FALSE, rownames=FALSE, font.size = "tiny"),

                           stargazer(unit.time.included[(1+round(nrow(unit.time.included)/3)):(1+2*round(nrow(unit.time.included)/3)), ], summary=FALSE, rownames=FALSE, font.size = "tiny"),

                           stargazer(unit.time.included[(1+2*round(nrow(unit.time.included)/3)):nrow(unit.time.included), ], summary=FALSE, rownames=FALSE, font.size = "tiny")))
      }
    }
  }

  ## Generating visual if requested:

  if(missing(output) == FALSE){
    if(output == "visual"){

      suppressMessages(library(ggplot2, quietly = TRUE))

      base_size <- 9
      p <- ggplot(coverage, aes(Time, Unit)) + geom_tile(aes(fill = N), colour = 'white') + scale_fill_gradient(low = 'white', high = 'darkblue') + theme_grey(base_size = base_size) + labs(x = '', y = '') + scale_x_discrete(expand = c(0, 0), breaks=pretty(as.numeric(as.character(coverage$Time)), n=20)) + scale_y_discrete(expand = c(0, 0)) + theme(legend.position = 'none', axis.text.x = element_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = 'grey50'), plot.margin = unit(c(5, 15, 5, 5), "pt")) 
      
      
      return(p)
    }
  }
}
