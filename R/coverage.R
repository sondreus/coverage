#' Coverage - Unit and Time
#'
#' The coverage package and associated function provides you with a visual, data frame or latex table summary of your time and unit coverage.
#' @param fit A fitted object.
#' @param timevar Time variable. Defaults to "year".
#' @param unitvar Unit variable. Defaults to "country".
#' @param data Data to be investigated. If none is supplied, attempts to use same data as "\code{fit}", first by considering its model matrix, and - if variables are missing (such as timevar and unitvar) - by looking for the source data in the global environment. (Optional)
#' @param variable.names Variables to be checked for coverage. If none is supplied, defaults to variables used in \code{fit}, or if fit not provided, all variables in data. (Optional)
#' @param output Desired extra output: "visual" (default), "data.frame", or "latex.table". First depends on \code{ggplot2}, last \code{stargazer}, both available on CRAN.
#' @param special.NA Variable that if missing will indicate "special" missingness. Can be used to distinguish observations with missing data from time-unit combinations which did not exist or were not considered. (Optional)
#' @param data.frequency Integer specifying the increments between observations. Defaults to 1.
#' @param ... Additional arguments passed to ggplot2's \code{theme} function, or \code{stargazer}, depending on output selected. (Optional)
#' @keywords coverage lm missingness missing NA
#' @export
#' @examples
#' library(WDI)
#' wdi.sample <- WDI(indicator=c("GDPPC" = "NY.GDP.PCAP.KD",
#'                              "services_gdp" = "BG.GSR.NFSV.GD.ZS",
#'                              "agriculture_gdp" = "NV.AGR.TOTL.ZS",
#'                              "telephones" = "IT.TEL.TOTL.P3"),
#'                              start=1970, end=2012,
#'                              country="all")
#'
#' lm.fit <- lm(GDPPC ~ ., data = wdi.sample)
#'
#' coverage(lm.fit)
#'
#' # One may also specify variables explicitly:
#' coverage(timevar = "year",
#'          unitvar = "country",
#'          variable.names = c("GDPPC",
#'                   "services_gdp",
#'                   "agriculture_gdp",
#'                   "telephones"),
#'          data = wdi.sample)
#'
#' # Or request data.frame or latex.table output:
#'
#' # For data.frame, use:
#' coverage(fit = lm.fit, output = "data.frame")
#'
#' # For latex table, use:
#' coverage(fit = lm.fit, output = "latex.table")
#'
#'


coverage <- function(fit,
                     timevar = "year",
                     unitvar = "country",
                     data,
                     variable.names,
                     special.NA,
                     output = c("visual", "data.frame", "latex.table")[1],
                     data.frequency = 1,
                     ...){

# Check that permissible output specified:
  if(!output %in% c("visual", "data.frame", "latex.table")){
    stop("Please select an appropriate output. Alternatives are: 'visual', 'data.frame', and 'latex.table'")
  }

# Check if data or fit provided
if(missing(data)){
  if(missing(fit)){
    stop("Coverage requires you to supply either data or a fitted object.")
  }

}

# If not specified, get variables to consider
if(missing(variable.names)){

  # from fitted object:
  if(!missing(fit)){
  variable.names <- all.vars(formula(fit))

  } else {
  # or just use all variables in data
  variable.names <- colnames(data)

  }
}

if(!missing(special.NA)){
  variable.names <- unique(c(variable.names, special.NA))
}

# Add timevar and unitvar to list of variables:
variable.names <- unique(c(variable.names, unitvar, timevar))

#  If not specified, get data to consider from fitted object:
if(missing(data)){

  # Check if all variables in model matrix & special.NA is not selected:
  # (Note: special.NA would otherwise be equal to NA, and thus pointless)
  if(sum(variable.names %in% all.vars(formula(fit))) == length(variable.names) &
     missing(special.NA)){
    if("Zelig" %in% class(fit)){
      data <-  na.omit(fit$originaldata)
    } else {
      data <-  fit$model
    }

    # Else attempt to find source data in parent environment:
    } else {
  if("Zelig" %in% class(fit)){
    data <-  as.data.frame(eval(fit$model.call[[3]], envir= parent.env(environment())))
  } else {
    data <-  as.data.frame(eval(fit$call[[3]], envir= parent.env(environment())))
  }
  }
}

# Subsetting data frame:
data <- data[order(data[, timevar]), variable.names]

# Assessing missingness:
data$missing <- !complete.cases(data)

# Check if any complete cases:
if(sum(!data$missing) == 0){
  stop("For these variables, your data has no complete cases")
}

# Generating a summary data frame detailing the unit and time combinations present in the data (and their number, if data is 3-dimensional):
coverage_df <- as.data.frame(table(data[!data$missing, unitvar], data[!data$missing, timevar]))
colnames(coverage_df) <- c("Unit", "Time", "N")
coverage_df[, c("Time", "N")] <- lapply(coverage_df[, c("Time", "N")], FUN = function(x){as.numeric(as.character(x))})
coverage_df[, "Unit"] <- as.character(coverage_df$Unit)

# Adding special N column if specified:
if(missing(special.NA) == FALSE){
  special.NA.df <- unique(data[,c(unitvar, timevar, special.NA)])
  colnames(special.NA.df) <- c("Unit", "Time", "special.NA")
  coverage_df <- merge(coverage_df, special.NA.df, all.x = TRUE, by = c("Time", "Unit"))
  coverage_df$N[is.na(coverage_df$special.NA)] <- NA
}

### Generating and return visual (default):
if(output == "visual"){
  library(ggplot2, quietly = TRUE)

  # Parameter tweaked to make things a bit more pretty
  base_size <- 9
  coverage_df <<- coverage_df

  # Eases alphabetic sort
  coverage_df$Unit <- as.factor(coverage_df$Unit)
  coverage_df$Time <- as.factor(coverage_df$Time)

  p <- ggplot(coverage_df, aes(Time, factor(coverage_df$Unit, levels = unique(coverage_df$Unit[sort(coverage_df$Unit, decreasing = TRUE)]))))
  p <- p+ geom_tile(aes(fill = N), colour = 'white')
  p <- p+ scale_fill_gradient(low = 'white', high = 'steelblue', na.value = "lightgrey")
  p <- p+ theme_grey(base_size = base_size) + labs(x = '', y = '') +
    scale_x_discrete(expand = c(0, 0),
                     breaks=pretty(as.numeric(as.character(coverage_df$Time)),
                                   n=20)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme(legend.position = 'none', axis.text.x = element_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = 'grey50'), plot.margin = unit(c(5, 15, 5, 5), "pt"), axis.text.y = element_text(size = 45/(sqrt(length(unique(coverage_df[,1]))))))+
    theme(...)
  return(p)
}

### Else generate summary data frame by unit:
# Constructing container data frame:
unit_coverage_df <- data.frame(Unit = unique(coverage_df$Unit[!coverage_df$N == 0]),
                               Time = rep(NA, length(unique(coverage_df$Unit[!coverage_df$N == 0]))))

# Summarizing time coverage by unit, storing it as set of unique values:
  for (i in 1:length(unique(unit_coverage_df$Unit))){
    unit_coverage_df$Time[[i]] <- list(unique(
      coverage_df$Time[coverage_df$Unit == unit_coverage_df$Unit[i] &
                         coverage_df$N > 0]))
  }

# Summarizing these as intervals:
unit_coverage_df$Time <- unlist(lapply(unit_coverage_df$Time, FUN = function(t){
  t <- unlist(t)

  # Find starts of intervals
  starts <- t[c(TRUE, t[-1] - t[-length(t)] != data.frequency)]

  # Find ends of intervals
  ends <- t[c(t[-1], 0) - t != data.frequency]

  # Combine the two
  intervals <- paste(starts, ends,
                           sep = "-")

  # Avoid double-listing lonely time-unit observations:
  intervals[ends == starts] <- starts[ends == starts]

  return(paste(intervals, collapse = ", "))
}))

# Summarizing total number of observations:
unit_coverage_df$Total_N <- unlist(lapply(unit_coverage_df$Unit, FUN = function(i){
  sum(coverage_df$N[coverage_df$Unit == i])
}))

# Reordering the rows (alphabetically)
unit_coverage_df <- unit_coverage_df[order(unit_coverage_df$Unit), ]

## Return data frame if requested
if(output == "data.frame"){
  return(unit_coverage_df)
}

## Return latex table if requested
if(output == "latex.table"){

  library(stargazer)

  # Changing column names to be more descriptive
  colnames(unit_coverage_df) <- c("Unit", "Covered time", "Total Observations")


      # If smaller than 75 rows, then one table
      if(nrow(unit_coverage_df) < 75){
        return(stargazer(unit_coverage_df, summary=FALSE, rownames=FALSE, font.size = "tiny", ...))}

      # If larger than 75 rows but smaller than 150, then two tables.
      if(nrow(unit_coverage_df) >= 75 & nrow(unit_coverage_df) < 150){
        return(output <- c(stargazer(unit_coverage_df[1:round(nrow(unit_coverage_df)/2), ], summary=FALSE, rownames=FALSE, font.size = "tiny", ...),

                           stargazer(unit_coverage_df[(1+round(nrow(unit_coverage_df)/2)):nrow(unit_coverage_df), ], summary=FALSE, rownames=FALSE, font.size = "tiny", ...)))}

      # If larger than 150, then three tables.
      if(nrow(unit_coverage_df) >= 150){
        return(output <- c(stargazer(unit_coverage_df[1:round(nrow(unit_coverage_df)/3), ], summary=FALSE, rownames=FALSE, font.size = "tiny", ...),

                           stargazer(unit_coverage_df[(1+round(nrow(unit_coverage_df)/3)):(1+2*round(nrow(unit_coverage_df)/3)), ], summary=FALSE, rownames=FALSE, font.size = "tiny", ...),

                           stargazer(unit_coverage_df[(1+2*round(nrow(unit_coverage_df)/3)):nrow(unit_coverage_df), ], summary=FALSE, rownames=FALSE, font.size = "tiny", ...)))
      }
    }

}
