require(readr)
require(plyr)
library(plyr) #I put these here becaues dplyr warned that I should.
library(dplyr)

# Set working directory
setwd("~/s17dvproject6-vaughn-cannata-martinez/00 Docs")

# Set filepath
file_path = "../../CSVs/PreETL_Restaurant_Inspection_Scores.csv"

# Create dataframe from original CSV
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Standardize column names
names(df) <- gsub("\\.+", " ", names(df))

str(df) # Run just the lines to here to get column types to use for getting the list of measures.

# Select string data as 'dimensions'
dimensions <- c("Restaurant Name", "Address", "Process Description", "Facility ID", "Zip Code")

# Select dates as 'dates'
dates <- c("Inspection Date")

# Select all remaining data as 'measures'
measures <- setdiff(names(df), union(dimensions, dates))

# Get rid of special characters in each column.
for(n in names(df)) {
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= " "), stringsAsFactors = FALSE)
}

# This function will replace NA data with an empty string
na2emptyString <- function (x) {
  x[is.na(x)] <- ""
  return(x)
}
# We'll apply this to all columns grouped as dimensions
if( length(dimensions) > 0) {
  for(d in dimensions) {
    # Change NA to the empty string.
    df[d] <- data.frame(lapply(df[d], na2emptyString), stringsAsFactors = FALSE)
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""), stringsAsFactors = FALSE)
    # Change & to and in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "), stringsAsFactors = FALSE)
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"), stringsAsFactors = FALSE)
  }
}

# Date-specific manipulations


if( length(dates) > 1 || ! is.na(dates)) {
  for(y in dates) {
    # Format as dates
    df[y] <- data.frame(lapply(df[y], function(y) as.Date(y, format = "%m/%d/%y")), stringsAsFactors = FALSE)
  }
}

#Measure-specific manipulations
na2zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions, and change NA to 0.
if(length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement=""), stringsAsFactors = FALSE)
    df[m] <- data.frame(lapply(df[m], na2zero), stringsAsFactors = FALSE)
    df[m] <- data.frame(lapply(df[m], function(m) as.numeric(as.character(m)))) # This is needed to turn measures back to numeric because gsub turns them into strings.
  }
}


# Take a look and make sure it's what you think it should be:
print(summary(df))

# Now write it to a new clean file.
write.csv(df, gsub("PreETL_", "", file_path), row.names=FALSE, na = "")

