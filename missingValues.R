# Function to convert codes, that are defines as missing values into NA in the data.frame
# This is part of a project to create a package to read epx-files from EpiData into R

# Johann Popp
# 2019-06-09
# Last modification: 2019-07-25
###########################################


epx.missing <- function(dat, info){
  dat <- dat
  info <- info
  
  # Extract value codes
  valueCodes <- lapply(info$valLabels, function(x) xml2::xml_attr(x, "value"))
  # Identify value label sets that have codes for missing values and where are those codes
  indexMissing <- lapply(info$valLabels, function(x) grep("missing=\"true\"", x))
  # Identify missing value codes
  missingCodes <- as.list(mapply(function(x, y) x[y], valueCodes, indexMissing))

  # Index the corresponding value label set for each of the variables
  indexValLabSet <- lapply(info$fieldValLabSets, function(x) which(xml2::xml_attr(info$valLabelSets, "id") == x))
  # Missings per variable
  missingsPerVar <- lapply(missingCodes[as.numeric(paste(indexValLabSet))],
                           paste, collapse = "|")
  missingsPerVar[missingsPerVar == ""] <- NA
  
  # Convert defined missing values into NA
  dat[
    mapply(function(x, y){
      grepl(y, x)
    },
    x = dat, 
    y = missingsPerVar) == 1
    ] <- NA

  # Convert undefined missing values in text fields (expressed as "NA") into NA
  dat[dat == "NA"] <- NA
 
  dat
}
