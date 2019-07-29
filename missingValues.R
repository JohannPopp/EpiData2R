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
  missingsPerVar <- missingCodes[as.numeric(paste(indexValLabSet))]
  
  # Convert defined missing values into NA
  
  # Add a first row containting defined missing codes for each variable 
  # and a second row, indicating which variable contains defined missings.
  dat2 <- rbind(lapply(missingsPerVar, function(x) {paste(x, collapse = "|")}),
                lapply(missingsPerVar, length) > 0,
                dat)
  
  # Set defined missings to NA for those variables that contain defined missings
  dat3 <- as.data.frame(apply(dat2[,dat2[2,] == TRUE], 2, function(x){
    x[grepl(x[1], x)] <- NA
    x
  }), stringsAsFactors = FALSE)
  
  # Enter variables with defined missings back into the data frame
  dat2[,dat2[2,] == TRUE] <- dat3
  
  # Delete rows 1 and 2 (that contain additional information about defined missings)
  dat2 <- dat2[-c(1,2),]
  # Reset rownames
  rownames(dat2) <- NULL
  
  # Convert undefined missing values in text fields (expressed as "NA") into NA
  dat2[dat2 == "NA"] <- NA
  
  
  dat2
}
