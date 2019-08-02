# Function to convert exracted information from an epx-file into a data.frame
# This is part of a project to create a package to read epx-files from EpiData into R

# Johann Popp
# 2019-06-23
# Last update: 2019-08-02
###########################################

epx.read <- function(x){
  if(length(x$records > 0)){
  records <- x$records
  fieldNames <- x$fieldNames
  fieldLabels <- x$fieldLabels
  fieldTypes <- x$fieldTypes
  
  # Add category "NA" for missing values in the records
  records <- paste("NA;", records, sep="")  
  # Construct a pattern to seperate individual variables in the records
  pattern <- paste(";", paste(fieldNames, "=", sep = "", collapse = "|;"), sep = "")
  
  # Enter values from the records into a data.frame.
    dat <- as.data.frame(
      lapply(
        # colnames(dat),
        fieldNames,
        function(y){
          records2 <- gsub(paste("[[:print:]]*", ";", y, "=", sep = ""), "", records)
          unlist(lapply(strsplit(records2, pattern), function(x) x[1]))
        }),
      stringsAsFactors = FALSE, col.names = fieldNames)
  # })

  # Delete double quotation marks at the beginning and end of text fields
  characterPattern <- "ftString|ftMemo|ftUpperString"
  if(sum(grepl(characterPattern, fieldTypes)) == 1) {
    dat[,grepl(characterPattern, fieldTypes)] <- 
    gsub("^\"|\"$", "", dat[,grepl(characterPattern, fieldTypes)])  
  } else {
    if(sum(grepl(characterPattern, fieldTypes)) > 1) {
      dat[,grepl(characterPattern, fieldTypes)] <-
        as.data.frame(
          apply(dat[,grep("ftString|ftMemo|ftUpperString", fieldTypes)], 
                2, 
                function(x){ 
                  x <- gsub("^\"|\"$", "", x)
                }
          ), 
          stringsAsFactors = FALSE)
    }
  }
 
  # Include variable labels
  attributes(dat)$variable.labels <- fieldLabels


  dat} else {"This data frame has no entries."}
}

