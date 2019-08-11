# read.EpiData
# A function to read EpiDatas epx-files directly into R.

# Arguments:
# x: an epx-file.
# raw.data: If FALSE, defined missings are ste to NA and 
## variables are convertet to R-classes according to their field type.
## If TRUE, the data.frame contains the original codes as character variables.

# Value
# If the epx-file contains only one data set, the output will be a data.frame. 
## If it is a relational data base with multiple data sets. The outcome will be a list of data frames.
# Study information, variable labels and information about data.frame-relations will be given as attributes.


# So far, the functions can lonly read non-encryted files.
# It depends on the package xml2.



# Johann Popp
# 2019-06-16
# Last update: 2019-08-08
###########################################

read.EpiData <- function(x, raw.data = FALSE){
  
  ##############################################
  # Step 1: Extracting Information
  ##############################################
  
  info <- list()
  
  # Read epx-data and remove name spaces
  info$epx <- xml2::xml_ns_strip(xml2::read_xml(x))
  
  # Extract general informations
  info$infoEpiData <- xml2::xml_find_all(info$epx, "//EpiData")
  info$infoStudy <- xml2::xml_find_all(info$epx, "//StudyInfo")
  info$infoSeparators <- unlist(xml2::xml_attrs(xml2::xml_find_all(info$epx, "//Settings")))
  # Extract information about data sets
  info$infoDataSets <- xml2::xml_find_all(info$epx, "//DataFile")
  # Extract informations about data set relations
  info$infoParentDataSet <- lapply(
    xml2::xml_find_all(info$epx, "//DataFileRelation"), 
    function(x){
      xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "dataFileRef")
    }
  )
  info$infoKeyVars <- lapply(
    lapply(
      xml2::xml_find_all(info$epx, "//KeyFields"), 
      function(x) xml2::xml_attr(xml2::xml_children(x), "fieldRef")), 
    paste, collapse = ";;")
  
  if(length(info$infoDataSets) > 1){
    info$infoRelations <- data.frame(data.set = xml2::xml_attr(info$infoDataSets, "id"),
                                     parent.data.set = unlist(info$infoParentDataSet),
                                     key.variables = unlist(info$infoKeyVars))
  } else {
    info$infoRelations <- "This is not a relational data base."
  }
  info <- info[-c(6,7)]

  
  # Function to extract information for each data set
  epxExtractDataSet <- function(df){
    
    # Extract data base entries
    records <- 
      xml2::xml_text(
        xml2::xml_find_all(
          xml2::xml_find_all(df, "Records"), 
          "Record")
      )
    
    # Extract field definitions
    datFields <- xml2::xml_find_all(df, ".//Field")
    # Field names
    fieldNames <- xml2::xml_attr(datFields, "id")
    # Field labels
    fieldLabels <- xml2::xml_text(datFields)
    # Field types
    fieldTypes <- xml2::xml_attr(datFields, "type")
    
    # Value label sets
    # Indicate which fields have which value label sets
    fieldValLabSets <- xml2::xml_attr(datFields, "valueLabelRef")
    # Extract value labels (all value label sets for all data sets)
    valLabelSets <- xml2::xml_find_all(info$epx, "//ValueLabelSet")
    valLabels <- lapply(valLabelSets, function(x) xml2::xml_find_all(x, ".//ValueLabel"))
    
    # Extract separators
    infoSeparators <- info$infoSeparators

    # Gather information in a list
    list(records = records, datFields = datFields, fieldNames = fieldNames, fieldLabels = fieldLabels, 
         fieldTypes = fieldTypes, fieldValLabSets = fieldValLabSets, valLabelSets = valLabelSets,
         valLabels = valLabels, infoSeparators = infoSeparators)
    
  }
  
  # Apply the extraction function to each data set.
  info$perDataSet <- lapply(info$infoDataSets, epxExtractDataSet)
  names(info$perDataSet) <- xml2::xml_attr(info$infoDataSets, "id")
  
  
  ################################
  # Step 2: Reading the information into data.frame.
  ################################
  
  # Define a function to read data set information into a data.frame
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
  
  # Apply the function to each data set.
  dat <- lapply(info$perDataSet, epx.read)
  
  
  #########################################
  # Step 3: Create a function to convert defined missing values into NA.
  #########################################
  
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
    indexValLabSet <- lapply(info$fieldValLabSets, 
                             function(x) which(xml2::xml_attr(info$valLabelSets, "id") == x))
    # Missings per variable
    missingsPerVar <- lapply(missingCodes[as.numeric(paste(indexValLabSet))],
                             paste, collapse = "|")
    missingsPerVar[missingsPerVar == ""] <- NA

    # missingsPerVar <- missingCodes[as.numeric(paste(indexValLabSet))]
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
  
  
  #############################
  # Step 4: Create a function to apply value labels and 
  #         convert the variables into R-classes according to their field type.
  #############################
  
  epx.class <- function(x, info){
    dat <- x
    info <- info
    
    # Extract field types
    fieldTypes <- info$fieldTypes
    # Extract value lable sets that contain codes for non-missing values
    catValLabelSets <- info$valLabelSets[lapply(info$valLabels, function(x) sum(!grepl("missing=\"true\"", x))) > 0]
    # Extract value labels
    catValLabels <- lapply(catValLabelSets, xml2::xml_children)
    # Extract value labels that do not code missing values
    validCategories <- lapply(catValLabels, function(x) x[grepl("missing=\"true\"", x) == FALSE])
    # value codes
    validValCodes <- lapply(validCategories, function(x) xml2::xml_attr(x, "value"))
    # Value labels
    validLabels <- lapply(validCategories, xml2::xml_text)

    # Indicate which value set matches which variable
    indexValLabelSet  <- as.numeric(
      lapply(
        info$fieldValLabSets, 
        function(x){
          which(
            grepl(x, xml2::xml_attr(catValLabelSets, "id")) == TRUE)
        }
      )
    )
    
    # Replace value codes with value labels
    dat <- as.data.frame(
      mapply(
        function(x, y, z){
          if(!is.na(y) & (z == "ftInteger" | z == "ftFloat")){
            # if(z == "ftInteger"|z == "ftFloat"){
            validLabels[[y]][as.numeric(lapply(x, function(x)  which(x == validValCodes[[y]])))]
          } else {
            if(!is.na(y) & z == "ftString"){
              validLabels[[y]][order(validValCodes[[y]])][factor(x)]
            } else {x}
          }
        }, 
        x = dat, y = indexValLabelSet, z = fieldTypes),
      stringsAsFactors = FALSE)
    
    
    
    # Convert integer fields
    dat[,fieldTypes == "ftAutoInc" | fieldTypes == "ftInteger" & is.na(indexValLabelSet)] <-
      as.integer(
        unlist(
          dat[,fieldTypes == "ftAutoInc" | fieldTypes == "ftInteger" & is.na(indexValLabelSet)])
      )
    
    # Convert numeric fields
    if(sum(fieldTypes == "ftFloat" & is.na(indexValLabelSet)) > 1){
      dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)] <-
        apply(dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)], 2, function(x) sub(info$infoSeparators[3], ".", x))#sub(info$infoSeparators[[1]][3], ".", x))
      dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)] <-
        apply(dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)], 2, as.numeric)
    } else { 
      if(sum(fieldTypes == "ftFloat" & is.na(indexValLabelSet)) == 1){
        dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)] <- 
          as.numeric(
            sub(
              info$infoSeparators[3],
              ".",
              dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)]
            )
          )
      }
    }
    
    
    # Convert categorical fields
    if(sum(!is.na(indexValLabelSet)) == 1){
      dat[, !is.na(indexValLabelSet)] <- 
        factor(dat[, !is.na(indexValLabelSet)])
    } else {
      if(sum(!is.na(indexValLabelSet)) > 1){
        dat[, !is.na(indexValLabelSet)] <-
          as.data.frame(lapply(dat[, !is.na(indexValLabelSet)], factor))
      }
    }

    # Convert date fields
    ## DMY format
    dmyPattern <- paste("%d", info$infoSeparators[[1]][1], "%m", info$infoSeparators[[1]][1], "%Y", sep = "")
    if(sum(fieldTypes == "ftDMYDate" | fieldTypes == "ftDMYAuto") > 1){
      dat[, fieldTypes == "ftDMYDate" | fieldTypes == "ftDMYAuto"] <- 
        lapply(dat[,fieldTypes == "ftDMYDate" | fieldTypes == "ftDMYAuto"], function(x) strptime(x, dmyPattern))
    } else {
      dat[,fieldTypes == "ftDMYDate" | fieldTypes == "ftDMYAuto"] <-  
        data.frame(strptime(dat[,fieldTypes == "ftDMYDate" | fieldTypes == "ftDMYAuto"], dmyPattern))
    }
    
    ## MDY format
    mdyPattern <- paste("%m", info$infoSeparators[[1]][1], "%d", info$infoSeparators[[1]][1], "%Y", sep = "")
    if(sum(fieldTypes == "ftMDYDate" | fieldTypes == "ftMDYAuto") > 1){
      dat[, fieldTypes == "ftMDYDate" | fieldTypes == "ftMDYAuto"] <- 
        lapply(dat[,fieldTypes == "ftMDYDate" | fieldTypes == "ftMDYAuto"], function(x) strptime(x, mdyPattern))
    } else {
      dat[,fieldTypes == "ftMDYDate" | fieldTypes == "ftMDYAuto"] <-  
        data.frame(strptime(dat[,fieldTypes == "ftMDYDate" | fieldTypes == "ftMDYAuto"], mdyPattern))
    }
    
    ## YMD format
    ymdPattern <- paste("%Y", info$infoSeparators[[1]][1], "%m", info$infoSeparators[[1]][1], "%d", sep = "")
    if(sum(fieldTypes == "ftYMDDate" | fieldTypes == "ftYMDAuto") > 1){
      dat[, fieldTypes == "ftYMDDate" | fieldTypes == "ftYMDAuto"] <- 
        lapply(dat[,fieldTypes == "ftYMDDate" | fieldTypes == "ftYMDAuto"], function(x) strptime(x, ymdPattern))
    } else {
      dat[,fieldTypes == "ftYMDDate" | fieldTypes == "ftYMDAuto"] <-  
        data.frame(strptime(dat[,fieldTypes == "ftYMDDate" | fieldTypes == "ftYMDAuto"], ymdPattern))
    }
    
    # Convert time
    timePattern <- paste("%H", info$infoSeparators[2], "%M", info$infoSeparators[2], "%S", sep = "")
    if(sum(fieldTypes == "ftTime" | fieldTypes == "ftTimeAuto") > 1){
      dat[, fieldTypes == "ftTime" | fieldTypes == "ftTimeAuto"] <- 
        lapply(dat[,fieldTypes == "ftTime" | fieldTypes == "ftTimeAuto"], function(x) strptime(x, timePattern))
    } else {
      dat[,fieldTypes == "ftTime" | fieldTypes == "ftTimeAuto"] <-  
        data.frame(strptime(dat[,fieldTypes == "ftTime" | fieldTypes == "ftTimeAuto"], timePattern))
    }
    
    # Convert to logical fields
    dat[, fieldTypes == "ftBoolean"] <- 
      dat[, fieldTypes == "ftBoolean"] == "Y"
    
    # Reassign variable labels
    attributes(dat)$variable.labels <- info$fieldLabels
    
    dat
  }
  
  
  #################################
  # Step 5: Apply conversion of missings to NA, translation of value codes into value labels and
  #         translation to appropiate R-classes to each data.frame.
  #################################
  
  suppressWarnings(
    if(raw.data == FALSE){
      # Combine dat and info in a data-set-wise list
      perDataSet <- mapply(function(dat, info) list(list(dat = dat, info = info)), dat, info$perDataSet)
      # Convert missings into NA
      dat2 <- lapply(perDataSet, function(x) epx.missing(x$dat, x$info))
      perDataSet <- mapply(function(dat, info) list(list(dat = dat, info = info)), dat2, info$perDataSet)
      # Apply value labels and convert to R-classes
      dat2 <- lapply(perDataSet,
                     function(x){
                       if(class(x[[1]]) == "data.frame"){
                         epx.class(x[[1]], x[[2]])
                       } else {"This data frame has no entries"}
                     })
      attributes(dat2) <- attributes(dat)
      dat2
    } else {
      dat2 <- dat
    }
  )
  
  
  ##############################
  # Step 6: Cleaning up and seting additional attributes.
  ##############################
  
  # Change the class from list to data.frame, if the file contains only one data set
  if(length(dat2) == 1){
    dat2 <- dat2[[1]]
  }
  
  # Add study information
  infoStudy <- lapply(xml2::xml_children(info$infoStudy), xml2::xml_text)
  names(infoStudy) <- xml2::xml_name(xml2::xml_children(info$infoStudy))
  attributes(dat2)$study.info <- t(data.frame(infoStudy))
  
  # Add relational information
  if(class(dat2) == "list"){
    attributes(dat2)$relations <- info$infoRelations
  }
  
  dat2
  
}


