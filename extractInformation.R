# Function to extract xml-type information from a epx-file that was produced with EpiData 4.6. 
# This expands the function to the case of multiple data sets in one file
# This is part of a project to create a package to read epx-files into R

# Johann Popp
# 2019-06-08
# Last update: 2019-08-02
###########################################

epx.extract <- function(x){
  # Read epx-data and remove name spaces
  epx <- xml2::xml_ns_strip(xml2::read_xml(x))
  
  # Extract general Informations
  infoEpiData <- xml2::xml_find_all(epx, "//EpiData")
  infoStudy <- xml2::xml_find_all(epx, "//StudyInfo")
  infoSeparators <- unlist(xml2::xml_attrs(xml2::xml_find_all(epx, "//Settings")))
  
  infoDataSets <- xml2::xml_find_all(epx, "//DataFile")
  infoParentDataSet <- lapply(
    xml2::xml_find_all(epx, "//DataFileRelation"), 
    function(x){
      xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "dataFileRef")
    }
  )
  infoKeyVars <- lapply(
    lapply(
      xml2::xml_find_all(epx, "//KeyFields"), 
      function(x) xml2::xml_attr(xml2::xml_children(x), "fieldRef")), 
    paste, collapse = ";;")
  
  
  
  
  # Extract information for each data set
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
    valLabelSets <- xml2::xml_find_all(epx, "//ValueLabelSet")
    valLabels <- lapply(valLabelSets, function(x) xml2::xml_find_all(x, ".//ValueLabel"))
    
    # Extract separators
    infoSeparators <- infoSeparators
    
    # Key variables
    # keyVars <- xml2::xml_attr(xml2::xml_find_all(df,  ".//Key"), "fieldRef")
    
    
    # Gather information in a list
    list(records = records, datFields = datFields, fieldNames = fieldNames, fieldLabels = fieldLabels, 
         fieldTypes = fieldTypes, fieldValLabSets = fieldValLabSets, valLabelSets = valLabelSets,
         valLabels = valLabels, infoSeparators = infoSeparators)
    
  }
  
  perDataSet <- lapply(infoDataSets, epxExtractDataSet)
  names(perDataSet) <- xml2::xml_attr(infoDataSets, "id")
  
  
  
  
  
  # Gather information in a list
  list(epx = epx, infoEpiData = infoEpiData, infoStudy = infoStudy, infoSeparators = infoSeparators,
       infoDataSets = infoDataSets, infoParentDataSet = infoParentDataSet,
       perDataSet = perDataSet, infoKeyVars = infoKeyVars)
}



