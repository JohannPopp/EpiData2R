# Function to combine the single functions of the project and bring everthing together.

# Arguments:
# x: an epx-file.
# raw.data: If FALSE, defined missings are ste to NA and 
## variables are convertet to R-classes according to their field type.
## If TRUE, the data.frame contains the original codes as character variables.

# Value
# If the epx-file contains only one data set, the output will be a data.frame. 
## It it is a relational data base wit multiple data sets. The outcome will be a list of data frames.
# Study information, variable labels and information about data.frame-relations will be given as attributes.

# This is part of a project to create a package to read epx-files from EpiData into R.

# So far, the functions depend on the package xml2


# Johann Popp
# 2019-06-16
# Last update: 2019-08-02
###########################################

read.EpiData <- function(x, raw.data = FALSE){
  info <- epx.extract(x)
  dat <- lapply(info[[7]], epx.read)

  # Combine dat and info in a data-set-wise list
  perDataSet <- mapply(function(dat, info) list(list(dat = dat, info = info)), dat, info[[7]])
  
  suppressWarnings(
    if(raw.data == FALSE){
      dat2 <- lapply(perDataSet, function(x) epx.missing(x$dat, x$info))
      perDataSet <- mapply(function(dat, info) list(list(dat = dat, info = info)), dat2, info[[7]])
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
  
  
  if(length(dat2) == 1){
    dat2 <- dat2[[1]]
  }

  # Extract study information
  infoStudy <- lapply(xml2::xml_children(info$infoStudy), xml2::xml_text)
  names(infoStudy) <- xml2::xml_name(xml2::xml_children(info$infoStudy))
  attributes(dat2)$study.info <- t(data.frame(infoStudy))
 
  # Collect relational information
  if(length(dat) > 1){
    attributes(dat2)$info.relations <- 
      data.frame(data.set = xml2::xml_attr(info$infoDataSets, "id"),
                 parent.data.set = unlist(info$infoParentDataSet),
                 key.variables = unlist(info$infoKeyVars))
  }

  dat2
}




##############
# The next step will be, to enable the reading of encrypted files