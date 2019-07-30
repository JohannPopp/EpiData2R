# EpiData2R

These R-functions are built to read EpiDatas epx-files directly into R. It already exists an R-package "epxToR" for this task, but it does not work on my system and it seems not to be updated any more. Because of this, I started to program my own functions.

The main function is `read.EpiData()` in the file "readEpiData.R". The functions `epx.extract()` (in "extractInformation.R"), `epx.read()` (in "readIntoDataFrame.R"), `epx.missing()` (in "missingValues.R") and `epx.class()` (in "convertToClasses.R") are helper functions, that perform single parts of the entire process. In a later stage, they will be integratet into the main functions.

The functions transforms epx-files into an R-`data.frame`. Defined missings will be set to `NA` and variables will be contvertet to R-classes according to their field type. If this does not work properly for some reasons, the original codes can be loaded into the `data.frame` as `character`-variables, by setting the argument `raw.data = TRUE`.

If the epx-file consists of one data set only, the output will be a data.frame. If the epx-file contains a relational data base with multiple data sets, it will be a list of multiple data.frames.

Variable labels, study information and information about the relation of data sets are given as attributes.

The function works fine with the example files "Beispielprojekt.epx", "marathon.epx", "sample.v3.epx" and "Clinical_Example.epx". Further testing is needed and I would appreciate feed back.

The function can not jet read encrypted epx-files. I would need much more expertise in cryptography to implement this feature. Perhaps there are some experts to help me out.