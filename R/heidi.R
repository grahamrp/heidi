#' Setup HEIDI authentication
#'
#' \code{setup} converts the supplied heidi organisation identifier (\code{userid}) and API key (\code{password}) into its base64 equivalent, which is then used to authenticate subsequent API calls. The returned value must be saved into a variable called "base64userpass" which is accessed by other functions in the \pkg{heidi} package.
#' @param userid Your 4-digit heidi organisation identifier. If you don't know it it is available from the HEIDI local administrator web interface. It appears under "Manage API Keys" once a key has been created within the web interface, under the "Organisation" field.
#' @param password This is the "API Key" specific to your organisation. The HEIDI local administrator may need to create an API key from within the HEIDI web interface.
#' @examples
#' base64userpass <- setup('1234', '1f1f1f1f-2e2e-3d3d-4c4c-5b5b5b5b5b5b')
setup <-function(userid,password) {
  ## base64 encode of user-pass
  return(RCurl::base64(paste(userid,password,sep=":")))
}

#' Call HEIDI API
#'
#' \code{callAPI} passes the supplied URL to HEIDI, authenticating using the global environment variable \code{base64userpass}, which should have already been assigned a value from \code{\link{setup}}. This function is used by the other \pkg{heidi} functions and should not normally be called directly by a user.
#' @param passURL A string containing a HEIDI URI and parameters, usually assembled by other \pkg{heidi} functions.
#' @keywords internal
callAPI <- function(passURL) {
  resp <- httr::GET(passURL, httr::add_headers(Authorization = paste("Basic  ",base64userpass,sep="")))
  if (resp$status_code==401) {
    stop("Authentication failed. Please check your API key and/or HEIDI organisation identifier")
  }
  if (resp$status_code==400) {
    print(resp)
    stop("A bad request was made. See response above from the API for more information.")
  }
  if (resp$status_code==200) {
    XMLdata <- httr::content(resp,as="parsed",type="text/xml", encoding="utf-8")
    return(XMLdata)
  }
}

#' List available Row Types
#'
#' \code{ListRowTypes} fetches the names and identifiers of the available row types and returns them in a dataframe.
#'
#' A Row Type is a key comparison dimension that occurs in many underlying tables. Examples are: Institution, Cost Centre.
#' @param year Optionally supply a year identifier to filter the list of available row types for that year.
#' @examples
#' \dontrun{
#' RowTypes <- ListRowTypes(year = '68682')  # 'year' refers to the year id
#' }
ListRowTypes <- function(year="") {
  passURL <- "https://heidi.hesa.ac.uk/api/1.0/rowtypes"
  if (year > 0) {
    passURL <- paste(passURL,"?year=",year,sep="")
  }
  XMLdata <- callAPI(passURL)
  XMLdata <- XML::getNodeSet(XMLdata, "//RowType")
  XMLdata <- data.frame(Id=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Id")),
                        Label=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Label")),
                        stringsAsFactors = FALSE)
  return(XMLdata)
}

#' List available Years
#'
#' \code{ListYears} fetches the names and identifiers of the available years and returns them in a dataframe.
#'
#' A Year is an academic year. Examples are: 2008/09, 2009/10.
#' @param rowtype Optionally supply a row type identifier to filter the list of available years for that row type.
#' @examples
#' \dontrun{
#' Years <- ListYears(rowtype = '3297')  # 'rowtype' refers to the row type id
#' }
ListYears <- function(rowtype="") {
  passURL <- "https://heidi.hesa.ac.uk/api/1.0/years"
  if (rowtype > 0) {
    passURL <- paste(passURL,"?rowtype=",rowtype,sep="")
  }
  XMLdata <- callAPI(passURL)
  XMLdata <- XML::getNodeSet(XMLdata, "//Year")
  XMLdata <- data.frame(Id=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Id")),
                        Label=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Label")),
                        stringsAsFactors = FALSE)
  return(XMLdata)
}

#' List available Domains
#'
#' \code{ListDomains} fetches the names and identifiers of the available domains and returns them in a dataframe.
#'
#' Domains are just a categorisation of Value Types to allow filtering of the hundreds of Value Types. Examples are: HESA Student, HESA Staff.
#' @examples
#' \dontrun{
#' Domains <- ListDomains()
#' }
ListDomains <- function() {
  passURL <- "https://heidi.hesa.ac.uk/api/1.0/domains"
  XMLdata <- callAPI(passURL)
  XMLdata <- XML::getNodeSet(XMLdata, "//Domain")
  XMLdata <- data.frame(Id=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Id")),
                        Label=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Label")),
                        stringsAsFactors = FALSE)
  return(XMLdata)
}

#' List available Value Types
#'
#' \code{ListValueTypes} fetches the names and identifiers of the available value types and returns them in a dataframe. Ordinarily, ids for row type, year and domain are provided to filter the list of value types.
#'
#' A Value Type is a measure. Examples are: Headcount, Full Time Equivalents, UCAS Accepted Applicants.
#' @param rowtype Supply a row type identifier to filter the list of available value types.
#' @param year Supply a year identifier to filter the list of available value types.
#' @param domain Supply a domain identifier to filter the list of available value types.
#' @examples
#' \dontrun{
#' ValueTypes <- ListValueTypes(rowtype = '3297', domain = '3308')
#' # Note, 'year' is unspecified to retrieve value types for all years
#' # applicable to the specified rowtype and domain
#' }
ListValueTypes <- function(rowtype="",year="",domain="") {
  passURL <- "https://heidi.hesa.ac.uk/api/1.0/valuetypes"
  params <- data.frame(param=c("rowtype=","year=","domain="),value=c(rowtype,year,domain),stringsAsFactors=FALSE)
  params <- paste(params[params$value>0,"param"],params[params$value>0,"value"],sep="")
  params <- gsub(", ","&",toString(params))
  if (params > 0) {
    passURL <- paste(passURL,"?",params,sep="")
  }
  XMLdata <- callAPI(passURL)
  XMLdata <- XML::getNodeSet(XMLdata, "//ValueType")
  XMLdata <- data.frame(Id=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Id")),
                        Label=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Label")),
                        stringsAsFactors = FALSE)
  return(XMLdata)
}

#' List available Fields
#'
#' \code{ListFields} fetches the names and identifiers of the available fields and returns them in a dataframe. Ordinarily, ids for row type, year and value type/domain are provided to filter the list of fields.
#'
#' A Field is dimension that breaks down a value type. For example, the value type 'HE students Full-time equivalent' could be broken down by the fields Subject, Gender, Nationality, Mode of Study.
#' @param rowtype Supply a row type identifier to filter the list of available fields.
#' @param year Supply a year identifier to filter the list of available fields.
#' @param valuetype Supply a value type identifier to filter the list of available fields.
#' @param domain Supply a domain identifier to filter the list of available fields. Not necessary if a value type is provided.
#' @examples
#' \dontrun{
#' Fields <- ListFields(rowtype = '3297', year = '68682', valuetype = '3517-3508')
#' }
ListFields <- function(rowtype="",year="",domain="",valuetype="") {
  passURL <- "https://heidi.hesa.ac.uk/api/1.0/fields"
  params <- data.frame(param=c("rowtype=","year=","domain=","valuetype="),value=c(rowtype,year,domain,valuetype),stringsAsFactors=FALSE)
  params <- paste(params[params$value>0,"param"],params[params$value>0,"value"],sep="")
  params <- gsub(", ","&",toString(params))
  if (params > 0) {
    passURL <- paste(passURL,"?",params,sep="")
  }
  XMLdata <- callAPI(passURL)
  XMLdata <- XML::getNodeSet(XMLdata, "//Field")
  XMLdata <- data.frame(Id=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Id")),
                        Label=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Label")),
                        stringsAsFactors = FALSE)
  return(XMLdata)
}

#' Get field info
#'
#' \code{GetField} fetches additional information about a given field.
#' @keywords internal
GetField <- function(fieldID) {
  passURL <- "https://heidi.hesa.ac.uk/api/1.0/field"
  passURL <- paste(passURL,"?field=",fieldID,sep="")
  XMLdata <- callAPI(passURL)
  XMLdata <- XML::getNodeSet(XMLdata, "//FieldCode")
  XMLdata <- data.frame(Id=sapply(XMLdata, function(x) XML::xmlValue(x)),
                        Label=sapply(XMLdata, function(x) XML::xmlGetAttr(x, "Label")),
                        stringsAsFactors = FALSE)
  return(XMLdata)
}

#' Get field info (XML)
#'
#' \code{GetField} fetches additional information about a given field and returns the result in the raw XML form.
#' @keywords internal
GetFieldXML <- function(fieldID) {
  passURL <- "https://heidi.hesa.ac.uk/api/1.0/field"
  passURL <- paste(passURL,"?field=",fieldID,sep="")
  XMLdata <- callAPI(passURL)
  return(XMLdata)
}

#' Get HEIDI data
#'
#' \code{GetData} retrieves the specified dataset from HEIDI. Ids for row type, year and value type are used to specify the required data, with up to five fields (field1, field 2, etc.) being provided to break the data down into further dimensions.
#'
#' \code{GetData} retrieves data for the specified \code{year} and \code{valuetype} (e.g. 2011/12 Student FTEs). The value type (which could be called a "measure") is then broken down into more detail by the \code{rowtype} and the optionally-supplied \code{fields}.
#'
#' Most of the functions in the \pkg{heidi} package exist to provide a means to query HEIDI and find out which datasets are available and to ascertain the specific ids of rowtypes, valuetypes, years and fields to feed to the GetData function.
#'
#' Note that the results are rounded by HEIDI (often to the nearest 5) so providing too many \code{fields} will increase the rounding error as the \code{valuetype} is broken down into smaller and smaller parts.
#' @param rowtype Supply a row type identifier specify the row type required. Must be compatible with the other arguments.
#' @param year Supply a year identifier to retrieve the value type for this year.
#' @param valuetype Supply a value type identifier to specify the measure required. Must be compatible with the other arguments.
#' @param field1,field2,field3,field4,field5 Optionally supply up to five fields with which to break down the value type. Must be compatible with the other arguments. \strong{A field id for a particular field is not consistent across years}.
#' @examples
#' \dontrun{
#' # Get HE students FTEs for each institution for 2012/13 broken down by course year
#' heidi_data <- GetData(rowtype = '3297', year = '68682', valuetype = '3517-3508', field1 = '70469')
#' }
#' @return Returns a dataframe containing the specified data. The data is in 'long' rather than 'wide' format, i.e. with one row per rowtype/field combination.
GetData <- function(rowtype,year,valuetype,field1="",field2="",field3="",field4="",field5="") {

  # --- Assemble and send query and store the results --------------------------
  passURL <- "https://heidi.hesa.ac.uk/api/1.0/datareport"
  params <- data.frame(param=c("rowtype=","year=","valuetype=","field=","field=","field=","field=","field="),value=c(rowtype,year,valuetype,field1,field2,field3,field4,field5),stringsAsFactors=FALSE)
  params <- paste(params[params$value>0,"param"],params[params$value>0,"value"],sep="")
  params <- gsub(", ","&",toString(params))
  passURL <- paste(passURL,"?",params,sep="")
  XML <- callAPI(passURL)

  # --- Convert XML into a dataframe -------------------------------------------
  # The XML a data specification section detailing each type of item in the
  # dataset, including their ids and names, which are used for the column names
  # A dataset will contain a minimum of 3 types of item, for the RowType,
  # ValueType and Year. There could be 0 to 8 additional "Field" items,
  # depending on the query.
  # The RowType, ValueType and Year are extracted into a dataframe first,
  # then (if present) the  # Fields are extracted and appended to the dataframe
  # as columns

  # Extract DataReportSpecification and store in a dataframe
  XMLSpec <- XML::getNodeSet(XML, "//DataReportSpecification")
  DataSpec <- data.frame(XML::xmlApply(XMLSpec[[1]], XML::xmlAttrs), stringsAsFactors=FALSE)

  # Extract the RowType and ValueType from the XML.
  # Each query can only return one year of data, so the Year can just be taken
  # from the Data Specification rather than the parsing the whole dataset
  RowTypeValue <- XML::xpathSApply(XML, '//RowTypeCode', XML::xmlValue)  # e.g. an Institution UKPRN
  RowTypeLabel <- XML::xpathSApply(XML, '//RowTypeCode/@Label')     # e.g. an Institution name
  Value <- XML::xpathSApply(XML, '//Value', XML::xmlValue)  # This is the dataset's measure

  # Create a dataframe with the RowType, ValueType and Year data
  data <- data.frame(
    RowTypeValue,
    RowTypeLabel,
    DataSpec[["Label","Year"]],  # Add the Year using the Data Specification
    Value,
    stringsAsFactors = FALSE)
  # Rename the columns, looking up the correct names from the Data Specification
  names(data) <- c(paste(DataSpec[["Label","RowType"]]),
                   paste(DataSpec[["Label","RowType"]],"_label",sep=""),
                   "Year",
                   paste(DataSpec[["Label","ValueType"]]))

  # Add Fields to the dataframe (if there are any)
  # If the Data Specification contains more than 3 items (i.e. more than
  # Row Type, Value Type and Year) then assume Fields are present
  if (length(DataSpec) > 3) {
    for (i in 1:(length(DataSpec) - 3)) {  # Fields occur first in the data spec
      # Construct an XPath expression to select field i
      f <- paste("//FieldCode[@FieldId=",DataSpec[["Id",i]],"]",sep="")
      # Extract the Field values (codes) and labels (descriptive names)
      FieldValue <- XML::xpathSApply(XML, f, XML::xmlValue)
      FieldLabel <- XML::xpathSApply(XML, f, XML::xmlGetAttr, 'Label')
      # Create a temporary dataframe with the Field data, rename the columns and
      # add them to the main dataframe
      fdata <- data.frame(FieldValue,
                          FieldLabel,
                          stringsAsFactors = FALSE)
      names(fdata) <- c(paste(DataSpec[["Label",i]]),
                        paste(DataSpec[["Label",i]],"_label",sep=""))
      data <- cbind(data,fdata)
    }
  }
  return(data)
}

