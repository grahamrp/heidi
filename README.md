<!-- README.md is generated from README.Rmd. Please edit that file -->
heidi
=====

The `heidi` package is a collection of R functions to access the [HEIDI API](http://www.heidi.ac.uk/index.php/content/view/39/87/) from [HESA](www.hesa.ac.uk), which provides access to higher education datasets for HEIDI subscribers.

The main benefits of using the API in R, rather than the HEIDI web interface are

-   Easy to select data broken down by several dimensions (no more repetitive drilling through the data explorer)
-   The data is provided in a 'long' rather than 'wide' format, making it easier to use later, e.g. in Excel pivot tables
-   Column headings are shorter and easier to understand
-   Data downloads can be scripted, for example to download multiple years of data

Installing
----------

To install the heidi package run the following commands in an R session.

``` r
# If you don't already have the devtools package, install it
install.packages("devtools")
# Use devtools to install heidi directly from github
# If you are behind a proxy server uncomment and run the two lines below
# library(httr)
# set_config(use_proxy(url = 'myproxy.ac.uk', 8080))
devtools::install_github("grahamrp/heidi", build_vignettes = TRUE)

# For more details on how to use the package view the quickstart vignette
browseVignettes(package = "heidi")
```

Usage
-----

Use `browseVignettes(package = "heidi")` for an introduction to the package and a detailed example.

This is a shorter example that downloads a dataset of HE student counts in 2012/13 for each institution and subject.

``` r
library(heidi)
# Setup authentication using your organisation's HEIDI API key
base64userpass <- setup('1234', '1f1f1f1f-2e2e-3d3d-4c4c-5b5b5b5b5b5b')

# Retrieve identifiers for Row Type, Value Type, Year and Field
id_rowtype <- subset(ListRowTypes(), 
                     Label == 'Institution', select = 'Id', drop = TRUE)
id_year <- subset(ListYears(rowtype = id_rowtype),
                  Label == '2012/13', select = 'Id', drop = TRUE)
id_valtype <- subset(ListValueTypes(rowtype = id_rowtype),
             Label == 'HE students Full-person equivalent', select = 'Id', drop = TRUE)
id_field <- subset(ListFields(rowtype = id_rowtype, year = id_year, valuetype = id_valtype),
                   Label == 'JACS subject area', select = 'Id', drop = TRUE)

# Get the data
heidi_data <- GetData(rowtype = id_rowtype,
                      year = id_year,
                      valuetype = id_valtype,
                      field1 = id_field)

# Write the data to a file
write.csv(heidi_data, file = 'fpe_by_inst_and_subject_12-13.csv')
```
