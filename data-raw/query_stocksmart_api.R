#' Test API to pull data.
#'
#' Will use this in GtiHub action to pull and update the data on a monthly basis
#'
#'


library(magrittr)
##  Get all entries
allids <- list()
allids$method <- "searchEntities"
allids$entName <- "%"
allids$scId <- "%"
allids$jurId <- "%"
allids$fmpId <- "%"
allids$ecoId <- "%"
jsonquery <- jsonlite::toJSON(allids, pretty = TRUE,auto_unbox = TRUE)

# url query
file <- httr::GET("https://www.st.nmfs.noaa.gov/stocksmart/sis_servlet",
                  query=list(jsonParam = jsonquery))

# pulls html content into a char
allEntitiesChar <- base::rawToChar(file$content)
# converts to a readable R object
allEntities <- jsonlite::fromJSON(allEntitiesChar)$data

stockids <- allEntities$id


## pull all id's assessment data
# url query
assess <- list()
assess$method <- "getAsmtYearsWithTimeSeriesDataForEntityList"
assess$entityIdList <-paste(c("10455","10026,10736"),collapse = ",")
assess$entityIdList <-paste(stockids,collapse = ",")

jsonquery <- jsonlite::toJSON(assess, pretty = TRUE,auto_unbox = TRUE)

file <- httr::GET("https://www.st.nmfs.noaa.gov/stocksmart/sis_servlet",
                  query=list(jsonParam = jsonquery))

# pulls html content into a char
assessChar <- base::rawToChar(file$content)
# converts to a readable R object
assessEntities <- jsonlite::fromJSON(assessChar)$asmtList

mainData <- allEntities %>%
  dplyr::left_join(.,assessEntities,by=c("id"="entity_id")) %>%
  dplyr::filter(!is.na(asmt_id)) %>%
  dplyr::arrange(name)


asmtids <- mainData %>%
  dplyr::pull(asmt_id)

## pull timeseries info

# url query
# ts <- list()
# ts$method <- "sortTimeSeriesForAsmtsByEntityName"
# ts$asmtIdsStr <- paste(c("11022","3895"),collapse = ",")
#
# jsonquery <- jsonlite::toJSON(ts, pretty = TRUE,auto_unbox = TRUE)
#
# file <- httr::GET("https://www.st.nmfs.noaa.gov/stocksmart/sis_servlet",
#                   query=list(jsonParam = jsonquery))
#
#
# # pulls html content into a char
# tsChar <- base::rawToChar(file$content)
# # converts to a readable R object
# tsEntities <- jsonlite::fromJSON(tsChar)

## pull time series data
# url query
excel <- list()
#excel$asmtList <- paste(c("10643","7920"),collapse = ",")
excel$dataType <- "TimeSeriesData"
excel$dataFormat <- "excel"
excel$partIndex <- "1"
excel$categories <- c("Catch","Abundance","Fmort","Recruitment")
excel$minYear <- "1872"
excel$maxYear <- "2032"

n <- 200
for (ifile in 1:ceiling(length(asmtids)/n)) {
  print(ifile)
  filenumber <- sprintf("%02d",ifile)

  start <- n*(ifile-1) + 1
  fin <- ifile*n

  idds <- asmtids[start:fin]
  idds <- idds[!is.na(idds)]

  excel$asmtList <- paste(idds,collapse = ",")

  jsonquery <- jsonlite::toJSON(excel, pretty = TRUE,auto_unbox = TRUE)

  file <- httr::GET("https://www.st.nmfs.noaa.gov/stocksmart/data-export-servlet",
                    query=list(jsonParam = jsonquery))


  httr::GET(file$url,httr::write_disk(path=here::here(paste0("data-raw/test2/",filenumber,".xlsx")),overwrite=T))

}
