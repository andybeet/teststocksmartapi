#' Test API to pull data.
#'
#' Will use this in GtiHub action to pull and update the data on a monthly basis
#'
#'


library(magrittr)
query_single_stocksmart_api <- function(id) {


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
  assess$entityIdList <-paste(id,collapse = ",")

  jsonquery <- jsonlite::toJSON(assess, pretty = TRUE,auto_unbox = TRUE)

  file <- httr::GET("https://www.st.nmfs.noaa.gov/stocksmart/sis_servlet",
                    query=list(jsonParam = jsonquery))

  # pulls html content into a char
  assessChar <- base::rawToChar(file$content)
  # converts to a readable R object
  assessEntities <- jsonlite::fromJSON(assessChar)$asmtList

  # mainData <- allEntities %>%
  #   dplyr::left_join(.,assessEntities,by=c("id"="entity_id"))
  #
  #
  # asmtids <- assessEntities$asmt_id

  return(assessEntities)

}
