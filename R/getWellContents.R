#' getWellContents -  Gets content information of a single container well.
#'
#' \code{getWellContents} Gets content information of a single container well.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerBarcode container barcode
#' @param containerWellNum number location of container's well
#' @param containerType entity type of container (default: "CONTAINER")
#' @param useVerbose  Use verbose communication for debugging (default: FALSE)
#' @export
#' @return RETURN returns a list $entity contains well information, $response contains the entire http response
#' @examples
#' \dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' well<-CoreAPIV2::getWellContents(login$coreApi,"VIA9","1", "VIAL")
#' CoreAPIV2::logOut(login$coreApi)
#' }
#' @author Craig Parman ngsAnalytics, ngsanalytics.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code{getWellContents} - Gets content information of a single container well.

getWellContents <-
  function(coreApi,
             containerBarcode,
             containerWellNum,
             containerType = "CONTAINER",
             useVerbose = FALSE) {
    containerType <- CoreAPIV2::odataCleanName(containerType)
    containerWellNum <- as.numeric(containerWellNum)
    resource <- "CELL"

    cellId <- CoreAPIV2::getContainerCellIds(coreApi, containerBarcode, containerType, useVerbose)$entity[containerWellNum]

    CoreAPIV2::case(
      grepl("[0-2]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        query <- paste0("(", cellId, ")?$expand=CONTENT($expand=IMPL_SAMPLE_LOT)")
      },
      grepl("[3-9]+\\.[0-9]+\\.[0-9]+", coreApi$semVer) ~ {
        query <- paste0("(", cellId, ")?$expand=CELL_CONTENTS($expand=SAMPLE_LOT)")
      }
    )

    header <-
      c("Content-Type" = "application/json;odata.metadata=minimal", Accept = "application/json")

    response <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )

    response <-
      list(entity = response$content, response = response$response)
  }
