% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCellContents.R
\name{getCellContents}
\alias{getCellContents}
\title{getCellContents -  Get information about container cell contents.}
\usage{
getCellContents(coreApi, containerBarcode, containerCellNum,
  useVerbose = FALSE)
}
\arguments{
\item{coreApi}{coreApi object with valid jsessionid}

\item{containerBarcode}{container barcode}

\item{containerCellNum}{container cel number as a string}

\item{useVerbose}{Use verbose communication for debugging}
}
\value{
RETURN returns a list $entity contains cell information, $response contains the entire http response
}
\description{
\code{getCellContents} - Get information about container cell contents.
}
\details{
\code{getCellContents} puts a cell lot in a container cell
}
\examples{
\dontrun{
api<-CoreAPI("PATH TO JSON FILE")
login<- CoreAPI::authBasic(api)
cell<-getCellContents(login$coreApi,"VIA9","1")
logOut(login$coreApi )
}
}
\author{
Craig Parman
}
