#' Growth data of 3 bacterial strains
#'
#' Example dataset of bacterial growth, recorded by measuring optical density at
#' 600 nm in a spectrophotometer (hours vs optical density). Contains data for the growth of 3 bacterial
#' strains (\code{strain1, strain2, strain3}) over 44 hours, with 3 replicates per
#' strain. The data is 48 observations x 4 variables and includes some NA values
#' to make things realistic.
#'
#' @author Angel Angelov
#'
#' @docType data
#'
#' @usage data("growthdata3")
#'
#' @format A data frame with 48 rows and 4 variables: \describe{
#'   \item{hours}{time after starting experiment, in hours}
#'   \item{strain1}{optical density values for strain1, measured at 600nm}
#'   \item{strain2}{optical density values for strain2, measured at 600nm}
#'   \item{strain3}{optical density values for strain3, measured at 600nm}}
#'
#' @keywords datasets
#'
#' @examples
#' data("growthdata3")
#'
#'
"growthdata3"
