#' Growth data of 3 bacterial strains (hours vs optical density)
#'
#' Example dataset of bacterial growth, recorded by measuring optical density at
#' 600 nm in a spectrophotometer. Contains data for the growth of 3 bacterial
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
#'   \item{strain1}{optical density values for strain1, measured at 600nm} ... }
#'
#' @keywords datasets
#'
#' @examples
#' data("growthdata3")
#'
#' # make it long, for using it in tidydrc()
#' growthdata3 %>% gather(sample, value, -hours)
#'
#'
"growthdata3"
