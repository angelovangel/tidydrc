#' Tidy wrapper for \code{drc}
#'
#' Tidy modelling of dose-response relationships with the \code{drc} package.
#'
#' This function is a tidy wrapper for the dose-response modeling functions of the \code{drc} package.
#' By default, the four-parameter log-logistic function \code{LL.4()} is used. All other
#' functions, implemented in \code{drc}, can be used when provided as the \code{model} parameter
#' to this function. The function returns a dataframe with list-columns (the data,
#' predictions and coefficients). It is thus easy to implement in tidy
#' workflows.
#'
#' @author Angel Angelov
#'
#' @param data dataframe with two or more variables, organized in a tidy way
#' @param dose Numeric, the dose variable (could be time, amount of some compound, ...)
#' @param response Numeric, the response variable (for example population size, optical
#'   density, ...)
#' @param model The model function from \code{drc}. Default is \code{LL.4()}
#' @param ... Variables to group by
#'
#' @return A tibble with list-columns containing the data, the predictions and the coefficients of the model.
#'
#' @usage tidydrc_model(data, dose, response, model, ...)
#'
#' @examples
#' ### Logistic regression of bacterial growth data ###
#' # using the built-in dataset growthdata3
#' library(tidyverse)
#' data(growthdata3)
#'
#' gdata <- growthdata3 %>% gather(strain, value, strain1:strain3)
#' llm.1 <- gdata %>% tidydrc_model(dose = hours, response = value, model = LL.5(), strain)
#' # get the coefficients of the model(s)
#' llm.1 %>% unnest(coefs)
#' # make a ggplot, facetting by strain
#' llm.1 %>% tidydrc_plot(confint = TRUE) + ggplot2::facet_grid(. ~ strain)
#'
#' ### Using the S.alba dataset from drc ###
#' llm <- tidydrc_model(S.alba, Dose, DryMatter, model = LL.4(), Herbicide)
#' llm %>% unnest(coefs)
#' # estimate the effective doses for several response levels, as a tibble
#' names(llm$drmod) <- as.character(llm$Herbicide)
#' map(llm$drmod, ED, c(10, 50, 90)) %>% map_df(as_tibble, rownames = "level", .id = "Herbicide")
#'
#' ### Fit a Michaelis-Menten kinetics model ###
#' # for the built-in Puromycin dataset and get the Km values with std. error
#' mm <- tidydrc_model(Puromycin, conc, rate, model = MM.3(), state)
#' names(mm$drmod) <- as.character(mm$state)
#' map(mm$drmod, ED, 50) %>% map_df(as_tibble, .id = "sample")
#'
#'
#' @seealso \link[tidydrc]{tidydrc_plot}
#' @export


 tidydrc_model <- function(data, dose, response, model = LL.4(), ...) {

 # using eclipsis (...) for group_by(...), so that arbitrary number of grouping variables can be used
 # like this dplyr works in the function, no need for quosure and !!! WHY??

  # rename columns to dose and response in order for the drm to work (formula interface problems)
  # my approach (to create new variables before modelling) is much fastet than using classical NSE approach, e.g.
  # f <- substitute(r ~ d) and later drm(eval(f), ...)
 # would something like eval(substitute(d), df, parent.frame()) work?
 d <- deparse(substitute(dose))
 r <- deparse(substitute(response))

 data$d <- data[[d]] # e.g. data[["time"]]
 data$r <- data[[r]]

  drm.func <- function(x) {
      drc::drm(r ~ d,
      fct = model,
      data = x)
  }

  # dataframe for predictions
  # add 10% below and above the data to the predictions
  preddf <- data.frame(
    dose = lseq(
      from = ifelse(min(data$d) != 0, min(data$d, na.rm = TRUE) - (0.1 * min(data$d, na.rm = TRUE)), 1),
      to = max(data$d, na.rm = TRUE) + (0.1 * max(data$d, na.rm = TRUE)),
      length.out = length(data$d)
               )
            )

  predict.fun <- function(x) {
    cbind(modelr::add_predictions(preddf, x),
          dplyr::as_tibble(predict(x, newdata = preddf, interval = "confidence"))
          )
  }

 coefs.fun <- function(x) {
   coef(x) %>% dplyr::tibble(parameter = names(.), value = .) # instead of tidy()
   }

 data %>% dplyr::group_by(...) %>%
          tidyr::nest() %>%
          dplyr::mutate(
            drmod = purrr::map(data, drm.func),
            pred = purrr::map(drmod, predict.fun),
            coefs = purrr::map(drmod, coefs.fun)
            #predint = purrr::map(drmod, function(x) as.tibble(predict(x, newdata = preddf, interval = "confidence")))
        )

 }
