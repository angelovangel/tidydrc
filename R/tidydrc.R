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
#' @usage tidydrc(data, dose, response, model, ...)
#'
#' @examples
#' ### Logistic regression of bacterial growth data ###
#' # using the built-in dataset growthdata3
#' data(growthdata3)
#' gdata <- growthdata3 %>% gather(strain, value, strain1:strain3)
#' llm.1 <- gdata %>% tidydrc(dose = hours, response = value, model = LL.4(), strain)
#' # get the coefficients of the model(s)
#' llm.1 %>% unnest(coefs)
#' # make a ggplot, facetting by strain
#' llm.1 %>% tidydrc_plot(confint = TRUE) + facet_grid(.~strain)
#'
#' ### Using the S.alba dataset from drc ###
#' llm <- tidydrc(S.alba, Dose, DryMatter, model = LL.4(), Herbicide)
#' llm %>% unnest(coefs)
#' # estimate the effective doses for several response levels, as a tibble
#' names(llm$drmod) <- as.character(llm$Herbicide)
#' EDs <- map(llm$drmod, ED, c(10, 50, 90))
#' map_df(EDs, as_tibble, rownames = "level", .id = "Herbicide")
#'
#' ### Fit a Michaelis-Menten kinetics model ###
#' # for the built-in Puromycin dataset and get the Km values with std. error
#' mm <- tidydrc(Puromycin, conc, rate, model = MM.3(), state)
#' names(mm$drmod) <- as.character(mm$state)
#' map(mm$drmod, ED, 50)
#'
#'
#' @seealso \link[tidydrc]{tidydrc_plot}
#' @export



# the fourth (and subsequent) arguments can be used for grouping by sample, treatment etc. before modelling
# if the fourth argument is not supplied, it fits a drm to all the dose-response values


# for example
# do_drm(S.alba, Dose, DryMatter, Herbicide)


####=====================
 tidydrc <- function(data, dose, response, model = LL.4(), ...) {

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
    dose = seq(min(data$d, na.rm = TRUE) - (0.1 * min(data$d, na.rm = TRUE)),
               max(data$d, na.rm = TRUE) + (0.1 * max(data$d, na.rm = TRUE)),
               length.out = length(data$d)
               )
            )

  predict.fun <- function(x) {
    cbind(modelr::add_predictions(preddf, x),
          as.tibble(predict(x, newdata = preddf, interval = "confidence"))
          )
  }

 coefs.fun <- function(x) {
   coef(x) %>% tibble(parameter = names(.), value = .) # instead of tidy()
   }

 data %>% group_by(...) %>%
          tidyr::nest() %>%
          mutate(
            drmod = purrr::map(data, drm.func),
            pred = purrr::map(drmod, predict.fun),
            coefs = purrr::map(drmod, coefs.fun)
            #predint = purrr::map(drmod, function(x) as.tibble(predict(x, newdata = preddf, interval = "confidence")))
        )

 }
