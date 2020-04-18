###########################################################
#' Saldae Engine using prophet package.
#' @author Farid Azouaou
#' @param tisefka  training data (data frame with time variable as rownames)
#' @param sald_holidays  information regarding holdays(based on country)
#' @param target_variable target variable
#' @return fitted object from Prophet

prophet_fit <- function(tisefka = NULL,target_variable= NULL ,sald_holidays = NULL) {
  # ..........Prepare data as required by the package .
  history <- data.frame(
    ds = prophet:::set_date(tisefka$date, tz = "CET"),
    y = dplyr::pull(tisefka,!!target_variable)
  )
  # .............................. model fitting step...........
  prophet_object <- prophet::prophet(df = history, fit = TRUE, holidays = sald_holidays)
  #------------------------------------------------------------
  return(prophet_object)
}
## End(Not run)

###########################################################
#' Saldae Engine using prophet package.
#' @author Farid Azouaou
#' @param prophet_object : fitted object from Prophet
#' @param ukud_unit : time units
#' @param asurif_zdat : steps ahead (forecast horizon)
#' @return forecast object (fitted value, forecast ,actuals , upper and lower bounds)

prophet_forecast_f <- function(prophet_object = NULL, asurif_zdat = 10, ukud_unit = NULL) {
  # .............................. make future data frame based on forecast horizon
  prphet_freq <- c("day", "week", "month", "quarter", "year", "1", 60, 3600)
  names(prphet_freq) <- c("days", "weeks", "months", "quarters", "years", "seconds", "minutes", "hours")

  # ukud_unit<- detect_date_auto(time_vect = tisefka$date,
  #                              n_new_dates = NULL)
  # future = data.frame(ds = ukud_unit)
  future <- prophet::make_future_dataframe(prophet_object, periods = asurif_zdat, freq = ukud_unit, include_history = TRUE)
  # ................. forecast prediction......................
  attr(future$ds, "tzone") <- "CET"
  attr(prophet_object$history$ds, "tzone") <- "CET"
  prophet_forecast <- predict(prophet_object, future)
  # .................. generate plot of the forecast object
  prophet_output <- list()
  prophet_output$method <- "Sadlae_prophet"
  prophet_output$mean <- tail(prophet_forecast$yhat, asurif_zdat)

  names(prophet_output$mean) <- as.POSIXct(tail(future$ds, asurif_zdat), tz = "CET")
  prophet_output$upper <- tail(prophet_forecast$yhat_upper, asurif_zdat)
  prophet_output$lower <- tail(prophet_forecast$yhat_lower, asurif_zdat)
  names(prophet_output$upper) <- names(prophet_output$lower) <- names(prophet_output$mean)
  #-----------------------------------------------------
  prophet_output$x <- prophet_object$history$y
  names(prophet_output$x) <- paste0(as.POSIXct(prophet_object$history$ds, tz = "CET"))
  prophet_output$fitted <- prophet_forecast$trend
  class(prophet_output) <- "forecast"
  return(prophet_output)
}
###########################################################
#' Saldae Engine(main) using prophet package.
#' @author Farid Azouaou
#' @param tisefka  training data (data frame with time variable as rownames)
#' @param sald_holidays  information regarding holdays(based on country)
#' @param asurif_zdat  steps ahead (forecast horizon)
#' @param target_variable target variable
#' @return forecast object.
#' @export

Saldae_prophet <- function(tisefka = NULL ,sald_holidays = NULL, asurif_zdat = 10) {
  ukud_unit <- SaldaeDataExplorer::detect_date_auto(
    time_vect = tisefka$date,
    n_new_dates = NULL
  )
  original_variable <- colnames(tisefka)[2]
  training_variable <- base::ifelse("Corrected"%in%colnames(tisefka),"Corrected",original_variable)

  prophet_object <- prophet_fit(tisefka = tisefka,target_variable = training_variable ,sald_holidays = sald_holidays)
  prophet_output <- prophet_forecast_f(prophet_object = prophet_object, ukud_unit = ukud_unit, asurif_zdat = asurif_zdat)
  prophet_output$ts_original <-tisefka[,c("date",original_variable)]
  return(prophet_output)
}



###########################################################
#' Saldae Engine(main) using prophet package.
#' @author Farid Azouaou
#' @param tisefka_list  training data (list of tibble objects (target variable corrected data date))
#' @param sald_holidays  information regarding holdays(based on country)
#' @param asurif_zdat  steps ahead (forecast horizon)
#' @param target_variables target variables
#' @return forecast object.

Saldae_prophet_multiple <- function(tisefka_list= NULL ,sald_holidays = NULL, asurif_zdat = 10){
  SA_fcast_object <- purrr::map(.x = tisefka_list,~Saldae_prophet(tisefka = .x,asurif_zdat = asurif_zdat))
  return(SA_fcast_object)
}

