

upper_unit_summary <- function(actuals = NULL, fcast = NULL, time_unit = NULL, basis_unit = NULL) {
  forecast_dat <- data.frame(DATE = c(names(actuals), names(fcast)), Actuals = c(actuals, rep(NA, length(fcast))), Forecast = c(rep(NA, length(actuals)), fcast))
  if (time_unit == "seconds") {
    #--------- unit which helps to determine on which frequency to summarize
    unit_format <- "%Y-%m-%d %H:%M:%S"
    #----------tasetta is when the upper unit does not have enough data for the last point (10 months in the last year)
  }
  if (time_unit == "minutes") {
    #--------- unit which helps to determine on which frequency to summarize
    unit_format <- "%Y-%m-%d %H:%M"
  }
  if (time_unit == "hours") {
    unit_format <- "%Y-%m-%d %H"
  }
  if (time_unit == "days") {
    unit_format <- "%Y-%m-%d"
  }
  if (time_unit == "months") {
    unit_format <- "%Y-%m"
  }
  if (time_unit == "years") {
    unit_format <- "%Y"
  }
  #--------------------- average
  if (!time_unit %in% c("weeks", "quarters")) {
    time_vect_group <- format(as.POSIXct(forecast_dat$DATE, tz = "CET"), unit_format)
  } else if (time_unit == "quarters") {
    time_vect_group <- paste(format(as.POSIXct(forecast_dat$DATE, tz = "CET"), "%Y"), lubridate::quarter(forecast_dat$DATE))
  } else if (time_unit == "weeks") {
    time_vect_group <- paste(format(as.POSIXct(forecast_dat$DATE, tz = "CET"), "%Y"), lubridate::week(forecast_dat$DATE))
  } else {
    stop("time unit not found")
  }

  #----------tasetta is when the upper unit does not have enough data for the last point (10 months in the last year)
  tasetta <- detect_number_basis_units_in_upper_unit(upper_unit = time_unit, basis_unit = basis_unit, last_date = tail(as.POSIXct(forecast_dat$DATE, tz = "CET"), 1))

  if (length(unique(tail(time_vect_group, tasetta))) > 1) {
    tasetta_dat <- tail(time_vect_group, tasetta)
    incomplete <- length(which(tasetta_dat == tail(tasetta_dat, 1)))
    time_vect_group <- head(time_vect_group, -incomplete)
    forecast_dat <- head(forecast_dat, -incomplete)
  }

  #-----------------------------
  aqerru <- detect_number_basis_units_in_upper_unit(upper_unit = time_unit, basis_unit = basis_unit, last_date = head(as.POSIXct(forecast_dat$DATE[!is.na(forecast_dat$Forecast)], tz = "CET"), 1))
  if (length(unique(head(time_vect_group[!is.na(forecast_dat$Forecast)], aqerru))) > 1) {
    aqerru_data <- head(time_vect_group[!is.na(forecast_dat$Forecast)], aqerru)
    incomplete <- length(which(aqerru_data == tail(aqerru_data, 1)))
    last_fcast <- head(which(!is.na(forecast_dat$Forecast)), 1) - 1
    forecast_dat$Forecast[(last_fcast - incomplete):last_fcast] <- forecast_dat$Actuals[(last_fcast - incomplete):last_fcast]
  }
  #------------------------------
  upper_unit_dates <- sort(seq(from = length(forecast_dat$Forecast), to = length(na.omit(forecast_dat$Actuals)) + 1, by = -length(tasetta)))
  upper_unit_dates <- as.POSIXct(as.matrix(forecast_dat$DATE[upper_unit_dates]), tz = "CET")


  upper_summary_mean <- forecast_dat %>%
    dplyr::mutate(upper_unit = time_vect_group) %>%
    dplyr::group_by(upper_unit) %>% # group by the day column
    dplyr::summarise(total_precip = mean(Forecast)) %>% # calculate the SUM of all precipitation that occurred on each day
    na.omit()
  upper_summary_mean <- upper_summary_mean %>% dplyr::mutate(DATE = tail(upper_unit_dates, nrow(upper_summary_mean)))

  #---------------------Sum
  upper_summary_sum <- forecast_dat %>%
    dplyr::mutate(upper_unit = time_vect_group) %>%
    dplyr::group_by(upper_unit) %>% # group by the day column
    dplyr::summarise(total_precip = sum(Forecast)) %>% # calculate the SUM of all precipitation that occurred on each day
    na.omit()
  upper_summary_sum <- upper_summary_sum %>% dplyr::mutate(DATE = tail(upper_unit_dates, nrow(upper_summary_sum)))
  #-------------------------
  colnames(upper_summary_sum) <- c("Time", "Sum", "DATE")
  colnames(upper_summary_mean) <- c("Time", "Average", "DATE")
  output <- list()
  output[["summary_sum"]] <- upper_summary_sum
  output[["summary_mean"]] <- upper_summary_mean
  #----------------------
  return(output)
}

#----------------- fcast summary wrapper

fcast_summary_wrapper <- function(fcast_object = NULL) {
  #----------------------------------
  upper_time_units <- SaldaeDataExplorer::possible_units_for_summary(time_vect = names(fcast_object$mean))
  fcast_summary <- lapply(upper_time_units, function(x) upper_unit_summary(actuals = fcast_object$x, fcast = fcast_object$mean, time_unit = x, basis_unit = upper_time_units[1]))
  names(fcast_summary) <- upper_time_units
  #----------------------------------
  return(fcast_summary)
}

#' Saldae convert fcast  object into data frame
#' @description t.b.d
#' @author Farid Azouaou
#' @param fcast_object forecast object
#' @param asurif_arzdat forecast horizon
#' @return tibble object containing actuals , corrected, forecast and prediction bands(upper and lower)
#' @export

sbed_forecast_aqerru <- function(fcast_object = NULL, asurif_arzdat = NULL,TimeZone= "CET") {
  if (!is.null(asurif_arzdat)) {
    fcast_object$mean <- head(fcast_object$mean, asurif_arzdat)
    fcast_object$upper <- head(fcast_object$upper, asurif_arzdat)
    fcast_object$lower <- head(fcast_object$lower, asurif_arzdat)
  }
  fcast_date <- as.POSIXct(c(as.character(fcast_object$ts_original$date), names(fcast_object$mean)),tz =TimeZone)
  corrected <- c(fcast_object$x, rep(NA, times = length(fcast_object$mean)))
  actuals <- c(dplyr::pull(fcast_object$ts_original,2), rep(NA, times = length(fcast_object$mean)))
  forecast <- c(rep(NA, times =nrow(fcast_object$ts_original)), fcast_object$mean)
  ufella <- c(rep(NA, times = nrow(fcast_object$ts_original)), fcast_object$upper)
  adda <- c(rep(NA, times =nrow(fcast_object$ts_original)), fcast_object$lower)
  fcast_df <- dplyr::bind_cols(date = fcast_date, actuals = actuals, corrected = corrected, forecast = forecast, upper = ufella, lower = adda)

  fcast_df <- fcast_df[, c("date", "actuals", "corrected", "forecast", "upper", "lower")]
  return(fcast_df)
}

#' Saldae display forecast results
#' @description t.b.d
#' @author Farid Azouaou
#' @param fcast_df forecast results in tibble object
#' @param target_variable target variable
#' @param background_colour background color
#' @return tibble object containing actuals , corrected, forecast and prediction bands(upper and lower)
#' @export

sekned_forecast_aqeru <- function(fcast_df = NULL, target_variable = NULL, background_colour = NULL,plot_settings=NULL) {
  if (sum(fcast_df$actuals - fcast_df$corrected, na.rm = TRUE) != 0) {
    corrected_indx <- which((fcast_df$actuals == fcast_df$corrected) == FALSE)
    fcast_df$corrected[-corrected_indx] <- NA
  } else {
    fcast_df$corrected <- rep(NA, nrow(fcast_df))
  }
  if(is.null(plot_settings)){
    actuals_col  <-  "#00AFBB"
    corrected_col <-  "darkgreen"
    forecast_col <-  "#E1AF00"
    ConfInt_col  <- "#EBCC2A"
  }else{
    actuals_col     <- plot_settings$colors_inu[1]
    corrected_col   <- plot_settings$colors_inu[2]
    forecast_col    <- plot_settings$colors_inu[3]
    ConfInt_col     <- plot_settings$colors_inu[3]
  }


  fcast_plot <- plotly::plot_ly(data = fcast_df) %>%
    plotly::add_lines(
      x = ~date, y = ~actuals,
      color = I(actuals_col), name = "oberservations"
    ) %>%
    plotly::add_trace(
      x = ~date, y = ~corrected,
      color = I(corrected_col), name = "corrected", type = "scatter", mode = "markers"
    ) %>%
    plotly::add_lines(x = ~date, y = ~forecast, color = I(forecast_col), name = "prediction") %>%
    plotly::add_ribbons(
      x = ~date, ymin = fcast_df$lower, ymax = fcast_df$upper,
      color = I(ConfInt_col), name = "95% confidence"
    )
  fcast_plot <- fcast_plot %>%
    plotly::layout(plot_bgcolor = background_colour, paper_bgcolor = background_colour, yaxis = list(title = target_variable),legend = list(orientation = "h", x = 0.35, y = 100)) %>%
    plotly::config(displaylogo = F)

  return(fcast_plot)
}



sekned_fcast_summary <- function(fcast_summary = NULL, time_unit = NULL, background_colour = NULL) {
  #----------------------------Forecast object in ggplot --
  plot_fcast_summary_mean <- plotly::plot_ly(
    data = fcast_summary[[time_unit]]$summary_mean, x = ~DATE, y = ~Average, type = "bar", name = "Forecast Summary(Average)",
    marker = list(color = "#ffcc5c")
  )
  plot_fcast_summary_mean <- plot_fcast_summary_mean %>%
    plotly::layout(plot_bgcolor = background_colour, paper_bgcolor = background_colour) %>%
    plotly::config(displaylogo = F)
  plot_fcast_summary_sum <- plotly::plot_ly(
    data = fcast_summary[[time_unit]]$summary_sum, x = ~DATE, y = ~Sum, type = "bar", name = "Forecast Summary(Sum)",
    marker = list(color = "#ffcc5c")
  )
  plot_fcast_summary_sum <- plot_fcast_summary_sum %>%
    plotly::layout(plot_bgcolor = background_colour, paper_bgcolor = background_colour) %>%
    plotly::config(displaylogo = F)
  #------------------------------------------------- Return output
  plot_output <- list()
  plot_output[["plot_fcast_summary_sum"]] <- plot_fcast_summary_sum
  plot_output[["plot_fcast_summary_mean"]] <- plot_fcast_summary_mean
  plot_output[["fcast_summary_sum"]] <- gridExtra::tableGrob(fcast_summary[[time_unit]]$summary_sum)
  plot_output[["fcast_summary_mean"]] <- gridExtra::tableGrob(fcast_summary[[time_unit]]$summary_mean)
  return(plot_output)
}

