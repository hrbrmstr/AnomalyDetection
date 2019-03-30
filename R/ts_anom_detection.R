#' Anomaly Detection Using Seasonal Hybrid ESD Test
#'
#' A technique for detecting anomalies in seasonal univariate time series where the input is a
#' series of <timestamp, count> pairs.
#'
#' @md
#' @name AnomalyDetectionTs
#' @param x Time series as a two column data frame where the first column consists of the
#'        timestamps and the second column consists of the observations.
#' @param max_anoms Maximum number of anomalies that S-H-ESD will detect as a percentage of the
#'        data.
#' @param direction Directionality of the anomalies to be detected. One of:
#'        `pos`, `neg`, `both`.
#' @param alpha The level of statistical significance with which to accept or reject anomalies.
#' @param only_last Find and report anomalies only within the last day or hr in the time series.
#'        One of `NULL`, `day`, `hr`.
#' @param threshold Only report positive going anoms above the threshold specified. One of:
#'        `None`, `med_max`, `p95`, `p99`.
#' @param e_value Add an additional column to the anoms output containing the expected value.
#' @param longterm Increase anom detection efficacy for time series that are greater than a month.
#'        See `Details`` below.
#' @param piecewise_median_period_weeks The piecewise median time window as described in Vallis,
#'        Hochenbaum, and Kejariwal (2014). Defaults to 2.
#' @param verbose Enable debug messages
#' @param na.rm Remove any NAs in timestamps.(default: `FALSE`)
#' @return The returned value is a data frame containing timestamps, values,
#'         and optionally expected values.
#' @references
#' - Vallis, O., Hochenbaum, J. and Kejariwal, A., (2014)
#'   "A Novel Technique for Long-Term Anomaly Detection in the Cloud", 6th USENIX, Philadelphia, PA.
#'   (<https://www.usenix.org/system/files/conference/hotcloud14/hotcloud14-vallis.pdf>)
#'  - Rosner, B., (May 1983), "Percentage Points for a Generalized ESD Many-Outlier
#'    Procedure", Technometrics, 25(2), pp. 165-172. (<https://www.jstor.org/stable/1268549>)
#' @examples
#' data(raw_data)
#'
#' ad_ts(raw_data, max_anoms=0.02, direction='both')
#'
#' # To detect only the anomalies on the last day, run the following:
#'
#' ad_ts(raw_data, max_anoms=0.02, direction='both', only_last="day")
#' @seealso [ad_vec()]
#' @export
AnomalyDetectionTs <- function(x, max_anoms = 0.10, direction = "pos",
                               alpha = 0.05, only_last = NULL, threshold = "None",
                               e_value = FALSE, longterm = FALSE,
                               piecewise_median_period_weeks = 2,
                               verbose = FALSE, na.rm = FALSE) {

  # Check for supported inputs types
  if (!is.data.frame(x)) {
    stop("data must be a single data frame.")
  } else {
    if (ncol(x) != 2 || !is.numeric(x[[2]])) {
      stop(paste0("data must be a 2 column data.frame, with the first column being ",
                  "a set of timestamps, and the second coloumn being numeric values.",
                  collapse = ""))
    }
    # Format timestamps if necessary
    if (!(class(x[[1]])[1] == "POSIXlt")) x <- format_timestamp(x)
  }

  # Rename data frame columns if necessary
  if (any((names(x) == c("timestamp", "count")) == FALSE)) {
    colnames(x) <- c("timestamp", "count")
  }

  if (!is.logical(na.rm)) stop("na.rm must be either TRUE or FALSE")

  # Deal with NAs in timestamps
  if (any(is.na(x$timestamp))) {
    if (na.rm) {
      x <- x[-which(is.na(x$timestamp)), ]
    } else {
      stop("timestamp contains NAs, please set na.rm to TRUE or remove the NAs manually.")
    }
  }

  # Sanity check all input parameters
  if (max_anoms > .49) {
    stop(paste("max_anoms must be less than 50% of the data points (max_anoms =",
               round(max_anoms * length(x[[2]]), 0), " data_points =", length(x[[2]]), ")."))
  } else if (max_anoms < 0) {
    stop("max_anoms must be positive.")
  } else if (max_anoms == 0) {
    warning("0 max_anoms results in max_outliers being 0.")
  }

  if (!direction %in% c("pos", "neg", "both")) {
    stop("direction options are: pos | neg | both.")
  }

  if (!(0.01 <= alpha || alpha <= 0.1)) {
    if (verbose) message("Warning: alpha is the statistical signifigance, and is usually between 0.01 and 0.1")
  }

  if (!is.null(only_last) && !only_last %in% c("day", "hr")) {
    stop("only_last must be either 'day' or 'hr'")
  }

  if (!threshold %in% c("None", "med_max", "p95", "p99")) {
    stop("threshold options are: None | med_max | p95 | p99.")
  }

  if (!is.logical(e_value)) {
    stop("e_value must be either TRUE or FALSE")
  }

  if (!is.logical(longterm)) {
    stop("longterm must be either TRUE or FALSE")
  }

  if (piecewise_median_period_weeks < 2) {
    stop("piecewise_median_period_weeks must be at greater than 2 weeks")
  }

  # -- Main analysis: Perform S-H-ESD

  # Derive number of observations in a single day.
  # Although we derive this in S-H-ESD, we also need it to be minutley later on so we do it here first.
  gran <- get_gran(x, 1)

  if (gran == "day") {
    num_days_per_line <- 7
    if (is.character(only_last) && only_last == "hr") {
      only_last <- "day"
    }
  } else {
    num_days_per_line <- 1
  }

  # Aggregate data to minutely if secondly
  if (gran == "sec" || gran == "ms") { # ref: https://github.com/twitter/AnomalyDetection/pull/69/files
    x <- format_timestamp(
      stats::aggregate(
        x[2],
        format(x[1], "%Y-%m-%d %H:%M:00"),
        sum)
    ) # ref: https://github.com/twitter/AnomalyDetection/pull/44
    gran <-  "min" # ref: https://github.com/twitter/AnomalyDetection/pull/98/files?diff=unified
  }

  period <- switch(
    gran,
    sec = 3600, # ref: https://github.com/twitter/AnomalyDetection/pull/93/files
    ms = 1000,  # ref: https://github.com/twitter/AnomalyDetection/pull/69/files
    min = 1440,
    hr = 24,
    # if the data is daily, then we need to bump the period to weekly to get multiple examples
    day = 7
  )
  num_obs <- length(x[[2]])

  if (max_anoms < 1 / num_obs) max_anoms <- 1 / num_obs

  # -- Setup for longterm time series

  # If longterm is enabled, break the data into subset data frames and store in all_data
  if (longterm) {
    # Pre-allocate list with size equal to the number of piecewise_median_period_weeks chunks in x + any left over chunk
    # handle edge cases for daily and single column data period lengths
    if (gran == "day") {
      # STL needs 2*period + 1 observations
      num_obs_in_period <- period * piecewise_median_period_weeks + 1
      num_days_in_period <- (7 * piecewise_median_period_weeks) + 1
    } else {
      num_obs_in_period <- period * 7 * piecewise_median_period_weeks
      num_days_in_period <- (7 * piecewise_median_period_weeks)
    }

    # Store last date in time series
    last_date <- x[[1]][num_obs]

    all_data <- vector(mode = "list", length = ceiling(length(x[[1]]) / (num_obs_in_period)))

    # Subset x into piecewise_median_period_weeks chunks
    for (j in seq(1, length(x[[1]]), by = num_obs_in_period)) {

      start_date <- x[[1]][j]
      end_date <- min(start_date + lubridate::days(num_days_in_period), x[[1]][length(x[[1]])])

      # if there is at least 14 days left, subset it, otherwise subset last_date - 14days
      if (difftime(end_date, start_date, units = "days") == as.difftime(num_days_in_period, units = "days")) {
        all_data[[ceiling(j / (num_obs_in_period))]] <- x[x[[1]] >= start_date & x[[1]] < end_date, ]
      } else {
        all_data[[ceiling(j / (num_obs_in_period))]] <-
          x[x[[1]] > (last_date - lubridate::days(num_days_in_period)) & x[[1]] <= last_date, ]
      }

    }

  } else {
    # If longterm is not enabled, then just overwrite all_data list with x as the only item
    all_data <- list(x)
  }

  # Create empty data frames to store all anoms and seasonal+trend component from decomposition
  all_anoms <- data.frame(timestamp = numeric(0), count = numeric(0))
  seasonal_plus_trend <- data.frame(timestamp = numeric(0), count = numeric(0))

  # Detect anomalies on all data (either entire data in one-pass, or in 2 week blocks if longterm=TRUE)
  for (i in 1:length(all_data)) {

    anomaly_direction <- switch(
      direction,
      "pos" = data.frame(one_tail = TRUE, upper_tail = TRUE), # upper-tail only (positive going anomalies)
      "neg" = data.frame(one_tail = TRUE, upper_tail = FALSE), # lower-tail only (negative going anomalies)
      "both" = data.frame(one_tail = FALSE, upper_tail = TRUE)
    ) # Both tails. Tail direction is not actually used.

    # detect_anoms actually performs the anomaly detection and returns the results in a list containing the anomalies
    # as well as the decomposed components of the time series for further analysis.
    s_h_esd_timestamps <- detect_anoms(all_data[[i]],
                                       k = max_anoms, alpha = alpha, num_obs_per_period = period,
                                       use_decomp = TRUE, use_esd = FALSE,
                                       one_tail = anomaly_direction$one_tail, upper_tail = anomaly_direction$upper_tail,
                                       verbose = verbose
    )

    # store decomposed components in local variable and overwrite s_h_esd_timestamps to contain only the anom timestamps
    data_decomp <- s_h_esd_timestamps$stl
    s_h_esd_timestamps <- s_h_esd_timestamps$anoms

    # -- Step 3: Use detected anomaly timestamps to extract the actual anomalies (timestamp and value) from the data
    if (!is.null(s_h_esd_timestamps)) {
      anoms <- all_data[[i]][(all_data[[i]][[1]] %in% s_h_esd_timestamps), ]
    } else {
      anoms <- data.frame(timestamp = numeric(0), count = numeric(0))
    }

    # Filter the anomalies using one of the thresholding functions if applicable
    if (threshold != "None") {

      # Calculate daily max values
      periodic_maxs <- tapply(x[[2]], as.Date(x[[1]]), FUN = max)

      # Calculate the threshold set by the user
      if (threshold == "med_max") {
        thresh <- stats::median(periodic_maxs)
      } else if (threshold == "p95") {
        thresh <- stats::quantile(periodic_maxs, .95)
      } else if (threshold == "p99") {
        thresh <- stats::quantile(periodic_maxs, .99)
      }
      # Remove any anoms below the threshold
      anoms <- anoms[anoms[[2]] >= thresh, ]

    }

    all_anoms <- rbind(all_anoms, anoms)
    seasonal_plus_trend <- rbind(seasonal_plus_trend, data_decomp)

  }

  # Cleanup potential duplicates
  all_anoms <- all_anoms[!duplicated(all_anoms[[1]]), ]
  seasonal_plus_trend <- seasonal_plus_trend[!duplicated(seasonal_plus_trend[[1]]), ]

  # -- If only_last was set by the user, create subset of the data that represent the most recent day
  if (!is.null(only_last)) {

    start_date <- x[[1]][num_obs] - lubridate::days(7)
    start_anoms <- x[[1]][num_obs] - lubridate::days(1)

    if (gran == "day") {
      # TODO: This might be better set up top at the gran check
      breaks <- 3 * 12
      num_days_per_line <- 7
    } else {
      if (only_last == "day") {
        breaks <- 12
      } else {
        # We need to change start_date and start_anoms for the hourly only_last option
        start_date <- lubridate::floor_date(x[[1]][num_obs] - lubridate::days(2), "day")
        start_anoms <- x[[1]][num_obs] - lubridate::hours(1)
        breaks <- 3
      }
    }

    # subset the last days worth of data
    x_subset_single_day <- x[x[[1]] > start_anoms, ]

    x_subset_week <- x[(x[[1]] <= start_anoms) & (x[[1]] > start_date), ]
    all_anoms <- all_anoms[all_anoms[[1]] >= x_subset_single_day[[1]][1], ]
    num_obs <- length(x_subset_single_day[[2]])

  }

  # Calculate number of anomalies as a percentage
  anom_pct <- (length(all_anoms[[2]]) / num_obs) * 100

  # If there are no anoms, then let's exit
  if (anom_pct == 0) {
    if (verbose) message("No anomalies detected.")
    return(data.frame())
  }

  # Fix to make sure date-time is correct and that we retain hms at midnight
  all_anoms[[1]] <- format(all_anoms[[1]], format = "%Y-%m-%d %H:%M:%S")

  # Store expected values if set by user
  if (e_value) {
    anoms <- data.frame(
      timestamp = all_anoms[[1]], anoms = all_anoms[[2]],
      expected_value = seasonal_plus_trend[[2]][as.character(as.POSIXlt(seasonal_plus_trend[[1]], tz = "UTC")) %in% all_anoms[[1]]],
      stringsAsFactors = FALSE
    )
  } else {
    anoms <- data.frame(timestamp = all_anoms[[1]], anoms = all_anoms[[2]],
                        stringsAsFactors = FALSE)
  }

  # Make sure we're still a valid POSIXct datetime.
  # TODO: Make sure we keep original datetime format and timezone.

  anoms$timestamp <- as.POSIXct(anoms$timestamp, tz = "UTC")

  class(anoms) <- c("tbl_df", "tbl", "data.frame")

  return(anoms)
}

#' @rdname AnomalyDetectionTs
#' @export
ad_ts <- AnomalyDetectionTs
