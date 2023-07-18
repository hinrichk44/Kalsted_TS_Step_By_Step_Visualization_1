# 1.0 Objectives ----


# Introduction to:
# - Time Plot
# - Autocorrelation
# - Seasonality 
# - Anomalies
# - Seasonal Decomposition (STL)
# - Time Series Regression (TSLM)




# 2.0 Libraries ----


library(tidyverse)
library(timetk)
library(lubridate)




# 3.0 Importing Data for Analysis ----


google_analytics_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_tbl 


mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl




# 4.0 Preparing Data for Analysis ---- 


# * Prepping the Google analytics data  ----


# Here we will be doing some data wrangling to get the dataset into a proper time series format
google_analytics_ts <- google_analytics_tbl %>%
  # ymd_h() comes from the lubridate package
  # It parses dates formatted as text strings that are in YMDH format into a date-time format
  # Here we are converting the dateHour column into a new column simply called "date" but with proper formatting
  # Always parse character dates into date or date-time
  # This makes them easier to work with for timetk and modeltime
  mutate(date = ymd_h(dateHour)) %>%
  select(-dateHour) %>%
  # We now have a 21,594 x 3 tibble
  # At this point we have date, name, and value as our three columns
  # timetk plotting functions work better with "long" formatted data
  pivot_longer(cols = pageViews:sessions)


# * Prepping the Mailchimp data  ----


# Here we are doing a count of opt-ins by day
# We are turning our raw data into a time series format
mailchimp_users_ts <- mailchimp_users_tbl %>%
  # summarize_by_time() comes from the timetk package
  # It's for aggregating time series using time stamps and value
  # Works like summarise() but for time series
  summarise_by_time(
    # .date_var is going to be the date column we want to summarize
    # In this case we are going to use the column "optin_time"
    .date_var = optin_time,
    # This is the metric for how we will summarize our timestamp data
    # We are picking "day" for this example, but could be month or year
    .by = "day",
    # We want to summarize the number of observations per day
    # n() performs a frequency count
    # We will create a new column called "optins" that will contain that summarization
    # So now we have a 608 x 2 tibble with the timestamp and then the number of observations per that timestamp
    # For example, on 2018-07-03 there were 10 people that opted in to receiving emails
    optins = n()
  ) %>%
  # pad_by_time comes from the timetk library
  # It performs time series padding
  # It fills in any gaps to convert the time series to a regular time series
  # So now we have the time stamps filled in, but the column for "optins" now has a bunch of NA values
  # We can fix that by adding in .pad_value and in this case we will make the value 0
  # This makes sense since there are no opt-ins for those filled in days
  # We now have a 634 x 2 tibble
  pad_by_time(.date_var = optin_time, 
              .by = "day",
              .pad_value = 0)




# 5.0 Plotting Time Series ----


# * Visualizing Mailchimp data ----


# Here we are doing a basic visualization of our time series data for Mailchimp
mailchimp_users_ts_plot <- mailchimp_users_ts %>%
  # plot_time_series() comes from the timetk package
  # optin_time will be on the x-axis and optins the y-axis
  plot_time_series(optin_time, optins)

mailchimp_users_ts_plot


# * Visualizing Google analytics data ----


# Here we are doing a basic visualization of our time series data for Google analytics
google_analytics_ts_plot <- google_analytics_ts %>%
  # We are adding the .color_var = name so we can distinguish between the types of data being plotted
  # We have page views, organic searches, and sessions all being plotted
  plot_time_series(date, value, .color_var = name)

google_analytics_ts_plot


# Here we are doing a facet visualization of our time series data for Google analytics
google_analytics_ts_facet_plot <- google_analytics_ts %>%
  group_by(name) %>%
  plot_time_series(date, value)

# So now we have three separate time series plots instead of all three on one plot
google_analytics_ts_facet_plot


# * Mutations/Transformations ----


# Log transformations are useful for modeling and visualization since they reduce variance
# It's non-parametic so it's easy to convert to and from using log() and exp()
# Here we are doing a log transformation of our time series data for Mailchimp
mailchimp_users_ts_log_plot <- mailchimp_users_ts %>%
  # For non-negative data, we can add 1 to get rid of -inf and NaN errors
  # This is simply called Log Plus 1 Transformation
  plot_time_series(optin_time, log(optins + 1))

mailchimp_users_ts_log_plot


# Here we are doing a log transformation of our time series data for Google analytics
google_analytics_ts_log_facet_plot <- google_analytics_ts %>%
  group_by(name) %>%
  plot_time_series(date, log(value + 1))

google_analytics_ts_log_plot


# * Smoother Adjustment for Mailchimp ----


# LOESS Smoother
# LOESS stands for Local Estimated Scatter Plot Smoothing
# It's very similar to a moving average, but with a polynomial regression that is fitted to a window of points
# Long windows provide better "trend" estimates
# Short windows help uncover "cyclical" seasonalities 
# The default smooth span selects 75% of the data to locally fit


# Here I am visualizing the Mailchimp data but without the smoother
mailchimp_users_ts_log_plot_2 <- mailchimp_users_ts %>%
  # Here I am removing the smoother from the plot
  # Now we just have the raw data being plotted
  plot_time_series(optin_time, log(optins + 1), .smooth = FALSE)

mailchimp_users_ts_log_plot_2


# Here I am visualizing the Mailchimp data but adjusting the window for smoothing
mailchimp_users_ts_log_plot_3 <- mailchimp_users_ts %>%
  # Here I am adjusting the smoothing period to 30 days instead of default 75 days
  plot_time_series(optin_time, log(optins + 1), .smooth_period = "30 days")

mailchimp_users_ts_log_plot_3


# You can do .smooth_span, .smooth_degree, .smooth_message and other changes
# Period is always fixed
# Span expands with your data


# * Smoother Adjustment for Google analytics ----


# Here I am visualizing the Google analytics data but adjusting the window for smoothing
google_analytics_ts_log_facet_plot_2 <- google_analytics_ts %>%
  group_by(name) %>%
  # Here I am adjusting the smoothing period to 30 days instead of default 90 days
  plot_time_series(date, log(value + 1), .smooth_period = "90 days")

google_analytics_ts_log_facet_plot_2


# * Static ggplot ----


# Here I am making the time series plot static instead of interactive
# The default from plot_time_series() is to make the plot an interactive plotly chart
mailchimp_users_ts_static_plot <- mailchimp_users_ts %>%
  # By changing .interactive to FALSE we make the plot static
  plot_time_series(optin_time, optins, .interactive = FALSE)

mailchimp_users_ts_static_plot




# 6.0 ACF Diagnostics ----


# * Autocorrelation and partial autocorrelation explained ----


# A time series may have a relationship to previous versions of itself. These are called "lags"
# Autocorrelation is a measure of the strength of the relationship to its lags
# The price of salmon in March is correlated to the price it was in February and in January
# How can January affect the price of salmon in March? Both directly and indirectly
# The price of salmon in January can go through February to affect the price of salmon in March
# It can also jump February and directly affect March


# For PARTIAL autocorrelation we only care about direct effects
# PACF is a variation of ACF that de-weights the lagged autocorrelation by a lags relationship to previous lags
# The rationale is that autocorrelation is artificially high because the relationship is already present in previous lags
# The graph you can show will have a range with all the lags you are looking at expressed as a bar chart
# If the bar for a given lag goes past the range, it has a non-zero affect on the current time period



# * Plotting both ACF and PACF ----


# An ACF Plot is a visual way of showing autocorrelation vs lags
# It can be used to detect lagged features, Fourier Series Periods, and date features via cycles


# Here we are plotting both acf and pcf for the Mailchimp time series data
mailchimp_users_ts_acf_plot <- mailchimp_users_ts %>%
  # plot_acf_diagnostics() comes from the timetk package
  plot_acf_diagnostics(optin_time, optins)

mailchimp_users_ts_acf_plot


# Here we are plotting both acf and pcf for the Mailchimp time series data
mailchimp_users_ts_acf_log_plot <- mailchimp_users_ts %>%
  # We are using a log transformation for this one
  # A lag of 0 is always 100% correlation
  # What tends to happen is that for each lag the correlation dies down
  # Spikes in these lags can indicate seasonality
  plot_acf_diagnostics(optin_time, log(optins + 1))

mailchimp_users_ts_acf_log_plot


# Here we are plotting both acf and pcf for the Mailchimp time series data
mailchimp_users_ts_acf_log_plot_2 <- mailchimp_users_ts %>%
  # Here we are adjusting the lags to get years worth of lags
  plot_acf_diagnostics(optin_time, log(optins + 1), .lags = "1 year")

mailchimp_users_ts_acf_log_plot_2


# Here we are plotting both acf and pcf for the Mailchimp time series data
mailchimp_users_ts_acf_log_plot_3 <- mailchimp_users_ts %>%
  # Here we are adjusting the lags to get the first 100 lags 
  plot_acf_diagnostics(optin_time, log(optins + 1), .lags = 100)

mailchimp_users_ts_acf_log_plot_3


# Here we are plotting both acf and pcf for the Mailchimp time series data
mailchimp_users_ts_acf_log_plot_4 <- mailchimp_users_ts %>%
  # Here we are adjusting the lags to get lags 25 through 100 
  plot_acf_diagnostics(optin_time, log(optins + 1), .lags = 25:100)

mailchimp_users_ts_acf_log_plot_4


# * Cross correlation ----


# Cross Correlation between a time series and lagged versions of a different time series (usually a predictor)


# Here we are going to adjust our Google time series to summarize things by day
google_analytics_ts_2 <- google_analytics_ts %>%
  pivot_wider(names_from = name, values_from = value) %>%
  # So now we have a 300 x 4 tibble
  # page views, organic searches, and sessions are all summed up per day
  # For example on 2019-05-08 there were 1664 page views, 482 organic searches, and 1039 sessions
  summarise_by_time(.date_var = date, 
                    .by = "day", 
                    # across() enables applying a function to multiple columns across a dataframe
                    across(pageViews:sessions, .fns = sum))


# Here we are combining our Mailchimp data with our Google analytics data
combined_data_ts <- mailchimp_users_ts %>%
  # There are lots of NA values, but that's because there are some time differences between the two datasets
  # Where the times are equal the data is filled in nicely
  # Overall we have a 634 x 5 tibble
  left_join(google_analytics_ts_2, by = c("optin_time" = "date")) %>%
  # We're just going to drop NA values to make things easier
  # We now officially have a 300 x 4 tibble
  drop_na()


# Here we are plotting both acf and pcf for the combined time series data
combined_data_ts_acf_plot <- combined_data_ts %>%
  # The .ccf_vars allows us to look at cross correlation
  # So now you can see if there are correlations between pageViews and subscribers for instance
  plot_acf_diagnostics(optin_time, optins, .ccf_vars = pageViews:sessions)

combined_data_ts_acf_plot


# Here we are plotting both acf and pcf for the combined time series data
combined_data_ts_acf_plot_2 <- combined_data_ts %>%
  plot_acf_diagnostics(optin_time, optins, 
                       .ccf_vars = pageViews:sessions,
                       # This time we are only looking at the CCF data by setting .show_ccf_vars_only = TRUE
                       # You can add .facet_ncol to show these charts side by side instead of on top of each other
                       .show_ccf_vars_only = TRUE)

combined_data_ts_acf_plot_2




# 7.0 Seasonality ----


# Seasonality plots are useful for detecting time based features (calendar features) that have cyclic/trend effects


# Here we are looking at the seasonal factors in the Google analyics dataset
seasonal_google_plot <- google_analytics_ts %>%
  group_by(name) %>%
  # plot_seasonal_diagnostics will show each value and how it's broken down by a calendar feature
  # For example we have pageViews by hour, weekday, week, month, and quarter
  plot_seasonal_diagnostics(date, value)

seasonal_google_plot


# Here we are looking at the seasonal factors in the Google analyics dataset, but with log transformation
seasonal_google_log_plot <- google_analytics_ts %>%
  group_by(name) %>%
  # For hourly pageViews there seems to be a spike at the ninth hour and then draws down
  plot_seasonal_diagnostics(date, log(value + 1))

seasonal_google_log_plot


# Here we are looking at the seasonal factors in the Google analyics dataset, but with log transformation
seasonal_google_log_plot_2 <- google_analytics_ts %>%
  group_by(name) %>%
  # Here we are just looking at the hourly seasonality by setting .feature_set to "hour"
  # You can include multiple features within feature set
  plot_seasonal_diagnostics(.date_var = date, 
                            .value = log(value + 1),
                            .feature_set = "hour")

seasonal_google_log_plot_2


# * Violin vs. Box Plot ----


# Here we are looking at the seasonal factors in the Google analyics dataset, but with log transformation
seasonal_google_log_plot_3 <- google_analytics_ts %>%
  group_by(name) %>%
  # Here we are converting the chart into a violin plot from a box plot
  plot_seasonal_diagnostics(.date_var = date, 
                            .value = log(value + 1),
                            .feature_set = c("hour", "wday.lbl"),
                            .geom = "violin")

seasonal_google_log_plot_3




# 8.0 Anomalies ----


# Anomalies are used for detecting events & possible data issues
# Frequency and Trend come from the STL Decomposition Plot


# * Plotting anomalies ----


# Here we are investigating if there are anomalies in the Mailchimp data
mailchimp_anomaly_plot <- mailchimp_users_ts %>%
  # plot_anomaly_diagnostics() puts red dots on values that are anomalies
  plot_anomaly_diagnostics(optin_time, optins)

mailchimp_anomaly_plot


# Here we are investigating if there are anomalies in the Mailchimp data
mailchimp_anomaly_plot_2 <- mailchimp_users_ts %>%
  plot_anomaly_diagnostics(.date_var = optin_time,
                           .value = optins,
                           # Default for alpha is 0.5
                           # It controls the width of the "normal" range (the gray ribbon bands)
                           # Lower values increase the width of the bands, making less conservative anomalies
                           # There are now fewer anomalies being highlighted since we reduced alpha
                           .alpha = 0.01)

mailchimp_anomaly_plot_2


# Here we are investigating if there are anomalies in the Mailchimp data
mailchimp_anomaly_plot_3 <- mailchimp_users_ts %>%
  plot_anomaly_diagnostics(.date_var = optin_time,
                           .value = optins,
                           .alpha = 0.01,
                           # The default for max_anomalies is 0.2
                           # Max Anomalies controls the maximum percentage of data allowed to be anomalous
                           # There are now much fewer anomalies being highlighted since we reduced it to .01 from .2
                           .max_anomalies = 0.01)

mailchimp_anomaly_plot_3


# Here we are investigating if there are anomalies in the Google analytics data
google_anomaly_plot <- google_analytics_ts %>%
  group_by(name) %>%
  # Looks like there are tons of anomalies in pageViews and sessions
  # organicSearches seems to be fine
  plot_anomaly_diagnostics(date, value)

google_anomaly_plot


# * Extracting anomalies ----


# All diagnostic plotting functions have corresponding data prep functions
# If you need underlying data post-transformation, use these functions


# Here we are getting the raw anomaly data that we've been charting
# We end up getting a 634 x 11 tibble
mailchimp_anomaly_data <- mailchimp_users_ts %>%
  tk_anomaly_diagnostics(.date_var = optin_time,
                         .value = optins,
                         .alpha = 0.01,
                         .max_anomalies = 0.01)




# 9.0 Seasonal Decomposition ----


# Seasonal decomposition is good for detecting seasonal cycles and visualizing trends
# It's also good for decomposing a time series to perform analysis on its parts
# STL stands for Seasonal, Trend by LOESS
# Trend: The long-term movement of the time series. It's like a projection (LOESS smoother)
# Seasonality: Oscillation around the trend. It's like a frequency
# This can be used for anomaly detection. Big spikes in the remainder tend to be anomalies
# The one downside is it's not very good for visualizing complex seasonalities


# Here we are viewing our seasonal decomposition of the Mailchimp data
mailchimp_stl_plot <- mailchimp_users_ts %>%
  # There's observed (actual data) a season, a trend and a remainder
  # Remainder is observed - season - trend
  # Seasonal adjustment is just removing the seasonal cycle (observed - season)
  plot_stl_diagnostics(optin_time, optins)

mailchimp_stl_plot


# Here we are viewing our seasonal decomposition of the Mailchimp data
mailchimp_stl_plot_2 <- mailchimp_users_ts %>%
  plot_stl_diagnostics(optin_time, optins,
                       # Changing frequency from daily to monthly
                       .frequency = "1 month",
                       .trend = "1 year")

mailchimp_stl_plot_2


# Here we are viewing our seasonal decomposition of the Google analytics data
google_stl_plot <- google_analytics_ts %>%
  group_by(name) %>%
  plot_stl_diagnostics(date, log(value + 1))

google_stl_plot




# 10.0 Time Series Regression Plot ----


# Here we are modeling our Mailchimp data as a linear regression
mailchimp_regression_plot <- mailchimp_users_ts %>%
  plot_time_series_regression(
    .date_var = optin_time,
    # "optins as a function of optin_time, plus weekday optin_time, plus month optin_time"
    # wday() and month() both come from the lubridate package
    optins ~ as.numeric(optin_time) + wday(optin_time, label = TRUE) + month(optin_time, label = TRUE)
  )

mailchimp_regression_plot


# Here we are modeling our Mailchimp data as a linear regression
mailchimp_regression_plot_2 <- mailchimp_users_ts %>%
  plot_time_series_regression(
    .date_var = optin_time,
    # "optins as a function of optin_time, plus weekday optin_time, plus month optin_time"
    # wday() and month() both come from the lubridate package
    optins ~ as.numeric(optin_time) + wday(optin_time, label = TRUE) + month(optin_time, label = TRUE),
    # Here we are showing which features have predictive power
    # We have an adjusted R-Squared of 0.01257, which obviously is low
    # Can be a sign that your data has large outliers
    # Try a transformation like log transformation
    .show_summary = TRUE
  )

mailchimp_regression_plot_2


# Here we are modeling our Mailchimp data as a linear regression
mailchimp_regression_plot_3 <- mailchimp_users_ts %>%
  plot_time_series_regression(
    .date_var = optin_time,
    # Here we are applying a log transformation to our data
    log(optins + 1) ~ as.numeric(optin_time) + wday(optin_time, label = TRUE) + month(optin_time, label = TRUE),
    # R-Squared has increased and the fitted values do look more suitable to the observed data
    .show_summary = TRUE
  )

mailchimp_regression_plot_3


# Here we are modeling our Google analytics data as a linear regression
google_regression_plot <- google_analytics_ts %>%
  group_by(name) %>%
  plot_time_series_regression(
    .date_var = date,
   # log value as a function of date, plus hour date, plus weekday date, plus month date
   # Make any cyclic features factors. This encodes as categorical data, and the ML algorithms will learn the cycles
    log(value + 1) ~ as.numeric(date) + as.factor(hour(date)) + wday(date, label = TRUE) + month(date, label = TRUE)
  )

google_regression_plot


# Here we are modeling our Google analytics data as a linear regression
google_regression_plot_2 <- google_analytics_ts %>%
  filter(name == "pageViews") %>%
  plot_time_series_regression(
    .date_var = date,
    # log value as a function of date, plus hour date, plus weekday date, plus month date
    # Make any cyclic features factors. This encodes as categorical data, and the ML algorithms will learn the cycles
    log(value + 1) ~ as.numeric(date) + as.factor(hour(date)) + wday(date, label = TRUE) + month(date, label = TRUE),
    # You cannot use show_summary() on multiple features
    # This is why we filtered to just pageViews
    .show_summary = TRUE
  )

google_regression_plot_2

