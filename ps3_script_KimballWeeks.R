library(tidyverse)
library(fpp3)

# ---- PART 2 ----
retail_raw <- read_csv("data/fredgraph.csv")

head(retail_raw)

#2.2a observation_date comes in as <date> so read_csv guessed right here.

#fixing column types
retail_raw <- read_csv("data/fredgraph.csv", na = ".", 
                       col_types = cols(
                         observation_date = col_date(format = "%Y-%m-%d"),
                         RSAFS = col_double(),
                         RSAFSNA = col_double()
                       )
                )

#pivoting the table long
retail_raw_long <- retail_raw |> 
  select(observation_date, RSAFS, RSAFSNA) |> 
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

retail_ts <- retail_raw_long |> 
  mutate(Month = yearmonth(observation_date)) |> 
  as_tsibble(index = Month, key = series_id)

head(retail_ts)

has_gaps(retail_ts)
#no gaps in either series

key_vars(canadian_gas)
index_var(canadian_gas)

#creating working copy of can gas
can_gas <- canadian_gas |> 
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

#making new copy of can gas keeping only recent dates
can_gas_recent <- can_gas |> 
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

#exctacting parts of the date
can_gas_recent <- can_gas_recent |> 
  mutate(year = year(Month), 
         month = month(Month, label = TRUE),
         quarter = quarter(Month))

#computing average volume of each calender month since 1990
can_gas_recent |> 
  as_tibble() |> 
  group_by(month) |> 
  summarize(avg_volume = mean(Volume))

#2.6b Since 1990 December has the highest average production (17.1) and June has the lowest (14.9).

#adding two columns based on month using yearmonth arithmetic
can_gas_recent |> 
  mutate(next_month = Month + 1,
         same_month_last_year = Month - 12) |> 
  head()

#doing analysis on quarters by using index_by which is group_by for time indexing
can_gas_q <- can_gas |> 
  filter(Month <= yearmonth("2004 Dec")) |> 
  index_by(Quarter = yearquarter(Month)) |> 
  summarize(Volume = sum(Volume))

head(can_gas_q)

#creating plot (shortcut)
can_gas_q |> autoplot(Volume)

#2.6d We sum because Volume is a flow, the total produced in the month, so a quarter's production is the total of its three months (the mean would give an average month instead).
#The quarterly series is still seasonal, Q1 and Q4 are higher than Q2 and Q3, just less sharp than the monthly series.

retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

#2.7b
#The following NSA plot has the same spike and dip seasonal pattern every year while the SA series does not have this same pattern. 
#While the seasonality of the retail and food service sales is adjusted for in the RSAFS series the long term pattern remains the same.
#We can see that during the great recession and Covid they have the same drops and the trend is almost identical.

#2.7c plotting Canadian gas production
can_gas |> 
  autoplot(Volume) +
  labs(
    title = "Canadian gas production",
    x = "time",
    y = "Volume (billion cubic metres)"
  ) + theme_minimal()

#2.7d same plot with a fitted line
can_gas |> 
  autoplot(Volume) +
  labs(
    title = "Canadian gas production",
    x = "time",
    y = "Volume (billion cubic metres)"
  ) + theme_minimal() + geom_smooth(method = 'lm')

#I added a fitted line (geom_smooth) to the Canadian gas production plot to see the general trend of the data.


# --- PART 3 ----

#3.1a The seasonal swings grow with the level up to about the mid 1980s, but then they shrink in the 1990s and 2000s even though production keeps rising. So it is not true over the whole sample.

can_gas <- can_gas |> 
  mutate(log_volume = log(Volume))

can_gas |> 
  autoplot(log_volume) +
  labs(
    title = "Canadian gas production",
    x = "time",
    y = "log(Volume)"
  ) + theme_minimal()


#3.1b
#After logging volume, a constant percent change becomes a constant absolute change, so the early fast growth flattens out and the trend looks more consistent.
#The swings in the middle of the sample get smaller, but the early swings get bigger and the late ones are still small. I think logs are not a great fit here because the
#seasonal swings did not grow only with the level. There were periods with more and less, which logs don't fix.


retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales)) |>
  pivot_longer(c(sales, log_sales), names_to = "scale", values_to = "value") |>
  ggplot(aes(x = Month, y = value)) +
  geom_line(color = "red") +
  facet_wrap(~ scale, ncol = 1, scales = "free_y",
             labeller = as_labeller(c(sales = "Millions of dollars",
                                      log_sales = "Log sales"))) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Logged vs unlogged, seasonally unadjusted",
    x = NULL, y = NULL
  ) +
  theme_minimal()


#3.1c The case for logging is stronger here than for can_gas. The December spikes grow steadily with the level of sales over the whole sample (about 62k in 1992 to 176k in 2025), which is the pattern logs are meant to fix. X-13 also chooses logs for this series in 4.3a.

#adding growth rates direct and logdiff
can_gas <- can_gas |> 
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |> autoplot(growth_direct)

#adding abs difference between direct and logdiff growth rates
can_gas <- can_gas |> 
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |> autoplot(growth_gap)

#The log difference approximation is very good here, the gap is usually under 0.4 percentage points and only gets to about 3 in months with growth over 20%.


#SA vs. NSA growth rates 
retail_ts <- retail_ts |> 
  group_by_key() |> 
  mutate(growth_mom = 100 * difference(log(sales))) |> 
  ungroup()

retail_ts |> 
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL) + 
  theme_minimal()

#3.3
#The mom growth for the SA and NSA series are much different. The seasonally adjusted retail series is much quieter and has smaller spikes and dips. This is because NSA retail sales swing a lot over the course of a year (December spike, January drop) so the series is very volatile. 
#As mentioned before in 2.7b after adjusting we remove a lot of the noise and can see more of the underlying trend of growth over time.


# ---- PART 4 -----

#4.1a

retail_ts |> 
  filter(series_id == "RSAFSNA") |> 
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales", 
    y = "Millions of dollars"
  ) +
  theme_minimal()

#4.1b 
retail_ts |> 
  filter(series_id == "RSAFS") |> 
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales", 
    y = "Millions of dollars"
  ) +
  theme_minimal()

#There is a big difference between the two seasonal plots here. We can see that after seasonally adjusting, the data does not contain the same patterns that we saw before. 


#4.1c

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production",
    y = "log(Volume)"
  ) +
  theme_minimal()

#Canadian gas production is typically highest in December and January and lowest in June, probably because of higher energy demand for heating in the cold weather.

#4.2
can_gas |> 
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas production",
    y = "log(Volume)"
  )


#4.3a
retail_nsa_x13 <- retail_ts |> 
  filter(series_id == "RSAFSNA") |> 
  model(x13 = X_13ARIMA_SEATS(sales)) |> 
  components()

retail_nsa_x13 |> autoplot()

#December is usually the highest, roughly 15% above trend on average, and January/February are the lowest at about 10% below. The swings have gotten smaller over time, December went from about 21% above trend in 1992 to about 10% recently.

#4.3b
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

#My adjustment matches the Census Bureau's well, mostly within about 0.2%. The biggest gaps are the Januaries after covid (2021-2024) at around 1.4-1.8%. I'm guessing they handled the covid outliers differently or tuned something that made theirs different than mine.


#4.4
can_gas_fit <- can_gas |> 
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |> 
  tidy() |> 
  select(term, estimate, std.error, statistic, p.value) |> 
  knitr::kable(digits = 3)

#The trend coefficient is 0.004 on log volume so gas production grows about 0.4% per month on average. The largest seasonal effect is June (season()year6), about 0.25 log points (roughly 22%) below January, with a t-stat of -4.78.


#4.4b
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

#The fitted line tracks the general trend and the seasonal pattern relatively well, but a straight line misses the shape of the trend, it is too high in the 1960s and 2000s and too low in the 1970s when growth was fastest and then slowed.

#4.4c
can_gas_fit |> gg_tsresiduals()

can_gas_fit |> 
  augment() |> 
  features(.innov, ljung_box, lag = 24)

#The residuals do not look like white noise. There is somewhat of a pattern and a autocorrelation
#There is definitely structure left in these residuals to be modeled further. The Ljung-Box test (Q = 7774, 24 lags) rejects the null of no autocorrelation at the 5% level with a p-value of essentially 0.

# ---- PART 5 -----

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

#5.1a The first forecast evaluated is January 1980 and the last is February 2005. The seasonal naive is much more accurate (MAE 0.56, RMSE 0.70) than the TSLM (MAE 4.01, RMSE 4.36) and both metrics agree.

#5.2a
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

#DM = -31.28 with p-value < 2.2e-16. We reject that the two models are equally accurate, the negative statistic means the seasonal naive has the smaller absolute errors so it is significantly better.


#5.3a
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

#The TSLM changed a lot more (MAE 4.01 to 2.09, RMSE 4.36 to 2.42) while the seasonal naive did not change at all since it only uses the same month last year.
#As we saw in 4.4b a single straight trend over the whole sample misses how growth slowed, so dropping the old data lets the trend line fit the recent growth better.
