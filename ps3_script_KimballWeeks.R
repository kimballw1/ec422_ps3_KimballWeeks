library(tidyverse)


# ---- PART 2 ----
retail_raw <- read_csv("data/fredgraph.csv")

head(retail_raw)

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



library(fpp3)

retail_ts <- retail_raw_long |> 
  mutate(Month = yearmonth(observation_date)) |> 
  as_tsibble(index = Month, key = series_id)

head(retail_ts)

key_vars(canadian_gas)
index_var(canadian_gas)

#creating working copy of can gas
can_gas <- canadian_gas |> 
  select(Month, Volume)

has_gaps(can_gas)

#making new copy of can gas keeping only recent dates
can_gas_recent <- can_gas |> 
  filter(Month >= yearmonth("1990 Jan"))

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
#While the seasonality of the retail and food service sales is adjusted for in the RSAFS series the long term patter remains the same.
#We can see that during the great recession and Covid they have the same drops and the trend is almost identical.

#plotting Canadian gas production
can_gas |> 
  autoplot(Volume) +
  labs(
    title = "Canadian gas production",
    x = "time",
    y = "Volume in oil production"
  ) + theme_minimal() + geom_smooth(method = 'lm')

#2.7d
#I included a fitted line to the canadian gas production series plot to understand the general trend of the data.


# --- PART 3 ----

can_gas <- can_gas |> 
  mutate(log_volume = log(Volume))

can_gas |> 
  autoplot(log_volume) +
  labs(
    title = "Canadian gas production",
    x = "time",
    y = "Volume in oil production"
  ) + theme_minimal()


#3.1b
#After logging volume in the Canadaian gas production series we see that as discussed in clas the constant percent change became the absolute change
#which reduced the swings in the middle of the data that we saw from before and the trend looks to be more consistent. I think that logs may actually be a poorer choice here becuase the 
#swings/seasonlity so to speak in the data was not increased soly on level. There where periods with more and less which I think can tell us a lot.


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


#The case for logging here is very weak as it does basically nothing so both are almost identical besides reducing Covids visual impact.

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
#The mom growth for the SA and NSA series are much different. The seasonally adjusted retail series is much quieter and has smaller spikes and dips. This is because retail sales change wildly over the coarse of a year so the series is very volatile. 
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
    title = "Seasonal Pattern in canadian oil produciton",
    y = "log(Volume)"
  ) +
  theme_minimal()





