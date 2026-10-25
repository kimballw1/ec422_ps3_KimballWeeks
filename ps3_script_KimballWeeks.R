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


can_gas_q <- can_gas |> 
  filter(Month <= yearmonth("2004 Dec")) |> 
  index_by(Quarter = yearquarter(Month)) |> 
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)



