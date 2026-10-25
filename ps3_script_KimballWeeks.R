library(tidyverse)

retail_raw <- read_csv("data/fredgraph.csv")


#fixing column types
retail_raw <- read_csv("data/fredgraph.csv", na = ".", 
                       col_types = cols(
                         observation_date = col_date(format = "%Y-%m-%d"),
                         RSAFS = col_double(),
                         RSAFSNA = col_double()
                       )
                )


