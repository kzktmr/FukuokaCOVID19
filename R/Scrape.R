library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(rvest)

reg <- tibble(
  code = c("00", "08"),
  region = c("福岡県全体", "福岡市中央区")
)
  
  
base_url <- "http://www.fihes.pref.fukuoka.jp/~idsc_fukuoka/idwr/idwr1/"

i <- 2

code <- reg$code[i]
region <- reg$region[i]
  
my_url <- str_c(base_url, code, ".html")

htm <- read_html(my_url)
tmp <- htm |> html_element("table") |> html_table(na.strings = "-")
dat <- tmp |> slice(2:4) |> select(-1) |> t() |> 
  as_tibble() |> rlang::set_names(c("year_week", "name", "value")) |> 
  mutate(year = str_extract(year_week, "^[0-9]{4}年"),
         year = str_extract(year, "[0-9]+"),
         week = str_extract(year_week, "(第).+(週)"),
         week = str_extract(week, "[0-9]+"),) |> 
  type_convert() |> drop_na() |> select(-year_week) |> 
  mutate(region = region)

dat

