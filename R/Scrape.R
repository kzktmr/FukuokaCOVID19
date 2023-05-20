library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(rvest)
library(ggplot2)

reg <- tibble(
  code = c("00", "08", "09", "10", "11", "12", "13", "14", "15", "42", 
           "57", "58", "59", "60", "65", "68", "74", "75", "77"),
  region = c("福岡県全体", "福岡市中央区", "福岡市博多区", "福岡市南区", "福岡市早良区", 
             "福岡市東区", "福岡市西区", "福岡市城南区", "北九州市", "久留米市", "宗像・遠賀", 
             "粕屋", "筑紫", "糸島", "田川", "北筑後", "南筑後", "京築", "嘉穂・鞍手")
)
  
ref_data <- read_csv("Data/reference_data.csv")

base_url <- "http://www.fihes.pref.fukuoka.jp/~idsc_fukuoka/idwr/idwr1/"

get_data <- function(i = 1){
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
    type_convert(col_types = "ccddd") |> 
    drop_na() |> select(-year_week) |> 
    mutate(name = case_when(name == "報告数" ~ "reports",
                            name == "定当"~ "per_sentinel",
                            TRUE ~ name),
           region = region)
  return(dat)
}

get_data(1)

get_data(1) |> select(-region) |> pivot_wider() |> 
  bind_rows(ref_data) |> arrange(year, week) |> 
  mutate(year_week = str_glue("{year}_{sprintf('%02d', week)}")) -> dat

dat |>   
  ggplot() + aes(x = year_week, y = per_sentinel) + geom_col() + theme_bw() +
  scale_x_discrete(labels = dat$week) +
  theme(axis.title = element_blank())
  



