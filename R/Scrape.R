library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(rvest)

region <- tibble(
  code = c("00", "08", "09", "10", "11", "12", "13", "14", "15", "42", 
           "57", "58", "59", "60", "65", "68", "74", "75", "77"),
  name = c("福岡県", "福岡市中央区", "福岡市博多区", "福岡市南区", "福岡市早良区", 
             "福岡市東区", "福岡市西区", "福岡市城南区", "北九州市", "久留米市", "宗像・遠賀", 
             "粕屋", "筑紫", "糸島", "田川", "北筑後", "南筑後", "京築", "嘉穂・鞍手")
)

# dat <- read_csv("Data/reference_data.csv") |> 
#   pivot_longer(reports:per_sentinel) |> 
#   mutate(region = "福岡県")

fn <- list.files("Data", "^data", full.names = TRUE) |> last()
dat <- read_csv(fn)

base_url <- "http://www.fihes.pref.fukuoka.jp/~idsc_fukuoka/idwr/idwr1/"

get_data <- function(i = 1){
  reg_code <- region$code[i]
  reg_name <- region$name[i]
  
  my_url <- str_c(base_url, reg_code, ".html")

  htm <- read_html(my_url)
  tmp <- htm |> html_element("table") |> html_table(na.strings = "-")
  dat <- tmp |> slice(2:4) |> select(-1) |> t() |> 
    as_tibble(.name_repair = "unique") |> 
    rlang::set_names(c("year_week", "name", "value")) |> 
    mutate(year = str_extract(year_week, "^[0-9]{4}年"),
           year = str_extract(year, "[0-9]+"),
           week = str_extract(year_week, "(第).+(週)"),
           week = str_extract(week, "[0-9]+"),) |> 
    type_convert(col_types = "ccddd") |> 
    drop_na() |> select(-year_week) |> 
    mutate(name = case_when(name == "報告数" ~ "reports",
                            name == "定当"~ "per_sentinel",
                            TRUE ~ name),
           region = reg_name) |> 
    select(year, week, name, value, region)
  return(dat)
}

tmp <- list()
for(i in 1:19){
  tmp[[i]] <- get_data(i)
}
tmp <- tmp |> bind_rows()

# tmp

dat_updated <- dat |> 
  left_join(tmp, by = join_by(year, week, name, region)) |> 
  mutate(value = if_else(is.na(value.y), value.x, value.y)) |> 
  select(year, week, name, value, region)
  
tmp_filterd <- tmp |>
  anti_join(dat, by = join_by(year, week, name, region))

dat <- dat_updated |> bind_rows(tmp_filterd)

today <-Sys.Date()

dat |> write_excel_csv(paste("data/data_", today, ".csv", sep = ""))
