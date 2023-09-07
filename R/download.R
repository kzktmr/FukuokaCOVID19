library(readxl)

# 新型コロナウイルス感染症に伴う救急搬送困難事案に係る状況調査について
# https://www.fdma.go.jp/disaster/coronavirus/post-1.html
tmp_file <- tempfile()
download.file("https://www.fdma.go.jp/disaster/coronavirus/items/coronavirus_data.xlsx", tmp_file)
tmp <- read_excel(tmp_file, skip = 5, col_names = FALSE) %>% 
  rename(pref = "...1", city = "...2") %>% 
  select(-pref) %>% filter(city %in% c("福岡市消防局", "北九州市消防局")) %>% 
  pivot_longer(-city) %>% 
  mutate(week = as.integer(str_extract(name, "[0-9]+")) - 2,
         date = lubridate::ymd("2020-03-30") + lubridate::weeks(week - 1 )) %>% 
  select(date, city, value)

write_csv(tmp, "Data/ambulance.csv")

# 全国医療機関の医療提供体制の状況
# https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000121431_00130.html#h2_free4
tmp_file <- tempfile()
download.file("https://www.mhlw.go.jp/content/001143591.xlsx", tmp_file)

tmp <-
  read_excel(tmp_file, skip = 9) |> 
  rename(no = "No.", pref = "都道府県", type = "...3") |> 
  select(-no) |> fill(pref) |> 
  filter(pref == "福岡県", stringr::str_detect(type, "数$")) |> 
  select(-pref) |> 
  pivot_longer(-type, names_to = "day") |> 
  mutate(date = as.Date(as.numeric(day), origin = "1899-12-30")) |> 
  select(date, type, value)

write_csv(tmp, "Data/nurse.csv")
