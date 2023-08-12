library(chromote)

# 療養状況等及び入院患者受入病床数等に関する調査について
# https://www.mhlw.go.jp/stf/seisakunitsuite/newpage_00023.html

html <- "https://www.mhlw.go.jp/stf/seisakunitsuite/newpage_00023.html"

b <- ChromoteSession$new()

b$Page$navigate(html)

x <- b$DOM$getDocument()
x
xlsx_node <- b$DOM$querySelectorAll(x$root$nodeId, 'li [href$=".xlsx"]')$nodeId
n <- length(xlsx_node)

date <- b$DOM$querySelectorAll(x$root$nodeId, 'div [class="m-grid"] li')$nodeId |> unlist()
date <- date[seq(n) * 3 - 2]

dst_files <- purrr::map_chr(date, function(x){b$DOM$getOuterHTML(x)|> unlist()}) |> 
  stringr::str_extract("[0-9]{4}年.+月.+日") |> 
  stringi::stri_trans_general("Fullwidth-Halfwidth") |> 
  as.Date("%Y年%m月%d日") |> paste0(".xlsx")

files <- purrr::map_chr(xlsx_node, function(x){b$DOM$getOuterHTML(x)|> unlist()}) |> 
  stringr::str_extract("href=\"(.*)\"><img", group = 1)
files <- paste0("https://www.mhlw.go.jp", files)

length(files) == length(dst_files)

destfiles <- paste0("mhlw/", dst_files)

download.file(files, destfiles)

b$close()

# ====


