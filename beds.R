library(chromote)

# 療養状況等及び入院患者受入病床数等に関する調査について
# https://www.mhlw.go.jp/stf/seisakunitsuite/newpage_00023.html

html <- "https://www.mhlw.go.jp/stf/seisakunitsuite/newpage_00023.html"

b <- ChromoteSession$new()

b
b$Browser$getVersion()
b$Page$navigate(html)
# b$screenshot()

b
x <- b$DOM$getDocument()
x
node <- b$DOM$querySelectorAll(x$root$nodeId, 'li [href$=".xlsx"]')$nodeId
unlist(node)


# //li[contains(text(), 'hogehoge')]


tmp <- purrr::map_chr(node, function(x){b$DOM$getOuterHTML(x)|> unlist()})

files <- stringr::str_extract(tmp, "href=\"(.*)\"><img", group = 1)

files <- paste0("https://www.mhlw.go.jp", files)
destfiles <- paste0("mhlw/", basename(files))
download.file(files, destfiles)

b$close()
