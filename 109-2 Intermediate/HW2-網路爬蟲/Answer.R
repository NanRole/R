library(rvest)
url <- 'https://tw.stock.yahoo.com/d/s/dividend_2330.html'
stock.market <- read_html(url, encoding='big5')

現金股利 <- stock.market %>%
            html_nodes('table+ table td:nth-child(3)') %>%
            html_text()

獲利年度 <- stock.market %>%
            html_nodes('td table+ table td:nth-child(1)') %>%
            html_text()

cash <- 現金股利[2:length(現金股利)] %>% as.numeric()
year <- 獲利年度[2:length(獲利年度)]

bp <- barplot(cash, names.arg=year, xlab=獲利年度[1],
              ylab=現金股利[1], main='台積電現金股利資料長條圖', col="skyblue")
text(bp, cash, labels=cash, pos=1)
average <- sum(cash) / length(cash)
average