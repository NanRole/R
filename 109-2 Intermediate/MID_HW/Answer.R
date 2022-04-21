# 第一題
# 1.1
prime <- function(n) {
  for (i in 1:n) {
    count <- 0
    for (j in 1:sqrt(i)) {
      if (i %% j == 0) {
        count <- count + 1 
      }
    }  
    if (count == 1) {
      print(i)
    }
  } 
}
prime(10)
# 1.2
dice <- function(n) {
  result <- sample(1:6, size=n, replace=TRUE)
  times <- integer(6)
  for (i in result) {
    for (j in 1:6) {
      if (i == j) {
        times[j] <- times[j] + 1
      }
    }
  }
  Max <- max(times)
  most <- which(times == Max)
  Min <- min(times)
  least <- which(times == Min)
  detail <- list(result, most, Max, least, Min)
  names(detail) <- c('投擲結果', '出現最多次數的點數是',
    '出現最多幾次', '出現最少次數的點數', '出現最少幾次')
  print(detail)
}
dice(10)
# 1.3
twentyfour <- function(a, b, c, d, e) {
  nums <- c(a, b, c, d, e)
  n24 <- which(nums == 24)
  if (length(n24) == 0) print("沒有24")
  else {
    df <- data.frame(n24, length(nums[nums > 24]), length(nums[nums < 24]))
    names(df) <- c("數字24所在位置", "比24大有幾個", "比24小有幾個")
    print(df)
  }
}
twentyfour(10, 24, 33, 1, 5)
# 1.4
modify <- function(v) {
  new.v <- as.integer(sqrt(v) * 10)
  pass <- ifelse(new.v >= 60, "過", "當")
  df <- data.frame(c('A', 'B', 'C'), v, new.v, pass)
  names(df) <- c('學生姓名', '調整前成績', '調整後成績', '過或當')
  print(df)
}
modify(sample(0:59, 3, replace=TRUE))
# 1.5
electricityBill <- function(kWh, business=FALSE, month=8) {
  summer <- (month >= 6 && month <= 9)
  total <- 0
  if (business == FALSE) {
    if (kWh > 0) {
      total <- total + ifelse(kWh < 120, kWh, 120) * 1.63
      kWh <- kWh - 120
    }
    if (kWh > 0) {
      diff <- 330 - 120
      total <- total + ifelse(kWh < diff, kWh, diff) * ifelse(summer, 2.38, 2.10)
      kWh <- kWh - diff
    }
    if (kWh > 0) {
      diff <- 500 - 330
      total <- total + ifelse(kWh < diff, kWh, diff) * ifelse(summer, 3.52, 2.89)
      kWh <- kWh - diff
    }
    if (kWh > 0) {
      diff <- 700 - 500
      total <- total + ifelse(kWh < diff, kWh, diff) * ifelse(summer, 4.80, 3.94)
      kWh <- kWh - diff
    }
    if (kWh > 0) {
      diff <- 1000 - 700
      total <- total + ifelse(kWh < diff, kWh, diff) * ifelse(summer, 5.66, 4.60)
      kWh <- kWh - diff
    }
    if (kWh > 0) {
      total <- total + kWh * ifelse(summer, 6.41, 5.03)
    }
  }
  else {
    if (kWh > 0) {
      total <- total + ifelse(kWh < 330, kWh, 330) * ifelse(summer, 2.53, 2.12)
      kWh <- kWh - diff
    }
    if (kWh > 0) {
      diff <- 700 - 330
      total <- total + ifelse(kWh < diff, kWh, diff) * ifelse(summer, 3.55, 2.91)
      kWh <- kWh - diff
    }
    if (kWh > 0) {
      diff <- 1500 - 700
      total <- total + ifelse(kWh < diff, kWh, diff) * ifelse(summer, 4.25, 3.44)
      kWh <- kWh - diff
    }
    if (kWh > 0) {
      total <- total + kWh * ifelse(summer, 6.431, 5.05)
    }
  }
  print(round(total))
}
electricityBill(800)

# 第二題
# 2.1
file = 'D:/Download/世界各主要國家之我國留學生人數.csv'
df <- read.csv(file)
print(df)
# 2.2
plt <- function(df, title, color) {
  x <- df$年度
  y <- df$總人數
  plot(x, y, main=title, type="o", col=color, xlab='年度', ylab='留學人數', lwd=2)
  text(x, y, round(y), cex=0.8)
}
En = df[c(1, 4, 7),]
Am = df[c(2, 5, 8),]
Jp = df[c(3, 6, 9),]
par(mfrow=c(3, 1))
plt(En, "英國", "orange")
plt(Am, "美國", "green")
plt(Jp, "日本", "lightblue")

# 第三題
# 3.1
本週台股加權指數漲跌幅 <- c(1230, -456, 890, 654, -230)
names(本週台股加權指數漲跌幅) <- c("星期一", "星期二", "星期三", "星期四", "星期五")
print(本週台股加權指數漲跌幅)
# 3.2
台股上週和本週交易量 <- matrix(c(850, 1250), nrow=1, byrow=TRUE)
colnames(台股上週和本週交易量) <- c("上週成交量(萬張)", "本周成交量(萬張)")
rownames(台股上週和本週交易量) <- "台股加權指數總成交量"
print(台股上週和本週交易量)
# 3.3
股市大盤解析 <- c("強勁反彈", "高檔震盪", "後勢看漲")
大盤操作評論 <- c("大量買進持有", "持續觀察不動", "少量賣出獲利")
操作評論來源 <- c("股達人", "股go", "潮股網")
股市大盤評論 <- data.frame(股市大盤解析, 大盤操作評論, 操作評論來源)
print(股市大盤評論)
# 3.4
台股加權指數列表 <- list(本週台股加權指數漲跌幅, 台股上週和本週交易量, 股市大盤評論)
names(台股加權指數列表) <- c("台股大盤本週漲跌幅", "台股兩週成交量", "台股評論")
print(台股加權指數列表)
# 3.5
資料提供 <- "亂談股市"
台股加權指數列表 <- c(台股加權指數列表, 資料提供來源=資料提供)
print(台股加權指數列表)