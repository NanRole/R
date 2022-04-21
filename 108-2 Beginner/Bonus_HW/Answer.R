#第一題
水仙花 <- function(n) {
  for (i in 100:n) {
    sum <- (i %/% 100)**3 + (i %/% 10 %% 10)^3 + (i %% 10)**3
    if (i == sum) {
      print(i)
      print('水仙花數')
    }
  }
}
水仙花(999)

#第二題
買入樂透號碼 <- function(a, b, c, d, e, f, g) {
  yourNum <- c(a, b, c, d, e, f, g)
  prizeNum <- c(28, 15, 37, 5, 17, 32, 8)
  count <- 0
  special <- FALSE
  for (i in 1:7) {
    for (j in 1:7) {
      if (i == 7 & yourNum[j] == prizeNum[i])
        special <- TRUE
      else if (yourNum[j] == prizeNum[i]) 
        count <- count + 1
    }
  }
  if (count == 6)
    cat('頭獎 2000000元\n')
  else if (count == 5 & special)
    cat('二獎 200000元\n')
  else if (count == 5)
    cat('三獎 20000元\n')
  else if (count == 4)
    cat('四獎 2000元\n')
  else if (count == 3)
    cat('五獎 200元\n')
  else
    cat('沒有中獎\n')
}
買入樂透號碼(3, 23, 5, 17, 28, 32, 10)
買入樂透號碼(3, 8, 15, 27, 28, 37, 42)

#第三題
compare <- function(v) {
  max <- v[1]
  min <- v[1]
  for (i in 2:9) {
    if (v[i] > max)
      max <- v[i]
    if (v[i] < min)
      min <- v[i]
  }
  cat('max =', max, '\n')
  cat('min =', min, '\n')
}
numbers <- c(11, 13, 17, 5, 23, 57, 319, 221, 307)
compare(numbers)

#第四題
AddNumbers <- function(n) {
  sum <- 0
  for (i in 1:n) 
    sum <- sum + i
  cat(sum, '\n')
}
AddNumbers(100)
AddNumbers(10)

#第五題
numbers <- function(n) {
  sum <- 0
  while (n != 0) {
    sum <- sum + n %% 10
    n <- n %/% 10
  }
  cat(sum, '\n')
}
numbers(1234)
numbers(5678)
