# B C C D D B C N C B

# 第一題
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

核准貸款 <- read.csv("D:/Download/核准貸款.csv", header=T, sep=',')
核准貸款

核准資料樹 <- rpart(核可貸款 ~ ., data=核准貸款,
                   method="class",
                   control = rpart.control(minsplit = 5))

fancyRpartPlot(核准資料樹)

newdata <- data.frame(年紀='年輕',工作='無',房子='無',信用='好')

predict(核准資料樹, newdata, type='class')

# 第二題
library(tm)
library(magrittr)

interview <- readLines("D:/Download/Elon_Musk_interview.txt")
corpus <- VCorpus(VectorSource(interview)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, c(stopwords("english"), "thats"))

tdm <- TermDocumentMatrix(corpus) %>% 
  as.matrix() %>%
  rowSums() %>%
  sort(decreasing = T) %>%
  head()

tdmdf <- data.frame(freq=tdm)
tdmdf

bf <- barplot(tdmdf$freq, names.arg = rownames(tdmdf), xlab='words',
              ylab='freq', main='前六多詞彙長條圖', col='orange')
text(bf, tdmdf$freq, labels=tdmdf$freq, pos=1)