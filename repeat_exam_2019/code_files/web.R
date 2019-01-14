#install package 
install.packages("rvest")
library(rvest)
#read link 
url <- 'https://twitter.com/weatherrte?lang=en'
web_page <- read_html(url)
#head & str data
head(web_page)
str(web_page)

#ranking tweet data 
warning <- html_nodes(web_page,'.tweet-text')
head(warning, 30)
str(warning)
#leghth of tweet data
length(warning)
data <- html_text(warning)
#show first 30 tweets 
head(data, 30)
str(data)


#Build a term-document matrix
dtm <- TermDocumentMatrix(data)
m <- as.matrix(data)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)
str(d)


barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="Red", main ="Most frequent words",
        ylab = "Word frequencies")

