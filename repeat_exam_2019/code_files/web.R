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
