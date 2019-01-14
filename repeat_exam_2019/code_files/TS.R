
data_p <- read.csv("C:/Users/PRATEEK PARASHER/Downloads/TS_1.csv")

data_p = scan()

plot.ts(data_p)

data_p =runif(n=251 , min= -12 , max= 24)

datats = ts(data = data_p,
            c(start = 1998,1) , c(end = 2018,10) , frequency = 12)

plot(datats)

class(datats)

### Decomposing Time Series (U)



frequency(datats)

length(datats)

decompose(datats, type = "additive")

plot(decompose(datats, type = "additive"))

library(forecast)

library(ggplot2)

# library(ggplot2 and forecast)
autoplot(decompose(datats, type = "additive"))

# alternatively the function stl could be used
plot(stl(datats, s.window="periodic"))

stl(datats, s.window="periodic")

# seasonal adjustment
mydatats = decompose(datats, "additive")

class(mydatats)

# we are subtracting the seasonal element
mydatatsadjusted = datats-mydatats$seasonal

# getting a plot
plot(mydatatsadjusted)

plot(mydatats$seasonal)

# a stl forecast from the package forecast
library(forecast)
plot(stlf(datats, method = "arima"))


