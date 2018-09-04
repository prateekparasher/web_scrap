

road_data <- read.csv("C:/Users/PRATEEK PARASHER/Documents/road_data.csv") 
warning_data <- read.csv("C:/Users/PRATEEK PARASHER/Documents/warning_dataset.csv") 
data <- cbind(road_data, warning_data)







install.packages("pwr")
library(pwr)
library(dplyr)
effect_size <- cohen.ES(test= "r", size= "large")
effect_size
sample_size <- pwr.r.test(r = effect_size$effect.size, sig.level = 0.05, power = 0.8)
sample_size
plot(sample_size)


head(data)

sample_data <- sample_n(data, 29)
'replace' = TRUE



cor.test( sample_data$accident, sample_data$month)


str(data)
