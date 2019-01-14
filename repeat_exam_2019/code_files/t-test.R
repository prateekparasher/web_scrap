install.packages("ggpubr")

my_data <- read.csv("C:/Users/PRATEEK PARASHER/Downloads/data_1.csv")

head(my_data)

str(my_data)

library(ggpubr)
ggboxplot(my_data$Lowest.Temp, 
          ylab = "Temp(C)", xlab = FALSE,
          ggtheme = theme_minimal())

library("ggpubr")
ggqqplot(my_data$Lowest.Temp, ylab = "Min Temp(C)",
         ggtheme = theme_minimal())

# One-sample t-test
res <- t.test(my_data$Lowest.Temp, mu = 2)
# Printing the results
res 


res$estimate


mean(my_data$Lowest.Temp)
sd(my_data$Lowest.Temp)


# A small p-value (typically ??? 0.05) indicates strong evidence against the null hypothesis, so you reject it.
# A large p-value (> 0.05) indicates weak evidence against the null hypothesis, so you fail to reject it.
# p-values very close to the cutoff (0.05) are considered to be marginal (could go either way). 

Dublin_temp_data <- read.csv("C:/Users/PRATEEK PARASHER/Downloads/data_1.csv") 
Dublin_rain_data <- read.csv("C:/Users/PRATEEK PARASHER/Downloads/data_2.csv") 

pwr.t.test(d = .8, sig.level = .05, power = .9, type = "one.sample",
           alternative = "two.sided")



