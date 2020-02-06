library(readr)
DiamondDataComplete <- read.csv(file.choose(), header=TRUE)
View(DiamondDataComplete)


#install.packages('neuralnet')
library(neuralnet)


s <- sample(nrow(DiamondDataComplete), size=10000, replace = FALSE, prob = NULL)
diamonds.subset <- DiamondDataComplete[s, ]
attach(diamonds.subset)

diamonds.subset$cut = factor(diamonds.subset$cut, levels = c('Fair','Good','Ideal', 'Premium','Very Good'), labels = c(0,1,2,3,4))
diamonds.subset$color = factor(diamonds.subset$color, levels = c('D','E','F','G','H','I','J'), labels = c(0,1,2,3,4,5,6))
diamonds.subset$clarity = factor(diamonds.subset$clarity, levels = c('I1','IF','SI1','SI2','VS1','VS2','VVS1','VVS2'), labels = c(0,1,2,3,4,5,6,7))

View(diamonds.subset)



library(tidyverse)
#install.packages("GGally")
library(GGally)

scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

diamonds.subset_nn <- diamonds.subset %>%
  mutate(carat = scale01(carat),
         table = scale01(table),
         depth = scale01(depth),
         price = scale01(price),
         x = scale01(x),
         y = scale01(y),
         z = scale01(z),
         cut = as.numeric(cut) - 1,
         color = as.numeric(color)- 1,
         clarity = as.numeric(clarity)-1) 


View(diamonds.subset_nn)

set.seed(123)

Diamond_NN1 <- neuralnet(cut ~ carat + table + depth + price + x + y + z + color + clarity, 
                         data = diamonds.subset_nn, 
                         linear.output = FALSE, 
                         likelihood = TRUE)



plot(Diamond_NN1, rep = 'best')


NN_5 <- neuralnet(cut ~ carat + table + depth + price + x + y + z + color + clarity, 
                       data = diamonds.subset_nn, 
                       linear.output = FALSE, 
                       likelihood = TRUE,
                       threshold = 0.01,
                       hidden = 5)



plot(NN_5)


NN_5$result.matrix


#Testing the resulting output

library(dplyr)

testing <- subset(diamonds.subset_nn, select = c("carat","table","depth","price","x","y","z","color","clarity"))
head(testing)

nn_results <- neuralnet::compute(NN_5, testing)

results <- data.frame(actual = diamonds.subset_nn$cut, prediction = nn_results$net.result)
results


accuracy <- sum(results$prediction)/length(diamonds.subset$cut)*100
print(accuracy)

#99.99995   -- accuracy of neural network model hidden = 5






