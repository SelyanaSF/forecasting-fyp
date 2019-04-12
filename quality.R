library(dplyr)
library(ggplot2)
Product_grade <- data.frame(grade=c('A','B','C'), return_lag_pattern=c(0,0,0))
Description <- c("Good Quality Product","Defective Product","End-of-life Product")
Reason <- c("Exchange with an upgrade model","Exchange with a replacement product","Receive return incentives")
Product_grade <- Product_grade %>% mutate(Description_of_product = Description, Reason_of_return = Reason)
## Monte Carlo Simulation for Multinomial Distribution 
my_prob <- c(0.05,0.1,0.85)
B <- 1000
random_grade <- function(){
  grade <- sample(c('a','b','c'), size = 24, prob=my_prob, replace = TRUE)
  return(grade)
}
sims_quality <- replicate(B, random_grade())
## Barchart
probability_of_grades <- as.data.frame(table(sims_quality)/(ncol(sims_quality)*nrow(sims_quality)))
Product_grade <- Product_grade %>% 
  mutate(probability_of_grades = probability_of_grades$Freq)
boxplot_q <- Product_grade %>%
   ggplot(aes(x=grade, y=probability_of_grades, fill=grade)) + geom_bar(width=0.5, stat="identity") + theme(aspect.ratio = 1.5/1) +scale_y_continuous()
