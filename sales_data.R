library(dplyr)
library(ggplot2)
library(tidyverse)
simulated_sales <- read.csv("Simulated sales and return data.csv", header=TRUE, sep=';')
Quality_Grade <- data.frame(Number=c(1:24))
for (i in 1:nrow(probability_of_grades)){
  model <- round(simulated_sales$Historical_Return_Quantity*probability_of_grades$Freq[i])
  Quality_Grade[, paste0("grade_", probability_of_grades$sims[i])] <- model
}
simulated_sales <- cbind(simulated_sales,Quality_Grade[2:(nrow(probability_of_grades)+1)])
simulated_sales 
