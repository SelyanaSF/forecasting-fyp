library(dplyr)
library(ggplot2)
library(openxlsx)
simulated_sales <- sales_data_new
Quality_Grade <- data.frame(Number=c(1:25))
for (i in 1:nrow(probability_of_grades)){
  model <- round(simulated_sales$Predicted_Return*probability_of_grades$Freq[i])
  Quality_Grade[, paste0("grade_", probability_of_grades$sims[i])] <- model
}
simulated_sales <- cbind(simulated_sales,Quality_Grade[c(2:4)])

write.xlsx(simulated_sales, file = "simulated_sales_excel.xlsx")