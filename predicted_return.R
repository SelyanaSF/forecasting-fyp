library(dplyr)
library(ggplot2)
#new parameter
p_2 <- 0.5023597
q_2 <- MLE_trial_q$parameters
delay_parm_2 <- matrix(0,24,1) 
for (each in 1:24) {
  s_2 <- (1-q)^(each-1)
  delay_parm_2[each,] <- s_2
}
# predicted return 
predicted_return <- round((p_2*q_2)*round(sales_matrix%*%delay_parm_2))
predicted_return <- c(0,0,predicted_return)
sales_data_new <- data.frame(sales_data$period,sales_data$estimated_sales,sales_data$historical_return,predicted_return[1:25])
names(sales_data_new) <- c("Period","Estimated_Sales","Historical_Return","Predicted_Return")

##Comparison
figure_2 <- ggplot(sales_data_new,aes(Period)) + 
  ggtitle("Parameter with MLE") +
  geom_line(aes(y=Historical_Return, color = "Historical_Return")) + 
  geom_line(aes(y=Predicted_Return, color = "Predicted_Return")) +
  scale_colour_manual(values=c("#CC0033","#000666"))

## Visualize error
MAE <- c(sales_data_new$Predicted_Return-sales_data_new$Historical_Return)
MAPE <- c((sales_data_new$Predicted_Return-sales_data_new$Historical_Return)/sales_data_new$Predicted_Return)
sales_data_new <- cbind(sales_data_new, MAE, MAPE)

sales_data_new[sales_data_new == "NaN"] <- NA
error_figure <- ggplot(sales_data_new, aes(Period)) +
  ggtitle("Prediction Error") + 
  geom_line(aes(y=MAPE, color="MAPE"))

## Mean of MAE and MAPE

mean_MAE <- sum(sales_data_new$MAE)/(t-1)
mean_MAPE <- sum(sales_data_new$MAPE, na.rm = TRUE)/(t-1)
