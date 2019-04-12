library(dplyr)
library(ggplot2)
y <- simulated_sales$Sales_Quantity
## Make the function for Bass diffusion model
bass_diff <- function(m,t1,t2,t) {
  data_per <- c(0)
  for (each in 1:t) {
    p <- sum(data_per)
    x <- t1*m + (t2-t1)*p - t2/m *p^2
    data_per[each+1] <- round(x)
  }
  return(data_per)
}
t <- 24
sales_data <- data.frame(bass_diff(50000,0.03,0.38,t))
sales_mat <- matrix(sales_data[2:25,])
## Set the delay parameter matrix
p <- 0.5
q <- 0.6
delay_parm <- matrix(0,24,1)
for (each in 1:24) {
  s <- (1-q)^(each-1)
  delay_parm[each,] <- s
}
## Set the iteration of sales_data matrix
sales_matrix <- matrix(0,24,24)
for (j in 1:24) {
  if (j == 1) {
    sales_matrix[j,1] <- sales_mat[j,]
  }
  else {
    sales_matrix[j,c(1:j)] <- sales_mat[c(j:1),]
  }
}
## Generate the predicted quantity
historical_return <- round((p*q)*round(sales_matrix%*%delay_parm))
historical_return <- matrix(c(0,0,historical_return))
sales_data <- data.frame(sales_data[1:25,1])
period <- c(0:24)
sales_data <- cbind(sales_data,historical_return[1:25,1],period)
names(sales_data) <- c("estimated_sales","historical_return","period")
# Visualize
figure_1 <- ggplot(sales_data, aes(period)) + 
  ggtitle("Forecasting") +
  geom_line(aes(y = estimated_sales, colour = c("var1"))) + 
  geom_line(aes(y = historical_return, colour = c("var2"))) +
  geom_point(aes(y=historical_return, colour="var2")) +
  scale_color_discrete(name="",labels = c("Estimated Sales", "Historical Return"))



