library(EnvStats)
## Generate the return data of Geometric Distribution Trial
set.seed(200)
prob_p <- round(prob_p,2)
my_data <- rgeom(sales_data$estimated_sales,prob_p)
# Search for estimated p 
MLE_trial_p <- egeom(my_data,method = "mle")

## Generate the return data using the prob p 
set.seed(200)
prob_2 <- MLE_trial_p$parameters
my_data_2 <- rgeom(sales_data$estimated_sales,prob_2)
## Search for conditional probability of p (which is q)
MLE_trial_q <- egeom(my_data_2, method="mle")


