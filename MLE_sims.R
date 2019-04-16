library(EnvStats)
## Generate the return data of Geometric Distribution Trial for p
set.seed(200)
my_data <- rgeom(sales_data$estimated_sales,0.5)
condprob_p <- egeom(my_data,method = "mle")
# Search for estimated q
set.seed(200)
my_data_2 <- rgeom(sales_data$estimated_sales,condprob_p$parameters)
MLE_trial_q <- egeom(my_data_2,method = "mle")



## Do the Bernoulli Trial 
B <- replicate(299, {
  s <- sample(c(0,1), size=10, replace=TRUE)
})
n <-1 
# formulation for the log likelihood for the binomial 
logL <- function(p) sum(log(dbinom(B, n, p)))

# search for estimated p (optimum):
estimated_p <- optimize(logL, lower=0, upper=1, maximum=TRUE)


