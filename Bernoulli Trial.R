B <- replicate (1000, {
  s <- sample(c(0,1), size= 200, replace = TRUE)
  mean(s)
  }
)
prob_p <- mean(B)
prob_p