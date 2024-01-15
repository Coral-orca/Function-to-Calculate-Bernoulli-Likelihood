# Formulating a function for bernoulli lielihood
# Plotting likelihoods for bernoulli distribution
# Define a function to compute likelihood
likelihood = function(n, y, theta) {
  return(theta^y * (1 - theta)^(n-y))
}

# Create a sequence of values of mortality rates
theta = seq(0.01, 0.99, 0.01)

# Plot the above data and function
plot(theta, likelihood(500, 60, theta))
abline(v = (60/500)) # Likelihood is maximised at this point

# Define a function to compute log likelihood
# Log likelihood is much easier and stable to interpret
loglike = function(n, y, theta) {
  return(y * log(theta) + (n-y) * log(1 - theta))
}

# Plot the sequence against the log likelihood of that sequence
plot(theta, loglike(500, 60, theta), type = "l")
abline(v = (60/500))