
# in - class grid search

set.seed(1859)
x_obs <- rbinom(10, 6, 0.6)
x_obs

# 1. make a vector of probabilities for $p$ using `seq(from = 0, to = 1, length.out = n)`
theta <- seq(from = 0.01, to = 0.99, by = 0.01)
# 1. Think about the prior you might use. Look at `curve(dbeta(x))`
prior_unif <- dunif(theta, 0, 1)

pr_lik <- sapply(X = theta, function(X)
  exp(sum(dbinom(x_obs, size = 6, prob = X, log = TRUE))))


pr_lik <- sapply(X = theta, function(X)
  prod(dbinom(x_obs, size = 6, prob = X, log = FALSE)))

plot(theta, pr_lik)

numerator <- pr_lik * prior_unif

# normalizing constant
denom <- sum(numerator)

posterior <- numerator/denom

sum(posterior)

plot(theta, posterior)
points(theta, prior_unif, col = "red")

xx <- 1:8
sum(xx/sum(xx))


curve(dbeta(x, 1 + sum(x_obs), 1 + sum(6 - x_obs)))

plot(theta, posterior)

posterior2 <- dbeta(theta, shape1 = (1 + sum(x_obs)), shape2 = (1 + sum(6 - x_obs)))

lines(theta, posterior2/sum(posterior2))


theta[which.max(posterior)]
# conjugate prior mean
(1 + sum(x_obs)) / (1 + sum(x_obs) + 1 + sum(6 - x_obs))

###
alpha_new <- 10 + sum(x_obs)

beta_new <- 90 + sum(6 - x_obs)

conj <- dbeta(theta, alpha_new, beta_new)
lines(theta, conj/sum(conj))


alpha_new / (alpha_new + beta_new)



curve(dbeta(x, 2, 2))
curve(dbeta(x, .8*6, (1-.8)*6))
curve(dbeta(x, 2, 2))

curve(dunif(x))

# 1. find the probability of each of these values of $p$ via `dbeta`
prior_dens <- dbeta(p, 2, 2)
plot(p, prior_dens)
# 1. find the likelihood for each of these values of $p$ via `dbinom`
log_lik <- function(x) (sum(dbinom(x_obs, size = 6, prob = x, log = TRUE)))


# 1. multiply these two columns together
# 1. normalize (divide by the sum)
