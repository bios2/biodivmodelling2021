# metropolis algo for mean of poisson

# poiss variates
set.seed(1859)
xs <- rpois(5, 27)

# likelihood

make_likelihood <- function(f = dpois){
  force(f)
  function(data, ...) sum(f(data, ..., log = TRUE))
}

poisson_loglike <- make_likelihood(dpois)

poisson_loglike(xs, lambda = 5)

normal_loglike <- make_likelihood(f = dnorm)

normal_loglike(5, mean = 10, sd = 2)

numerator <- function(v, data = xs)  poisson_loglike(data, lambda = exp(v)) + normal_loglike(v, mean = 1, sd = 2)

grid <- seq(from= -2, to = 5, length.out = 30)
y <- purrr::map_dbl(grid, ~ numerator(.))

plot(grid, exp(y)/sum(exp(y)))


# proposal function

propose_new <- function(x, sig_tune) rnorm(1, mean = x, sd = sig_tune)


# start value

chain <- numeric(5000)


chain[1] <- 0.1

for (i in 2:length(chain)){

  start <- chain[i-1]

  new <- propose_new(start, .2)

  r <- exp(numerator(new) - numerator(start))

  p_accept <- min(1, r)

  chain[i] <- ifelse(runif(1) < p_accept, new, start)
}

plot(exp(chain))
plot(exp(chain[250:500]), type = "l")
plot(density(exp(chain[250:500])))

median(exp(chain))


## conjugate

curve(dgamma(x, (25/15)^2, 25/15^2), xlim = c(1, 50))

plot(density(exp(chain)))

curve(dgamma(x, (25/15)^2 + sum(xs),
             25/15^2 + length(xs)),
      xlim = c(1, 50),
      add = TRUE)
