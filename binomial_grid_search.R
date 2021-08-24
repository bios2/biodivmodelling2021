
x_obs <- rbinom(10, 6, 0.6)
x_obs

like_binom <- function(p, d = x_obs){
  exp(sum(dbinom(d, size = 6, prob = p, log = TRUE)))
}


grid_searched <- tibble(
  p = seq(from = 0.01, to = .99, length.out = 200),
  density_p = dbeta(p, .03*20, (1-.03*20)),
  likelihood = map_dbl(p, loglik_binom),
  prior_times_likelihood = density_p * likelihood,
  post = prior_times_likelihood / sum (prior_times_likelihood)
)

grid_searched$likelihood

grid_searched %>%
  ggplot(aes(x = p, y = likelihood)) +
  geom_line() +
  geom_line(aes(y = prior_times_likelihood), lwd = 2, col = "darkred")  +
  geom_line(aes(y = post))

grid_searched %>%
  ggplot(aes(x = p, y = post)) + geom_line() +
  geom_line(aes(y = density_p))

grid_searched$post %>% sum
grid_searched$density_p %>% sum

curve(dbeta(x, .3*10, .7*10))
