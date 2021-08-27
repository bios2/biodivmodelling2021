## Various demos done in-class by Andrew

m <- matrix(rnorm(25), ncol = 5)
m

b <- matrix(rbinom(25, 1, .1), ncol = 5)
inter <- b*m

not_zero <- which(abs(inter) > .01, arr.ind = TRUE )
inter[not_zero]

pick_one <- function(m) matrix(m[sample(1:nrow(m), 1),], ncol = 2)


zero <- which(abs(inter) < .01, arr.ind = TRUE )


targ_zero <- pick_one(zero)
targ_non <- pick_one(not_zero)

inter[matrix(not_zero[2,], ncol = 2)]








curve(exp(-3*x), xlim = c(0, 4))
curve(exp(-.7*x), xlim = c(0, 4), add = TRUE)


quad <- function(x, a, b, c) a * (x - b)^2 + c
curve(quad(x, 20, 4, -3), xlim = c(-5,10))
curve(quad(x, 200, -3, 1000), xlim = c(-5,10), add = TRUE)


curve(exp(quad(x, -5, 0,0)), xlim = c(0,5))

curve(exp(quad(x, -.2, 0,0)), xlim = c(0,5), add = TRUE)








inter[targ_zero] <- inter[targ_non]
inter[targ_non] <- 0
inter





vrai_moy <- 17
n <- 30
set.seed(1859)
plantes <- rpois(n, vrai_moy)
plantes

## model bayesian
## y ~ Poisson(lambda)
## lambda ~ gamma


curve(dgamma(x, 20, 3), xlim = c(0,25))

# posterior
## gamma(a + sum(y), b + n)
n
sum(plantes)

curve(dgamma(x, 20 + 490, 3 + 30), add = TRUE, col = "red")


llogs <- (rlnorm(3000, meanlog =  log(25), sdlog = log(4)))
mean(log(llogs))
sd(log(llogs))
log(4)
hist(llogs)
median(llogs)

exp(log(25) + 4^2/2)
sd(llogs)
exp(4)


## rejection sampling -------------

so_many_ns <- sample(1:25, 1e4, replace = TRUE)
so_many_ns
hist(so_many_ns)

lil_nums <- dbinom(so_many_ns, size = 25, prob = .2)/(dunif(so_many_ns, min = 0, max = 25) * 2)

# plot(so_many_ns, dbinom(so_many_ns, size = 25, prob = .2))

testers <- runif(1e4)
randos <- so_many_ns[testers < lil_nums]
length(randos)

par(mfrow = c(2,1))
hist(randos)
hist(rbinom(length(randos), size = 25, prob = .2))



so_many_ns <- runif(1e6, min = 0, max = 25)
so_many_ns
hist(so_many_ns)

lil_nums <- dnorm(so_many_ns, mean = 10, sd = 1.5)/(dunif(so_many_ns, min = 0, max = 25) * 7)

# plot(so_many_ns, dbinom(so_many_ns, size = 25, prob = .2))

testers <- runif(1e4)
randos <- so_many_ns[testers < lil_nums]
length(randos)

mean(randos)
sd(randos)

par(mfrow = c(2,1))
hist(randos)
hist(rnorm(length(randos), mean = 10, sd = 1.5))
