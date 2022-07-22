install.packages("hnp")
library(hnp)
?hnp
anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
family = gaussian, data = anorexia)
head(anorexia)
table(anorexia$Treat)
f.fun <- function(y.) gamlss(cbind(y., m-y.) ~ tr,
family=BI, data=my.data)
? f.fun
head(my.data)
y <- rBI(30, bd=50, mu=rep(c(.2, .5, .9), each=10))
m <- 50
tr <- gl(3, 10)
fit3 <- gamlss(cbind(y, m-y) ~ tr, family=BI)
# diagfun
d.fun <- function(obj) resid(obj)
# simfun
s.fun <- function(n, obj) {
mu <- obj$mu.fv
bd <- obj$bd
rBI(n, bd=bd, mu=mu)
}
# fitfun
my.data <- data.frame(y, tr, m)
# fitfun
my.data <- data.frame(y, tr, m)
y <- rpois(30, lambda=rep(c(.5, 1.5, 5), each=10))
tr <- gl(3, 10)
fit1 <- glm(y ~ tr, family=poisson)
# diagfun
d.fun <- function(obj) cooks.distance(obj)
# simfun
s.fun <- function(n, obj) {
lam <- fitted(obj)
rpois(n, lambda=lam)
}
# fitfun
my.data <- data.frame(y, tr)
f.fun <- function(y.) glm(y. ~ tr, family=poisson, data=my.data)
# hnp call
hnp(fit1, newclass=TRUE, diagfun=d.fun, simfun=s.fun, fitfun=f.fun)
## Example no. 2: Implementing gamma model using package gamlss
# load package
require(gamlss)
# model fitting
y <- rGA(30, mu=rep(c(.5, 1.5, 5), each=10), sigma=.5)
tr <- gl(3, 10)
fit2 <- gamlss(y ~ tr, family=GA)
# diagfun
d.fun <- function(obj) resid(obj) # this is the default if no
# diagfun is provided
# simfun
s.fun <- function(n, obj) {
mu <- obj$mu.fv
sig <- obj$sigma.fv
rGA(n, mu=mu, sigma=sig)
}
# fitfun
my.data <- data.frame(y, tr)
f.fun <- function(y.) gamlss(y. ~ tr, family=GA, data=my.data)
# hnp call
hnp(fit2, newclass=TRUE, diagfun=d.fun, simfun=s.fun,
fitfun=f.fun, data=data.frame(y, tr))
## Example no. 3: Implementing binomial model in gamlss
# model fitting
y <- rBI(30, bd=50, mu=rep(c(.2, .5, .9), each=10))
m <- 50
tr <- gl(3, 10)
fit3 <- gamlss(cbind(y, m-y) ~ tr, family=BI)
# diagfun
d.fun <- function(obj) resid(obj)
# simfun
s.fun <- function(n, obj) {
mu <- obj$mu.fv
bd <- obj$bd
rBI(n, bd=bd, mu=mu)
}
# fitfun
my.data <- data.frame(y, tr, m)
my.data
## Example no. 3: Implementing binomial model in gamlss
# model fitting
y <- rBI(30, mu=rep(c(0.8)))
## Example no. 3: Implementing binomial model in gamlss
# model fitting
y <- rbinom(30, 0.8)
? rbinom
## Example no. 3: Implementing binomial model in gamlss
# model fitting
y <- rbinom(30, prob = 0.8)
## Example no. 3: Implementing binomial model in gamlss
# model fitting
y <- rbinom(30, 30, prob = 0.8)
y
## Example no. 3: Implementing binomial model in gamlss
# model fitting
y <- rbinom(30, 1, prob = 0.8)
y
## Example no. 3: Implementing binomial model in gamlss
# model fitting
y <- rbinom(30, 1, prob = 0.6)
y
fit3 <- gamlss(y ~ tr, family=BI)
fit <- glm(y ~z, family = binomial)
y <- rbinom(30, 1, prob = 0.6)
z <- rbinom(30, 30, prob = 0.8)
fit <- glm(y ~z, family = binomial)
? hnp
hnp(fit)
## Example no. 3: Implementing binomial model in gamlss
# model fitting
y <- rbinom(300, 1, prob = 0.6)
z <- rbinom(300, 30, prob = 0.8)
fit <- glm(y ~ z, family = binomial)
hnp(fit)
fit2 <- glm(y ~ z, family = binomial(link = "log"))
hnp(fit2)
anova(fit2)
? glm
fit2 <- glm(y ~ z, family = binomial(link = "log"))
hnp(fit2)
y <- rbinom(300, 1, prob = 0.6)
z <- rbinom(300, 30, prob = 0.8)
fit <- glm(y ~ z, family = binomial)
hnp(fit)
fit2 <- glm(y ~ z, family = binomial(link = "log"))
hnp(fit2)
fit2 <- glm(y ~ z, family = binomial(link = "log"))
hnp(fit2)
fit2 <- glm(y ~ z, family = binomial(link = "log"))
hnp(fit2)
fit2 <- glm(y ~ z, family = binomial(link = "log"))
hnp(fit2)
fit2 <- glm(y ~ z, family = binomial(link = "log"))
hnp(fit2)
fit2 <- glm(y ~ z, family = binomial(link = "log"))
hnp(fit2)
fit2 <- glm(y ~ z, family = binomial(link = "log"))
hnp(fit2)
_