# 2000 values from F-Distribution
rand.f <- rf(2000, df1=200, df=150)

# Looks sort of normally distributed
hist(rand.f, freq=FALSE, ylim=c(0, 2.6),
     main="Esses dados são de uma distribuição normal?",
     ylab="Frequência",
     xlab="Distruição F")
# Add the density line on top
lines(density(rand.f))

install.packages('car')
library(car)
qqPlot(rand.f, distribution="norm")
shapiro.test(rand.f)

# Distribuição uniforme
vunif <- runif(300, min = 5, max = 10)
hist(vunif)
qqPlot(vunif, distribution="norm")
shapiro.test(vunif)

# Distribuição normal
vnorm <- rnorm(300, mean = 10, sd = 2)
hist(vnorm)
qqPlot(vnorm, distribution="norm")
shapiro.test(vnorm)

# Distribuição Poisson
poisson <- rpois (300, lambda = 3)
hist(poisson)
qqPlot(poisson, distribution="norm")
