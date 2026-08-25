# Vetor com uma sequência de 1 a 1.000.000
x <- 1:1000000
y1 <- numeric(length(x))

# Receberos resultados
for (i in 1:length(x)){
  y1[i] <- x[i]^2
}

# Calcula o quadrado de cada número da sequ~encia em x usando a regra da reciclagem
y2 <- x^2

# Confere os resutados
identical(y1, y2)

# Tempo de execução suando for()
y1 <- numeric(length(x))
system.time(
  for(i in 1:length(x)){
    y1[i] <- x[i]^2
  }
)
system.time(
y2 <- x^2
)

# Distribuição uniforme
vunif <- runif(30, min = 5, max = 10)
hist(vunif)
shapiro.test(vunif)

# Distribuição normal
vnorm <- rnorm(30, mean = 10, sd = 2)
hist(vnorm)
shapiro.test(vnorm)

# Distribuição Poisson
poisson <- rpois (300, lambda = 3)
hist(poisson)
shapiro.test(poisson)