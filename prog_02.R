# Criação de um vetor
vec <- c(84.334, 90.445, 70.654, 67.677, 91.912, 84.765, 94.213, 88.987, 77.983, 40.999, 140.986)

# Observar o vetor vec
vec

# Verificar modo/tipo do vetor vec
mode(vec)

# Verificar tamanho do vetor vec
length(vec)

# Verificar estrutura do vetor vec
str(vec)

# Verificar classe do vetor vec
class(vec)

# Observar os 6 primeiros elementos do objeto
head(vec)

# Observar apenas os 4 primeiros elementos do objeto
head(vec, 4)

# Observar os útlimos elementos do objeto
tail(vec)

# Ordenar o vetor vec criado - Ordem Decrescente
sort(vec, decreasing = TRUE)

# Ordenar o vetor vec criado
order(vec)

# Verificar os elementos específicos por posições no vetor
vec[c(1, 5, 9)]

# Arredondar valores do bec
round(vec)

# Criar um gráfico
boxplot(vec)

# Média
mean(vec)

# Desvio
sd(vec)

# Mediana
median(vec)

# Sumário Estatístico dos dados
summary(vec)
hist(vec)
boxplot(vec)

# Se há normalidade dos dados brutos (se p > 0,05 a distribuição é normal)
shapiro.test(vec)