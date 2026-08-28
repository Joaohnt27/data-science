# Leitura dos dados da URL
data_file <- 'http://openmv.net/file/six-point-board-thickness.csv'
boards <- read.csv(data_file)

# Visualizar as primeiras linhas
head(boards)

# Teste de Normalidade inicial (com outliers)
shapiro.test(boards$Pos1)

# Cálculo dinâmico dos quartis e da Amplitude Interquartílica para a coluna Pos1
q1 <- quantile(boards$Pos1, 0.25)
q3 <- quantile(boards$Pos1, 0.75)
iqr <- q3 - q1

# Definição dos limites para remoção de outliers
minpos1 <- q1 - 1.5 * iqr
maxpos1 <- q3 + 1.5 * iqr

# Remoção de outliers utilizando o comando subset
vlpos <- subset(boards, Pos1 > minpos1 & Pos1 < maxpos1)

# Teste de Normalidade final (sem outliers)
shapiro.test(vlpos$Pos1)

# Comparação visual antes e depois do corte
par(mfrow = c(1, 2)) # Divide a tela em 2 gráficos
boxplot(boards$Pos1, main = "Com Outliers", ylab = "Pos1")
boxplot(vlpos$Pos1, main = "Sem Outliers", ylab = "Pos1")
par(mfrow = c(1, 1)) # Restaura visualização padrão