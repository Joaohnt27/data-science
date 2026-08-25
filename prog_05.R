getwd()

setwd("C:/Users/839463/Desktop/DataScience/")

data_file = 'http://openmv.net/file/six-point-board-thickness.csv'

boards <- read.csv(data_file)

head(boards)

summary(boards) # Estatistica descritiva
lengths(boards)
sd(boards$Pos1)

plot(boards[300:400,2], type='l')
plot(boards[1:5000,5], type ='l')

all_f <- boards[300:400, 2:7]

boxplot(all_f, ylab="Thickness [mils]")

# Mostrar valores que são maiores que 1705
novo <- all_f[all_f$Pos1 > 1705,]
novo

# Ou pode fazer usando subset
novo1 <- subset(all_f, Pos1 > 1705 & Pos2 > 1695)
novo1

novo2 <- subset(all_f, Pos1 > 1705 & Pos2 > 1695 & Pos3 > 1695)
novo2

# Para verificar se a distribuição é normal = faz teste de shapiro (se maior que 0,05 é normal)
# nesse caso, não normal
shapiro.test(boards$Pos1) #p > 0.05 dist normal

# Nesse caso, não normal / usar quartis
summary(boards$Pos1)

# Como q1 = 1670 e q3 = 1705, a d = q3-q1 logo d = 
d = 1705-1670
d

minpos1=1670-1.5*d
minpos1
maxpos1=1705+1.5*d
maxpos1

vlpos <- subset(boards, Pos1 > minpos1 & Pos1 < maxpos1)
df1 <- vlpos$Pos1
df1
plot(boards$Pos1, type='l')
plot(df1, type='l')

# Fazer para: Pos2, Pos3 e Pos4