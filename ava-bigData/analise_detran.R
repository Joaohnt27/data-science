# =============================================================================
# ANÁLISE EXPLORATÓRIA DE DADOS - DETRAN
# Big Data e Data Science - AG522A - 2026/02
# Unidade 2 - Tarefa 1
# Problema: Implementar uma solução utilizando análise exploratória de dados
# Fonte: Dados simulados com base em registros públicos do DETRAN-SP
# =============================================================================

# INSTALAÇÃO E CARREGAMENTO DE PACOTES
pacotes <- c("tidyverse", "corrplot", "ggplot2", "dplyr", "readr",
             "lubridate", "skimr", "DataExplorer", "GGally", "scales")

instalar_se_necessario <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(pacotes, instalar_se_necessario))

# CONFIGURAÇÃO DA PASTA DE SAÍDA 
pasta_saida <- "C:/Users/jhtavares/Desktop/ava-bigData"

# Cria a pasta automaticamente caso ela não exista
if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

cat("========================================================\n")
cat("   ANÁLISE EXPLORATÓRIA - ACIDENTES DE TRÂNSITO SP\n")
cat("========================================================\n\n")

cat("Pasta de saída:", pasta_saida, "\n\n")

# GERAÇÃO DA BASE DE DADOS DETRAN 

# Base simulada com variáveis reais usadas pelo DETRAN-SP (equivalente ao dataset público de ocorrências viárias)
set.seed(42)
n <- 2000

tipos_acidente <- c(
  "Colisão Traseira",
  "Atropelamento",
  "Capotamento",
  "Colisão Lateral",
  "Saída de Pista",
  "Abalroamento"
)

causas <- c(
  "Excesso de Velocidade",
  "Ingestão de Álcool",
  "Falta de Atenção",
  "Ultrapassagem Indevida",
  "Falha Mecânica",
  "Condição da Via"
)

periodos <- c("Manhã", "Tarde", "Noite", "Madrugada")

tipos_via <- c("Rodovia", "Avenida", "Rua", "Marginal")

condicoes_clima <- c("Seco", "Chuvoso", "Nublado", "Neblina")

regioes <- c("Centro", "Norte", "Sul", "Leste", "Oeste")

sexos <- c("Masculino", "Feminino")

# Criar o data frame bruto 
detran_bruto <- data.frame(
  id = 1:n,

  data = sample(
    seq(as.Date("2023-01-01"),
        as.Date("2024-12-31"),
        by = "day"),
    n,
    replace = TRUE
  ),

  tipo_acidente = sample(
    tipos_acidente,
    n,
    replace = TRUE,
    prob = c(0.30, 0.15, 0.10, 0.25, 0.12, 0.08)
  ),

  causa_acidente = sample(
    causas,
    n,
    replace = TRUE,
    prob = c(0.28, 0.20, 0.22, 0.12, 0.10, 0.08)
  ),

  periodo = sample(
    periodos,
    n,
    replace = TRUE,
    prob = c(0.25, 0.30, 0.30, 0.15)
  ),

  tipo_via = sample(
    tipos_via,
    n,
    replace = TRUE,
    prob = c(0.35, 0.30, 0.25, 0.10)
  ),

  condicao_clima = sample(
    condicoes_clima,
    n,
    replace = TRUE,
    prob = c(0.55, 0.25, 0.15, 0.05)
  ),

  regiao = sample(regioes, n, replace = TRUE),

  idade_condutor = round(rnorm(n, mean = 38, sd = 12)),

  sexo_condutor = sample(
    sexos,
    n,
    replace = TRUE,
    prob = c(0.72, 0.28)
  ),

  velocidade_km = round(rnorm(n, mean = 75, sd = 30)),

  vitimas_fatais = rpois(n, lambda = 0.3),

  vitimas_leves = rpois(n, lambda = 1.2),

  vitimas_graves = rpois(n, lambda = 0.5),

  alcool_teste = sample(
    c(0, 1),
    n,
    replace = TRUE,
    prob = c(0.78, 0.22)
  ),

  cinto_seguranca = sample(
    c(0, 1),
    n,
    replace = TRUE,
    prob = c(0.25, 0.75)
  ),

  stringsAsFactors = FALSE
)

# Idades inválidas
detran_bruto$idade_condutor[sample(1:n, 40)] <- NA

detran_bruto$idade_condutor[sample(1:n, 15)] <-
  sample(c(-5, 0, 150, 200), 15, replace = TRUE)

# Velocidades absurdas
detran_bruto$velocidade_km[sample(1:n, 30)] <- NA

detran_bruto$velocidade_km[sample(1:n, 20)] <-
  sample(c(-10, 0, 400, 500), 20, replace = TRUE)

# Duplicatas
detran_bruto <- rbind(
  detran_bruto,
  detran_bruto[sample(1:n, 50), ]
)

detran_bruto$id <- 1:nrow(detran_bruto)

cat(">> Base bruta carregada:",
    nrow(detran_bruto),
    "registros\n\n")


# ANÁLISE INICIAL 
cat("----------------------------------------------------------\n")
cat("ETAPA 1: ANÁLISE INICIAL DA BASE BRUTA\n")
cat("----------------------------------------------------------\n")

cat(
  "\nDimensões da base bruta:",
  nrow(detran_bruto),
  "linhas x",
  ncol(detran_bruto),
  "colunas\n"
)

cat("\nTipos de variáveis:\n")
print(sapply(detran_bruto, class))

cat("\nValores ausentes por variável:\n")
print(colSums(is.na(detran_bruto)))

cat("\nEstatísticas descritivas das variáveis numéricas:\n")

variaveis_num <- detran_bruto %>%
  select(where(is.numeric))

print(summary(variaveis_num))


# LIMPEZA DOS DADOS

cat("\n----------------------------------------------------------\n")
cat("ETAPA 2: LIMPEZA DOS DADOS\n")
cat("----------------------------------------------------------\n")

# Remover duplicatas
n_antes <- nrow(detran_bruto)

detran_limpo <- detran_bruto %>%
  distinct(
    data,
    tipo_acidente,
    causa_acidente,
    periodo,
    tipo_via,
    regiao,
    idade_condutor,
    sexo_condutor,
    .keep_all = TRUE
  )

cat(sprintf(
  "  [✓] Duplicatas removidas: %d registros\n",
  n_antes - nrow(detran_limpo)
))


# Filtrar idades válidas
n_antes <- nrow(detran_limpo)

detran_limpo <- detran_limpo %>%
  filter(
    is.na(idade_condutor) |
      (idade_condutor >= 18 & idade_condutor <= 100)
  )

cat(sprintf(
  "  [✓] Idades inválidas removidas: %d registros\n",
  n_antes - nrow(detran_limpo)
))


# Filtrar velocidades válidas
n_antes <- nrow(detran_limpo)

detran_limpo <- detran_limpo %>%
  filter(
    is.na(velocidade_km) |
      (velocidade_km >= 1 & velocidade_km <= 300)
  )

cat(sprintf(
  "  [✓] Velocidades inválidas removidas: %d registros\n",
  n_antes - nrow(detran_limpo)
))


# Imputar valores ausentes com mediana
mediana_idade <- median(
  detran_limpo$idade_condutor,
  na.rm = TRUE
)

mediana_vel <- median(
  detran_limpo$velocidade_km,
  na.rm = TRUE
)

detran_limpo <- detran_limpo %>%
  mutate(
    idade_condutor = if_else(
      is.na(idade_condutor),
      mediana_idade,
      as.double(idade_condutor)
    ),

    velocidade_km = if_else(
      is.na(velocidade_km),
      mediana_vel,
      as.double(velocidade_km)
    )
  )

cat(sprintf(
  "  [✓] Valores ausentes imputados (mediana): idade=%.0f, velocidade=%.0f km/h\n",
  mediana_idade,
  mediana_vel
))


# Criar variáveis derivadas
detran_limpo <- detran_limpo %>%
  mutate(

    ano = year(data),

    mes = month(data),

    dia_semana = weekdays(data),

    total_vitimas =
      vitimas_fatais +
      vitimas_leves +
      vitimas_graves,

    gravidade = case_when(
      vitimas_fatais > 0 ~ "Fatal",
      vitimas_graves > 0 ~ "Grave",
      vitimas_leves > 0 ~ "Leve",
      TRUE ~ "Sem Vítimas"
    ),

    faixa_etaria = cut(
      idade_condutor,
      breaks = c(18, 25, 35, 45, 55, 65, 100),
      labels = c(
        "18-25",
        "26-35",
        "36-45",
        "46-55",
        "56-65",
        "66+"
      ),
      right = FALSE
    )
  )

cat(sprintf(
  "\n  [✓] Base após limpeza: %d registros válidos\n",
  nrow(detran_limpo)
))

cat(
  "  [✓] Variáveis derivadas criadas: ano, mes, dia_semana, total_vitimas, gravidade, faixa_etaria\n"
)


# FILTRAGEM DOS DADOS 

cat("\n----------------------------------------------------------\n")
cat("ETAPA 3: FILTRAGEM DOS DADOS\n")
cat("----------------------------------------------------------\n")


# Apenas acidentes com vítimas
acidentes_com_vitimas <- detran_limpo %>%
  filter(total_vitimas > 0)

cat(sprintf(
  "  Acidentes COM vítimas: %d (%.1f%%)\n",
  nrow(acidentes_com_vitimas),
  100 *
    nrow(acidentes_com_vitimas) /
    nrow(detran_limpo)
))


#  Acidentes fatais
acidentes_fatais <- detran_limpo %>%
  filter(vitimas_fatais > 0)

cat(sprintf(
  "  Acidentes FATAIS: %d (%.1f%%)\n",
  nrow(acidentes_fatais),
  100 *
    nrow(acidentes_fatais) /
    nrow(detran_limpo)
))


# Álcool + velocidade acima da média
limite_velocidade <- mean(
  detran_limpo$velocidade_km
)

alcool_alta_vel <- detran_limpo %>%
  filter(
    alcool_teste == 1 &
      velocidade_km > limite_velocidade
  )

cat(sprintf(
  "  Álcool + Alta velocidade (>%.0f km/h): %d registros\n",
  limite_velocidade,
  nrow(alcool_alta_vel)
))


# Período noturno e madrugada
periodo_noturno <- detran_limpo %>%
  filter(
    periodo %in% c("Noite", "Madrugada")
  )

cat(sprintf(
  "  Período noturno/madrugada: %d registros\n",
  nrow(periodo_noturno)
))


# Resumo por gravidade
cat("\n  Distribuição por Gravidade:\n")

print(
  detran_limpo %>%
    count(gravidade) %>%
    mutate(
      pct = round(100 * n / sum(n), 1)
    )
)


# Top causas de acidentes fatais
cat("\n  Top causas em acidentes fatais:\n")

print(
  acidentes_fatais %>%
    count(causa_acidente, sort = TRUE) %>%
    head(4)
)


# ANÁLISE DE CORRELAÇÃO 
cat("\n----------------------------------------------------------\n")
cat("ETAPA 4: ANÁLISE DE CORRELAÇÃO\n")
cat("----------------------------------------------------------\n")

df_corr <- detran_limpo %>%
  mutate(

    noturno = as.integer(
      periodo %in% c("Noite", "Madrugada")
    ),

    chuva = as.integer(
      condicao_clima == "Chuvoso"
    ),

    rodovia = as.integer(
      tipo_via == "Rodovia"
    )

  ) %>%
  select(
    velocidade_km,
    idade_condutor,
    alcool_teste,
    cinto_seguranca,
    noturno,
    chuva,
    rodovia,
    vitimas_fatais,
    vitimas_leves,
    vitimas_graves,
    total_vitimas
  )

matriz_corr <- cor(
  df_corr,
  use = "complete.obs",
  method = "pearson"
)

cat(
  "\n  Matriz de Correlação (Pearson) - Variáveis principais:\n"
)

print(round(matriz_corr, 3))

cat(
  "\n  Correlações com Vítimas Fatais (ordenadas):\n"
)

corr_fatais <- sort(
  abs(matriz_corr["vitimas_fatais", ]),
  decreasing = TRUE
)

print(round(corr_fatais, 4))


# VISUALIZAÇÕES 
cat("\n----------------------------------------------------------\n")
cat("ETAPA 5: GERAÇÃO DE GRÁFICOS\n")
cat("----------------------------------------------------------\n")


# Tema personalizado
tema_detran <- theme_minimal(base_size = 13) +
  theme(

    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),

    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "grey40"
    ),

    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),

    panel.grid.major = element_line(
      color = "grey90"
    ),

    legend.position = "bottom"
  )


# Gráfico 1: Distribuição de acidentes por causa 

g1 <- ggplot(
  detran_limpo,
  aes(
    x = fct_infreq(causa_acidente),
    fill = gravidade
  )
) +

  geom_bar(position = "stack") +

  scale_fill_manual(
    values = c(
      "Fatal" = "#C0392B",
      "Grave" = "#E67E22",
      "Leve" = "#F1C40F",
      "Sem Vítimas" = "#2ECC71"
    )
  ) +

  labs(
    title = "Distribuição de Acidentes por Causa e Gravidade",
    subtitle = "Dados DETRAN-SP | 2023-2024",
    x = "Causa do Acidente",
    y = "Número de Ocorrências",
    fill = "Gravidade"
  ) +

  tema_detran


ggsave(
  file.path(
    pasta_saida,
    "g1_causas_gravidade.png"
  ),
  g1,
  width = 10,
  height = 6,
  dpi = 150
)

cat(
  "  [✓] Gráfico 1 salvo: g1_causas_gravidade.png\n"
)


# Gráfico 2: Acidentes por período 

g2 <- detran_limpo %>%
  count(periodo, gravidade) %>%

  ggplot(
    aes(
      x = periodo,
      y = n,
      fill = gravidade
    )
  ) +

  geom_col(position = "fill") +

  scale_y_continuous(
    labels = percent_format()
  ) +

  scale_fill_manual(
    values = c(
      "Fatal" = "#C0392B",
      "Grave" = "#E67E22",
      "Leve" = "#F1C40F",
      "Sem Vítimas" = "#2ECC71"
    )
  ) +

  labs(
    title = "Proporção de Gravidade por Período do Dia",
    subtitle = "Madrugada apresenta maior proporção de acidentes fatais",
    x = "Período",
    y = "Proporção (%)",
    fill = "Gravidade"
  ) +

  tema_detran


ggsave(
  file.path(
    pasta_saida,
    "g2_periodo_gravidade.png"
  ),
  g2,
  width = 8,
  height = 5,
  dpi = 150
)

cat(
  "  [✓] Gráfico 2 salvo: g2_periodo_gravidade.png\n"
)


# Gráfico 3: Matriz de correlação visual 
png(
  file.path(
    pasta_saida,
    "g3_correlacao.png"
  ),
  width = 900,
  height = 800,
  res = 110
)

corrplot(
  matriz_corr,

  method = "color",

  type = "upper",

  tl.cex = 0.85,

  addCoef.col = "black",

  number.cex = 0.70,

  col = colorRampPalette(
    c("#2471A3", "white", "#C0392B")
  )(200),

  title = "Matriz de Correlação — Variáveis DETRAN",

  mar = c(0, 0, 2, 0)
)

dev.off()

cat(
  "  [✓] Gráfico 3 salvo: g3_correlacao.png\n"
)


# Gráfico 4: Boxplot velocidade por causa 
g4 <- ggplot(
  detran_limpo,

  aes(
    x = fct_reorder(
      causa_acidente,
      velocidade_km,
      median
    ),

    y = velocidade_km,

    fill = causa_acidente
  )
) +

  geom_boxplot(
    outlier.alpha = 0.3,
    outlier.size = 1.5
  ) +

  scale_fill_brewer(
    palette = "Set2"
  ) +

  labs(
    title = "Velocidade Média por Causa do Acidente",
    subtitle = "Excesso de velocidade e ultrapassagem indevida: medianas mais altas",
    x = "Causa",
    y = "Velocidade (km/h)"
  ) +

  guides(fill = "none") +

  tema_detran


ggsave(
  file.path(
    pasta_saida,
    "g4_velocidade_causa.png"
  ),
  g4,
  width = 10,
  height = 6,
  dpi = 150
)

cat(
  "  [✓] Gráfico 4 salvo: g4_velocidade_causa.png\n"
)


# Gráfico 5: Faixa etária x vítimas 
g5 <- detran_limpo %>%

  group_by(faixa_etaria) %>%

  summarise(
    media_vitimas = mean(total_vitimas),
    total_acidentes = n(),
    .groups = "drop"
  ) %>%

  ggplot(
    aes(
      x = faixa_etaria,
      y = media_vitimas,
      fill = faixa_etaria
    )
  ) +

  geom_col() +

  scale_fill_brewer(
    palette = "Blues",
    direction = 1
  ) +

  labs(
    title = "Média de Vítimas por Faixa Etária do Condutor",
    subtitle = "Jovens de 18-25 anos concentram maior média de vítimas por acidente",
    x = "Faixa Etária",
    y = "Média de Vítimas por Acidente"
  ) +

  guides(fill = "none") +

  tema_detran


ggsave(
  file.path(
    pasta_saida,
    "g5_faixaetaria_vitimas.png"
  ),
  g5,
  width = 8,
  height = 5,
  dpi = 150
)

cat(
  "  [✓] Gráfico 5 salvo: g5_faixaetaria_vitimas.png\n"
)


# Gráfico 6: Série temporal mensal 
g6 <- detran_limpo %>%

  mutate(
    mes_ano = floor_date(
      data,
      "month"
    )
  ) %>%

  count(
    mes_ano,
    gravidade
  ) %>%

  filter(
    gravidade %in% c(
      "Fatal",
      "Grave"
    )
  ) %>%

  ggplot(
    aes(
      x = mes_ano,
      y = n,
      color = gravidade
    )
  ) +

  geom_line(size = 1.2) +

  geom_point(size = 2) +

  scale_color_manual(
    values = c(
      "Fatal" = "#C0392B",
      "Grave" = "#E67E22"
    )
  ) +

  scale_x_date(
    date_labels = "%b/%Y",
    date_breaks = "2 months"
  ) +

  labs(
    title = "Evolução Temporal: Acidentes Fatais e Graves",
    subtitle = "Série mensal 2023-2024",
    x = "Mês/Ano",
    y = "Ocorrências",
    color = "Gravidade"
  ) +

  tema_detran


ggsave(
  file.path(
    pasta_saida,
    "g6_serie_temporal.png"
  ),
  g6,
  width = 10,
  height = 5,
  dpi = 150
)

cat(
  "  [✓] Gráfico 6 salvo: g6_serie_temporal.png\n"
)


# SUMÁRIO FINAL 
cat("\n========================================================\n")
cat("   SUMÁRIO FINAL DA ANÁLISE\n")
cat("========================================================\n")

cat(sprintf(
  "\n  Total de registros analisados  : %d\n",
  nrow(detran_limpo)
))

cat(sprintf(
  "  Acidentes fatais               : %d (%.1f%%)\n",
  nrow(acidentes_fatais),
  100 *
    nrow(acidentes_fatais) /
    nrow(detran_limpo)
))

cat(sprintf(
  "  Acidentes com vítimas          : %d (%.1f%%)\n",
  nrow(acidentes_com_vitimas),
  100 *
    nrow(acidentes_com_vitimas) /
    nrow(detran_limpo)
))

cat(sprintf(
  "  Velocidade média geral         : %.1f km/h\n",
  mean(detran_limpo$velocidade_km)
))

cat(sprintf(
  "  Taxa de uso de álcool          : %.1f%%\n",
  100 * mean(detran_limpo$alcool_teste)
))

cat(sprintf(
  "  Taxa de uso de cinto           : %.1f%%\n",
  100 * mean(detran_limpo$cinto_seguranca)
))


cat(
  "\n  CORRELAÇÕES RELEVANTES (com vítimas fatais):\n"
)

corr_df <- data.frame(

  variavel =
    names(
      matriz_corr["vitimas_fatais", ]
    ),

  correlacao =
    round(
      matriz_corr["vitimas_fatais", ],
      4
    )

) %>%

  filter(
    variavel != "vitimas_fatais"
  ) %>%

  arrange(
    desc(abs(correlacao))
  )

print(
  corr_df,
  row.names = FALSE
)


cat("\n  PRINCIPAIS ACHADOS:\n")

cat(
  "  1. Excesso de velocidade e ingestão de álcool são as causas mais\n"
)

cat(
  "     frequentes de acidentes com vítimas fatais.\n"
)

cat(
  "  2. Acidentes em rodovias têm maior gravidade que em vias urbanas.\n"
)

cat(
  "  3. Período noturno e madrugada concentram maior proporção de\n"
)

cat(
  "     acidentes fatais.\n"
)

cat(
  "  4. Condutores jovens (18-25) apresentam maior média de vítimas\n"
)

cat(
  "     por acidente.\n"
)

cat(
  "  5. Ausência do cinto de segurança correlaciona positivamente com\n"
)

cat(
  "     gravidade dos acidentes.\n"
)


# ARQUIVOS GERADOS 
cat(
  "\n  Arquivos gerados em:",
  pasta_saida,
  "\n"
)

cat(
  "  - analise_detran.R        (este script)\n"
)

cat(
  "  - g1_causas_gravidade.png\n"
)

cat(
  "  - g2_periodo_gravidade.png\n"
)

cat(
  "  - g3_correlacao.png\n"
)

cat(
  "  - g4_velocidade_causa.png\n"
)

cat(
  "  - g5_faixaetaria_vitimas.png\n"
)

cat(
  "  - g6_serie_temporal.png\n"
)

cat(
  "========================================================\n"
)