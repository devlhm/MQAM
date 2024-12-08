install.packages("pacman") # gerenciador de pacotes

library(pacman)

#pacotes básicos
pacman::p_load(psych, pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr,
               lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr, magrittr,
               hrbrthemes, car, DescTools, cluster, factoextra, corrplot, psych)

dataset <- read.csv("dataset.csv")
dataset <- dataset %>% select(-X, -track_id) #remove colunas inuteis
dataset <- dataset %>% distinct() #remove linhas duplicadas

df <- dataset %>% select(danceability, energy, loudness, acousticness, instrumentalness, valence, tempo)
df_scaled <- scale(df) #normaliza os dados

#===================================================================

# Exibindo as estatísticas descritivas
df %>% summary()

#===================================================================

# Gráficos

create_boxplot <- function(data, col, graph_title) {
  ggplot(data, aes(y=col)) +
    geom_boxplot() +
    theme_ipsum() +
    ggtitle(graph_title)
}

create_dplot <- function(data, col, graph_title) {
  ggplot(data = data, aes(x = col)) +
    geom_density() +
    theme_ipsum() +
    stat_summary(aes(xintercept = after_stat(x), y = 0),
      fun = mean, geom = "vline",
      orientation = "y") +
    ggtitle(graph_title)
}

create_barplot <- function(data, col, graph_title) {
  ggplot(data, aes(x = col)) +
    geom_bar(stat="count", width = 0.7, fill = "purple") +
    ggtitle(graph_title) +
    theme_ipsum()
}

create_dplot(df, df$speechiness, "Fala")
create_dplot(df, df$danceability, "Dançabilidade")
create_dplot(df, df$acousticness, "Acusticidade")
create_dplot(df, df$energy, "Energia")
create_dplot(df, df$tempo, "Tempo")
create_dplot(df, df$valence, "Valência")
create_dplot(df, df$loudness, "Altura")
create_dplot(df, df$liveness, "Público")
create_dplot(df, df$instrumentalness, "Instrumentalidade")


create_boxplot(df, df$speechiness, "Fala")
create_boxplot(df, df$danceability, "Dançabilidade")
create_boxplot(df, df$acousticness, "Acusticidade")
create_boxplot(df, df$energy, "Energia")
create_boxplot(df, df$tempo, "Tempo")
create_boxplot(df, df$valence, "Valência")
create_boxplot(df, df$loudness, "Altura")
create_boxplot(df, df$liveness, "Público")
create_boxplot(df, df$instrumentalness, "Instrumentalidade")

#=============================================================================

# Selecionando colunas de 'df', exceto 'track_genre'
colunas <- names(df)

# Loop para calcular e imprimir desvio padrão e variância de cada coluna
for (col in colunas) {
  # Verifica se a coluna é numérica
  if (is.numeric(df[[col]])) {
    # Desvio padrão
    desvio_padrao <- sd(df[[col]], na.rm = TRUE)
    print(paste("Desvio padrão de", col, ":", desvio_padrao))

    # Variância
    variancia <- var(df[[col]], na.rm = TRUE)
    print(paste("Variância de", col, ":", variancia))

    cat("\n") # Adiciona uma linha em branco para legibilidade
  }
}

#=============================================================================

# funcao que calcula moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Loop para calcular e imprimir a moda de cada coluna numérica
for (col in colunas) {
  # Verifica se a coluna é numérica
  if (is.numeric(df[[col]])) {
    # Calcula a moda
    moda <- getmode(df[[col]])
    print(paste("Moda de", col, ":", moda))
  }
}

#=============================================================================

# Gerar matriz de correlação
matriz_correlacao <- cor(df, use = "pairwise.complete.obs")

# Visualizar a matriz de correlação
print(matriz_correlacao)

# Visualizar a matriz de correlação como um gráfico
corrplot(
  matriz_correlacao,
  method = "circle",    # Método de exibição (círculos)
  type = "upper",       # Exibe apenas a metade superior
  tl.col = "black",     # Cor dos rótulos
  tl.srt = 45           # Rotação dos rótulos
)

#===========================================================================

# Calcular o KMO
kmo_resultado <- KMO(matriz_correlacao)

# Visualizar o resultado
print(kmo_resultado)

#===========================================================================

# Realizar o teste de Bartlett
bartlett_resultado <- cortest.bartlett(matriz_correlacao, n = nrow(df))

# Visualizar os resultados com explicações mais claras
cat("Resultado do Teste de Bartlett (Teste de esfericidade):
    \nValor de chi-quadrado do teste de Bartlett:", bartlett_resultado$chisq,
    "\nGraus de liberdade:", bartlett_resultado$df,
    "\nValor de p:", bartlett_resultado$p.value, "\n")

#===========================================================================

#seleciona o numero de fatores baseado no critério de Kaiser-Guttman
nfactors <- sum(eigen(matriz_correlacao)$values > 1)

print(nfactors)

#===========================================================================

# Realizar a análise fatorial sem rotação
fa_resultado <- fa(df, nfactors = nfactors, rotate = "none")

# Visualizar os resultados da análise fatorial
print(fa_resultado)

# Visualizar a matriz de cargas fatoriais
print(fa_resultado$loadings)

# Visualizar a variância explicada por cada fator
print(fa_resultado$Vaccounted)

#===========================================================================

# Realizar a análise fatorial rotacioada
fa_resultado_rotated <- fa(df, nfactors = nfactors, rotate = "varimax")

# Visualizar os resultados da análise fatorial
print(fa_resultado_rotated)

# Visualizar a matriz de cargas fatoriais
print(fa_resultado_rotated$loadings)

# Visualizar as comunalidades dos fatores para cada variável
print(fa_resultado_rotated$communality)

# Visualizar a variância explicada por cada fator
print(fa_resultado_rotated$Vaccounted)

#===========================================================================
