install.packages("pacman") # gerenciador de pacotes

library(pacman)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr, magrittr, hrbrthemes, car, DescTools) #pacotes basicos


dataset <- read.csv(file.choose())
dataset <- dataset %>% distinct() #remove linhas duplicadas

df <- dataset %>% select(popularity, explicit, danceability, energy, loudness, valence, instrumentalness, acousticness)

# Convertendo 'valence' para fator
df <- df %>%
  mutate(valence_binary = ifelse(valence >= 0.5, 1, 0)) %>% # Cria a nova coluna
  mutate(valence_binary = as.factor(valence_binary))

# Convertendo 'explicit' para fator
df <- df %>% mutate(explicit = as.factor(explicit))

#=============================================================================

# Excluindo a coluna 'valence_binary' e exibindo as estatísticas descritivas
df %>% select(-valence_binary) %>% summary()

#=============================================================================

# Selecionando colunas de 'df', exceto 'valence_binary'
colunas <- names(df)[names(df) != "valence_binary"]

# Loop para calcular e imprimir desvio padrão e variância de cada coluna numérica
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

colunas <- names(df)[names(df) != "valence_binary"]

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
    moda <- Mode(df[[col]])
    print(paste("Moda de", col, ":", moda))
  }
}

#=============================================================================

# Função para criar boxplot
create_boxplot <- function(data, col, graph_title) {
  ggplot(data, aes(y=col)) +
    geom_boxplot() +
    theme_ipsum() +
    ggtitle(graph_title)
}

# Função para criar gráfico de densidade
create_dplot <- function(data, col, graph_title) {
  ggplot(data=data, aes(x=col)) +
    geom_density() +
    theme_ipsum() +
    stat_summary(aes(xintercept = after_stat(x), y = 0), fun = mean, geom = "vline", orientation = "y")+
    ggtitle(graph_title)
}

# Função para criar gráfico de barras
create_barplot <- function(data, col, graph_title) {
  ggplot(data, aes(x=col))+
    geom_bar(stat="count", width=0.7, fill="purple")+
    ggtitle(graph_title) +
    theme_ipsum()
}

create_dplot(df, df$valence, "Positividade")
create_boxplot(df, df$valence, "Positividade")

create_barplot(df, df$explicit, "Explícita")

create_dplot(df, df$popularity, "Popularidade")
create_boxplot(df, df$popularity, "Popularidade")

create_dplot(df, df$danceability, "Dançabilidade")
create_boxplot(df, df$danceability, "Dançabilidade")

create_dplot(df, df$energy, "Energia")
create_boxplot(df, df$energy, "Energia")

create_dplot(df, df$loudness, "Altura")
create_boxplot(df, df$loudness, "Altura")

create_dplot(df, df$acousticness, "Acusticidade")
create_boxplot(df, df$acousticness, "Acusticidade")

create_dplot(df, df$instrumentalness, "Instrumentalidade")
create_boxplot(df, df$instrumentalness, "Instrumentalidade")

#=============================================================================

# Ajustando o modelo de regressão logística
modelo_logistico <- glm(valence_binary ~ popularity + explicit + danceability + energy + loudness, data = df, family = binomial)

# Resumo do modelo
summary(modelo_logistico)

#=============================================================================

# Coeficientes do modelo
coeficientes <- summary(modelo_logistico)$coefficients

# Odds Ratio
odds_ratio <- exp(coeficientes[, "Estimate"]) 

# Intervalos de Confiança (95%)
conf_int <- exp(confint(modelo_logistico))

# Resultados
resultado_odds <- data.frame(
  Coeficiente = coeficientes[, "Estimate"],
  Odds_Ratio = odds_ratio,
  `IC Inferior` = conf_int[, 1],
  `IC Superior` = conf_int[, 2]
)

print(resultado_odds)

#=============================================================================

# Log-likelihood do modelo ajustado
log_likelihood <- logLik(modelo_logistico)
print(paste("Log-likelihood do modelo:", as.numeric(log_likelihood)))

# Log-likelihood do modelo nulo
log_likelihood_null <- logLik(glm(valence_binary ~ 1, data = df, family = binomial))

# Cálculo de McFadden's R^2
mcfadden_r2 <- 1 - (as.numeric(log_likelihood) / as.numeric(log_likelihood_null))
print(paste("McFadden's R^2:", mcfadden_r2))

#=============================================================================

# Previsão usando o modelo ajustado
predicoes <- predict(modelo_logistico, newdata = df, type = "response")

# Converter previsões em binário (0 ou 1) com base no ponto de corte de 0.5
predicoes_binarias <- ifelse(predicoes >= 0.5, 1, 0)

# Criar tabela de confusão
tabela_confusao <- table(Real = df$valence_binary, Predito = predicoes_binarias)

# Calcular Sensibilidade, Especificidade e Acurácia
sensibilidade <- tabela_confusao[2, 2] / sum(tabela_confusao[2, ])
especificidade <- tabela_confusao[1, 1] / sum(tabela_confusao[1, ])
acuracia <- sum(diag(tabela_confusao)) / sum(tabela_confusao)

# Resultados
print("Tabela de Confusão:")
print(tabela_confusao)

print(paste("Sensibilidade:", sensibilidade))
print(paste("Especificidade:", especificidade))
print(paste("Acurácia:", acuracia))

#=============================================================================

# 1. Tendência dos Resíduos
residuos <- residuals(modelo_logistico)
previsoes <- predict(modelo_logistico)

# Gráfico de dispersão dos resíduos
ggplot(data = NULL, aes(x = previsoes, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Resíduos vs Previsões",
       x = "Previsões",
       y = "Resíduos") +
  theme_minimal()

# 2. Autocorrelação dos Resíduos
# Teste de Durbin-Watson
dw_test <- dwtest(modelo_logistico)
print(dw_test)

# 3. Multicolinearidade
vif_result <- vif(modelo_logistico)
print(vif_result)

# Interpretar VIF
vif_interpretacao <- ifelse(vif_result < 5, "Sem multicolinearidade significativa", 
                            ifelse(vif_result < 10, "Multicolinearidade moderada", 
                                   "Multicolinearidade alta"))
resultados_vif <- data.frame(Variavel = names(vif_result), VIF = vif_result, Interpretação = vif_interpretacao)
print(resultados_vif)
