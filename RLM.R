install.packages("pacman") # gerenciador de pacotes

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr, magrittr, hrbrthemes, car) #pacotes basicos

head(dataset)

table(dataset$track_genre)

selected_columns <- c("popularity", "duration_ms", "explicit", "danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "valence", "tempo", "track_genre")

df <- dataset

remove_outliers <- function(data, col) {
  quartiles <- quantile(col, probs=c(.25, .75), na.rm = FALSE)
  IQR <- IQR(col)
  
  Lower <- quartiles[1] - 1.5*IQR
  Upper <- quartiles[2] + 1.5*IQR 
  
  return(subset(data, col > Lower & col < Upper))
}

df <- remove_outliers(df, df$duration_ms)
df <- remove_outliers(df, df$speechiness)
df <- remove_outliers(df, df$instrumentalness)
df <- remove_outliers(df, df$loudness)

df <- df %>% filter(popularity > 0)

# funcao que calcula moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# tabelas de dist. de frequencia
table(df$track_genre)
df$popularity.cut = cut(df$popularity, breaks=c(-1, 25, 50, 75, 100))
table(df$popularity.cut)

#media de popularity
media_popularity <- mean(df$popularity, na.rm = TRUE) 
print(media_popularity)

#moda de popularity
moda_popularity <- getmode(df$popularity) 
print(moda_popularity)

#mediana de popularity
mediana_popularity <- median(df$popularity, na.rm = TRUE)
print(mediana_popularity)

#desvio padrao de popularity
desvio_padrao_popularity <- sd(df$popularity, na.rm = TRUE)
print(desvio_padrao_popularity)

#varinacia de popularity
variancia_popularity <- var(df$popularity, na.rm = TRUE)
print(variancia_popularity)

# Transformar a coluna 'explicit' e a 'track_genre' em fator
df$explicit <- as.factor(df$explicit)
#df$track_genre <- as.factor(df$track_genre)
df$key <- as.factor(df$key)
df$mode <- as.factor(df$mode)
df$time_signature <- as.factor(df$time_signature)

# Criar o modelo de regressão linear múltipla
modelo <- lm(formula = popularity ~ explicit + danceability + energy + loudness 
             + speechiness + acousticness + valence
             + key + mode + tempo + time_signature, data = df)

modelo <- step(modelo, direction="backward")

# Resumo do modelo
modelo_summary <- summary(modelo)
modelo_summary
# 1. Erro-padrão, t-valor e p-valor para cada coeficiente
coeficientes <- modelo_summary$coefficients
print(coeficientes)

# 2. R² (R-squared)
r_squared <- modelo_summary$r.squared
print(paste("R-squared:", r_squared))

# 3. R² ajustado (Adjusted R-squared)
r_squared_adj <- modelo_summary$adj.r.squared
print(paste("Adjusted R-squared:", r_squared_adj))

# 4. Estatística F e seu p-valor associado
f_statistic <- modelo_summary$fstatistic
f_value <- f_statistic[1]  # Valor da estatística F
f_p_value <- pf(f_value, f_statistic[2], f_statistic[3], lower.tail = FALSE) # P-valor associado
print(paste("F-statistic:", f_value))
print(paste("P-value da F-statistic:", f_p_value))

# Lista das variáveis independentes para as regressões simples
variaveis_independentes <- c("explicit", "danceability", "energy", "loudness",
                             "speechiness", "acousticness", "instrumentalness", "valence")

# Criar uma lista para armazenar os modelos de regressão
modelo2 <- list()

# Loop para criar um modelo de regressão linear simples para cada variável independente
for (var in variaveis_independentes) {
  # Criar a fórmula da regressão
  formula_regressao <- as.formula(paste("popularity ~", var))
  
  # Criar o modelo de regressão linear
  modelo2 <- lm(formula = formula_regressao, data = df)
  
  # Armazenar o modelo na lista
  modelo2[[var]] <- summary(modelo2)
  
  # Imprimir o resumo do modelo
  print(paste("Regressão de popularity sobre", var))
  print(modelo2[[var]])
}

# 1. Diagnóstico dos resíduos do modelo
par(mfrow = c(1, 2)) # Configura dois gráficos lado a lado

# 2. Gráfico Q-Q Plot para verificar a normalidade dos resíduos
qqnorm(modelo$residuals)
qqline(modelo$residuals, col = "red") # Adiciona uma linha de referência

# 3. Gráfico de Resíduos vs Ajustes (Fitted Values) para verificar a homoscedasticidade
plot(modelo$fitted.values, modelo$residuals,
     xlab = "Valores Ajustados (Fitted Values)",
     ylab = "Resíduos",
     main = "Resíduos vs Fitted Values")
abline(h = 0, col = "red") # Linha horizontal no zero
