install.packages("pacman") # gerenciador de pacotes
library(pacman)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
                       ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny,                       
               stringr, tidyr, magrittr, hrbrthemes, car) #pacotes basicos

head(dataset)


table(dataset$track_genre)

genre_count <- length(unique(dataset$track_genre))
print(paste("Contagem de gêneros:", genre_count))

selected_genres <- c("disco", "classical", "forro", "rock", "jazz", "pop", "reggae", "country", "mpb", "electronic")

df <- dataset %>% filter(track_genre %in% selected_genres)

df <- df %>% select("artists", "track_name", "track_genre", "popularity", "danceability")

# tabelas de dist. de frequencia
table(df$track_genre)
df$danceability.cut = cut(df$danceability, breaks=c(0, 0.25, 0.50, 0.75, 1))
table(df$danceability.cut)

# funcao que calcula moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# medidas de dispersao e posicao
for(genre in selected_genres) {
  print(genre)
  genre_danceability <- filter(df, track_genre == genre)$danceability
  print(paste("media:", mean(genre_danceability) %>% round(5)))
  print(paste("moda:", getmode(genre_danceability) %>% round(5)))
  print(paste("mediana:", median(genre_danceability) %>% round(5)))
  variance <- var(genre_danceability) %>% round(5)
  print(paste("variancia:", variance))
  print(paste("desvio padrao:", sqrt(variance) %>% round(5)))
  
  
  print("=====================================")
}

# Grafico de densidade
dplot <- ggplot(data=df, aes(x=danceability, group=track_genre, fill=track_genre)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~track_genre) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.7, "lines"),
    axis.ticks.x=element_blank(),
    axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  ) +
  stat_summary(aes(xintercept = after_stat(x), y = 0), fun = mean, geom = "vline", orientation = "y")+
  ggtitle("Gráfico de densidade da danceability (0 ≤ x ≤ 1)")

dplot

# Boxplot
bplot <- ggplot(df, aes(x=track_genre, y=danceability, color=track_genre)) +
  geom_boxplot() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
  )

bplot

print("Testes estatísticos:")
print("==============================")
for(genre in selected_genres) {
  print(genre)
  
  genre_danceability <- filter(df, track_genre == genre)$danceability
  
  print("p-valor do teste de normalidade Shapiro-Wilk:")
  shapiro_result <- shapiro.test(genre_danceability)
  is_normal = "NORMAL"
  if(shapiro_result$p.value < 0.05) {
    is_normal <- "NÃO É NORMAL"
  }
  paste(shapiro_result$p.value, "-", is_normal) %>% print()
  print("==============================")
}

print("Teste de homocedasticidade de Levene:")
leveneTest(
  danceability ~ track_genre,
  df
)

print("Análise de variância de um fator:")
anova_res = aov(danceability ~ track_genre, data = df)
summary(anova_res)
