str(before_drift)

# Função para criar um objeto de detecção de drift
drifter <- function(){
  obj <- list()
  attr(obj, 'class') <- 'drifter'
  return(obj)
}

# Função para detecção de drift baseada em distância
dist_based <- function(){
  obj <- drifter()
  class(obj) <- append('dist_based', class(obj))
  return(obj)
}

# Função para detecção de drift baseada em erro
error_based <- function(){
  obj <- drifter()
  class(obj) <- append('error_based', class(obj))
  return(obj)
}

# Função para detecção de drift com múltiplos critérios
multi_criteria <- function(){
  obj <- drifter()
  class(obj) <- append('multi_criteria', class(obj))
  return(obj)
}

# Função para redefinir o objeto de detecção de drift
reset <- function(obj){
  UseMethod('reset')
}

# Função de reset padrão
reset.default <- function(obj){
  return(0)
}

# Aplicação do algoritmo Page-Hinkley
page_hinkley_detection <- function(data, alpha = 0.05, delta = 0.1) {
  # Inicialização de variáveis
  n <- nrow(data)
  drift_points <- c()
  s <- 0
  h <- 0
  v <- 0
  
  # Loop para detecção de drift
  for (i in 1:n) {
    x <- data$difference[i]  # Alteração aqui para usar o conjunto de dados antes do drift
    s <- max(0, s + x - delta)
    h <- max(h, s)
    v <- v + x
    
    # Verificação de drift
    if (h > alpha) {
      drift_points <- c(drift_points, i)
      s <- 0
      h <- 0
      v <- 0
    }
  }
  
  # Salvar o resultado da detecção de drift para posterior comparação
  drift_results <- list(
    algorithm = 'Page-Hinkley',
    drift_points = drift_points
  )
  
  return(drift_results)
}

# Utilização do algoritmo Page-Hinkley no conjunto de dados before_drift
page_hinkley_results <- page_hinkley_detection(before_drift)
print(page_hinkley_results)


# Plotar um gráfico para visualizar o ponto de mudança (drift) pelo Page-Hinkley
library(ggplot2)

ggplot(before_drift, aes(x = 1:nrow(before_drift), y = difference)) +
  geom_line() +
  geom_vline(xintercept = page_hinkley_results$drift_points, linetype = "dashed", color = "red") +
  labs(x = "Índice dos voos", y = "Diferença em relação ao baseline", title = "Verificação do Drift - Page-Hinkley") +
  theme_minimal()

# Calcular a medida de dissimilaridade entre os dados antes e depois do drift
dissimilarity <- sum(abs(before_drift$difference - after_drift$difference))

# Realizar o teste de hipótese para comparar as médias antes e depois do drift
t_test <- t.test(before_drift$difference, after_drift$difference)
p_value <- t_test$p.value

# Imprimir a medida de dissimilaridade e o valor-p do teste de hipótese
print(paste("Medida de Dissimilaridade:", dissimilarity))
print(paste("Valor-p do Teste de Hipótese:", p_value))

