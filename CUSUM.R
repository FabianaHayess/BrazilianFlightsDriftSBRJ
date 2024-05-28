# Filtrar as decolagens do SBRJ (Aeroporto Santos Dumont)
bfd_sbrj_partida <- bfd[bfd$depart == "SBRJ", ]

# Verificar as primeiras linhas do novo dataset
head(bfd_sbrj_partida)

# Definir a função drifter e suas variantes
drifter <- function(){
  obj <- list()
  attr(obj, 'class') <- 'drifter'
  return(obj)
}

dist_based <- function(){
  obj <- drifter()
  class(obj) <- append('dist_based', class(obj))
  return(obj)
}

error_based <- function(){
  obj <- drifter()
  class(obj) <- append('error_based', class(obj))
  return(obj)
}

multi_criteria <- function(){
  obj <- drifter()
  class(obj) <- append('multi_criteria', class(obj))
  return(obj)
}

reset <- function(obj){
  UseMethod('reset')
}

reset.default <- function(obj){
  return(0)
}

# Carregar a biblioteca necessária
library(dplyr)

# Converter as datas para o formato adequado, se necessário
bfd_sbrj_partida$expected_depart <- as.POSIXlt(bfd_sbrj_partida$expected_depart)

# Adicionar uma coluna de mês aos dados
bfd_sbrj_partida$month <- format(bfd_sbrj_partida$expected_depart, "%Y-%m")

# Calcular o tempo médio de atraso por mês
avg_delay_per_month <- bfd_sbrj_partida %>%
  group_by(month) %>%
  summarise(avg_delay = mean(delay_depart, na.rm = TRUE))

# Calcular o desvio padrão dos atrasos por mês
std_deviation_per_month <- bfd_sbrj_partida %>%
  group_by(month) %>%
  summarise(std_deviation = sd(delay_depart, na.rm = TRUE))

# Calcular a quantidade total de voos com atraso por mês
total_delayed_flights_per_month <- bfd_sbrj_partida %>%
  group_by(month) %>%
  summarise(total_delayed_flights = sum(delay_depart > 0, na.rm = TRUE))

# Visualizar as métricas calculadas
print(avg_delay_per_month)
print(std_deviation_per_month)
print(total_delayed_flights_per_month)

#Comparação de Métricas

# Comparação entre meses adjacentes
comparison_data <- data.frame(
  month = avg_delay_per_month$month[-1],  # Remove o primeiro mês para evitar comparação com um mês inexistente anterior
  avg_delay = avg_delay_per_month$avg_delay[-1],  # Remove o primeiro mês
  std_deviation = std_deviation_per_month$std_deviation[-1],  # Remove o primeiro mês
  total_delayed_flights = total_delayed_flights_per_month$total_delayed_flights[-1]  # Remove o primeiro mês
)

# Calcula as diferenças entre meses consecutivos
comparison_data$avg_delay_diff <- c(NA, diff(comparison_data$avg_delay))
comparison_data$std_deviation_diff <- c(NA, diff(comparison_data$std_deviation))
comparison_data$total_delayed_flights_diff <- c(NA, diff(comparison_data$total_delayed_flights))

# Visualize a tabela de comparação
print(comparison_data)

#GRAFICOS TEMPORAIS

library(ggplot2)

# Converter a coluna 'month' para formato Date para garantir a ordem correta no gráfico
avg_delay_per_month$month <- as.Date(paste0(avg_delay_per_month$month, "-01"))

# Gráfico Temporal: Evolução do Tempo Médio de Atraso por Mês
ggplot(avg_delay_per_month, aes(x = month, y = avg_delay)) +
  geom_line() +
  labs(title = "Evolução do Tempo Médio de Atraso por Mês",
       x = "Mês",
       y = "Tempo Médio de Atraso") +
  theme_minimal()

# Gráfico Temporal: Quantidade Total de Voos com Atraso por Mês
ggplot(total_delayed_flights_per_month, aes(x = month, y = total_delayed_flights)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Quantidade Total de Voos com Atraso por Mês",
       x = "Mês",
       y = "Quantidade Total de Voos com Atraso") +
  theme_minimal()



#CUSUM

# Definir o valor de referência (baseline) para os atrasos dos voos
baseline <- mean(bfd_sbrj_partida$delay_depart, na.rm = TRUE)

# Definir o limite de detecção para o CUSUM (ajuste conforme necessário)
cusum_limit <- 50

# Calcular as diferenças entre os atrasos observados e o valor de referência
bfd_sbrj_partida$difference <- bfd_sbrj_partida$delay_depart - baseline

# Inicializar o CUSUM como 0
cusum <- 0

# Criar vetor para armazenar os pontos de mudança (drift)
drift_points <- c()

# Aplicar o algoritmo CUSUM
for (i in 1:nrow(bfd_sbrj_partida)) {
  # Calcular o CUSUM como a soma acumulada das diferenças
  cusum <- max(0, cusum + bfd_sbrj_partida$difference[i] - cusum_limit)
  
  # Imprimir o valor atual do CUSUM e o índice i
  print(paste("CUSUM:", cusum, "Índice:", i))
  
  # Verificar se o CUSUM é NA ou excede o limite de detecção
  if (!is.na(cusum) && cusum > cusum_limit) {
    # Adicionar o ponto de mudança à lista de drift_points
    drift_points <- c(drift_points, i)
    
    # Reinicializar o CUSUM
    cusum <- 0
  }
}

# Visualizar os pontos de mudança (drift)
print(drift_points)

# Salvar o resultado da detecção de drift para posterior comparação
drift_results <- list(
  algorithm = 'CUSUM',
  drift_points = drift_points
)

# Plotar um gráfico para visualizar o ponto de mudança (drift)
library(ggplot2)

ggplot(bfd_sbrj_partida, aes(x = 1:nrow(bfd_sbrj_partida), y = delay_depart)) +
  geom_line() +
  geom_vline(xintercept = 33, linetype = "dashed", color = "red") +
  labs(x = "Índice dos voos", y = "Atraso (minutos)", title = "Identificação do Ponto de Mudança") +
  theme_minimal()


# Medida de Dissimilaridade e Teste de Hipótese

# Verificar se houve detecção de drift
if (length(drift_results$drift_points) > 0) {
  # Calcular a medida de dissimilaridade entre os períodos antes e depois do ponto de mudança
  drift_points <- drift_results$drift_points
  before_drift <- bfd_sbrj_partida[1:(drift_points - 1), ]
  after_drift <- bfd_sbrj_partida[drift_points:nrow(bfd_sbrj_partida), ]
  
  dissimilarity <- abs(mean(before_drift$delay_depart) - mean(after_drift$delay_depart))
  print(paste("Medida de Dissimilaridade:", dissimilarity))
  
  # Realizar um teste de hipótese para verificar a significância das mudanças
  p_value <- t.test(before_drift$delay_depart, after_drift$delay_depart)$p.value
  print(paste("Valor-p do Teste de Hipótese:", p_value))
} else {
  print("Nenhum ponto de mudança (drift) foi detectado.")
}
