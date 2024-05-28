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

# Função para criar um objeto de detecção de drift KSWIN
dd_kswin <- function(window_size = 100, stat_size = 30, alpha = 0.005, data = NULL, save_file = FALSE) {
  obj <- drifter()
  obj$window_size <- window_size
  obj$stat_size <- stat_size
  obj$alpha <- alpha
  obj$p_value <- 0
  obj$n <- 0
  obj$save_file <- save_file
  obj$output_file <- "drift_results.txt"  # Nome do arquivo de saída
  
  if ((obj$alpha < 0) | (obj$alpha > 1)) stop("Alpha must be between 0 and 1", call = FALSE)
  if (obj$window_size < 0) stop("window_size must be greater than 0", call = FALSE)
  if (obj$window_size < obj$stat_size) stop("stat_size must be smaller than window_size")
  
  if (missing(data)){
    obj$window <- c()
  } else {
    obj$window <- data
  }
  
  class(obj) <- append("dd_kswin", class(obj))
  return(obj)
}

# Função para atualizar o objeto de detecção de drift KSWIN com novos dados
update.dd_kswin <- function(obj, x) {
  obj$n <- obj$n + 1
  currentLength <- nrow(obj$window)
  if (is.null(currentLength)){
    currentLength <- 0
  }
  if (currentLength >= obj$window_size){
    obj$window <- obj$window[-1, drop=FALSE]
    rnd_window <- sample(x = obj$window[1:(length(obj$window) - obj$stat_size)], size = obj$stat_size)
    
    ks_res <- ks.test(rnd_window, obj$window[(length(obj$window) - obj$stat_size):length(obj$window)], exact = TRUE)
    st <- unlist(ks_res[1])
    obj$p_value <- unlist(ks_res[2])
    
    if((obj$p_value < obj$alpha) & (st > 0.1)){
      obj$window <- obj$window[(length(obj$window) - obj$stat_size):length(obj$window)]
      obj$window <- rbind(obj$window, x)
      if (obj$save_file) {
        write.table(data.frame(time = Sys.time(), event = "Drift detected"), file = obj$output_file, append = TRUE, sep = "\t", col.names = FALSE)
      }
      return(list(obj = obj, pred = TRUE))
    } else {
      obj$window <- rbind(obj$window, x)
      return(list(obj = obj, pred = FALSE))
    }
  } else {
    obj$window <- rbind(obj$window, x)
    return(list(obj = obj, pred = FALSE))
  }
}

# Função para ajustar o modelo de detecção de drift KSWIN aos dados
fit.dd_kswin <- function(obj, data) {
  for (i in 1:length(data)){
    output <- update.dd_kswin(output$obj, data[i])
  }
  
  if (obj$save_file) {
    # Salvar o resultado da detecção de drift para posterior comparação
    drift_results <- list(
      algorithm = 'KSWIN',
      drift_points = output$obj$drift_points
    )
    write.table(data.frame(time = Sys.time(), event = "Detection complete"), file = obj$output_file, append = TRUE, sep = "\t", col.names = FALSE)
    return(list(obj = output$obj, results = drift_results))
  } else {
    return(output$obj)
  }
}

# Função para realizar a detecção de drift KSWIN em uma série temporal
detect.dd_kswin <- function(obj, series) {
  non_na <- base::which(stats::complete.cases(series))
  data <- series[non_na, ]
  
  # Perform change point detection using KSWIN
  kswin_result <- rep(FALSE, length(data))
  output <- update.dd_kswin(obj, data[1])
  for (i in 1:length(data)){
    output <- update.dd_kswin(output$obj, data[i])
    kswin_result[i] <- output$pred
  }
  
  inon_na <- rep(FALSE, length(non_na))
  n <- length(kswin_result)
  if (n > 1)
    inon_na[kswin_result[1:(n-1)]] <- TRUE
  
  i <- rep(NA, nrow(series))
  i[non_na] <- inon_na
  detection <- data.frame(idx = 1:length(i), event = i, type = "")
  detection$type[i] <- "drift"
  
  return(detection)
}

# Gerar dados de exemplo antes e depois do drift
set.seed(123)
before_drift <- rnorm(50, mean = 0, sd = 1)
after_drift <- rnorm(50, mean = 2, sd = 1)

# Calcular a medida de dissimilaridade entre os dados antes e depois do drift
dissimilarity <- sum(abs(before_drift - after_drift))
print(paste("Medida de Dissimilaridade:", dissimilarity))

# Calcular as médias antes e depois do drift
before_drift_mean <- mean(before_drift)
after_drift_mean <- mean(after_drift)

print(paste("Média antes do Drift:", before_drift_mean))
print(paste("Média depois do Drift:", after_drift_mean))

# Realizar o teste de hipótese para comparar as médias antes e depois do drift
t_test <- t.test(before_drift, after_drift)
p_value <- t_test$p.value

print(paste("Valor-p do Teste de Hipótese:", p_value))
