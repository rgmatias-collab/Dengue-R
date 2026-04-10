# =============================
# PACOTES
# =============================
library(readxl)
library(data.table)
library(parallel)

# =============================
# CARREGAR DADOS
# =============================
df <- read_excel("C:/Users/rgmat/OneDrive/Documentos/espacialização - ba/Monte carlo/Barreiras/Barreiras_pareada.xlsx")


df$data <- as.Date(df$data)
df <- df[order(df$data), ]

casos <- as.numeric(df$casos)
temp  <- as.numeric(df$temp)

n <- length(casos)

# =============================
# 📏 ESCALAS LOG (melhor p/ DCCA)
# =============================

scales <- c(
  4, 5, 7, 9, 11, 13, 16, 20, 23, 28, 33, 38, 45, 52, 60, 69, 79, 91,
  104, 119, 135, 154, 174, 198, 223, 252, 285, 321, 362, 407, 457, 513,
  575, 645, 723, 809, 905, 1011, 1130, 1261, 1407
)


# =============================
# DCCA OTIMIZADO
# =============================
dcca_sliding_fast <- function(x, y, scales) {
  
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  # perfis acumulados
  x_cum <- cumsum(x - mean(x))
  y_cum <- cumsum(y - mean(y))
  
  n <- length(x)
  rho <- numeric(length(scales))
  
  for (k in seq_along(scales)) {
    
    s <- scales[k]
    
    t <- 0:(s-1)
    t_mean <- mean(t)
    denom <- sum((t - t_mean)^2)
    
    Fxy_vals <- numeric(n - s + 1)
    Fx_vals  <- numeric(n - s + 1)
    Fy_vals  <- numeric(n - s + 1)
    
    idx <- 1
    
    for (start in 1:(n - s + 1)) {
      
      end <- start + s - 1
      
      X <- x_cum[start:end]
      Y <- y_cum[start:end]
      
      X_mean <- mean(X)
      Y_mean <- mean(Y)
      
      beta_x <- sum((t - t_mean)*(X - X_mean)) / denom
      beta_y <- sum((t - t_mean)*(Y - Y_mean)) / denom
      
      alpha_x <- X_mean - beta_x * t_mean
      alpha_y <- Y_mean - beta_y * t_mean
      
      trend_x <- alpha_x + beta_x * t
      trend_y <- alpha_y + beta_y * t
      
      Xd <- X - trend_x
      Yd <- Y - trend_y
      
      Fxy_vals[idx] <- mean(Xd * Yd)
      Fx_vals[idx]  <- mean(Xd^2)
      Fy_vals[idx]  <- mean(Yd^2)
      
      idx <- idx + 1
    }
    
    Fxy <- mean(Fxy_vals)
    Fx  <- mean(Fx_vals)
    Fy  <- mean(Fy_vals)
    
    rho[k] <- Fxy / sqrt(Fx * Fy)
  }
  
  return(rho)
}

# =============================
#  IMPUTAÇÃO DE ZEROS
# =============================
simular_zeros <- function(casos, prob = 0.5, ruido = TRUE) {
  
  novos <- casos
  zeros_idx <- which(casos == 0)
  
  for (i in zeros_idx) {
    
    if (runif(1) < prob) {
      
      if (i > 1 && i < length(casos)) {
        
        media_local <- mean(c(casos[i-1], casos[i+1]))
        
        if (ruido) {
          std_local <- sd(c(casos[i-1], casos[i+1]))
          novos[i] <- media_local + rnorm(1, 0, std_local + 1e-6)
        } else {
          novos[i] <- media_local
        }
        
      } else {
        novos[i] <- mean(casos)
      }
    }
  }
  
  return(novos)
}

# =============================
# DCCA ORIGINAL
# =============================
rho_original <- dcca_sliding_fast(casos, temp, scales)

# =============================
# MONTE CARLO PARALELO
# =============================
library(parallel)
library(parallel)

n_sim <- 500
n_cores <- detectCores() - 1

cl <- makeCluster(n_cores)

# Exportar funções e variáveis
clusterExport(cl, c("casos", "temp", "scales", "simular_zeros", "dcca_sliding_fast"))

# Rodar simulações
resultados <- parLapply(cl, 1:n_sim, function(i) {
  casos_sim <- simular_zeros(casos)
  dcca_sliding_fast(casos_sim, temp, scales)
})

stopCluster(cl)

# Converter para matriz
resultados <- do.call(rbind, resultados)

# Agora você pode calcular estatísticas
rho_mean <- colMeans(resultados)
rho_low  <- apply(resultados, 2, quantile, 0.025)
rho_high <- apply(resultados, 2, quantile, 0.975)

# =============================
# 📉 PLOT
# =============================
plot(scales, rho_original, type = "b", pch = 16, col = "blue",
     ylim = range(c(rho_low, rho_high)),
     xlab = "Escala (dias)",
     ylab = "ρ-DCCA",
     main = "Robustez DCCA (Monte Carlo)")

lines(scales, rho_mean, type = "b", pch = 17, col = "red")

polygon(c(scales, rev(scales)),
        c(rho_low, rev(rho_high)),
        col = rgb(0,0,1,0.2), border = NA)

legend("topright",
       legend = c("Original", "Média simulada", "IC 95%"),
       col = c("blue", "red", rgb(0,0,1,0.2)),
       lty = c(1,1,NA),
       pch = c(16,17,15))

# =============================
# ⏱ DIAGNÓSTICO
# =============================
print(scales)
print(round(apply(resultados, 2, sd), 4))


# =============================
# Criar dataframe final para salvar
# =============================
df_save <- data.frame(
  escala = scales,
  rho_original = rho_original,
  rho_media_simulada = rho_mean,
  IC_2.5 = rho_low,
  IC_97.5 = rho_high
)

# =============================
# Salvar na mesma pasta do arquivo original
# =============================
pasta <- "C:/Users/rgmat/OneDrive/Documentos/espacialização - ba/Monte carlo/Barreiras"
nome_saida <- file.path(pasta, "dcca_resultados_bar_temp1.xlsx")
write.xlsx(df_save, nome_saida)

cat("Arquivo salvo em:", nome_saida, "\n")



