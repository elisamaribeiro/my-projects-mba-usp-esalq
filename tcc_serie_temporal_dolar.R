########################### SÉRIE DO DÓLAR 2014-2024 ###########################

######################### BIBLIOTECAS #############################
# Instalar pacotes
install_packages <- function(packages) {
  # Verificar se os pacotes já estão instalados
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  # Instalar apenas os pacotes não instalados
  if(length(new_packages)) install.packages(new_packages)
  
  # Carregar os pacotes instalados
  invisible(lapply(packages, library, character.only = TRUE))
}

# Lista de pacotes a serem instalados
packages <- c(
  "rugarch", "fGarch", "aTSA", "corrr", "GGIR", "xts", 
  "quantmod", "BETS", "rbcb", "dygraphs", "forecast", 
  "urca", "tseries", "strucchange", "grnn", "tsfgrnn", 
  "corrplot", "readxl", "ggplot2", "fracdiff"
)

# Instalar e carregar os pacotes
install_packages(packages)


################## DADOS INICIAIS ###############################
# Carregar os dados da cotação diária do dólar
dados <- read_excel("serie_dolar_cepea.xlsx", col_names = c("Data", "Dolar"))
dados_teste <- read_excel("serie_dolar_teste_cepea.xlsx", col_names = c("Data", "Dolar"))

# Converter os dados em uma série temporal
dados <- as.xts(ts(read_excel("serie_dolar_cepea.xlsx")))
dados_serie <- as.numeric(dados[, 2])
head(dados)
hist(dados[, 2])

dados_teste <- as.xts(ts(read_excel("serie_dolar_teste_cepea.xlsx")))
dados_teste_v <- as.numeric(dados_teste[, 2])


############### TESTAR DENSIDADE DISTRIBUTIVA E NORMALIDADE JARQUE-BERA ###############
# Extrair os dados da segunda coluna
dados_densidade <- dados[, 2]

# Calcular média e desvio padrão dos dados
dados_densidade_mean <- mean(dados_densidade)
dados_densidade_sd <- sd(dados_densidade)

# Ajustar o estilo do gráfico
estilo <- theme(axis.text.x = element_text(face = "plain", color = "black"),  
                axis.text.y = element_text(face = "plain", color = "black"),  
                axis.line = element_line(color = "black", size = 1.5),         
                panel.background = element_rect(fill = "white"),               
                plot.title = element_blank())                                  

# Criar o histograma dos dados
p <- ggplot(data.frame(x = dados_densidade), aes(x = dados_densidade)) +
  geom_histogram(binwidth = 0.1, fill = "grey", color = "black", alpha = 0.5) +
  labs(x = "Valores", y = "Densidade") +
  estilo +
  stat_function(fun = dnorm, args = list(mean = dados_densidade_mean, sd = dados_densidade_sd), color = "black")

# Exibir o gráfico
print(p)

# Testar JARQUE-BERA
jarque.bera.test(dados_densidade)


############################## ESTIMAÇÃO DE MODELOs ############################
# Montar uma regressão simples com intercepto (01)
X <- dados[, 2] / 10000
head(X)
lags <- 0
t <- 1:2314
z <- diff(X)
n <- length(z)
reg <- lm(X ~ t)
summary(reg)


# Montar uma regressão simples sem intercepto (02)
X <- dados[, 2] / 10000
lags <- 0
t <- 1:2314
z <- diff(X)
n <- length(z)
reg <- lm(X ~ t - 1)
summary(reg)


# Montar regressão com uma diferença (03)
X <- dados[, 2] / 10000
lags <- 0
t <- 1:2314
z <- diff(X)
n <- length(z)
zdiff <- embed(z, lags + 1)[, 1]/1000
zlag <- X[(lags + 1):n]
reg <- lm(zdiff ~ zlag - 1)
summary(reg)


# Montar um modelo GLM (04)
X <- dados[ , 2] / 10000
lags <- 0
t <- 1:2314
z <- diff(X)
n <- length(z)
reg <- glm(X ~ t)
summary(reg)

# Montar regressão com uma diferença no GLM (05)
zdiff <- embed(z, lags + 1)[, 1]
zlag <- X[(lags + 1):n]
reg <- glm(zdiff ~ zlag - 1)
summary(reg)


######################### TESTE DE NORMALIDADE NOS MODELOS #####################
# SHAPIRO-WILK
# Teste no modelo 
n <- nrow(X)
if (n > 5000) {
  X <- tail(X, 5000)
}
X_vector <- as.vector(coredata(X))
shapiro_result <- shapiro.test(X_vector)
print(shapiro_result)


# Teste no resíduo do modelo
res <- resid(reg)
plot(fitted(reg), res)
abline(0,0)
qqnorm(res)
qqline(res)
plot(density(res))

n <- length(res)
if (n > 5000) {
  res <- tail(res, 5000)
}
res_vector <- as.vector(res)
shapiro_result <- shapiro.test(res_vector)
print(shapiro_result)


######################## TESTE RAIZ UNITÁRIA NOS MODEL0S #######################
# DICKEY-FULLER para raiz unitária
df <- ur.df(X, type = "none", lags = 0)
summary(df)
df <- ur.df(X, type = "none", lags = 1)
summary(df)
df <- ur.df(X, type = "none", lags = 2)
summary(df)
df <- ur.df(X, type = "none", lags = 3)
summary(df)
df <- ur.df(X, type = "none", lags = 4)
summary(df)
df <- ur.df(X, type = "none", lags = 5)
summary(df)
df <- ur.df(X, type = "none", lags = 6)
summary(df)
df <- ur.df(X, type = "none", lags = 7)
summary(df)
df <- ur.df(X, type = "none", lags = 8)
summary(df)
df <- ur.df(X, type = "none", lags = 9)
summary(df)
df <- ur.df(X, type = "none", lags = 10)

summary(df)

# ADF-AUGMENTED DICKEY FULLER TEST
adf.test(X, k=1)

# Dickey-Fuller Aumentado (ADF)
summary(ur.df(X))

# Teste de raiz unitária de Elliot, Rothenberg e Stock (ERS)
summary(ur.ers(X))

# Teste de raiz unitária KPSS (Kwiatkowski-Phillips-Schmidt-Shin)
summary(ur.kpss(X))

# Teste de raiz unitária de Phillips-Perron (PP)
summary(ur.pp(X))

# Teste de raiz unitária de Schmidt-Phillips (SP)
summary(ur.sp(X))

# Zivot--Andrews test for unit roots
summary(ur.za(X))


############################ ANÁLISE DE AUTOCORRELAÇÃO #########################
# Autocorrelação (ACF)
acf_result <- acf(X)
print(acf_result)


# Autocorrelação parcial (PACF) 
pacf_result <- pacf(X)
print(pacf_result)


################################ COMBINACAO ARIMA ##############################
# Ajustar modelos ARIMA com diferentes ordens
ar1 <- arima(dados[, 2], order = c(1, 0, 0))
ar2 <- arima(dados[, 2], order = c(2, 0, 0))
ar3 <- arima(dados[, 2], order = c(3, 0, 0))
ar4 <- arima(dados[, 2], order = c(4, 0, 0))
ar5 <- arima(dados[, 2], order = c(5, 0, 0))
ar6 <- arima(dados[, 2], order = c(6, 0, 0))
ar7 <- arima(dados[, 2], order = c(7, 0, 0))
ar8 <- arima(dados[, 2], order = c(8, 0, 0))
ar9 <- arima(dados[, 2], order = c(9, 0, 0))
ar10 <- arima(dados[, 2], order = c(10, 0, 0))

# Calcular os critérios de informação para cada modelo
aic_values <- c(AIC(ar1), AIC(ar2), AIC(ar3), AIC(ar4), AIC(ar5), AIC(ar6), AIC(ar7), AIC(ar8), AIC(ar9), AIC(ar10))
bic_values <- c(BIC(ar1), BIC(ar2), BIC(ar3), BIC(ar4), BIC(ar5), BIC(ar6), BIC(ar7), BIC(ar8), BIC(ar9), BIC(ar10))

# Escolher o modelo com o menor critério de informação
best_model_aic <- which.min(aic_values)
best_model_bic <- which.min(bic_values)

# Exibir os critérios de informação e o modelo selecionado
print("Critério de Informação de Akaike (AIC):")
print(aic_values)
print(paste("Melhor modelo (AIC): ARIMA(", best_model_aic, ", 0, 0)"))

print("Critério de Informação Bayesiano (BIC):")
print(bic_values)
print(paste("Melhor modelo (BIC): ARIMA(", best_model_bic, ", 0, 0)"))


# Ajustar modelos MA
ma1 <- arima(dados[, 2], order = c(0,0,1))
ma2 <- arima(dados[, 2], order = c(0,0,2))
ma3 <- arima(dados[, 2], order = c(0,0,3))
ma4 <- arima(dados[, 2], order = c(0,0,4))
ma5 <- arima(dados[, 2], order = c(0,0,5))
ma6 <- arima(dados[, 2], order = c(0,0,6))
ma7 <- arima(dados[, 2], order = c(0,0,7))
ma8 <- arima(dados[, 2], order = c(0,0,8))
ma9 <- arima(dados[, 2], order = c(0,0,9))
ma10 <- arima(dados[, 2], order = c(0,0,10))

# Calcular os critérios de informação para cada modelo
aic_values_ma <- c(AIC(ma1), AIC(ma2), AIC(ma3), AIC(ma4), AIC(ma5), AIC(ma6), AIC(ma7), AIC(ma8), AIC(ma9), AIC(ma10))
bic_values_ma <- c(BIC(ma1), BIC(ma2), BIC(ma3), BIC(ma4), BIC(ma5), BIC(ma6), BIC(ma7), BIC(ma8), BIC(ma9), BIC(ma10))

# Escolher o modelo com o menor critério de informação
best_model_aic_ma <- which.min(aic_values_ma)
best_model_bic_ma <- which.min(bic_values_ma)

# Exibir os critérios de informação e o modelo selecionado
print("Critério de Informação de Akaike (AIC) para modelos MA:")
print(aic_values_ma)
print(paste("Melhor modelo (AIC) para MA:", best_model_aic_ma))

print("Critério de Informação Bayesiano (BIC) para modelos MA:")
print(bic_values_ma)
print(paste("Melhor modelo (BIC) para MA:", best_model_bic_ma))


# Escolher o modelo ARMA com base no critério de informação (por exemplo, AIC)
if (aic_values[best_model_aic] < aic_values_ma[best_model_aic_ma]) {
  best_arma_model <- arima(dados[, 2], order = c(best_model_aic, 0, 0))
} else {
  best_arma_model <- arima(dados[, 2], order = c(0, 0, best_model_aic_ma))
}

# Exibir o melhor modelo ARMA selecionado
print("Melhor modelo ARMA:")
print(best_arma_model)


# criar um modelo ARIMA
best_model_arima <- arima(dados[, 2], order = c(best_model_aic, 0, 0))
print(best_model_arima)

# Testar o modelo
arma <- arima(dados[, 2], order = c(1, 0, 0))
print(arma)


#################### AUTOCORRELAÇÃO DOS RESÍDUOS DO MODELO #####################
# Obter os resíduos do modelo ARIMA
residuos <- residuals(arma)

# Teste de Ljung-Box para os resíduos do modelo ARIMA
ljung_box_test <- Box.test(residuos, lag = 20, type = "Ljung-Box")

# Exibir o resultado do teste
print(ljung_box_test)


############################ PREVISÕES COM ARIMA ###############################
# Ajustar o modelo ARIMA aos dados
modelo_arima <- arima(dados[, 2], order = c(1, 0, 0))

# Fazer previsões para os próximos 250 dias
previsoes <- forecast(modelo_arima, h = 250)

# Extrair os valores previstos
valores_previstos <- previsoes$mean

# Extrair os resíduos das previsões
residuos_previsoes <- previsoes$residuals

# Calcular o erro médio absoluto (MAE)
MAE <- mean(abs(dados_teste_v - valores_previstos))

# Calcular o erro quadrático médio (RMSE)
RMSE <- sqrt(mean((dados_teste_v - valores_previstos)^2))

# Calcular o erro médio percentual absoluto (MAPE)
MAPE <- mean(abs((dados_teste_v - valores_previstos) / dados_teste_v)) * 100

# Exibir as métricas de desempenho
print(paste("Erro Médio Absoluto (MAE):", MAE))
print(paste("Erro Quadrático Médio (RMSE):", RMSE))
print(paste("Erro Médio Percentual Absoluto (MAPE):", MAPE))

# Visualizar os valores previstos pelo modelo ARIMA
print(valores_previstos)

# Visualizar os intervalos de confiança
# Ajustar o estilo do gráfico
estilo <- theme(axis.text.x = element_text(face = "plain", color = "black"),  
                axis.text.y = element_text(face = "plain", color = "black"),  
                axis.line = element_line(color = "black", size = 1.5),         
                panel.background = element_rect(fill = "white"),               
                plot.title = element_blank())                                  

# Visualizar os valores previstos pelo modelo ARIMA
autoplot(previsoes) + estilo +
  labs(x = "Dias", y = "Dólar comercial (R$)")



####################### TESTE DE HETEROCEDASTICIDADE DE ARIMA ##################
# Teste ARCH para verificar a heterocedasticidade da série
arch.test(arma) 



################################# MODELOS GARCH #################################

##### Especificação do modelo GARCH (sGARCH) de ordem (1,1) e modelo de distribuição t de Student
spec1 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                   mean.model=list(armaOrder=c(1,0,0), include.mean=TRUE),  
                   distribution.model="std")

# Ajuste do modelo GARCH aos dados
garch2 = ugarchfit(spec = spec1, data = dados[, 2])
garch2@fit$matcoef
print(garch2)

# Previsão com o modelo GARCH
pred_garch2 <- ugarchforecast(fitORspec = garch2, n.ahead = 250)
pred_garch2


##### Especificação do modelo GARCH (sGARCH) de ordem (1,1) e modelo de distribuição normal assimétrica (snorm)
spec2 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                   mean.model=list(armaOrder=c(1,0,0), include.mean=TRUE),  
                   distribution.model="snorm")

# Ajuste do modelo GARCH aos dados
garch3 = ugarchfit(spec = spec2, data = dados[, 2])
garch3@fit$matcoef
print(garch3)

# Previsão com o modelo GARCH
pred_garch3 <- ugarchforecast(fitORspec = garch3, n.ahead = 250)
pred_garch3



##### Especificação do modelo EGARCH (eGARCH) de ordem (1,1) e modelo de distribuição t de Student
spec_egarch1 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), 
                           mean.model = list(armaOrder = c(1, 0, 0), include.mean = TRUE),  
                           distribution.model = "std")

# Ajuste do modelo EGARCH aos dados
garch_egarch1 <- ugarchfit(spec = spec_egarch1, data = dados[, 2])
garch_egarch1@fit$matcoef
print(garch_egarch1)

# Previsão com o modelo EGARCH
pred_egarch1 <- ugarchforecast(fitORspec = garch_egarch1, n.ahead = 250)
pred_egarch1



##### Especificação do modelo EGARCH (eGARCH) de ordem (1,1) e modelo de distribuição normal assimétrica (snorm)
spec_egarch2 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), 
                           mean.model = list(armaOrder = c(1, 0, 0), include.mean = TRUE),  
                           distribution.model = "snorm")

# Ajuste do modelo EGARCH aos dados
garch_egarch2 <- ugarchfit(spec = spec_egarch2, data = dados[, 2])
garch_egarch2@fit$matcoef
print(garch_egarch2)

# Previsão com o modelo EGARCH
pred_egarch2 <- ugarchforecast(fitORspec = garch_egarch2, n.ahead = 250)
pred_egarch2






#################### AUTOCORRELAÇÃO DOS RESÍDUOS DO MODELO #####################
## Obter os resíduos do modelo GARCH (garch2)
residuos_garch2 <- residuals(garch2)

# Teste de Ljung-Box para os resíduos
ljung_box_test2 <- Box.test(residuos_garch2, lag = 20, type = "Ljung-Box")

# Exibir o resultado do teste
print(ljung_box_test2) # 


## Obter os resíduos do modelo GARCH (garch3)
residuos_garch3 <- residuals(garch3)

# Teste de Ljung-Box para os resíduos
ljung_box_test3 <- Box.test(residuos_garch3, lag = 20, type = "Ljung-Box")

# Exibir o resultado do teste
print(ljung_box_test3) # 


## Obter os resíduos do modelo GARCH (garch_egarch1)
residuos_egarch1 <- residuals(garch_egarch1)

# Teste de Ljung-Box para os resíduos
ljung_box_test4<- Box.test(residuos_egarch1, lag = 20, type = "Ljung-Box")

# Exibir o resultado do teste
print(ljung_box_test4) # 


## Obter os resíduos do modelo GARCH (garch_egarch2)
residuos_egarch2 <- residuals(garch_egarch2)

# Teste de Ljung-Box para os resíduos
ljung_box_test5<- Box.test(residuos_egarch2, lag = 20, type = "Ljung-Box")

# Exibir o resultado do teste
print(ljung_box_test5) # 



############################### GRÁFICOS #######################################
# Ajustar o estilo do gráfico
estilo <- theme(axis.text.x = element_text(face = "plain", color = "black"),  
                axis.text.y = element_text(face = "plain", color = "black"),  
                axis.line = element_line(color = "black", size = 1.5),         
                panel.background = element_rect(fill = "white"),               
                plot.title = element_blank())                                  

# Criar um gráfico para visualizar os valores previstos pelo modelo GARCH
autoplot(sigma(garch_egarch1)) + estilo +
  labs(x = "Dias", y = "Volatilidade")

# Extrair valores previstos e intervalos de confiança
previsoes <- as.numeric(pred_egarch1@forecast$seriesFor)
lower <- as.numeric(pred_egarch1@forecast$seriesFor - 1.96 * sqrt(pred_egarch1@forecast$sigmaFor^2))
upper <- as.numeric(pred_egarch1@forecast$seriesFor + 1.96 * sqrt(pred_egarch1@forecast$sigmaFor^2))

# Criar um data frame com os dados
dados <- data.frame(Dias = 1:250, Previsao = previsoes, Lower = lower, Upper = upper)

# Plotar os valores previstos com intervalo de confiança
ggplot(dados, aes(x = Dias, y = Previsao)) +
  geom_line() +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  estilo +
  labs(x = "Dias", y = "Volatilidade")

### Obrigada por chegar até aqui!

############################################## FIM ###################################################
