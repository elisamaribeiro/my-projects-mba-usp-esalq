##################################################################
# TCC - PROJEÇÃO DO DÓLAR
# MBA USP ESALQ
# DATA SCIENCE e ANALYTICS
################################################################

# Instalação e Carregamento de Todos os Pacotes ---------------------------

pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS","feasts",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal","devtools","transformr","gganimate")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

# Buscar o caminho do diretório onde está salvo a base de dados

base_dolar <- read_excel("serie_dolar_cepea.xlsx")

# Lendo a base de dados

View(base_dolar)

# Transformando em série temporal, com frequência de 250 dias úteis

dolar=ts(base_dolar[2], start = c(2000,1,3), end=c(2024,1,10),frequency = 250)

autoplot(dolar) +
  theme_bw()

# Analisando os dados mês a mês

monthplot(dolar, col.base=1,lty.base = 2)

# Separando a série em duas janelas de tempo

sdolar=window(dolar, start=c(2000,1), end=c(2023,1))
plot(sdolar)
teste=window(dolar, start=c(2023,1), end=c(2024,1))
plot(teste)
length(teste)

# plotando as duas séries juntas para checagem

autoplot(dolar) +
  autolayer(sdolar, series="Série de treino (R$/USD)") +
  autolayer(teste, series="Série de teste (R$/USD)") +
  scale_color_grey() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_line(color = "gray98"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")

# Análise da Série

ggtsdisplay(sdolar, theme=theme_grey())
acf(sdolar)
pacf(sdolar)

# Teste de Estacionariedade

testeDF_dolar=ur.df(sdolar)
summary(testeDF_dolar)

# Conclusão: p-value 0.307 > 0.01 (99% confiança) - ACEITO H0,
# portanto a série NÃO é estacionária, precisa ser diferenciada

# Verificar quantas diferenciações são necessárias

ndiffs(sdolar)

# Realizar a diferenciação

difsdolar=diff(sdolar)
ggtsdisplay(difsdolar)

# Teste de Estacionariedade da série diferenciada

teste_dolardif=ur.df(difsdolar)
summary(teste_dolardif)

# Estimando um modelo inicial pelo autoarima para os dados originais

modelo_original_dolar=auto.arima(sdolar, trace=T)

# Estimando um modelo inicial pelo autoarima para os dados com uma diferenciação
modelo_dif_dolar=auto.arima(difsdolar,trace = T)

# validação e diagnóstico

checkresiduals(modelo_original_dolar)

# 1. teste de Ljung-Box p-value = 2.2e-16<0.01, rejeitamos H0, resíduos são
# correlacionados

# 2. Normalidade dos resíduos (DANDO ERRO)
ks.test(modelo_original_dolar$residuals, "pnorm", mean(modelo_original_dolar$residuals),
        sd(modelo_original_dolar$residuals), ties="midrank")
# p-valor = 0.04891 > 0,01 - Aceita H0, ou seja, resíduos normais

# confirmada a não existência de autocorrelação serial e normalidade dos resíduos
# Podemos verificar a estacionariedade de variância
# verificar se existe efeitos ARCH

ArchTest(modelo_original_dolar$residuals)

# p-valor 2.2e-16 < 0,01, rejeita-se H0, sugere a existência
# de efeitos ARCH

# Previsao para a série de dólar 

prev_dolar=forecast::forecast(modelo_original_dolar, h=500)

prev_dolar

autoplot(prev_dolar) +
  scale_color_grey() +
  theme_bw()

# Verificar a acurácia

forecast::accuracy(prev_dolar, teste)

## FIM!



