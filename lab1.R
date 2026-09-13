library(tidyverse)
library(summarytools)

dados <- read_csv("brasileirao_2026.csv")

head(dados)
str(dados)
summary(dados)
tail(dados)

jogos_realizados <- dados %>%
  filter(!is.na(gols_mandante) & !is.na(gols_visitante))

nrow(jogos_realizados)

jogos_futuro <- dados %>%
  filter(is.na(gols_mandante) | is.na(gols_visitante))

nrow(jogos_futuro)

#===============================================================================
# DEFININDO PARÂMETROS
#===============================================================================

# médias dos mandantes
medias_mandante <- jogos_realizados %>%
  group_by(time = time_mandante) %>%
  summarise(
    theta_m = mean(gols_mandante),
    phi_m = mean(gols_visitante)
  )

# médias dos visitantes
medias_visitante <- jogos_realizados %>%
  group_by(time = time_visitante) %>%
  summarise(
    theta_v = mean(gols_visitante),
    phi_v = mean(gols_mandante)
  )

# tabela dos parâmetros
parametros_times <- medias_mandante %>%
  inner_join(medias_visitante, by = "time")

print(parametros_times)

#===============================================================================
# ANÁLISE MANDANTES/VISITANTES
#===============================================================================

# verificando a quantidade de jogos por time como mandante
freq(jogos_realizados$time_mandante, order = "Freq")

#                       Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ------------------- ------ --------- -------------- --------- --------------
#        Athletico-PR     13      5.28           5.28      5.28           5.28
#               Bahia     13      5.28          10.57      5.28          10.57
#         Corinthians     13      5.28          15.85      5.28          15.85
#            Cruzeiro     13      5.28          21.14      5.28          21.14
#          Fluminense     13      5.28          26.42      5.28          26.42
#       Internacional     13      5.28          31.71      5.28          31.71
#            Mirassol     13      5.28          36.99      5.28          36.99
#              Santos     13      5.28          42.28      5.28          42.28
#               Vasco     13      5.28          47.56      5.28          47.56
#          Bragantino     12      4.88          52.44      4.88          52.44
#         Chapecoense     12      4.88          57.32      4.88          57.32
#            Coritiba     12      4.88          62.20      4.88          62.20
#            Flamengo     12      4.88          67.07      4.88          67.07
#              Grêmio     12      4.88          71.95      4.88          71.95
#           Palmeiras     12      4.88          76.83      4.88          76.83
#                Remo     12      4.88          81.71      4.88          81.71
#             Vitória     12      4.88          86.59      4.88          86.59
#         Atlético-MG     11      4.47          91.06      4.47          91.06
#            Botafogo     11      4.47          95.53      4.47          95.53
#           São Paulo     11      4.47         100.00      4.47         100.00
#                <NA>      0                               0.00         100.00
#               Total    246    100.00         100.00    100.00         100.00

# verificando a quantidade de jogos por time como visitante
freq(jogos_realizados$time_visitante, order = "Freq")

#                       Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# ------------------- ------ --------- -------------- --------- --------------
#         Atlético-MG     13      5.28           5.28      5.28           5.28
#            Botafogo     13      5.28          10.57      5.28          10.57
#            Coritiba     13      5.28          15.85      5.28          15.85
#            Flamengo     13      5.28          21.14      5.28          21.14
#           Palmeiras     13      5.28          26.42      5.28          26.42
#                Remo     13      5.28          31.71      5.28          31.71
#           São Paulo     13      5.28          36.99      5.28          36.99
#             Vitória     13      5.28          42.28      5.28          42.28
#        Athletico-PR     12      4.88          47.15      4.88          47.15
#               Bahia     12      4.88          52.03      4.88          52.03
#          Bragantino     12      4.88          56.91      4.88          56.91
#         Chapecoense     12      4.88          61.79      4.88          61.79
#         Corinthians     12      4.88          66.67      4.88          66.67
#            Cruzeiro     12      4.88          71.54      4.88          71.54
#          Fluminense     12      4.88          76.42      4.88          76.42
#              Grêmio     12      4.88          81.30      4.88          81.30
#       Internacional     12      4.88          86.18      4.88          86.18
#            Mirassol     12      4.88          91.06      4.88          91.06
#              Santos     11      4.47          95.53      4.47          95.53
#               Vasco     11      4.47         100.00      4.47         100.00
#                <NA>      0                               0.00         100.00
#               Total    246    100.00         100.00    100.00         100.00

# vemos que os times não tem a amesma quantidade de partidas até o momento

#===============================================================================
# FUNÇÃO DE CLASSIFICAÇÃO
#===============================================================================

funcao_classificacao <- function(jogos) {
  mandante <- jogos %>%
    transmute(time = time_mandante,
              pontos = case_when(
                gols_mandante > gols_visitante ~ 3,
                gols_mandante == gols_visitante ~ 1,
                TRUE ~ 0),
              vitorias = case_when(
                gols_mandante > gols_visitante ~ 1,
                TRUE ~ 0),
              gols_marcados = gols_mandante,
              gols_sofridos = gols_visitante)
  visitante <- jogos %>%
    transmute(time = time_visitante,
              pontos = case_when(
                gols_visitante > gols_mandante ~ 3,
                gols_visitante == gols_mandante ~ 1,
                TRUE ~ 0),
              vitorias = case_when(
                gols_visitante > gols_mandante ~ 1,
                TRUE ~ 0),
              gols_marcados = gols_visitante,
              gols_sofridos = gols_mandante)
  bind_rows(mandante, visitante) %>%
    group_by(time) %>%
    summarise(
      pontos = sum(pontos),
      vitorias = sum(vitorias),
      saldo_gols = sum(gols_marcados) - sum(gols_sofridos),
      gols_marcados = sum(gols_marcados),
      gols_sofridos = sum(gols_sofridos),
      .groups = "drop"
    ) %>%
    arrange(desc(pontos), desc(vitorias), desc(saldo_gols), desc(gols_marcados)) %>%
    mutate(posicao = row_number())
}

funcao_classificacao(jogos_realizados) # teste para verificar como a função retorna

#===============================================================================
# SIMULAÇÃO
#===============================================================================

# para ter maior eficiência, vamos fazer os cálculos fora da função de simulação
jogos_futuro_par <- jogos_futuro %>%
  left_join(parametros_times %>% select(time, theta_m, phi_m), by = c("time_mandante" = "time")) %>%
  left_join(parametros_times %>% select(time, theta_v, phi_v), by = c("time_visitante" = "time")) %>%
  mutate(lambda_m = (theta_m + phi_v) / 2,
         lambda_v = (theta_v + phi_m) / 2)

# monte carlo ----

N <- 10000
n_times <- nrow(parametros_times)
n_fut <- nrow(jogos_futuro_par)

campeao <- character(N)
rebaixados <- vector("list", N)
pontos_campeao <- numeric(N)
houve_desempate <- logical(N)
# matriz para guardar pontos de cada time em cada simulação (útil p/ perguntas 5 e 6)
pontos_por_time <- matrix(NA_real_, nrow = N, ncol = n_times,
                          dimnames = list(NULL, parametros_times$time))

set.seed(13092026)
# gerando da poison fora do loop para maior eficiência
gols_m_mat <- matrix(rpois(n_fut * N, jogos_futuro_par$lambda_m), nrow = n_fut, ncol = N)
gols_v_mat <- matrix(rpois(n_fut * N, jogos_futuro_par$lambda_v), nrow = n_fut, ncol = N)

for (i in 1:N) {
  gols_simulados <- jogos_futuro_par %>%
    mutate(gols_mandante = gols_m_mat[, i],
           gols_visitante = gols_v_mat[, i]) %>%
    select(rodada, time_mandante, gols_mandante, time_visitante, gols_visitante)
  todos_jogos <- bind_rows(jogos_realizados, gols_simulados)
  classificacao <- funcao_classificacao(todos_jogos)
  campeao[i] <- classificacao$time[1]
  rebaixados[[i]] <- tail(classificacao$time, 4)
  pontos_campeao[i] <- classificacao$pontos[1]
  houve_desempate[i] <- classificacao$pontos[1] == classificacao$pontos[2]
  pontos_por_time[i, classificacao$time] <- classificacao$pontos
}

#===============================================================================
# RESULTADOS
#===============================================================================

# 1. probabilidade de cada time ser campeão ----
sort(prop.table(table(campeao)), decreasing = TRUE)

# campeao
#     Flamengo    Palmeiras Athletico-PR   Fluminense  Atlético-MG        Bahia     Cruzeiro 
#       0.5398       0.4352       0.0217       0.0028       0.0002       0.0002       0.0001 

# 2. probabilidade de cada time ser rebaixado ----
todos_times <- parametros_times$time
prob_rebaixamento <- sapply(todos_times, function(t) mean(sapply(rebaixados, function(r) t %in% r))) # saber se o time t foi rebaixado na simulação i  e calcular a proporção de TRUEs
sort(prob_rebaixamento, decreasing = TRUE)

  # Chapecoense          Remo      Mirassol         Vasco Internacional        Grêmio        Santos 
  #      0.9977        0.7626        0.5740        0.5609        0.5006        0.1912        0.1535 
  #     Vitória      Botafogo   Corinthians     São Paulo    Bragantino      Coritiba   Atlético-MG 
  #      0.1495        0.0411        0.0310        0.0301        0.0035        0.0021        0.0011 
  #       Bahia      Cruzeiro  Athletico-PR      Flamengo    Fluminense     Palmeiras 
  #      0.0006        0.0005        0.0000        0.0000        0.0000        0.0000 

# 3. probabilidade de ser decidido no desempate (campeão) ----
mean(houve_desempate) # 0.0585

# 4. valor esperado de pontos do campeão ----
mean(pontos_campeao) # 77.0353

# 5. pontos para uma equipe ter pelo menos 90% de chance de ser campeão ----
foi_campeao <- outer(1:N, todos_times, function(i, t) campeao[i] == t) # 10000x20: marca em cada linha em foi o campeão da simulação i
pontos_vec <- as.vector(pontos_por_time)
campeao_vec <- as.vector(foi_campeao)
tapply(campeao_vec, pontos_vec, mean) # P(campeão | pontos = p), para cada p observado
# 80 (91.6181%)

# 6. pontos necessários para que um time tenho pelo menos 95% de chance de não ser rebaixado ----


