library(tidyverse)

dados <- read_csv("https://raw.githubusercontent.com/p253596/lab1-me524/refs/heads/main/brasileirao_2026.csv")

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
