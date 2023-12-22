## Analise de resultados Brasileirao 2003/2022 e visualizacao com mapas ##

## Instalar e Carregar pacotes ##
install.packages("ggplot2")
install.packages("data.table")
install.packages("dplyr")
install.packages("tidyr")
install.packages("sf")
install.packages("viridis")

require("ggplot2")
require("data.table")
require("dplyr")
require("tidyr")
require("sf")
require("viridis")

### Carregar os Dados ###
dados <- fread('/cloud/project/campeonato-brasileiro-full.csv')

dados.golsmandante = dados %>%
  separate(data, into = c("dia", "mes", "ano")) %>%
  mutate(ano_estado = paste(ano, mandante_Estado)) %>%
  group_by(ano_estado) %>%
  summarise(gols_mandante = sum(mandante_Placar)) %>%
  ungroup()

dados.golsvisitante = dados %>%
  separate(data, into = c("dia", "mes", "ano")) %>%
  mutate(ano_estado = paste(ano, visitante_Estado)) %>%
  group_by(ano_estado) %>%
  summarise(gols_visitante = sum(visitante_Placar)) %>%
  ungroup()

dados.golsmandante$ano_estado == dados.golsvisitante$ano_estado

## Criar um dataframe pra receber o saldos de gols ##
df.gols = data.frame(
  Estado = dados.golsmandante$ano_estado,
  gols = c(dados.golsvisitante$gols_visitante + dados.golsmandante$gols_mandante)
)

## Separando ano do estado ##
df.gols = separate(df.gols, Estado, into = c('Ano', 'Estado'))

## Carregar dados de shape do mapa ##
shx = st_read('/cloud/project/UFEBRASIL.shx')
shp = st_read('/cloud/project/UFEBRASIL.shp')

shp$NM_ESTADO

## Vetor de Siglas ##
siglas_estados <- c(
  "RO", "AC", "AM", "RR",
  "PA", "AP", "TO", "MA",
  "PI", "CE", "RN", "PB",
  "PE", "AL", "SE", "BA",
  "MG", "ES", "RJ", "SP",
  "PR", "SC", "RS", "MS",
  "MT", "GO", "DF"
)

## Coluna de Siglas no shp ##
shp$Estado = siglas_estados

## Fazer o Merge do shape com o meu df.goals ##
gols.shp = merge(shp, df.gols, by = 'Estado')

## Montar Mapas - ggplot2 ##
grafico = ggplot() +
  geom_sf(data = shp, col = 'black', size = 0.5, fill = 'beige') +
  geom_sf(data = gols.shp, col = 'black', size = 0.5, aes(fill = gols)) +
  facet_wrap(~Ano) +
  scale_fill_viridis(discrete = F, alpha = 0.7) +
  labs(
    title = 'Gols do Brasileirão, dados de 2003-2022',
    size = 25
  ) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 30, hjust = 0.5),
    strip.text = element_text(size = 25),
    legend.key.size = unit(2, 'cm'),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25, vjust = 0.75)
  )

## Grafico de gols por ano ##
df.gols.agrupado = df.gols %>%
  group_by(Ano)

df.gols.soma = df.gols.agrupado %>%
  summarise(
    soma = sum(gols)
  )

graficobarra = ggplot(df.gols.soma, aes(x = soma, y = Ano)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Gols do Brasileirão por Ano",
    x = "Gols",
    y = "Ano"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 30, hjust = 0.5),
    axis.title = element_text(size = 25)
  ) +
  geom_text(aes(label = soma), hjust = 1.15, color = "white")

## Grafico de gols por Estado ##
df.gols.agrupado = df.gols %>%
  group_by(Estado)

df.gols.soma = df.gols.agrupado %>%
  summarise(
    soma = sum(gols)
  )

graficobarra = ggplot(df.gols.soma, aes(x = soma, y = Estado)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Gols do Brasileirão por Estado",
    x = "Gols",
    y = "Estado"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 30, hjust = 0.5),
    axis.title = element_text(size = 25)
  ) +
  geom_text(aes(label = soma), hjust = -0.15, color = "black")



