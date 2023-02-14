# 1) Análise exploratória dos dados (Exploratory Data Analysis - EDA)




## Pacotes necessários

install.packages("ggExtra")
install.packages("gridExtra")
install.packages("ggthemes")
install.packages("viridis")

library(plotly) # tornar gráficos do ggplot interativos
library(tidyverse) # ferramentas globais de análise e visualização de dados
library(knitr) # exibir tabelas no "plots"
library(kableExtra) # customizar tabelas no knitr
library(ggExtra) # adicionar plotagens marginais ao ggplot
library(gridExtra) # plotar vários gráficos juntos
library(ggthemes) # temas extras para o ggplot
library(viridis) # cores para daltonismo
library(scales)



## Carregamento da Base de Dados

# Base com as ações e métricas
base_geral <- read.csv(file = "statusinvest-busca-avancada.csv",
                     header = TRUE, # informa que a primeira linha contém o nome das variáveis
                     sep = ";", # informa que as colunas são separadas por ";"
                     dec = ",") # informa que o separador de decimais é ","

# Composição do ibovespa
tickers_ibov <- read.csv(file = "IBOVDia_05-07-22.csv",
                         sep = ";",
                         dec = ",")

class(base_geral)
class(tickers_ibov)

kable(base_geral) %>% 
  kable_styling(bootstrap_options = "striped",
                font_size = 20)




## Exploração Inicial dos Dados

base_geral %>% head()
base_geral %>% tail()

str(base_geral)
# Há uma variável "character" e as demais são numéricas contínuas. Não há necessidade de
# se alterar o tipo de nenhuma variável

view(base_geral %>% count(TICKER))
# A contagem de todas as observações da variável TICKER é igual a 1. Significa que não
# há linhas com ações repetidas.




## Estatísticas Descritivas
base_geral %>% summary()
base_geral %>% summary() %>% view()
# A distribuição em quartis mostra algumas variáveis com valores mínimos e máximos muito
# distantes dentro da dimensão das variáveis. 




## Primeira Limpeza dos Dados

# Selecionando as ações do índice Ibovespa. Neste trabalho, serão análisadas apenas as ações
# que compõem o índice Ibovespa (composição de 05/07/2022)
tickers_ibov <- tickers_ibov %>% rename("TICKER" = 1,
                                        "qtd_teorica" = 4,
                                        "particip_ibov" = 5,
                                        "nome_empresa" = 2)

tickers_ibov_uso <- tickers_ibov %>% select(TICKER,
                                            qtd_teorica,
                                            particip_ibov,
                                            nome_empresa)

base_ibov <- inner_join(base_geral, tickers_ibov_uso, by = "TICKER") 

# Renomeando algumas variáveis
base_ibov <- base_ibov %>% rename("M.BRUTA" = "MARGEM.BRUTA",
                                  "M.EBIT" = "MARGEM.EBIT",
                                  "M.LIQUIDA" = "MARG..LIQUIDA",
                                  "DIV.LIQUIDA.EBIT" = "DIVIDA.LIQUIDA...EBIT",
                                  "valor_mercado" = 30
                                  )




## Visualização de Dados

# DY - Dividend Yield 
hist(base_geral$DY)
hist(base_ibov$DY)

boxplot(base_geral$DY,
        horizontal = T)
boxplot(base_ibov$DY, 
        horizontal = T)
# Nota-se que as observações para esta variável estão menos dispersas apenas com as ações do 
# Ibovespa. Como o trabalho abordará apenas este índice, seguiremos utilizando a base_ibov.

mean(base_ibov$DY, na.rm = T)

ggplotly(
base_ibov %>% 
  ggplot(aes(x = "", y = DY)) +
  geom_boxplot(fill = "#2ecc71") +
  geom_hline(yintercept = 6.558, color = "brown") + # média
  coord_flip() +
  ylab("Dividen Yield (%)") +
  xlab("") +
  theme_stata())


  
# não interativo (adcionar detalhes de labels [função stat_summary] https://appsilon.com/ggplot2-boxplots/)
base_ibov %>% 
  ggplot() +
  geom_boxplot(aes(y = DY),
               fill = "lightblue",
               outlier.colour = "red",
               outlier.shape = 15,
               outlier.size = 1.6) +
  geom_hline(yintercept = mean(base_ibov$DY, na.rm = T),
             color = "purple") +
  coord_flip() +
  labs(title = "Distribuição do Indicador DY",
       y = "Dividend Yield (%)",
       x = "") +
  theme_classic(base_size = 16) +
  theme(panel.background = element_rect(fill = "gray97"))





# Os outliers não estão muito distantes dentro da dimensionalidade dos dados. 
# Os outliers extremos, 33.94, 31.84 e 29.42 são reais - são os dividendos pagos pelas ações
# BRAP4, PETR4 e PETR3, respectivamente.

base_ibov$DY %>% quantile(c(0.25, 0.5, 0.75), na.rm = T)

base_ibov %>% 
  ggplot(aes(x = DY)) +
  geom_density() + 
  geom_vline(xintercept = 1.7675, color = "green") + 
  geom_vline(xintercept = 4.08, color = "blue") + 
  geom_vline(xintercept = 7.9475, color = "brown") + 
  geom_vline(xintercept = mean(base_ibov$DY, na.rm = T), color = "red") +
  theme_bw()

base_ibov %>% 
  ggplot(aes(x = DY)) +
  geom_histogram(fill = "lightblue") +
  geom_vline(xintercept = 1.7675, color = "darkgreen") + 
  geom_vline(xintercept = 4.08, color = "blue") + 
  geom_vline(xintercept = 7.9475, color = "brown") + 
  geom_vline(xintercept = mean(base_ibov$DY, na.rm = T), color = "red") +
  theme_bw()

# A média dos dividendos está inserida entre a mediana e o terceiro quartil, pois os dados não 
# estão muito dispersos. Há algumas empresas que pagaram altos dividendos, próximo aos 30% ao
# ano, mas estes outliers não foram capazes de trazer a média para fora dos quartis.  


# Podemos verificar se os indicadores de eficiência, como a margem líquida, e indicadores de
# rentabilidade, como ROE e ROA, possuem relação com a distribuição de dividendos. 


# DY x Margem Líquida (razão entre lucro líquido e receita líquida após dedução de impostos)
# {inserir a correlação de pearson nesse gráfico e nos outros abaixo}
base_ibov %>% 
      ggplot(aes(x = M.LIQUIDA, y = DY)) +
      geom_point(color = "black") +
      geom_smooth(formula = y ~ x, method = "lm",
                  se = F,
                  color = "brown",
                  size = 1.2) +
      labs(title = "Dividend Yield x Margem Líquida",
           y = "Dividend Yield (%)",
           x = "Margem Líquida") +
      theme_classic(base_size = 16) +
      theme(panel.background = element_rect(fill = "grey97"))

# A margem líquida demonstra se uma empresa tem bom retorno a partir do custo de produzir seu
# produto ou prestar seu serviço. Apesar de a correlação não ser muita alta, há uma tendência
# positiva entre as variáveis, o que pode significar que a empresa tende a pagar melhores divi-
# dendo com melhores resultados. O investidor também pode olhar como um fator de segurança e 
# evitar empresas com resultados ruins distribuindo altos dividendos.


# DY x ROE (Return on Equity)
base_ibov %>% 
  ggplot(aes(x = ROE, y = DY)) +
  geom_point(color = "black") +
  geom_smooth(formula = y ~ x, method = "lm",
              se = F,
              color = "brown",
              size = 1.2) +
  labs(title = "Dividend Yield x ROE",
       y = "Dividend Yield (%)",
       x = "ROE") +
  theme_classic(base_size = 16) +
  theme(panel.background = element_rect(fill = "grey97"))

# O ROE é o retorno sobre o patrimônio líquido e demonstra a eficiência da empresa em gerar
# lucro a partir de seus recursos. Há também uma tendência positiva entre as variáveis. O in-
# vestidor pode utilizar esta relação para não apenas buscar empresas com potencial de paga-
# mento de bons dividendos, mas também buscar empresas que fazem a distribuição de um dividen-
# dentro de suas capacidades financeiras, uma vez que empresas com ROEs mais elevados obtiveram
# maior lucro em relação ao seu patrimônio líquido.

# DY x ROA (Return os Assets)
base_ibov %>% 
  ggplot(aes(x = ROA, y = DY)) +
  geom_point(color = "black") +
  geom_smooth(formula = y ~ x, method = "lm",
              se = F,
              color = "brown",
              size = 1.2) +
  labs(title = "Dividend Yield x ROA",
       y = "Dividend Yield (%)",
       x = "ROA") +
  theme_classic(base_size = 16) +
  theme(panel.background = element_rect(fill = "grey97"))

# O ROA é o retorno sobre os ativos. Da mesma forma que o ROE, apresenta tendência positiva com
# a distribuição de dividendos e podem ser avaliados assim como o exemplo acima.


# P/L - Preço/Lucro
ggplotly(
base_ibov %>% 
  ggplot(aes(x = "", y = P.L)) +
  geom_boxplot(fill = "blue") +
  coord_flip() + 
  theme_stata()
)

# não interativo
base_ibov %>% 
  ggplot(aes(y = P.L)) +
  geom_boxplot(fill = "lightblue",
               outlier.colour = "red",
               outlier.shape = 15,
               outlier.size = 1.6) +
  labs(title = "Distribuição do Indicador P/L",
       y = "P/L - Preço sobre Lucro") +
  coord_flip() + 
  theme_classic(base_size = 16) +
  theme(panel.background = element_rect(fill = "grey97"))

# P/L é a razão entre o preço do papel e o lucro obtido pela empresa por cada ação individual-
# mente. Indica quanto o mercado está dispoto a pagar pela ação. Um P/L muito alto pode indicar
# uma ação acima do seu preço "justo" e um P/L baixo pode demonstrar pouca confiança do mercado
# no papel.

# Existe um outlier no valor de -1397,74. É o P/L da empresa LWSA3. O P/L é obtido dividindo-se
# o preço do papel pelo seu respectivo lucro. O outlier indica que a empresa teve um grande pre-
# juízo por ação.

base_ibov$P.L %>% quantile(c(0.25, 0.5, 0.75))
base_ibov$P.L %>% mean() 

base_ibov %>% 
  filter(between(P.L, -150, 150)) %>% 
  ggplot(aes(x = P.L)) +
  geom_density(size = 0.75) +
  geom_vline(xintercept = 3.6875, color = "blue", size = 0.8) + # Primeiro quartil
  geom_vline(xintercept = 8.56, color = "orange", size = 0.8) + # Mediana
  geom_vline(xintercept = 16.4350, color = "limegreen", size = 0.8) + # Terceiro quartil
  geom_vline(xintercept = base_ibov$P.L %>% mean(), color = "red", size = 1) + # Média
  labs(title = "Densidade das Observações do Indicador P/L",
       x = "P/L",
       y = "") +
  theme_classic(base_size = 16) +
  theme(panel.background = element_rect(fill = "grey97"))

# A média, -1.67, é negativa e menor que o primeiro quartil, 3.68. Isto se deve ao outlier men-
# cionado acima exercer forte influência sobre a média. Portanto é incorreto tomá-la de forma
# isolada para caracterizar o índice, afirmando que as ações estariam "baratas". Para ter esta 
# informação, podemos remover este outlier e fazer outra observação.

base_ibov_sem_outlier_pl <- base_ibov %>% filter(TICKER != "LWSA3") # removendo o outlier

base_ibov_sem_outlier_pl %>% 
  ggplot(aes(x = "", y = P.L)) +
  geom_boxplot(fill = "blue") +
  coord_flip() +
  theme_bw()

base_ibov_sem_outlier_pl$P.L %>% quantile(c(0.25, 0.5, 0.75)) 
# As medidas de posição mudaram pouco, pois não sofrem grande influência de outliers

base_ibov_sem_outlier_pl$P.L %>% mean() # a média teve grande alteração
                                          
base_ibov_sem_outlier_pl %>% 
  filter(between(P.L, -150, 150)) %>% 
  ggplot(aes(x = P.L)) +
  geom_density(size = 0.75) +
  geom_vline(xintercept = 3.77, color = "blue", size = 0.8) + # Primeiro quartil
  geom_vline(xintercept = 8.67, color = "orange", size = 0.8) + # Mediana
  geom_vline(xintercept = 16.50, color = "green", size = 0.8) + # Terceiro quartil
  geom_vline(xintercept = 14.01, color = "red", size = 0.8) + # Média
  labs(title = "Densidade das Observações do Indicador P/L",
       x = "P/L",
       y = "") +
  theme_classic(base_size = 16) +
  theme(panel.background = element_rect(fill = "grey97"))

# Com a remoção do outlier a nova média está contida entre a mediana e o terceiro quartil e re-
# presenta com maior veracidade o indicador P/L da ações do Ibovespa. Adotar este valor, 14.01,
# para representar o índice é mais razoável, uma vez que o outlier removido pode ser um caso 
# atípico com o papel LWSA3.

  

# P/VP
# Assim como o P/L, o P/VP é uma forma de se verificar o preço relativo de um papel, mas tomando
# em consideração o valor patrimonial da empresa desta vez. O valor patrimonial é o valor real
# de uma empresa e é obtido calculando os bens e os direitos da companhia, retirados os valores
# de despesa. 

base_ibov$P.VP %>% quantile(c(0.25, 0.5, 0.75))

base_ibov%>% 
  ggplot(aes(x = P.VP)) + 
  geom_density() +
  geom_vline(xintercept = 1.0975, color = "blue") + 
  geom_vline(xintercept = 1.6050, color = "orange") +  
  geom_vline(xintercept = 2.5325, color = "green") + 
  geom_vline(xintercept = mean(base_ibov$P.VP), color = "red") +
  xlab("P/VP") +
  ylab("Densidade de Observações") +
  theme_stata()

ggplotly(
base_ibov %>% 
  ggplot(aes(x = "", y = P.VP)) + 
  geom_boxplot(fill = "lightblue") +
  geom_hline(yintercept = mean(base_ibov$P.VP), color = "red") +
  coord_flip() +
  theme_bw()
)

# não interativo
base_ibov %>% 
  ggplot(aes(x = "",
             y = P.VP)) + 
  geom_boxplot(fill = "lightblue",
               outlier.colour = "red",
               outlier.shape = 15,
               outlier.size = 1.6) +
  geom_hline(yintercept = mean(base_ibov$P.VP), 
             color = "red2",
             size = 0.8) +
  coord_flip() +
  labs(title = "Distribuição do Indicador P/VP",
       y = "P/VP",
       x = "") +
  theme_classic(base_size = 16) +
  theme(panel.background = element_rect(fill = "grey97"))


# O P/VP é a divisão do valor do papel pelo valor patrimonial correspondente a cada ação. Se es-
# tá abaixo de 1, a ação está sendo negociada abaixo do seu que ela realmente vale - ação barata.
# Quando acima de 1, a empresa significa que o papel é negociado acima do valor real da empresa.

# Analisando a distribuição dos valores, percebe-se que (contar quantas observações entre quartis)
# a maior parte das ações está entre um valor de 0 e 4, e outliers não muito discrepantes dentro da
# dimensionalidade dos dados. 

# Os outliers negativos, -2.77 e -1.51, são das empresas GOL e Azul, ambas companhias aéreas. Es-
# tão sendo negociadas abaixo de seu valor real. Um dos fatores que explica a baixa oferta do mer-
# cado no papel, pode ser a instabilidade durante a pandemia e o período de recuperação. Os maio-
# outliers positivos, 11.47 e 8.98, são as empresas Minerva Foods e WEG, ambas com forte presença
# em importação de seus produtos.

# A média está contida entre a mediana e o terceiro quartil. É pouco afetada pela presença de pou-
# cos outliers e, se adotada para classificar o índice, pode-se dizer as ações estão sendo negocia-
# das acima de seu valor real, em média. Além disso, se comparada dentro do gráfico de densidade
# com a média do P/L, estão posicionadas de forma semelhante, e reforçam a utilidade dos dois indi-
# cadores para se avaliar o preço relativo de um papel.

g1 <- base_ibov_sem_outlier_pl %>% 
  filter(between(P.L, -150, 150)) %>% 
  ggplot(aes(x = P.L)) +
  geom_density() +
  geom_vline(xintercept = 3.77, color = "blue") + # Primeiro quartil
  geom_vline(xintercept = 8.67, color = "orange") + # Mediana
  geom_vline(xintercept = 16.50, color = "green") + # Terceiro quartil
  geom_vline(xintercept = 14.01, color = "red") + # Média
  xlab("P/L") +
  ylab("") +
  theme_classic(base_size = 12) +
  theme(panel.background = element_rect(fill = "grey97"))

g2 <- base_ibov%>% 
  ggplot(aes(x = P.VP)) + 
  geom_density() +
  geom_vline(xintercept = 1.0975, color = "blue") + 
  geom_vline(xintercept = 1.6050, color = "orange") +  
  geom_vline(xintercept = 2.5325, color = "green") + 
  geom_vline(xintercept = mean(base_ibov$P.VP), color = "red") +
  xlab("P/VP") +
  ylab("") +
  theme_classic(base_size = 12) +
  theme(panel.background = element_rect(fill = "grey97"))

grid.arrange(g1,
             g2,
             top = "Comparação entre os Indicadores P/L e P/VP")

# Valor de mercado (primeiras 20 observações)
base_ibov[1:20,] %>% 
  ggplot(aes(x = valor_mercado,
             y = reorder(TICKER, +valor_mercado))) +
  geom_col(aes(fill = valor_mercado)) +
  scale_fill_gradient2(low = "lightblue",
                       high = "darkblue",
                       mid = "lightblue") +
  labs(x = "Valor de Mercado",
       y = "",
       title = "Valor de Merc. das Empr. do Ibovespa") + 
  theme_bw(base_size = 16) +
  theme(panel.background = element_rect(fill = "grey97")) +
  scale_x_continuous(labels = label_number(scale = 1e-9,
                                           prefix = "R$",
                                           suffix = "b",
                                           accuracy = 1))

# Há uma grande variação no valor de mercado das companhias listadas no índice. Podemos comparar
# O valor de mercado com algumas variáveis de resultado da companhia.


# Valor de mercado x DY
ggplotly(
base_ibov %>% 
  ggplot(aes(x = valor_mercado,
             y = DY)) +
  geom_point() +
  geom_smooth(formula = y ~ x, 
              method = "lm",
              se = F) +
  theme_stata()
)

# não interativo
base_ibov %>% 
  ggplot(aes(x = valor_mercado,
             y = DY)) +
  geom_point() +
  geom_smooth(formula = y ~ x, 
              method = "lm",
              se = F,
              color = "brown") +
  labs(title = "Valor de Mercado x DY",
       x = "Valor de Mercado",
       y = "DY") +
  theme_classic(base_size = 16) +
  theme(panel.background = element_rect(fill = "grey97")) +
  scale_x_continuous(labels = label_number(scale = 1e-9,
                                           prefix = "R$",
                                           suffix = "b",
                                           accuracy = 1))
  
# Apesar da tendência positiva, os dados estão muito dispersos. Há empresas pagando altos dividen-
# mesmo com valores de mercado mais baixos, e o oposto também ocorre. 

base_ibov %>% filter(DY == 2.44) %>% select(TICKER, 
                                            nome_empresa,
                                            valor_mercado,
                                            DY)

# ITUB4 (Itaú) com alto de valor de mercado e baixos dividendos.

base_ibov %>% filter(DY == 21.45) %>% select(TICKER, 
                                             nome_empresa,
                                             valor_mercado,
                                             DY)

# MRFG3 (Marfrig) com baixo valor de mercado e altos dividendos.


# Valor de mercado x Margem líquida
ggplotly(
  base_ibov %>% 
    ggplot(aes(x = valor_mercado,
               y = M.LIQUIDA)) +
    geom_point() +
    geom_smooth(formula = y ~ x, 
                method = "lm",
                se = F) +
    theme_stata()
)

# não interativo
base_ibov %>% 
  ggplot(aes(x = valor_mercado,
             y = M.LIQUIDA)) +
  geom_point() +
  geom_smooth(formula = y ~ x, 
              method = "lm",
              se = F) +
  theme_stata()

# Observa-se pouca correlação entre as variáveis.

# Valor de mercado x  ROE
ggplotly(
  base_ibov %>% 
    ggplot(aes(x = valor_mercado,
               y = ROE)) +
    geom_point() +
    geom_smooth(formula = y ~ x, 
                method = "lm",
                se = F) +
    theme_stata()
)

# não interativo
base_ibov %>% 
  ggplot(aes(x = valor_mercado,
             y = ROE)) +
  geom_point() +
  geom_smooth(formula = y ~ x, 
              method = "lm",
              se = F) +
  theme_stata()

grid.arrange(base_ibov %>% 
               ggplot(aes(x = valor_mercado,
                          y = M.LIQUIDA)) +
               geom_point() +
               geom_smooth(formula = y ~ x, 
                           method = "lm",
                           se = F,
                           color = "brown") +
               xlab("Valor de Mercado") +
               ylab("Margem Líquida") +
               theme_classic(base_size = 12) +
               theme(panel.background = element_rect(fill = "grey97")) +
               scale_x_continuous(labels = label_number(scale = 1e-9,
                                                        prefix = "R$",
                                                        suffix = "b",
                                                        accuracy = 1)),
             base_ibov %>% 
               ggplot(aes(x = valor_mercado,
                          y = ROE)) +
               geom_point() +
               geom_smooth(formula = y ~ x, 
                           method = "lm",
                           se = F,
                           color = "brown") +
               xlab("Valor de Mercado") +
               ylab("ROE") +
               theme_classic(base_size = 12) +
               theme(panel.background = element_rect(fill = "grey97")) + 
               scale_x_continuous(labels = label_number(scale = 1e-9,
                                                        prefix = "R$",
                                                        suffix = "b",
                                                        accuracy = 1)),
             top = "Valor de Mercado x Margem Líquida e ROE")

# Novamente uma baixa correlação.

# Pode-se observar que tomar o valor da companhia de forma isolada para avaliá-la pode levar ao in-
# vestidor a conclusões precipitadas, uma vez que o resultado da empresa não está diretamente liga-
# ao seu valor. 




## Missing Values

is.na(base_ibov)
sum(is.na(base_ibov)) # há 95 missing values

# Verificando as Linhas que Contém NAs
linhas_com_NAs <- base_ibov[rowSums(is.na(base_ibov)) > 0, ] 

kable(linhas_com_NAs) %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "bordered"),
                font_size = 12)

# Como podemos ver, em 45% das linhas há missing values. No caso da variável DY, alvo
# deste projeto, faz sentido, pois a empresa não pagou dividendos. Em outras variáveis,
# são dados que a empresa não informou ou não estão disponíveis. Portando, não se pode
# remover as linhas com missing values, pois se perderiam muitas observações da base de
# dados. Além disso, substituí-los pela média da variável não aparenta ser justo, pois 
# estaria criando informações que não condizem com o mundo real e com o objetivo do tra-
# balho. O mais correto me parece ser eliminá-los.

linhas_sem_NAs <- na.omit(base_ibov)
# Não faz sentido limitar o trabalho apenas a estas ações.




## Limpeza da base de dados (Data Cleaning)

# Podemos começar removendo algumas variáveis que não serão utilizadas no trabalho. Nesta primeira
# limpeza, serão removidas algumas variáveis que não são citadas em trabalhos acadêmicos como rele-
# vantes a uma análise fundamentalistas. Ao decorrer do trabalho, essas variáveis podem ser reapro-
# veitadas, caso necessário.

# Nas literaturas de referência, os principais índices utilizados são: DY, P/L, P/VP, LPA, Dividend
# Payout (DP), Valor de mercado, Lucro líquido, ROE, PVPA, VPA.

base_ibov_data_clean <- base_ibov %>% select(1, 3:5, 7:11, 18:20, 27, 28, 30, 32, 33)

# Na variável DY, os missing values são empresas que não pagaram dividendos no período, portanto
# podem ser substituídos por zero.

base_ibov_data_clean %>% filter(is.na(DY)) # verificando os missing values na variável DY
base_ibov_data_clean$DY[is.na(base_ibov_data_clean$DY)] <- 0 # substituindo-os por 0
is.na(base_ibov_data_clean$DY) # retorno FALSE
base_ibov_data_clean %>% filter(DY == 0)

base_ibov_data_clean[80:90,1:11] %>% 
  kable(align = "c",
        caption = "Base de Dados Tratada") %>% 
  kable_classic(html_font = "courier new") %>% 
  kable_styling(bootstrap_options = "striped",
                font_size = 14)

# Devido aos outliers, os missing values poderiam ser substituídos pela mediana das variáveis, pois
# a média estão muito deslocadas em alguns casos, como na variável P/L.


###################################################################################################

# 2) Análise com aprendizado não supervisionado - unsupervised machine learning

# Os dados serão preparados para aplicação da técnica de agrupamentos/clustering, a fim de que o 
# resultado seja utilizado para explorar os padrões dos papéis agrupados.




## Pacotes necessários

install.packages("ape")

library(cluster) # algoritmo de cluster
library(dendextend) # junta dendogramas em um objeto para comparação
library(factoextra) # algoritmo de cluster e visualizacao
library(fpc) # algoritmo de cluster e visualizacao
library(ape) # funções para dendrogramas mais complexos com a função plot()




## Preparação dos dados

base_cluster <- base_ibov_data_clean # separando um dataframe apenas para o cluster
base_cluster <- base_cluster %>% column_to_rownames(var = "TICKER") # nomeando as obs com os tickers
base_cluster <- base_cluster %>%  select(1:13) # retirando as variáveis valor de mercado, particip ibov e nome da empresa

# Como a dimensão dos dados não é a mesma, será necessário padronizá-los (média igual a 0 e desvio padrão igual a 1).
base_cluster_padronizada <- scale(base_cluster)

# Vizualização da base padronizada {essa vizualização será inclusa no artigo - pintar os NAs de vermelho ou laranja}
base_cluster_padronizada %>% kable(align = "c",
                                   caption = "Base de Dados Padronizada"
                                   ) %>% 
                             kable_classic(full_width = F,
                               html_font = "Courier New"
                             ) %>% 
                              kable_styling(
                                bootstrap_options = c("bordered", "striped"),
                                font_size = 14)




## Clustering hierárquico

# Começaremos com um clustering hierárquico, uma vez que a quantidade de agrupamentos ainda não está definida para se 
# utilizar de clustering não hierárquico.

# Calculando a matriz de distâncias
matriz_dist <- base_cluster_padronizada %>% dist(method = "euclidean")

# Aplicando o algoritmo de clustering
cluster_base_ibov <- hclust(matriz_dist,
                            method = "single")
dendograma_cluster_ibov <- as.dendrogram(cluster_base_ibov)




## Análise dos agrupamentos formados

# Visualizando o dendograma - agrupamentos formados
# A função set() permite modificar atributos do dendograma e customizá-lo
# {deixar esse dendograma mais bonito para o artigo}
par(cex = 0.5) # tamanho da fonte definido antes da função plot
dendograma_cluster_ibov %>% 
                      set("nodes_pch", 15) %>%
                      set("nodes_cex", 0.4) %>% 
                      set("nodes_col", "skyblue") %>% 
                      plot(main = "Cluster de Ações do Ibovespa",
                           sub = "",
                           ylab = "Clusters",
                           xlab = "")


## Escolha da quantidade de clusters

# Como o clustering hierárquico agrupa até todos as observações foramarem um único agrupamento, a
# escolha do número de clusters pode ser difícil. Há uma ténica que auxilia nessa escolha chamada
# de método Elbow. {explicar o método brevemente}

# Método elbow

base_cluster_padronizada %>% fviz_nbclust(FUN = hcut,
                                          method = "wss",
                                          k.max = 20,
                                          linecolor = "brown") +
                              geom_vline(xintercept = 5,
                                         linetype = "dashed",
                                         size = 1) +
                              labs(title = "Quantidade Adequada de Agrupamentos",
                                   x = "Número de Agrupamentos",
                                   y = "Soma dos Quadrados Totais Intragrupos") + 
                              theme_classic(base_size = 16) +
                              theme(panel.background = element_rect(fill = "grey97"))
  

# Pela curva, pode-se visualizar uma suavização a partir do quarto cluster. Este número de agru-
# pamentos será escolhido primeiramente para se continuar com a análise, mas poderá ser alterado 
# ao decorrer do trabalho.

# Visualizando o dendograma com os clusters destacados
dendograma_cluster_ibov %>% 
  set("labels_col",
      value = c("blue", "orange", "skyblue", "black"), k = 4) %>% 
  set("branches_k_color",
      value = c("blue", "orange", "skyblue", "black"), k = 4) %>% 
  set("nodes_pch", 15) %>%
  set("nodes_cex", 0.4) %>% 
  set("nodes_col", "grey") %>% 
  plot(main = "Cluster de Ações do Ibovespa",
       ylab = "Clusters",
       xlab = "")

cluster_base_ibov %>% rect.hclust(k = 4)

# 86 observações foram agrupadas em um único cluster. Isso será verificado, mas pode-se observar que
# a causa podem ser os outliers presentes nas outras 4 observações. Após uma primeira análise, será
# criado um dataframe sem estas 4 observações e aplicada a técnica novamente sobre a base de dados.




## Análise dos agrupamentos 

# Será criada uma variável na base de dados para indicar a qual cluster cada observação pertence.

cluster4 <- cutree(cluster_base_ibov, k = 4) # vetor com as observações e os clusters
table(cluster4)
base_cluster4 <- data.frame(cluster4) # transforma o vetor em um dataframe 
base_cluster4

base_ibov_result_cluster4 <- cbind(base_cluster, base_cluster4) # junta os dataframes
base_ibov_result_cluster4 <- base_ibov_result_cluster4 %>% rename("CLUSTER" = 14)

base_ibov_result_cluster4 %>% arrange(CLUSTER) %>% kable(align = "c",
                                   caption = "Base de Dados com Resultado do Agrupamento") %>% 
                              kable_classic(full_width = F,
                                            html_font = "Courier New") %>% 
                              kable_styling(bootstrap_options = c("bordered", "striped"),
                                            font_size = 14)


# Como as observações do cluster 1 serão novamente clusterizadas sem as dos outros 3 clusters, vamos analisar apenas
# os agrupamentos 2, 3 e 4.

# Visuzalizando os clusters 2, 3 e 4
base_ibov_result_cluster4 %>% filter(CLUSTER !=1) %>% arrange(CLUSTER) %>% 
  kable(align = "c",
        caption = "Observações dos Clusters 2, 3 e 4") %>% 
  kable_classic(full_width = F,
                html_font = "Courier New") %>% 
  kable_styling(bootstrap_options = c("bordered", "striped"),
                font_size = 14)


# Podemos utilizar da técnica de clustering não hierárquico e comparar se estas observações também formaram clusters se-
# parados.

# Clustering não hierárquico - kmeans

cluster_base_ibov_kmeans <- kmeans(na.omit(base_cluster_padronizada),
                                   centers = 4) # vamos manter os 4 clusters para comparação

# Visualizando o resultado

fviz_cluster(object = cluster_base_ibov_kmeans,
             data = na.omit(base_cluster_padronizada),
             geom = "text",
             main = "Cluster Ações Ibovespa",
             repel = T)

# Pode-se observar que os papéis MGLU3, LWSA3 e AZUL4 formaram um cluster distante. No método hierárquico estas 3 ações
# também ficaram em clusters distintos da grande massa de observações. Vamos voltar ao resultado do clustering hierárquico
# para analisar os agrupamentos 2, 3 e 4.


# Cluster 2 - MGLU3 e AZUL4
base_ibov_result_cluster4 %>% filter(CLUSTER == 2)

# Nota-se que ambas as ações apresentam algumas semelhanças entre seus indicadores:
# A AZUL4 não distribuiu dividendos e o valor pago pela MGLU3 foi muito baixo. Estes dois setores, Varejo e companhias
# aéreas tem como característica a baixa distribuição de dividendos, uma vez que empresas deste setor optam por reinvestir
# seus lucros para modernização da frota de aeronaves, como a Azul, e o aumento de marketshare, como a Magazine Luiza.
# {citar matérias sobre investimentos da azul e magalu}

# Indicadores M.BRUTA, M.EBIT, P.EBIT semelhantes {explicar cada um}


# Cluster 3 - BRAP4
base_ibov_result_cluster4 %>% filter(CLUSTER == 3)

# Dividendos muito altos


# Cluster 4 - LWSA3
base_ibov_result_cluster4 %>% filter(CLUSTER == 4)

# O papel LWSA3 apresenta um outlier extremo da variável P/L, indício de um resultado ruim da empresa no período. Outros
# indicadores corroboram, como ROE, ROA e ROIC, todos negativos, apresentando retornos ruins da empresa sobre seu patri-
# mônio, ativos e capital investido.






# Reaplicando a clusterização apenas nas ações do cluster 1 original

# Serão removidos da base de dados de clustering as observações que formaram os clusters "outliers", para anarlisar apenas
# as observações que formaram o cluster 1.

# Visualizando as observações que serão analisadas
base_ibov_result_cluster4 %>% filter(CLUSTER == 1) %>%  
  kable(align = "c",
        caption = "Observações do Cluster 1") %>% 
  kable_classic(html_font = "courier new") %>% 
  kable_styling(bootstrap_options = c("striped"),
                font_size = 13)

# Criando uma base para reanálise apenas com estas observações e padronizando os dados
base_cluster_reanalise <- base_ibov_result_cluster4 %>% filter(CLUSTER == 1) %>% 
                                                        select(1:13)
base_cluster_reanalise_padronizada <- base_cluster_reanalise %>% scale()


# Verificando a quantidade adequada de agrupamentos
base_cluster_reanalise_padronizada %>%
  na.omit() %>% 
  fviz_nbclust(FUN = kmeans,
               method = "wss",
               k.max = 20) + # indica 6 clusters
  geom_vline(xintercept = 7,
             linetype = "dashed",
             size = 1) +
  geom_vline(xintercept = 5,
             linetype = "dashed",
             size = 1,
             color = "seagreen") +
  theme_classic(base_size = 16) + 
  theme(panel.background = element_rect(fill = "grey97")) +
  labs(title = "Quantidade Adequada de Agrupamentos",
       x = "Número de Agrupamentos",
       y = "Soma dos Quadrados Totais Intragrupos")


na.omit(base_cluster_reanalise_padronizada) %>% fviz_nbclust(FUN = kmeans,
                                                             method = "silhouette",
                                                             k.max = 20) # indica 4 clusters

# Há duas quantidades indicadas, 4 ou 6. Vamos rodar com as duas quantidades e compará-las.



# Aplicando o clustering não hierárquico

# 4 clusters
set.seed(0)
cluster_base_reanalise4 <- kmeans(na.omit(base_cluster_reanalise_padronizada),
                                  centers = 4)
# 6 clusters
set.seed(0)
cluster_base_reanalise6 <- kmeans(na.omit(base_cluster_reanalise_padronizada),
                                  centers = 6)

# Visualizando e comparando os agrupamentos formados
plot_cluster_kmeans4 <- fviz_cluster(object = cluster_base_reanalise4,
                                     data = na.omit(base_cluster_reanalise_padronizada),
                                     geom = "text",
                                     main = "Cluster Não Hierárquico - 4 agrupamentos",
                                     repel = T) +
                        theme_classic(base_size = 12) +
                        theme(panel.background = element_rect(fill = "grey97")) +
                        labs(x = "",
                             y = "")
                        

plot_cluster_kmeans6 <- fviz_cluster(object = cluster_base_reanalise6,
                                     data = na.omit(base_cluster_reanalise_padronizada),
                                     geom = "text",
                                     main = "Cluster Não Hierárquico - 6 agrupamentos",
                                     repel = T) +
                        theme_classic(base_size = 12) +
                        theme(panel.background = element_rect(fill = "grey97")) +
                        labs(x = "",
                             y = "")

grid.arrange(plot_cluster_kmeans4,
             plot_cluster_kmeans6,
             top = "Comparação entre Resultados")


# Vamos utilizar o agrupamento com 6 clusters devido ao objetivo do trabalho.
grupos_reanalise_kmeans6 <- cluster_base_reanalise6$cluster %>% data.frame() # qual ação pertence a cada cluster


# Criando um dataframe com os clusters acima
base_cluster_reanalise_result_kmeans6 <- inner_join(
  base_cluster_reanalise %>% rownames_to_column(var = "TICKER"),
  grupos_reanalise_kmeans6 %>% rownames_to_column(var = "TICKER")) %>% 
  rename("CLUSTER" = 15)

base_cluster_reanalise_result_kmeans6 %>% 
  arrange(CLUSTER) %>% 
  kable(align = "c",
        caption = "Base de Dados Resultado Cluster Kmeans") %>% 
  kable_classic(html_font = "courier new") %>% 
  kable_styling(bootstrap_options = "striped",
                font_size = 13)


# Análise do resultado do clustering não hierárquico

# Análise descritiva das médias

# O papel HAPV3 formou um cluster sozinho, com 4 e 6 agrupamentos. Vamos comparar as observações com as médias
# dos outros agrupamentos
medias_base_kmeans6 <- base_cluster_reanalise_result_kmeans6 %>% 
  group_by(CLUSTER) %>% 
  summarise(QTD_OBS = n(),
            DY = mean(DY) %>% round(2),
            P.L = mean(P.L) %>% round(2),
            P.VP = mean(P.VP) %>% round(2),
            M.BRUTA = mean(M.BRUTA) %>% round(2),
            M.EBIT = mean(M.EBIT) %>% round(2),
            M.LIQUIDA = mean(M.LIQUIDA) %>% round(2),
            P.EBIT = mean(P.EBIT) %>% round(2),
            EV.EBIT = mean(EV.EBIT) %>% round(2),
            ROE = mean(ROE) %>% round(2),
            ROA = mean(ROA) %>% round(2),
            ROIC = mean(ROIC) %>% round(2),
            VPA = mean(VPA) %>% round(2),
            LPA = mean(LPA) %>% round(2))

medias_base_kmeans6 %>% 
  kable(align = "c",
        caption = "Análise Descritiva das Médias dos Clusters") %>%
  kable_classic(html_font = "courier new") %>% 
  kable_styling(bootstrap_options = "striped",
                font_size = 13)

# Destacando pontos principais (no artigo estará tudo explicado)

# Cluster 1 
# DY bem abaixo dos outros clusters; PL muito acima dos outros clusters; margem ebit e líquida bem abaixo dos
# outros; p ebit e ev ebit muito acima; roe roa e roic muito abaixo; lpa muito abaixo

# Cluster 2
# margem bruta margem ebit e líquida um pouco acima dous outros, mas as demais variáveis estão
# mais próximas de uma média aparente. Podem ser ações mais "normais"

# Cluster 3
# DY alto; pl médio; pvp mais alto de todos; margem bruta ebit e líquida médias; roa roa e roic altos - bom resultado;

# Cluster 4
# DY mais alto de todos; margens bruta ebit e liquida altos; roe roa e roic altos
# vpa e lpa bem altos

# Cluster 5
# DY baixo; PL alto; margem bruta alta e demais baixas; roe roa e roic baixos; vpa alto

# Cluster 6
# DY baixo; pl negativo; lpa baixo


########################################################################################################################

## Refazendo a clusterização com menos variáveis

# primeiro com as ações da base de dados

# selecionando as variáveis que serão utilizadas
base_cluster_reanalise_2 <- base_ibov_result_cluster4 %>% 
                            select(1:4, 6, 9:11, 13)

# padronizando as observações
base_cluster_reanalise_2_padronizada <- base_cluster_reanalise_2 %>% scale()

# Visualizando a base padronizada
base_cluster_reanalise_2_padronizada %>% 
  kable(align = "c",
        caption = "Base de Dados Padronizada") %>% 
  kable_classic(html_font = "courier new") %>% 
  kable_styling(bootstrap_options = "striped",
                font_size = 13)

 
# Conferindo a quantidade adequada de clusters

base_cluster_reanalise_padronizada %>%
  na.omit() %>% 
  fviz_nbclust(FUN = kmeans,
               method = "wss",
               k.max = 20) + # indica 6 clusters
  geom_vline(xintercept = 7,
             linetype = "dashed",
             size = 1) +
  geom_vline(xintercept = 5,
             linetype = "dashed",
             size = 1,
             color = "seagreen") +
  theme_classic(base_size = 16) + 
  theme(panel.background = element_rect(fill = "grey97")) +
  labs(title = "Quantidade Adequada de Agrupamentos",
       x = "Número de Agrupamentos",
       y = "Soma dos Quadrados Totais Intragrupos")

# método wss
base_cluster_reanalise_2_padronizada %>% 
  na.omit() %>% 
  fviz_nbclust(FUN = kmeans,
               method = "wss",
               k.max = 20)

# método silhouette  
base_cluster_reanalise_2_padronizada %>%
  na.omit() %>% 
  fviz_nbclust(FUN = kmeans,
               method = "silhouette",
               k.max = 20)
  
# o método silhouette indica 2 agrupamentos, no wss pode-se observar 5 e 7 agrupamentos possíveis,
# mas vamos fazer com 4 e 6 para comparar resultados.

set.seed(0)
cluster_reanalise_2_k4 <- kmeans(na.omit(base_cluster_reanalise_2_padronizada),
                                 centers = 4)

set.seed(0)
cluster_reanalise_2_k6 <- kmeans(na.omit(base_cluster_reanalise_2_padronizada),
                                 centers = 6)

plot_reanalise_2_k4 <- fviz_cluster(object = cluster_reanalise_2_k4,
                                    data = na.omit(base_cluster_reanalise_2_padronizada),
                                    geom = "text",
                                    main = "Cluster Não Hierárquico - 4 agrupamentos",
                                    repel = T) +
                       theme_classic(base_size = 12) +
                       theme(panel.background = element_rect(fill = "grey97")) +
                       labs(x = "",
                       y = "")

plot_reanalise_2_k6 <- fviz_cluster(object = cluster_reanalise_2_k6,
                                    data = na.omit(base_cluster_reanalise_2_padronizada),
                                    geom = "text",
                                    main = "Cluster Não Hierárquico - 6 agrupamentos",
                                    repel = T) +
                       theme_classic(base_size = 12) +
                       theme(panel.background = element_rect(fill = "grey97")) +
                       labs(x = "",
                           y = "")

grid.arrange(plot_reanalise_2_k4,
             plot_reanalise_2_k6)  



# agora sem as ações que formaram os agrupamentos outliers

base_cluster_reanalise_3 <- base_cluster_reanalise %>%
                            select(1:4, 6, 9:11, 13)

# padronizando as observações
base_cluster_reanalise_3_padronizada <- base_cluster_reanalise_3 %>% scale()

# Visualizando a base padronizada
base_cluster_reanalise_3_padronizada %>% 
  kable(align = "c",
        caption = "Base de Dados Padronizada") %>% 
  kable_classic(html_font = "courier new") %>% 
  kable_styling(bootstrap_options = "striped",
                font_size = 12)

# verificando a quantidade adequada de clusters

# método wss
base_cluster_reanalise_3_padronizada %>%
  na.omit() %>%
  fviz_nbclust(FUN = kmeans,
               method = "wss",
               k.max = 20)

# método silhouette
base_cluster_reanalise_3_padronizada %>%
  na.omit() %>%
  fviz_nbclust(FUN = kmeans,
               method = "silhouette",
               k.max = 20)

# vamos continuar com 4 e 6 clusters

set.seed(0)
cluster_reanalise_3_k4 <- kmeans(na.omit(base_cluster_reanalise_3_padronizada),
                                 centers = 4)

set.seed(0)
cluster_reanalise_3_k6 <- kmeans(na.omit(base_cluster_reanalise_3_padronizada),
                                 centers = 6)

# Visualizar os clusters

plot_reanalise_3_k4 <- fviz_cluster(object = cluster_reanalise_3_k4,
                                    data = na.omit(base_cluster_reanalise_3_padronizada),
                                    geom = "text",
                                    main = "Cluster Não Hierárquico - 4 agrupamentos",
                                    repel = T) +
                       theme_classic(base_size = 12) +
                       theme(panel.background = element_rect(fill = "grey97")) +
                       labs(x = "",
                            y = "")

plot_reanalise_3_k6 <- fviz_cluster(object = cluster_reanalise_3_k6,
                                    data = na.omit(base_cluster_reanalise_3_padronizada),
                                    geom = "text",
                                    main = "Cluster Não Hierárquico - 6 agrupamentos",
                                    repel = T) +
                       theme_classic(base_size = 12) +
                       theme(panel.background = element_rect(fill = "grey97")) +
                       labs(x = "",
                            y = "")

grid.arrange(plot_reanalise_3_k4,
             plot_reanalise_3_k6)

# vamos seguir com a reanálise 3 de 6 clusters.

# visualizando os agrupamentos formados

grupos_reanalise3_k6 <- cluster_reanalise_3_k6$cluster %>% data.frame()

base_cluster_result_reanalise3_k6 <- inner_join(
  base_cluster_reanalise_3 %>% rownames_to_column(var = "TICKER"),
  grupos_reanalise3_k6 %>% rownames_to_column(var = "TICKER"),
  by = "TICKER"
) %>% rename("CLUSTER" = 11)

base_cluster_result_reanalise3_k6 %>%
  kable(align = "c",
        caption = "Base de Dados Resultado Cluster Kmeans") %>% 
  kable_classic() %>% 
  kable_styling(bootstrap_options = "bordered",
                font_size = 12,
                html_font = "courier new")

# Análise do resutaldo da reanálise de clustering não hierárquico

# Análise descritiva das médias

medias_reanalise3_k6 <- base_cluster_result_reanalise3_k6 %>%
  group_by(CLUSTER) %>%
  summarise(QTD_OBS = n(),
            DY = mean(DY) %>% round(2),
            P.L = mean(P.L) %>% round(2),
            P.VP = mean(P.VP) %>% round(2),
            M.BRUTA = mean(M.BRUTA) %>% round(2),
            M.LIQUIDA = mean(M.LIQUIDA) %>% round(2),
            ROE = mean(ROE) %>% round(2),
            ROA = mean(ROA) %>% round(2),
            ROIC = mean(ROIC) %>% round(2),
            LPA = mean(LPA) %>% round(2))

medias_reanalise3_k6 %>% 
  kable(align = "c",
        caption = "Análise Descritiva das Médias dos Clusters") %>% 
  kable_classic() %>% 
  kable_styling(bootstrap_options = "striped",
                html_font = "courier new",
                font_size = 13)

# vamos comparar as ações que compuseram cada cluster (velha / nova)

# Cluster 1
base_cluster_reanalise_result_kmeans6 %>% filter(CLUSTER == 1)
base_cluster_result_reanalise3_k6 %>% filter(CLUSTER == 1)


# (A ação HAPV3 manteve-se no cluster, porém agora com a inclusão do papel EMBR3)
# Nota-se indicadores de rentabilidade (roe, roa e roic) baixos, juntamente de lpas baixos, mostrando
# um baixo desempenho das empresas. Além disso, nota-se grande diferença entre a margem bruta e lí-
# quida corroborando a situação. porém nota-se uma valorização do preço dos papeis em relação ao 
# seus ganhos líquidos (p/l mais alto dos agrupamentos). do ponto de vista de desempenho, pode-se
# considerar os papéis caros para o resultado apresentado.


# Cluster 2
base_cluster_reanalise_result_kmeans6 %>% filter(CLUSTER == 2)
base_cluster_result_reanalise3_k6 %>% filter(CLUSTER == 2)

# (alguns papéis se mantiveram no cluster 2)
# nota-se que foram reunidos os papeis com os melhores indicadores de eficiência (margem bruta e 
# líquida), analisando as observações individualmente, não houve outliers jogando essa média para cima.
# além disso, apresenta boa eficiência, demonstrada pelos indicadores roe roa e roic.


# Cluster 3
base_cluster_reanalise_result_kmeans6 %>% filter(CLUSTER == 3)
base_cluster_result_reanalise3_k6 %>% filter(CLUSTER == 3)

# O cluster 3 reuniu empresas que tiveram, em geral, um bom resultado. Bons indicadores de eficiência e
# rentabilidade, um bom lucro por ação e uma boa distribuição de dividendos, mas sem se destacarem 
# muito em algum campo.

# Cluster 4

base_cluster_reanalise_result_kmeans6 %>% filter(CLUSTER == 4)
base_cluster_result_reanalise3_k6 %>% filter(CLUSTER == 4)

# maiores dividendos. alta rentabilidade indicada pelos roe, roa e roic e lpa. boa eficiencia demons-
# trada pelas margens bruta e líquida.


# CLuster 5
base_cluster_result_reanalise3_k6 %>% filter(CLUSTER == 5)

# assim como o cluster 1, o cluster 5 tem dividendos muito baixos, uma alta valorização de seus papeis
# em relação ao lucro das empresas, porém com empresas que tiveram uma rentabilidade um pouco superior
# (roe roa e roic um pouco mais altos). possivelmente o que separou as observações.

# Cluster 6
base_cluster_result_reanalise3_k6 %>% filter(CLUSTER == 6)

# o cluster 6 reune ações com eficiencia baixa, demonstrada pelas margens. porém se diferencia do clus-
# ter 5 pelo menor preço relativo de seus papéis (pvp menor e p/l negativo - ações com prejuízo)




# decidi fazer a análise descritiva das médias com 4 agrupamentos, pois ainda acho que não está claro
# a rotulação dos agrupamentos.

# separando qual ação pertence a cada clusters
grupos_reanalise3_k4 <- cluster_reanalise_3_k4$cluster %>% data.frame()

# criando um dataframe com o resultado do k4
base_cluster_result_reanalise3_k4 <- inner_join(
  base_cluster_reanalise_3 %>% rownames_to_column(var = "TICKER"),
  grupos_reanalise3_k4 %>% rownames_to_column(var = "TICKER"),
  by = "TICKER"
) %>% rename("CLUSTER" = 11)

# visualizando
base_cluster_result_reanalise3_k4 %>% 
  kable(align = "c") %>% 
  kable_classic() %>% 
  kable_styling(bootstrap_options = "striped",
                html_font = "courier new",
                font_size = 13)

# análise descritiva das médias dos clusters

medias_reanalise3_k4 <- base_cluster_result_reanalise3_k4 %>% 
  group_by(CLUSTER) %>% 
  summarise(QTD_OBS = n(),
            DY = mean(DY) %>% round(2),
            P.L = mean(P.L) %>% round(2),
            P.VP = mean(P.VP) %>% round(2),
            M.BRUTA = mean(M.BRUTA) %>% round(2),
            M.LIQUIDA = mean(M.LIQUIDA) %>% round(2),
            ROE = mean(ROE) %>% round(2),
            ROA = mean(ROA) %>% round(2),
            ROIC = mean(ROIC) %>% round(2),
            LPA = mean(LPA) %>% round(2))

medias_reanalise3_k4 %>% 
  kable(align = "c",
        caption = "Análise Descritiva das Médias dos Agrupamentos") %>% 
  kable_classic() %>% 
  kable_styling(bootstrap_options = "striped",
                html_font = "courier new",
                font_size = 13)


base_cluster_result_reanalise3_k4 %>% filter(CLUSTER == 1)
base_cluster_result_reanalise3_k4 %>% filter(CLUSTER == 2)
base_cluster_result_reanalise3_k4 %>% filter(CLUSTER == 3)
base_cluster_result_reanalise3_k4 %>% filter(CLUSTER == 4)

# conclusão: com 4 agrupamentos, algumas observações acabam poluindo demais os clusters, então seguire-
# mos com 6.

#######################################################################################################

# 3) Análise com aprendizado supervisionado - supervised machine learning

# Objetivo: estudar a influência das variáveis sobre o indicador DY e verificar a possibilidade de um
# modelo de regressão para previsão do pagamento de dividendos.

# Pacotes necessários
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Separando um dataframe para esta análise
base_rl <- base_cluster %>% select(1:4, 6, 9:11, 13)

# Avaliando a correlação entre as variáveis com cor.test ou chart.Correlation
chart.Correlation(base_rl, 
                  histogram = T,
                  pch = "+")

# as variáveis ROE, ROA, ROIC e LPA apresentação correlações maiores. Vamos verificar o modelo
# com essas variáveis

# Estimando o modelo

model_dy_ibov <- lm(formula = DY ~ ROE + ROA + ROIC + LPA,
                    data = base_rl)

# Analisando o resultado do modelo
summary(model_dy_ibov)
summ(model_dy_ibov,
     confint = T,
     digits = 4,
     ci.width = .95)

# R²: 0.58
# Teste F: p-value 7.956e-14 (precisa ser < 0.05)
# estatística t: foi < 0.05 apenas para ROIC e LPA

# o modelo passou (pvalue teste f < 0.05)
# {explorar stepwise - retirar variáveis com pvalue t > 0.05 - ver livro e código do professor}

# Fazendo uma predição para teste

predict(object = model_dy_ibov,
        data.frame(ROE = 34.56,
                   ROA = 15.03,
                   ROIC = 24.67, 
                   LPA = 11.5))

# ao utilizar os mesmos valores da petr3, o valor previsto é muito diferente. Farei outros testes,
# mas já imagino que este modelo não terá muita utilidade, pois há diversas outras variáveis que 
# influenciam o DY.



# apenas duas variáveis foram estatisticamentes significantes, e o intercepto também não foi. É errado 
# simplesmente eliminá-los



## procedimento stepwise

# aplicando o procedimento stepwise
step_model_dy_ibov <- step(model_dy_ibov,
                           k = 3.841459)

# verificando os novos resultados
summary(step_model_dy_ibov)

summ(step_model_dy_ibov,
     confint = T,
     digits = 6,
     ci.width = .95)


# após o procedimento, foram mantidas as variáveis ROIC e LPA no modelo. O p-value do intercepto ainda não
# passa e o R² manteve-se.

# realizando uma previsão com o novo modelo para teste

predict(object = step_model_dy_ibov,
        data.frame(ROIC = 24.67, 
                   LPA = 11.5))


## predições

# separando as observações
base_predict <- base_cluster %>%
  na.omit() %>% 
  select(ROE, ROA, ROIC, LPA)

base_predict_stepwise <- base_cluster %>% 
  na.omit() %>% 
  select(ROIC, LPA)

base_predict_dy <- base_cluster %>% 
  na.omit() %>% 
  select(DY)

base_predict[1:10,]
base_predict_dy


# primeiro modelo model_dy_ibov

predict_modelo_1 <- predict(object = model_dy_ibov,
                            base_predict[1:10,])


# segundo modelo step_model_dy_ibov
predict_modelo_stepwise <- predict(object = step_model_dy_ibov,
                                   base_predict_stepwise[1:10,])


# criando um dataframe para comparação
base_result_predict <- data.frame(base_predict_dy[1:10,],
                                  predict_modelo_1,
                                  predict_modelo_stepwise)

base_result_predict <-  base_result_predict %>% rename('DY' = 1,
                               'Prev. modelo 1' = 2,
                               'Prev. modelo stepwise' = 3)
base_result_predict %>% 
  kable(align = "c",
        caption = "Resultado das Predições") %>% 
  kable_classic(html_font = "courier new") %>% 
  kable_styling(bootstrap_options = "striped",
                font_size = 14)


## Verificação dos pressupostos de modelos de regressão por mínimos quadrados ordinários

# Teste de verificação da aderência dos resíduos à normalidade
sf.test(model_dy_ibov$residuals)
sf.test(step_model_dy_ibov$residuals)

# Histograma dos resíduos do modelo
base_rl %>%
  mutate(residuos = model_dy_ibov$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(model_dy_ibov$residuals),
                            sd = sd(model_dy_ibov$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

# Multicolinearidade

base_rl %>% select(ROE, ROA, ROIC, LPA) %>% 
  correlation(method = "pearson") %>%
  plot()

# Heterocedasticidade

ols_test_breusch_pagan(model_dy_ibov)
ols_test_breusch_pagan(step_model_dy_ibov)





