# Instalações, se necessário
#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
#devtools::install_github("datazoompuc/datazoom.amazonia")
#devtools::install_github("Cepesp-Fgv/cepesp-r")
#install.packages("tidyverse")
#install.packages("DT")

# Carrega pacote do datazoom.amazonia
library(datazoom.amazonia)
# Carrega pacote do CEPESP Data
library(cepespR)

library(tidyverse)
library(DT)


# 1 - EMBARGOS DO IBAMA
# Baixa do Data Zoom Amazônia as Áreas Embargadas pelo Ibama
embargos <- load_ibama(dataset = "areas_embargadas", raw_data = TRUE, 
                    language = "pt", legal_amazon_only = FALSE)
# Visualiza
glimpse(embargos)

# Retira símbolos da coluna cpf_ou_cnpj
embargos$cpf_ou_cnpj <- gsub("\\.","",as.character(embargos$cpf_ou_cnpj))
embargos$cpf_ou_cnpj <- gsub("\\-","",as.character(embargos$cpf_ou_cnpj))
embargos$cpf_ou_cnpj <- gsub("\\/","",as.character(embargos$cpf_ou_cnpj))

# Baixa do Cepesp os deputados federais eleitos em 2018
deputados_eleitos <- get_candidates(year = "2018", position = "Deputado Federal", only_elected = T)
# Visualiza
glimpse(deputados_eleitos)

# Une os dois dataframes pelo CPF do infrator ambiental e CPF do candidato - verifica se são os mesmos
joined_df <- inner_join(deputados_eleitos, embargos, 
                           by = c("CPF_CANDIDATO" = "cpf_ou_cnpj"))
# Visualiza
View(joined_df)

# Mostra diretório atual
getwd()
# salva resultado
write.csv(anos,"Deputados eleitos em 2018 que têm embargos do Ibama.csv", row.names = FALSE) 





# 2 - CONCESSÕES LEGAIS DE MINERAÇÃO
# Baixa informações das minas que estão sendo exploradas legalmente no Brasil
minera <- load_sigmine(dataset = 'sigmine_active', raw_data = TRUE, language = "pt")

# Nomes das colunas
colnames(minera)

# Quantas linhas
nrow(minera)

# Cinco primeiras linhas
head(minera, 5)

# Resumo e tipos das colunas
glimpse(minera)

# Quantos são processos divididos pela fase de exploração
how_many <- unique(minera$fase)
length(how_many)
how_many

# Retira a coluna de geometria em cópia do dataframe - está com erro de fomatação
minera_no_geom <- sf::st_drop_geometry(minera)


# Na fase CONCESSÃO DE LAVRA, quais são as 20 empresas que mais têm registros 
filter(minera_no_geom, fase=="CONCESSÃO DE LAVRA") %>% 
  group_by(nome) %>% 
  summarize(total=n()) %>%    
  arrange(desc(total)) %>% 
  head(20)


# Na Amazônia e no Pará, quais são as substâncias com mais lavra
filter(minera_no_geom, (uf == "AM" | uf == "PA") & fase=="CONCESSÃO DE LAVRA") %>% 
  group_by(subs) %>% 
  summarize(total=n()) %>%    
  arrange(desc(total)) %>% 
  head(10)


# Empresas no AM e PA com CONCESSÃO DE LAVRA para BAUXITA
filter(minera_no_geom, (uf == "AM" | uf == "PA") & subs == "BAUXITA" & fase=="CONCESSÃO DE LAVRA") %>% 
  group_by(nome) %>% 
  summarize(total=n()) %>%    
  arrange(desc(total)) %>% 
  head(10)


# Evolução da CONCESSÃO DE LAVRA no AM
#lag() - Calcula uma versão defasada de uma série temporal, deslocando a base de tempo de volta por um determinado número de observações

#No caso aqui, podemos calcular a diferença no número de CONCESSÃO DE LAVRA ano após ano
anos <- filter(minera_no_geom, uf=="AM" & fase=="CONCESSÃO DE LAVRA") %>% 
  group_by(ano) %>% 
  summarize(total=n()) %>%
  mutate(ano_anterior=lag(total), variacao=total-ano_anterior) 
anos

# Mostra diretório atual
getwd()
# salva resultado
write.csv(anos,"Evolução da CONCESSÃO DE LAVRA no AM.csv", row.names = FALSE) 


# Evolução de LICENCIAMENTO no AM
#lag() - Calcula uma versão defasada de uma série temporal, deslocando a base de tempo de volta por um determinado número de observações

#No caso aqui, podemos calcular a diferença no número de CONCESSÃO DE LAVRA ano após ano
anos <- filter(minera_no_geom, uf=="AM" & fase=="LICENCIAMENTO") %>% 
  group_by(ano) %>% 
  summarize(total=n()) %>%
  mutate(ano_anterior=lag(total), variacao=total-ano_anterior) 
anos

# salva resultado
write.csv(anos,"Evolução da LICENCIAMENTO no AM.csv", row.names = FALSE) 


# Fase de LICENCIAMENTO no AM
# group_by pelo nome das empresas e substâncias
filter(minera_no_geom, uf=="AM" & fase=="LICENCIAMENTO") %>% 
  group_by(nome, subs) %>% 
  summarize(total=n()) %>% 
  arrange(desc(total)) %>% 
  head(20)


# Fase de LICENCIAMENTO no AM
# group_by pelo nome das empresa/pessoas e substâncias
# apenas nos anos de 2019, 2020, 2021 e 2022
filter(minera_no_geom, uf=="AM" & fase=="LICENCIAMENTO" &  (ano == 2019 | ano == 2020 | ano == 2021 | ano == 2022) ) %>% 
  group_by(nome, subs) %>% 
  summarize(total=n()) %>% 
  arrange(desc(total)) %>% 
  head(20)


# No PA, qual a porcentagem das subs em CONCESSÃO DE LAVRA em relação ao total geral
percent_subs_pa <- minera_no_geom %>% 
  group_by(uf, subs) %>% 
  filter(uf=="PA" & fase=="CONCESSÃO DE LAVRA") %>% 
  summarize(total=n()) %>% 
  mutate(percent=total/sum(total, na.rm=T)*100) %>% 
  arrange(-percent)

# Usando a biblioteca DT (DataTables) que nos permite criar tabelas pesquisáveis com o plug-in para jQuery
datatable(percent_subs_pa)
