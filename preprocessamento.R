require(readr)
require(dplyr)
require(tidyr)
require(lubridate)
require(stringr)
rm(list=ls())
#https://dadosabertos.camara.leg.br/swagger/api.html#staticfile

# arquivos <- list.files(pattern = 'Ano-*.*')
# 
# colunas <- cols(
#   cpf = col_character(),
#   ideCadastro = col_integer(),
#   txtNumero = col_character(),
#   datEmissao = col_character(),
#   txtPassageiro = col_character(),
#   txtTrecho = col_character(),
#   vlrRestituicao = col_double(),
#   urlDocumento = col_character()
#   )
# locale <- locale(decimal_mark = ".")
# 
# d <- lapply(arquivos,read_delim,delim = ';',col_types=colunas,locale = locale)
# 
# df <- d %>% bind_rows()
# 
# 
# saveRDS(df,file = 'cota_parlamentar_original.RDS')
df <- readRDS(df,file = 'cota_parlamentar_original.RDS')


## Substitui colunas de subcotas e especificacaosubcota por coluna de tipo agregandoas duas informações
cotas.subcotas <- df %>%
  select(numSubCota,txtDescricao,numEspecificacaoSubCota,txtDescricaoEspecificacao) %>%
  unique() %>%
  unite('codTipo', c(numSubCota,numEspecificacaoSubCota),remove = F,sep = '.') %>% 
  unite('descTipo', c(txtDescricao,txtDescricaoEspecificacao),remove = T,sep = ' - ',na.rm = T)

df <- df %>% 
  left_join(cotas.subcotas) %>% 
  select(-c(numSubCota,txtDescricao,numEspecificacaoSubCota,txtDescricaoEspecificacao))

#Define variaveis que indicam se houve glosa e sua porcentagem; sessão legislativa (o ano relativo ao inicio do mandato)
#Algumas variaveis numericas devem ser tratadas como categoricas
#Codifica a variavel indTipoDocumento
df <- df %>% 
  mutate(porc.glosa = vlrGlosa/vlrDocumento,
         houve.glosa = case_when(vlrGlosa>0~'sim',T~'nao'),
         sessao.leg = ((numAno - 2007) %% 4)+1) %>%
  mutate(across(c(ideCadastro,nuCarteiraParlamentar,nuLegislatura,codLegislatura,numAno,numMes,sessao.leg),as.factor)) %>% 
  mutate(indTipoDocumento = factor(indTipoDocumento,levels = c(0,1,2),labels = c('Nota Fiscal','Recibo','Despesa Exterior')))

#Corrige alguns anos da data de emissao, imputando 20 como caracteres iniciais de todos os anos
emi <- df$datEmissao
substr(emi,1,2) <- '20'
df$datEmissao <- emi

#Constroi metrica que mede intervalo em meses entre emissão de documento de despesa e competencia da prestacao de contas
df <- df %>%
  mutate(datEmissao2 = ymd_hms(datEmissao),
         numDia = '1') %>% 
  unite('datCompetencia',c(numAno,numMes,numDia),sep = '-',remove = F) %>% 
  mutate(datCompetencia = ymd(datCompetencia),
         idadeGasto = interval(datCompetencia,datEmissao2) %/% months(1)) %>% 
  select(-c(numDia,datEmissao2))
#Para linhas com intervalo maior ou igual a 2 anos, considera nulo o intervalo, visto que a chance de haver erro de digitação nesses casos é grande
df <- df %>%
  mutate(idadeGasto = case_when(abs(idadeGasto)<24~idadeGasto))

#Formata cpf e cnpj e cria flag que indica se é um outro ou outra coisa
df <- df %>% 
  mutate(txtCNPJCPF = str_remove_all(string = txtCNPJCPF,pattern = '\\.'),
         txtCNPJCPF = str_remove_all(string = txtCNPJCPF,pattern = '\\-'),
         txtCNPJCPF = str_remove_all(string = txtCNPJCPF,pattern = '/'),
         txtCNPJCPF = str_trim(txtCNPJCPF),
         flag.CNPJCPF = case_when(str_length(txtCNPJCPF)==11~'CPF',
                                  str_length(txtCNPJCPF)==14~'CNPJ')
  ) 

#Converte string 'data de emissão' em tipo Date
df <- df %>% mutate(datEmissao = str_sub(datEmissao,start = 1,end = 10),
                    datEmissao = ymd(datEmissao))

#Cria coluna cnpj empresa
df <- df %>%
  mutate(cnpj8 = case_when(flag.CNPJCPF=='CNPJ'~str_sub(txtCNPJCPF,1,8)),
         cnpj8 = case_when(cnpj8!='00000000'~cnpj8)) #Ignora cnpj8 zerado(seria BB, mas é, possivelmente erro de digitacao)
  
                     




# df <- readRDS('cota_parlamentar_preprocessado.RDS')
# mini <- readRDS('mini_cota_parlamentar.RDS')


df$idGasto <- 1:nrow(df)

i <- sample(x = 1:nrow(df),size = 100000,replace = F)
mini <- df[i,]


saveRDS(df,file = 'cota_parlamentar_preprocessado.RDS')

saveRDS(mini,file = 'mini_cota_parlamentar.RDS')

