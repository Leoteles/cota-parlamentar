require(readr)
require(dplyr)
require(tidyr)
require(lubridate)
require(DBI)
require(RSQLite)
require(stringr)

rm(list=ls())
df0 <- readRDS(file = 'cota_parlamentar_preprocessado.RDS')#[1:1000,]


df <- df0 %>%
  select(txNomeParlamentar,cpf,txtFornecedor,txtCNPJCPF,flag.CNPJCPF) %>%
  filter(flag.CNPJCPF=='CNPJ') %>%
  mutate(cnpj8 = str_sub(txtCNPJCPF,1,8),
         inicio = '***',
         meio.cpf = str_sub(cpf,4,9),
         fim = '**'
  ) %>%
  filter(cnpj8!='00000000') %>% #Ignora este cnpj(seria BB)
  unite('cpf_ofuscado',c('inicio','meio.cpf','fim'),sep = '') %>%
  unique()


deputados <- read_delim('./deputados.csv',delim = ';') %>%
  select(nome,nomeCivil)

df <- df %>%
  left_join(deputados,by = c('txNomeParlamentar'='nome')) %>%
  mutate(nomeCivil = case_when(is.na(nomeCivil) ~ txNomeParlamentar,
                          T ~ nomeCivil),
         nomeCivil = toupper(nomeCivil),
         nomeCivil = iconv(nomeCivil,from='UTF-8',to='ASCII//TRANSLIT'))#Os nomes na base cnpj estao sem acentuacao e com caixa alta

con <- dbConnect(SQLite(), "d:/documents/cnpj/base_sqlite_pronta_qsacnpj/sqlite_dados_qsa_cnpj_04-07-20/bd_dados_qsa_cnpj.db")

# Add some data
copy_to(con,df,overwrite = T)


##################################################
#1. Info sobre as empresas das despesas:
cad.reduzido <- con %>%
  tbl(sql("
          SELECT  cnpj,cnae_fiscal,codigo_natureza_juridica,data_inicio_atividade,cep,uf,municipio,
          capital_social_empresa,porte_empresa,
          opcao_pelo_simples,opcao_pelo_mei,situacao_especial
          FROM cnpj_dados_cadastrais_pj
          WHERE
          identificador_matriz_filial = 1
          AND substr(cnpj,1,8) in (select distinct cnpj8 from df)
          ")) %>%
  collect()


##################################################
# 2. Empresa diretamente relacionada ao deputado

possiveis_socios_deputados <- con %>%
  tbl(sql("
          SELECT  distinct cnpj,identificador_socio,nome_socio,cnpj_cpf_socio,cpf_representante_legal
          FROM cnpj_dados_socios_pj
          WHERE
            cnpj_cpf_socio in (select distinct cpf_ofuscado from df)
          ")) %>%
  collect()

res <- possiveis_socios_deputados %>%
  inner_join(df,by = c('cnpj_cpf_socio'='cpf_ofuscado')) %>%
  select(cnpj,cnpj_cpf_socio,nome_socio,txNomeParlamentar,nomeCivil) %>%
  unique() %>%
  rowwise() %>%
  mutate(distancia = adist(x = nome_socio,y = nomeCivil,costs=list(substitution=2,insertion=1, deletion=1))) %>%
  mutate(considerado.igual = case_when(nome_socio==nomeCivil |
                                         distancia<=7 ~ T,T~F)) %>%
  filter(distancia<=12)

identificados.manualmente <- c('JOSE EDUARDO MARTINS CARDOZO',
                               'EUNICIO LOPES DE OLIVEIRA',
                               'SANDRO DA MABEL ANTONIO SCODRO',
                               'ANTONIO CARLOS CHAMARIZ RAMOS',
                               'GIVALDO DE SA GOUVEIA CARIMBAO',
                               'VIRGILIO GUIMARAES DE PAULA',
                               'REGIS FERNANDES DE OLIVEIRA',
                               'CLARISSA GAROTINHO BARROS ASSED MATHEUS DE OLIVEIRA',
                               'JOSE SANTANA DE VASCONCELLOS MOREIRA',
                               'THEMISTOCLES DE SAMPAIO PEREIRA',
                               'JERONIMO DE OLIVEIRA REIS',
                               'LUIZ CARLOS CAETANO',
                               'CLAUDIO CASTANHEIRA DIAZ')

socios_deputados <- res %>%
  mutate(considerado.igual = case_when(nome_socio %in% identificados.manualmente ~ T,T~considerado.igual)) %>%
  filter(considerado.igual) %>%
  select(-c(distancia,considerado.igual)) %>%
  mutate(cnpj8 = str_sub(cnpj,1,8)) %>%
  unique()


deputados_com_gastos_em_empresas_proprias <- df %>%
  inner_join(socios_deputados,by=c('cnpj8'='cnpj8','cpf_ofuscado'='cnpj_cpf_socio'))



# save.image(file = 'temp.RData')
# load(file = 'temp.RData')
# con <- dbConnect(SQLite(), "d:/documents/cnpj/base_sqlite_pronta_qsacnpj/sqlite_dados_qsa_cnpj_04-07-20/bd_dados_qsa_cnpj.db")


##################################################
# 3. Empresa indiretamente relacionada ao deputado
#    Procura gastos com empresas de socios dos deputados (em outras empresas, como uma triangulacao)

#Primeiramente, encontra todos os socios de deputados e todas as sua empresas

#Empresas cujo socio e deputado
copy_to(con,socios_deputados,overwrite = T)
#Outras empresas de socios de deputados
socios_de_deputados <- con %>%
  tbl(sql("with soc as (
          SELECT  distinct nome_socio,cnpj_cpf_socio
          FROM cnpj_dados_socios_pj
          WHERE
          cnpj in (select distinct cnpj from socios_deputados)
  )
          SELECT  distinct t1.cnpj,t2.*
          FROM cnpj_dados_socios_pj t1 inner join soc t2 on t1.nome_socio = t2.nome_socio and t1.cnpj_cpf_socio = t2.cnpj_cpf_socio
          ")) %>%
  collect()

#Da tabela acima, so interessam para a analise aquelas empresas 
# que prestaram servicos para deputados
empresas_alvo <- socios_de_deputados %>% filter(cnpj %in% cad.reduzido$cnpj)

x <- socios_deputados %>%
  #Obtem a relacao deputados -> socios de deputados
  inner_join(socios_de_deputados,suffix = c('_deputado','_socio'),by = c('cnpj'='cnpj')) %>% 
  select(-c(txNomeParlamentar,nomeCivil))
names(x) <- c('cnpj_deputado','cpf_deputado','nome_deputado','cnpj8_deputado','nome_socio','cnpj_cpf_socio')

#Filtra linhas em que socio e deputado sao iguais
x <- x %>% 
  filter(cpf_deputado!=cnpj_cpf_socio)

#Procura as empresas dos socios acima
empresas_dos_socios <- x %>%
  inner_join(empresas_alvo,by = c('cnpj_cpf_socio'='cnpj_cpf_socio','nome_socio'='nome_socio')) %>% 
  mutate(cnpj8 = str_sub(cnpj,1,8))


deputados_com_gastos_em_empresas_de_socios <- df %>% 
  inner_join(empresas_dos_socios,by = c('cnpj8'='cnpj8','cpf_ofuscado'='cpf_deputado'))



###Analise do caso anterior
x <- df0 %>% 
  select(txNomeParlamentar,cpf,txtFornecedor,txtCNPJCPF,flag.CNPJCPF,vlrDocumento) %>%
  filter(flag.CNPJCPF=='CNPJ') %>% 
  mutate(cnpj8 = str_sub(txtCNPJCPF,1,8),
         inicio = '***',
         meio.cpf = str_sub(cpf,4,9),
         fim = '**'
  ) %>% 
  filter(cnpj8!='00000000') %>% #Ignora este cnpj(seria BB)
  unite('cpf_ofuscado',c('inicio','meio.cpf','fim'),sep = '') %>% 
  select(txNomeParlamentar,cpf_ofuscado,txtFornecedor,txtCNPJCPF,cnpj8,vlrDocumento) %>% 
  group_by(txNomeParlamentar,cpf_ofuscado,txtCNPJCPF,cnpj8) %>% 
  summarise(txtFornecedor = max(txtFornecedor),total = sum(vlrDocumento,na.rm = T))


x2 <- x %>% filter(txtCNPJCPF %in% deputados_com_gastos_em_empresas_de_socios$txtCNPJCPF)

x3 <- df0[df0$txtCNPJCPF=='26198515000484',]
#write.csv(x3,file='o tempo cota.csv')


# save.image(file = 'temp2.RData')
# load(file = 'temp2.RData')


dbDisconnect(con)
####################

# save.image(file = 'preproc_cnpj.RData')
load(file = 'preproc_cnpj.RData')

#Analise 1:
cnpj.features <- cad.reduzido %>%
  mutate(cnpj8 = str_sub(cnpj,1,8),
         cnpj = NULL,
         data_inicio_atividade = ymd(data_inicio_atividade))
#Analise 2:
cnpj.features <- cnpj.features %>% 
  mutate(empresa_propria = cnpj8 %in% deputados_com_gastos_em_empresas_proprias$cnpj8)

#Analise 3:
cnpj.features <- cnpj.features %>% 
  mutate(empresa_socio = cnpj8 %in% deputados_com_gastos_em_empresas_de_socios$cnpj8)

saveRDS(cnpj.features,file='cnpj_features.RDS')







# # cad <- con %>%
# #   tbl(sql("
# #           SELECT  cnpj,razao_social,nome_fantasia,
# #                   cnae_fiscal,
# #                   codigo_natureza_juridica,data_inicio_atividade,cep,uf,municipio,correio_eletronico,
# #                   capital_social_empresa,porte_empresa,
# #                   opcao_pelo_simples,data_opcao_pelo_simples,data_exclusao_simples,
# #                   opcao_pelo_mei,situacao_especial,data_situacao_especial
# #           FROM cnpj_dados_cadastrais_pj
# #           WHERE
# #             identificador_matriz_filial = 1
# #             AND substr(cnpj,1,8) in (select cnpj8 from cnpjs)
# #           ")) %>%
# #   collect()
# # saveRDS(cad,file='cad_18var.RDS')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# socios.dep <- con %>%
#   tbl(sql("
#           SELECT  distinct t2.*,t1.cnpj,t1.identificador_socio,t1.nome_socio,t1.cnpj_cpf_socio,t1.cpf_representante_legal
#           FROM cnpj_dados_socios_pj t1
#           INNER JOIN df t2 on t2.cpf_ofuscado=t1.cnpj_cpf_socio and t2.cnpj8 = substr(t1.cnpj,1,8)
#           WHERE
#           substr(t1.cnpj,1,8) in (select cnpj8 from cnpjs)
#           ")) %>%
#   collect()
# 
# 
# 
# socios <- con %>%
#   tbl(sql("
#           SELECT  distinct cnpj,identificador_socio,nome_socio,cnpj_cpf_socio,cpf_representante_legal
#           FROM cnpj_dados_socios_pj
#           WHERE
#             substr(cnpj,1,8) in (select distinct cnpj8 from df)
#           ")) %>%
#   collect()
# 
# 
# 
# # saveRDS(socios,file='socios.RDS')
# # copy_to(con, socios,overwrite = T)
# 
# 
# 
# 
# cnpjs <- df %>%
#   filter(flag.CNPJCPF=='CNPJ') %>% 
#   group_by(cnpj8) %>%
#   summarise(cnpjmat = min(txtCNPJCPF),nome = max(txtFornecedor))
# 
# 
# #tabelas <- as.data.frame(dbListTables(con))
# 
# # Add some data
# copy_to(con, cnpjs,overwrite = T)
# 
# 
# 
# cnaes <- con %>% tbl('tab_cnae') %>% collect()
# cod.responsavel <- con %>% tbl('tab_qualificacao_responsavel_socio') %>% collect()
# cod.nat.jur <- con %>% tbl('tab_natureza_juridica') %>% collect()
# 
# #Procura empresas de deputados
# dep_cnpj <- df %>% select(txNomeParlamentar,cpf,cnpj8) %>% unique() %>% 
#   mutate(inicio = '***',
#          meio.cpf = str_sub(cpf,4,9),
#          fim = '**'
#   ) %>% 
#   unite('cpf_ofuscado',c('inicio','meio.cpf','fim'),sep = '')
# copy_to(con,dep_cnpj,overwrite = T)
# 
# socios.dep <- con %>%
#   tbl(sql("
#           SELECT  distinct t2.*,t1.cnpj,t1.identificador_socio,t1.nome_socio,t1.cnpj_cpf_socio,t1.cpf_representante_legal
#           FROM cnpj_dados_socios_pj t1
#           INNER JOIN dep_cnpj t2 on t2.cpf_ofuscado=t1.cnpj_cpf_socio and t2.cnpj8 = substr(t1.cnpj,1,8)
#           WHERE
#             substr(t1.cnpj,1,8) in (select cnpj8 from cnpjs)
#           ")) %>%
#   collect()
# # saveRDS(socios,file='socios.RDS')
# 
# 
# # socios <- con %>%
# #   tbl(sql("
# #           SELECT  distinct cnpj,identificador_socio,nome_socio,cnpj_cpf_socio,cpf_representante_legal
# #           FROM cnpj_dados_socios_pj 
# #           WHERE 
# #             substr(cnpj,1,8) in (select cnpj8 from cnpjs)
# #           ")) %>% 
# #   collect()
# # saveRDS(socios,file='socios.RDS')
# # copy_to(con, socios,overwrite = T)
# 
# socios.pf <- socios %>%
#   mutate(cnpj8 = str_sub(cnpj,1,8),
#          cnpj_cpf_socio = case_when(nchar(cnpj_cpf_socio)==11~str_sub(cnpj_cpf_socio,4,9)),
#          cpf_representante_legal = str_sub(cpf_representante_legal,4,9))
# 
# 
# dbDisconnect(con)
# 
# 
# 
# df.socios1 <- df %>%
#   select(txNomeParlamentar,cpf,cnpj8) %>%
#   unique() %>%
#   filter(!is.na(cpf)) %>% 
#   mutate(meio.cpf = str_sub(cpf,4,9)) %>% 
#   inner_join(socios.pf,by = c('cnpj8'='cnpj8','meio.cpf'='cnpj_cpf_socio')) %>% 
#   filter(!(txNomeParlamentar=='Glaustin da Fokus' & nome_socio=='PAULO ROBERTO EVANGELISTA DE LIMA')) %>% 
#   filter(!(txNomeParlamentar=='MARCONDES GADELHA' & nome_socio=='FLAVIA MARIA BEZERRA DE MENEZES')) %>% 
#   filter(!(txNomeParlamentar=='Roman' & nome_socio=='NICOLAS ROTHER GIL')) %>% 
#   filter(!(txNomeParlamentar=='CARLOS ALBERTO CANUTO' & nome_socio=='ANA MARIA ANTUNES AMORIM'))
# 
# #Nome muito diferente, desconsiderar linhas
# 
# 
# df.rep.legal <- df %>%
#   select(txNomeParlamentar,cpf,cnpj8) %>%
#   unique() %>%
#   filter(!is.na(cpf)) %>% 
#   mutate(meio.cpf = str_sub(cpf,4,9)) %>% 
#   inner_join(socios.pf,by = c('cnpj8'='cnpj8','meio.cpf'='cpf_representante_legal'))
# 
# 
# 
# socios2 <- con %>%
#   tbl(sql("select * from socios where 
#           ")) %>%
#   collect()
# #saveRDS(socios,file='socios.RDS')

