require(ggplot2)
require(dplyr)
require(lubridate)
rm(list=ls())

gastos <- readRDS(file = 'cota_parlamentar_preprocessado.RDS')

cnpj.features <- readRDS(file='./cnpj_features.RDS')

d <- gastos %>%
  left_join(cnpj.features,by = c('cnpj8'='cnpj8')) %>% 
  mutate(idade.empresa = difftime(datEmissao,data_inicio_atividade,units = 'days'),
         idade.empresa = as.numeric(idade.empresa),
         txNomeParlamentar = toupper(txNomeParlamentar))

rm(gastos,cnpj.features)

#https://g1.globo.com/distrito-federal/noticia/apos-ser-flagrado-por-app-deputado-devolve-a-camara-r-727-por-13-refeicoes-no-mesmo-dia.ghtml
# Deputado Celso Maldener, que apresentou 12 notas fiscais de refei��es consumidas em um �nico dia e em um unico estabelecimento, em valores que variam entre R$23,00 e R$87.78, totalizando R$ 727,78.
maldener <- d %>%
  filter(txNomeParlamentar=='CELSO MALDANER' &
           datEmissao=='2011-09-05'&
           codTipo=='13.0' &
           txtCNPJCPF=='00984060000277')


maldener$vlrRestituicao %>% sum(na.rm = T)

#https://medium.com/data-science-brigade/o-que-a-resposta-do-dep-marcon-%C3%A0-rosie-t%C3%AAm-a-nos-dizer-sobre-o-trabalho-da-serenata-de-amor-c7f898a4655f
marcon <- d %>% 
  filter(txNomeParlamentar=='MARCON' & datEmissao=='2017-02-26')


#https://www.jornaldocomercio.com/_conteudo/2017/05/politica/562699-parlamentares-federais-gauchos-esclarecem-despesas-fiscalizadas-por-levantamento-eletronico.html
assismelo <-  d %>% 
  filter(txNomeParlamentar=='ASSIS MELO' & vlrDocumento==103.6)


#https://www.em.com.br/app/noticia/politica/2013/01/13/interna_politica,343003/deputados-utilizam-verba-de-custeio-para-pagar-gastos-de-campanha.shtml
ruicarneiro <- d %>% 
  filter(txNomeParlamentar=='RUY CARNEIRO'& numAno=='2012' & codTipo=='15.0')


hiran <- d %>% 
  filter(txNomeParlamentar=='HIRAN GON�ALVES' & txtCNPJCPF=='13182427000108')


#https://istoe.com.br/rosa-weber-autoriza-inquerito-contra-utilizacao-irregular-de-cota-parlamentar/
atosdois <- d %>% 
  filter(txtCNPJCPF=='13182427000108')
atosdois %>% select(txNomeParlamentar,sgPartido) %>% unique()


#https://pnoticias.com.br/noticia/politica/238862-deputados-usam-cota-parlamentar-para-alugar-carros-de-empresas-investigadas-por-fraude
brlocadora <- d %>% filter(txtCNPJCPF=='10644834000193')


#https://www.otempo.com.br/politica/andre-moura-e-outros-29-deputados-sao-investigados-por-fraudes-1.1389605
cloud_technology <- d %>% filter(txtCNPJCPF=='17589509000114')


suspeitos <- bind_rows(
  assismelo %>% select(idGasto) %>% mutate(caso = 'assismelo'),
  atosdois %>% select(idGasto) %>% mutate(caso = 'atosdois'),
  brlocadora %>% select(idGasto) %>% mutate(caso = 'brlocadora'),
  cloud_technology %>% select(idGasto) %>% mutate(caso = 'cloud_technology'),
  hiran %>% select(idGasto) %>% mutate(caso = 'hiran'),
  maldener %>% select(idGasto) %>% mutate(caso = 'maldener'),
  marcon %>% select(idGasto) %>% mutate(caso = 'marcon'),
  ruicarneiro %>% select(idGasto) %>% mutate(caso = 'ruicarneiro')
)



saveRDS(suspeitos,file='./suspeitos.RDS')






# 
# igpm <- read.table('inflacao.csv',sep = '\t',header = T)
# 
# periodo <- igpm[1:166,]
# x <- 1+((periodo$indice)/100)
# total <- 1
# for (i in 1:166){
#   total <- total*x[i]
# }
# 
# 7075 * 2.268
# 16047/7075