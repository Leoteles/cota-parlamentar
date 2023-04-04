require(caret)
require(ggplot2)
require(dplyr)
require(lubridate)
require(solitude)
#require(keras)
setwd("D:/OneDrive - ufmg.br/especializacao ia/15 TCC/tcc01")
rm(list=ls())

# gastos <- readRDS(file = 'cota_parlamentar_preprocessado.RDS')
# #gastos <- readRDS(file = 'mini_cota_parlamentar.RDS')
# #unique(gastos$numAno)
# cnpj.features <- readRDS(file='./cnpj_features.RDS')
# 
# #Adiciona casos suspeitos
# suspeitos <- readRDS('./suspeitos.RDS')
# 
# 
# d <- gastos %>%
#   left_join(cnpj.features,by = c('cnpj8'='cnpj8')) %>%
#   mutate(idade.empresa = difftime(datEmissao,data_inicio_atividade,units = 'days'),
#          idade.empresa = as.numeric(idade.empresa)) %>%
#   filter(!is.na(vlrDocumento)) %>% #Retira linhas sem o valor do documento
#   filter(vlrDocumento-vlrGlosa>=vlrLiquido) %>% #Retira linhas em que o valor do documento, glosa e valor liguido são incompativeis
#   mutate(suspeitos = idGasto %in% suspeitos$idGasto)#Adiciona casos supeitos conhecidos
# 
# features <- d %>%
#   select(vlrDocumento,
#          capital_social_empresa,
#          idade.empresa,
#          sessao.leg,
#          idadeGasto,
#          numAno,numMes,
#          flag.CNPJCPF,
#          empresa_propria,
#          empresa_socio,
#          opcao_pelo_mei,
#          porte_empresa,
#          codTipo
#          ) %>%
#   mutate(sessao.leg = as.numeric(sessao.leg),
#          numMes = as.numeric(numMes),
#          numAno = as.numeric(numAno),
#          #flag.CNPJCPF = case_when(flag.CNPJCPF=='CNPJ'~1,T~0),
#          #empresa_socio = as.numeric(empresa_socio),
#          #empresa_propria = as.numeric(empresa_propria),
#          opcao_pelo_mei = case_when(opcao_pelo_mei=='S'~1,T~0)
#          )
# 
# dummies <- dummyVars(~ ., data = features)
# #x <- head(predict(dummies, newdata = features))
# features <- predict(dummies, newdata = features) %>% as.data.frame()
# 
# y <- d %>%
#   select(idGasto,vlrDocumento,vlrGlosa,vlrLiquido,vlrRestituicao,porc.glosa,houve.glosa,suspeitos) %>%
#   #Define glosa significativa quando ha 50% ou mais de glosa
#   mutate(glosa.sig = case_when(is.na(porc.glosa) | (porc.glosa<0.9) ~ 'N',T~'S'))
# 
# #A principio, retira missing cases
# idx <- complete.cases(features)
# #y2 <- y[!idx,] #conferir se nao ha casos suspeitos com missing features
# #any(y2$suspeitos==T)
# features <- features[idx,]
# y <- y[idx,]
# 

# #Reduz bases para apenas anos com suspeitos
# anos <- cbind(y,features) %>%
#   select(numAno,suspeitos) %>%
#   group_by(numAno) %>% 
#   summarise(qtd=sum(suspeitos))
# anos.com.suspeitos <- anos$numAno[anos$qtd>0]
# i.anos <- features$numAno%in% anos.com.suspeitos
# features <- features[i.anos,]
# y <- y[i.anos,]
# 
# 
# save(y,features,file='./temp04.RData')

load('./temp04.RData')

###################################
## Isolation Forest


i <- sample(nrow(features))#Ordem aleatoria
features <- features[i,]
y <- y[i,]


# run the isolationForest algorithm
NUMTREE <- 100
PSI <- 256
iforest <- isolationForest$new(num_trees = NUMTREE,sample_size = PSI) # initiates a solitude object
iforest$fit(features) # fits an isolation forest for the dataframe

#score <- iforest$predict(features) %>% arrange(id)
score1 <- iforest$predict(features[1:1500000,])
score2 <- iforest$predict(features[-1:1500000,])
score <- rbind(score1,score2) %>% arrange(id)

#plot(density(score$anomaly_score)) # density plot


# quantiles of anomaly scores
quantile(score$anomaly_score
         , probs = seq(0.5, 1, length.out = 11)
)
limite <- quantile(score$anomaly_score,probs = .95)

analise <- cbind(y,anomaly_score = score$anomaly_score) %>% mutate(outlier = anomaly_score>limite)

# analise %>% ggplot() +
#   geom_boxplot(aes(y = anomaly_score,x = glosa.sig))+
#   coord_cartesian(ylim=c(0,0.3))

analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = suspeitos))+
  coord_cartesian(ylim=c(0,0.3))

# analise %>% ggplot() +
#   geom_boxplot(aes(y = anomaly_score,x = glosa.ou.suspeito))+
#   coord_cartesian(ylim=c(0,0.3))


#save.image('temp01.RData')

media.e.mediana <- analise %>% group_by(suspeitos) %>% summarise(mean(anomaly_score),median(anomaly_score))





##############################################################
# !!!treinar apenas os dados inliers, normais. retirar os conhecidos outliers
# !!!levar vlr ao valor atual corrigido c a inflacao?
# !!!Solucao p dados categoricos
# !!!Colocar flag de cpf e de cnpj invalido
# !!!Buscar casos caricatos para testar detecção
# !!!tuning do autoencoder