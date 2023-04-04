require(ggplot2)
require(dplyr)
require(lubridate)
#library(iml)
library(solitude)
setwd("D:/OneDrive - ufmg.br/especializacao ia/15 TCC/tcc01")
rm(list=ls())

#gastos <- readRDS(file = 'cota_parlamentar_preprocessado.RDS')
gastos <- readRDS(file = 'mini_cota_parlamentar.RDS')

cnpj.features <- readRDS(file='./cnpj_features.RDS')


#gastos <- gastos[1:1000000,]
d <- gastos %>%
  left_join(cnpj.features,by = c('cnpj8'='cnpj8')) %>% 
  mutate(idade.empresa = difftime(datEmissao,data_inicio_atividade,units = 'days'),
         idade.empresa = as.numeric(idade.empresa))

features <- d %>%
  select(vlrDocumento,capital_social_empresa,idade.empresa,sessao.leg,idadeGasto) %>% 
  mutate(sessao.leg = as.numeric(sessao.leg))

y <- d %>% select(vlrGlosa,porc.glosa,houve.glosa)

# features %>% ggplot()+
#   geom_point(aes(x=capital_social_empresa,y=vlrDocumento,color=idade.empresa))+
#   scale_x_log10()+scale_y_log10()
# 
# require(GGally)
# ggpairs(features)

idx <- complete.cases(features)
features <- features[idx,]
y <- y[idx,]

rm(cnpj.features,d,gastos)
# run the isolationForest algorithm
iforest = isolationForest$new(num_trees = 100,sample_size = 256) # initiates a solitude object
iforest$fit(features) # fits an isolation forest for the dataframe
score <- iforest$predict(features) %>% arrange(id)
plot(density(score$anomaly_score)) # density plot

# quantiles of anomaly scores
quantile(score$anomaly_score
         , probs = seq(0.5, 1, length.out = 11)
)
limite <- quantile(score$anomaly_score,probs = .95)

analise <- cbind(features,y,score) %>% mutate(outlier = anomaly_score>limite)


analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = houve.glosa))

analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.5))

analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.9))

analise %>% ggplot() +
 geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.99))



###############################################################
require(isotree)
iso <- isolation.forest(features, ntrees = 100, nthreads = 1)
anomaly_score <- predict(iso, features)

plot(density(anomaly_score)) # density plot

# quantiles of anomaly scores
quantile(anomaly_score
         , probs = seq(0.5, 1, length.out = 11)
)
limite2 <- quantile(anomaly_score,probs = .95)

analise2 <- cbind(features,y,pred) %>% mutate(outlier = anomaly_score>limite2)


analise2 %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = houve.glosa))

analise2 %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.5))

analise2 %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.9))

analise2 %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.99))





##############################################################
!!!Solucao p dados categoricos
!!!Colocar flag de cpf e de cnpj invalido
!!!Buscar casos caricatos para testar detecção