require(caret)
require(ggplot2)
require(dplyr)
require(lubridate)
require(keras)
#library(iml)
#library(solitude)
setwd("D:/OneDrive - ufmg.br/especializacao ia/15 TCC/tcc01")
rm(list=ls())

#gastos <- readRDS(file = 'cota_parlamentar_preprocessado.RDS')
gastos <- readRDS(file = 'mini_cota_parlamentar.RDS')
#unique(gastos$numAno)
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

#######################

prop <- 0.8
i.train <- sample(nrow(features),prop*nrow(features))
df_train <- features[i.train,]
df_test <- features[-i.train,]

preprocessParams <- df_train %>% preProcess(method=c("range"))
print(preprocessParams)

x_train <- df_train %>%
  predict(object = preprocessParams) %>% as.matrix()

x_test <- df_test %>%
  predict(object = preprocessParams) %>% as.matrix()

x_tudo <- features %>%
  predict(object = preprocessParams) %>% as.matrix()

####Criacao do modelo
model <- keras_model_sequential()
model %>%
  layer_dense(units = 15, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 10, activation = "tanh") %>%
  layer_dense(units = 15, activation = "tanh") %>%
  layer_dense(units = ncol(x_train))

summary(model)

model %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam"
)

checkpoint <- callback_model_checkpoint(
  filepath = "model.hdf5", 
  save_best_only = TRUE, 
  #period = 1,
  save_freq = 'epoch',
  verbose = 1
)

early_stopping <- callback_early_stopping(patience = 5)

model %>% fit(
  x = x_train, 
  y = x_train, 
  epochs = 100, 
  batch_size = 32,
  validation_data = list(x_test, x_test), 
  callbacks = list(checkpoint, early_stopping)
)

# summary(model)
# pred_train <- predict(model, x_train)
# mse_train <- apply((x_train - pred_train)^2, 1, sum)
# pred_test <- predict(model, x_test)
# mse_test <- apply((x_test - pred_test)^2, 1, sum)

pred_tudo <- predict(model, x_tudo)
#mse_tudo <- apply((x_tudo - pred_tudo)^2, 1, sum)

mae <- abs(x_tudo - pred_tudo) %>% rowSums()
mse <- apply((x_tudo - pred_tudo)^2, 1, sum)

score <- data.frame(mse,anomaly_score = mae)

plot(density(score$anomaly_score)) # density plot

# quantiles of anomaly scores
quantile(score$anomaly_score
         , probs = seq(0.5, 1, length.out = 11)
)
limite <- quantile(score$anomaly_score,probs = .95)

analise <- cbind(features,y,anomaly_score = score$anomaly_score) %>% mutate(outlier = anomaly_score>limite)

analise <- analise %>% mutate(glosa_cat = case_when(porc.glosa<=0.5 ~'menor_que_05',
                                                    porc.glosa<0.9 ~'05_a_09',
                                                    porc.glosa<0.99 ~'09_a_099',
                                                    T ~'maior_que_099'))
analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.99))+
  coord_cartesian(ylim=c(0,0.01))


analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = glosa_cat))

analise %>% ggplot()+
  geom_density(aes(x = anomaly_score,fill=outlier),alpha=0.3)+
  scale_x_log10()

analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = houve.glosa))

analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.5))

analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.9))+
  coord_cartesian(ylim=c(0,0.05))

analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.99))







#######################

# plot(density(score$anomaly_score)) # density plot
# 
# # quantiles of anomaly scores
# quantile(score$anomaly_score
#          , probs = seq(0.5, 1, length.out = 11)
# )
# limite <- quantile(score$anomaly_score,probs = .95)
# 
# analise <- cbind(features,y,score) %>% mutate(outlier = anomaly_score>limite)
# 
# 
# analise %>% ggplot() +
#   geom_boxplot(aes(y = anomaly_score,x = houve.glosa))
# 
# analise %>% ggplot() +
#   geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.5))
# 
# analise %>% ggplot() +
#   geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.9))
# 
# analise %>% ggplot() +
#  geom_boxplot(aes(y = anomaly_score,x = porc.glosa>=0.99))




##############################################################
!!!treinar apenas os dados inliers, normais. retirar os conhecidos outliers
!!!levar vlr ao valor atual corrigido c a inflacao?
!!!Solucao p dados categoricos
!!!Colocar flag de cpf e de cnpj invalido
!!!Buscar casos caricatos para testar detecção
!!!tuning do autoencoder