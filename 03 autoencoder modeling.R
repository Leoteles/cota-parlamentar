require(caret)
require(ggplot2)
require(dplyr)
require(lubridate)
require(keras)
#library(iml)
#library(solitude)
rm(list=ls())

gastos <- readRDS(file = 'cota_parlamentar_preprocessado.RDS')
#gastos <- readRDS(file = 'mini_cota_parlamentar.RDS')
#unique(gastos$numAno)
cnpj.features <- readRDS(file='./cnpj_features.RDS')


#gastos <- gastos[1:1000000,]

d <- gastos %>%
  left_join(cnpj.features,by = c('cnpj8'='cnpj8')) %>% 
  mutate(idade.empresa = difftime(datEmissao,data_inicio_atividade,units = 'days'),
         idade.empresa = as.numeric(idade.empresa)) %>% 
  filter(!is.na(vlrDocumento)) %>% #Retira linhas sem o valor do documento
  filter(vlrDocumento-vlrGlosa>=vlrLiquido)#Retira linhas em que o valor do documento, glosa e valor liguido s�o incompativeis
  

features <- d %>%
  select(vlrDocumento,
         capital_social_empresa,
         idade.empresa,
         sessao.leg,
         idadeGasto,
         numAno,numMes,
         flag.CNPJCPF,
         empresa_propria,
         empresa_socio,
         opcao_pelo_mei,
         porte_empresa,
         codTipo
         ) %>% 
  mutate(sessao.leg = as.numeric(sessao.leg),
         numMes = as.numeric(numMes),
         numAno = as.numeric(numAno),
         #flag.CNPJCPF = case_when(flag.CNPJCPF=='CNPJ'~1,T~0),
         #empresa_socio = as.numeric(empresa_socio),
         #empresa_propria = as.numeric(empresa_propria),
         opcao_pelo_mei = case_when(opcao_pelo_mei=='S'~1,T~0)
         )

dummies <- dummyVars(~ ., data = features)
x <- head(predict(dummies, newdata = features))
features <- predict(dummies, newdata = features) %>% as.data.frame()

y <- d %>%
  select(vlrDocumento,vlrGlosa,vlrLiquido,vlrRestituicao,porc.glosa,houve.glosa) %>%
  #Define glosa significativa quando ha 50% ou mais de glosa
  mutate(glosa.sig = case_when(is.na(porc.glosa) | (porc.glosa<0.9) ~ 'N',
                               T~'S'))
#Variavel de saida sera glosa significativa
y <- y$glosa.sig

#A principio, retira missing cases
idx <- complete.cases(features)
features <- features[idx,]
y <- y[idx]

####################################
#Divide conjuntos anormais e normais
i.anormal <- y=='S'
sum(i.anormal)

features.anormal <-features[i.anormal,]
y.anormal <- y[i.anormal]
#Prepara dados para o treinamento (apenas com dados normais)
features <- features[!i.anormal,]
y <- y[!i.anormal]


rm(cnpj.features,d,gastos)
#######################
#Divide os dados em grupo de treino, validacao e teste
split_train_test_val <- function(N,prob.train){
  i <- 1:N
  i.train <- sample(N,prob.train*N)
  i.val.test <- i[-i.train]
  i.val <- sample(i.val.test,length(i.val.test)/2)
  i.test <- setdiff(i.val.test,i.val)
  return(list(train = i.train,val = i.val,test = i.test))
}


i <- split_train_test_val(nrow(features),prob.train = 0.6)

df.train <- features[i$train,]
df.val <- features[i$val,]
#Conjunto de testes ter� tamb�m os dados anormais
df.test <- rbind(features[i$test,],rbind(features.anormal))
y.test <-  c(y[i$test],y.anormal)


preprocessParams <- df.train %>% preProcess(method=c("range"))
print(preprocessParams)

x_train <- df.train %>%
  predict(object = preprocessParams) %>% as.matrix()

x_val <- df.val %>%
  predict(object = preprocessParams) %>% as.matrix()

x_test <- df.test %>%
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
  epochs = 20, 
  batch_size = 32,
  validation_data = list(x_val, x_val), 
  callbacks = list(checkpoint, early_stopping)
)



# summary(model)
# pred_train <- predict(model, x_train)
# mse_train <- apply((x_train - pred_train)^2, 1, sum)
# pred_test <- predict(model, x_test)
# mse_test <- apply((x_test - pred_test)^2, 1, sum)

pred_test <- predict(model, x_test)


mae <- abs(x_test - pred_test) %>% rowSums()
mse <- apply((x_test - pred_test)^2, 1, sum)

score <- data.frame(mse,anomaly_score = mae)

plot(density(score$anomaly_score)) # density plot

# quantiles of anomaly scores
quantile(score$anomaly_score
         , probs = seq(0.5, 1, length.out = 11)
)
limite <- quantile(score$anomaly_score,probs = .95)

analise <- cbind(df.test,resposta = y.test,anomaly_score = score$anomaly_score) %>% mutate(outlier = anomaly_score>limite)

analise %>% ggplot() +
  geom_boxplot(aes(y = anomaly_score,x = resposta))+
  coord_cartesian(ylim=c(0,1))


#save.image('temp01.RData')

analise %>% group_by(resposta) %>% summarise(mean(anomaly_score),median(anomaly_score))
#0.5
# resposta `mean(anomaly_score)` `median(anomaly_score)`
# <chr>                    <dbl>                   <dbl>
#   1 N                      0.00378                 0.00334
# 2 S                      0.00555                 0.00356


#0.8
# A tibble: 2 x 3
resposta `mean(anomaly_score)` `median(anomaly_score)`
<chr>                    <dbl>                   <dbl>
  1 N                       0.0100                 0.00921
2 S                       0.0111                 0.00956


##############################################################
