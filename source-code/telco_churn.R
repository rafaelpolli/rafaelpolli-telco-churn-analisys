library(ggplot2)
library(skimr)
library(gridExtra)
library(GGally)
library(fastDummies)
library(dummies)
library(caret)

#Importando dataset
setwd("E:/FIA/telco-customer-churn")
telco <- read.table(file = "WA_Fn-UseC_-Telco-Customer-Churn.csv", header = T, sep = ",")

head(telco)
str(telco)

#Ajustando categoria da variável SeniorCitizen
telco$SeniorCitizen <- as.factor(telco$SeniorCitizen)
telco$customerID <- NULL

#Visualizando informações do dataset
summary(telco)
skim(telco)

##Variáveis quantitativas
#Valores pagos e tempo de serviço
grid.arrange(ggplot(telco, aes(y = MonthlyCharges)) + 
               geom_boxplot(),
             ggplot(telco, aes(y = TotalCharges)) + 
               geom_boxplot(),
             ggplot(telco, aes(y = tenure)) + 
               geom_boxplot(), ncol=3)

grid.arrange(ggplot(telco, aes(x = MonthlyCharges, fill = Churn)) + 
               geom_histogram(),
             ggplot(telco, aes(x = TotalCharges, fill = Churn)) + 
               geom_histogram(),
             ggplot(telco, aes(x = tenure, fill = Churn)) + 
               geom_histogram(), ncol=3)

#Correlação
ggcorr(telco)

##Variáveis qualitativas
#Informações sobre o clinte
grid.arrange(ggplot(telco, aes(x=gender, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=SeniorCitizen, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=Partner, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=Dependents, fill=Churn))+ geom_bar(position = "dodge"), ncol=2)

#Serviços de telefone
grid.arrange(ggplot(telco, aes(x=PhoneService, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=MultipleLines, fill=Churn))+ geom_bar(position = "dodge"), ncol=2)

#Serviços de internet
grid.arrange(ggplot(telco, aes(x=InternetService, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=OnlineSecurity, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=OnlineBackup, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=DeviceProtection, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=TechSupport, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=StreamingTV, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=StreamingMovies, fill=Churn))+ geom_bar(position = "dodge"), ncol=3)

#Pagamento e contrato
grid.arrange(ggplot(telco, aes(x=Contract, fill=Churn))+ geom_bar(position = "dodge"),
             ggplot(telco, aes(x=PaperlessBilling, fill=Churn))+ geom_bar(position = "dodge"), ncol=2)

ggplot(telco, aes(x=PaymentMethod, fill=Churn))+ geom_bar(position = "dodge")


#Churn
ggplot(telco, aes(x=Churn, fill=Churn))+ geom_bar(position = "dodge")

##Criando Modelo
options(scipen=999)
set.seed(123)

table(telco$Churn)

#Balanceando Base
Base1 <- subset(telco, telco$Churn=="Yes")
Base0 <- subset(telco, telco$Churn=="No")
dt = sort(sample(nrow(Base0), 1869))
Amostra_0 <- Base0[dt,]
base_balanceada = rbind(Base1, Amostra_0)
table(base_balanceada$Churn)

#Modelo glm
modelo <- glm(Churn ~
                .,
              family=binomial(link='logit'),data=base_balanceada)
summary(modelo)

pred = predict(modelo,telco, type = "response")

#Cria uma nova base com a base original+valores preditos
base_final = cbind(telco, pred)
View(base_final)

#Criar a resposta final usando o ponto de corte
base_final$resposta <- as.factor(ifelse(base_final$pred>0.7, "Yes", "No"))
View(base_final)

#Tabela de classificação
table(base_final$Churn,base_final$resposta)

#Removendo variáveis utilizando 90% de confiança

##Modelo2
base_balanceada2 <- dummy_cols(base_balanceada, 
                               select_columns = c("MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection",
                                                         "TechSupport", "StreamingTV", "StreamingMovies ", "Contract", "PaymentMethod"),
                                                          remove_first_dummy = TRUE)
base_balanceada2 <- base_balanceada2[, -c(7,8,9,10,11,12,13,14,15,17)]
names(base_balanceada2) <- gsub(" ", "", names(base_balanceada2))
names(base_balanceada2) <- gsub("\\(", "", names(base_balanceada2))
names(base_balanceada2) <- gsub(")", "", names(base_balanceada2))
str(base_balanceada2)


modelo2 <- glm(Churn ~
                 . - StreamingTV_Yes,
               family=binomial(link='logit'), data=base_balanceada2)
summary(modelo2)

##Modelo3
modelo3 <- glm(Churn ~
                 . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic,
               family=binomial(link='logit'), data=base_balanceada2)
summary(modelo3)

#Modelo4
modelo4 <- glm(Churn ~
                 . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes,
               family=binomial(link='logit'), data=base_balanceada2)
summary(modelo4)

#Modelo5
modelo5 <- glm(Churn ~
                 . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
               - PaymentMethod_Mailedcheck,
               family=binomial(link='logit'), data=base_balanceada2)
summary(modelo5)

#Modelo6
modelo6 <- glm(Churn ~
                 . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
               - PaymentMethod_Mailedcheck - Partner,
               family=binomial(link='logit'), data=base_balanceada2)
summary(modelo6)

#Modelo7
modelo7 <- glm(Churn ~
                 . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
               - PaymentMethod_Mailedcheck - Partner - InternetService_No,
               family=binomial(link='logit'), data=base_balanceada2)
summary(modelo7)

#Modelo8
modelo8 <- glm(Churn ~
                 . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
               - PaymentMethod_Mailedcheck - Partner - InternetService_No - OnlineSecurity_Nointernetservice,
               family=binomial(link='logit'), data=base_balanceada2)
summary(modelo8)

#Modelo9
modelo9 <- glm(Churn ~
                 . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
               - PaymentMethod_Mailedcheck - Partner - InternetService_No - OnlineSecurity_Nointernetservice
               - OnlineBackup_Nointernetservice,
               family=binomial(link='logit'), data=base_balanceada2)
summary(modelo9)

#Modelo10
modelo10 <- glm(Churn ~
                 . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
               - PaymentMethod_Mailedcheck - Partner - InternetService_No - OnlineSecurity_Nointernetservice
               - OnlineBackup_Nointernetservice - DeviceProtection_Nointernetservice,
               family=binomial(link='logit'), data=base_balanceada2)
summary(modelo10)

#Modelo11
modelo11 <- glm(Churn ~
                  . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
                - PaymentMethod_Mailedcheck - Partner - InternetService_No - OnlineSecurity_Nointernetservice
                - OnlineBackup_Nointernetservice - DeviceProtection_Nointernetservice - TechSupport_Nointernetservice,
                family=binomial(link='logit'), data=base_balanceada2)
summary(modelo11)

#Modelo12
modelo12 <- glm(Churn ~
                  . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
                - PaymentMethod_Mailedcheck - Partner - InternetService_No - OnlineSecurity_Nointernetservice
                - OnlineBackup_Nointernetservice - DeviceProtection_Nointernetservice - TechSupport_Nointernetservice
                - StreamingTV_Nointernetservice,
                family=binomial(link='logit'), data=base_balanceada2)
summary(modelo12)

#Modelo13
modelo13 <- glm(Churn ~
                  . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
                - PaymentMethod_Mailedcheck - Partner - InternetService_No - OnlineSecurity_Nointernetservice
                - OnlineBackup_Nointernetservice - DeviceProtection_Nointernetservice - TechSupport_Nointernetservice
                - StreamingTV_Nointernetservice - gender,
                family=binomial(link='logit'), data=base_balanceada2)
summary(modelo13)

#Modelo14
modelo14 <- glm(Churn ~
                  . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
                - PaymentMethod_Mailedcheck - Partner - InternetService_No - OnlineSecurity_Nointernetservice
                - OnlineBackup_Nointernetservice - DeviceProtection_Nointernetservice - TechSupport_Nointernetservice
                - StreamingTV_Nointernetservice - gender - MultipleLines_Yes,
                family=binomial(link='logit'), data=base_balanceada2)
summary(modelo14)

#Modelo15
modelo15 <- glm(Churn ~
                  . - StreamingTV_Yes - PaymentMethod_Creditcardautomatic - TechSupport_Yes
                - PaymentMethod_Mailedcheck - Partner - InternetService_No - OnlineSecurity_Nointernetservice
                - OnlineBackup_Nointernetservice - DeviceProtection_Nointernetservice - TechSupport_Nointernetservice
                - StreamingTV_Nointernetservice - gender - MultipleLines_Yes - InternetService_Fiberoptic,
                family=binomial(link='logit'), data=base_balanceada2)
summary(modelo15)

#Testando Modelo

telco2 <- dummy_cols(telco, 
                     select_columns = c("MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection",
                     "TechSupport", "StreamingTV", "StreamingMovies ", "Contract", "PaymentMethod"),
                     remove_first_dummy = TRUE)

telco2 <- telco2[, -c(7,8,9,10,11,12,13,14,15,17)]
names(telco2) <- gsub(" ", "", names(telco2))
names(telco2) <- gsub("\\(", "", names(telco2))
names(telco2) <- gsub(")", "", names(telco2))
str(telco2)

pred = predict(modelo15, telco2, type = "response")

#Cria uma nova base com a base original+valores preditos
base_final = cbind(telco, pred)
View(base_final)

#Criar a resposta final usando o ponto de corte
base_final$resposta <- as.factor(ifelse(base_final$pred>0.7, "Yes", "No"))
View(base_final)

#Tabela de classificação
confusionMatrix(base_final$Churn, base_final$resposta)

