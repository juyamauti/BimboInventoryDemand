# Grupo Bimbo Inventory Demand
# Dataset: https://www.kaggle.com/c/grupo-bimbo-inventory-demand
# The goal in this project is to create a develop a model to 
# accurately forecast inventory demand based on historical sales data

# loading necessary packages
library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
library(reshape2)
library(MLmetrics)

# loading datasets
df_cliente <- fread("cliente_tabla.csv", header = TRUE, sep = ",", encoding = "UTF-8")
head(df_cliente)
dim(df_cliente)

df_produto <- fread("producto_tabla.csv", header = TRUE, sep = ",", encoding = "UTF-8")
head(df_produto)
dim(df_produto)

df_town <- fread("town_state.csv", header = TRUE, sep = ",", encoding = "UTF-8")
head(df_town)
dim(df_town)

# loading train dataset
df_train <- fread("train.csv", header = TRUE, sep = ",", encoding = "UTF-8")
head(df_train)
dim(df_train)

# the df_train dataset has 74.180.464 observations and 11 variables

# Since the train dataset is too big, we're going to get a 100.000 rows' sample
df_sample <- sample_n(df_train, size = 100000)
dim(df_sample)

# Removing df_train object
rm(df_train)

# Saving the sample into "AmostraBimbo.csv" so we don't have to load train dataset again
write.csv(df_sample, "AmostraBimbo.csv")

# Reading the sample file
df_sample <- fread("AmostraBimbo.csv", header = TRUE, sep = ",", encoding = "UTF-8")
View(df_sample)

# Removing column #1 with row number
df_sample$V1 <- NULL

# Convert df_sample to dataframe
class(df_sample)
df_sample <- as.data.frame(df_sample)

# EDA

# Checking dataset statistics
summary(df_sample)

# Checking datatypes
str(df_sample)

# Checking missing values
colSums(is.na(df_sample))

# There are no missing values in this sample dataset

# Checking "Semana" distribution
ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Semana), binwidth = 0.5)

# Checking "Agencia_ID" distribution
ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Agencia_ID), binwidth = 200)

#Checking "Canal_ID" distribution
ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Canal_ID), binwidth = 0.5)

# Other distributions
ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Venta_uni_hoy), binwidth = 5)

ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Dev_uni_proxima), binwidth = 10)

ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Dev_proxima), binwidth = 100)

ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Demanda_uni_equil), binwidth = 100)

# Checking for outliers
ggplot(data = df_sample, mapping = aes(y = Agencia_ID)) +
  geom_boxplot()

ggplot(data = df_sample, mapping = aes(y = Demanda_uni_equil)) +
  geom_boxplot()

# Selecting the item that is over 3000
df_sample[df_sample$Demanda_uni_equil > 3000,]
df_town[df_town$Agencia_ID == 2030]

ggplot(data = df_sample, mapping = aes(y = Venta_uni_hoy)) +
  geom_boxplot()

ggplot(data = df_sample, mapping = aes(y = Venta_hoy)) +
  geom_boxplot()

# It seems like observation 3885 is an outlier so we are going to remove this line
df_sample <- df_sample[-c(3885), ]

# Checking correlation between variables
col_num <- sapply(df_sample, is.numeric)
data_cor <- cor(df_sample[,col_num])
melted_cormat <- melt(data_cor)
head(melted_cormat)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = round(value,2)), color = "black", size = 2)


ggplot(data = df_sample) +
  geom_point(mapping = aes(x = Agencia_ID, y = Demanda_uni_equil))

ggplot(data = df_sample) +
  geom_point(mapping = aes(x = Venta_uni_hoy, y = Demanda_uni_equil))

ggplot(data = df_sample) +
  geom_point(mapping = aes(x = Venta_hoy, y = Demanda_uni_equil))

# Using dplyr to group/join data and get some insights

# Top 10 sum of "Demanda_uni_equil" by State
df_sample %>%
  inner_join(df_town, by = 'Agencia_ID') %>%
  select(State, Demanda_uni_equil) %>%
  group_by(State) %>%
  summarize(ave_Demanda = sum(Demanda_uni_equil)) %>%
  arrange(desc(ave_Demanda))

# Top 10 sum of "Demanda_uni_equil" by Town
df_sample %>%
  inner_join(df_town, by = 'Agencia_ID') %>%
  select(Town, Demanda_uni_equil) %>%
  group_by(Town) %>%
  summarize(ave_Demanda = sum(Demanda_uni_equil)) %>%
  arrange(desc(ave_Demanda))

# Top 10 sum of "Demanda_uni_equil" by NombreCliente
df_sample %>%
  inner_join(df_cliente, by = 'Cliente_ID') %>%
  select(NombreCliente, Demanda_uni_equil) %>%
  group_by(NombreCliente) %>%
  summarize(ave_Demanda = sum(Demanda_uni_equil)) %>%
  arrange(desc(ave_Demanda))

# Top 10 sum of "Demanda_uni_equil" by NombreProducto
df_sample %>%
  inner_join(df_produto, by = 'Producto_ID') %>%
  select(NombreProducto, Demanda_uni_equil) %>%
  group_by(NombreProducto) %>%
  summarize(ave_Demanda = sum(Demanda_uni_equil)) %>%
  arrange(desc(ave_Demanda))

# Searching for distinct values in town
unique(df_town$State)

# Most important variables for the model using varImp
modelo <- train(Demanda_uni_equil ~ ., data = df_sample, method = "lm")
vImp <- varImp(modelo) 

# In this plot we can see variable importance for predicting Demanda_uni_equil
ggplot(vImp) +
  geom_bar(stat='identity')

vImp

# Separating data into train/test
linha <- sample(1:nrow(df_sample), 0.7 * nrow(df_sample))
df_train <- df_sample[linha,]
df_test <- df_sample[-linha,]

dim(df_train)
dim(df_test)

# Normalizing train dataset
df_n <- scale(df_train[,-11])
df_train_normalized <- as.data.frame(cbind(df_n, df_train$Demanda_uni_equil))
rm(df_n)
colnames(df_train_normalized)[11] <- "Demanda_uni_equil"

View(df_train_normalized)

# Normalizing test dataset
df_n2 <- scale(df_test[,-11])
df_test_normalized <- as.data.frame(cbind(df_n2, df_test$Demanda_uni_equil))
rm(df_n2)
colnames(df_test_normalized)[11] <- "Demanda_uni_equil"

View(df_test_normalized)

# Creating the model with all variables and without pre processing
modelo_v1 <- lm(Demanda_uni_equil ~ ., data = df_train)

summary(modelo_v1)

previsao1 <- predict(modelo_v1, df_test)

MSE1 = MSE(y_pred=previsao1, y_true=df_test$Demanda_uni_equil)
MAE1 = MAE(y_pred=previsao1, y_true=df_test$Demanda_uni_equil)
RMSE1 = RMSE(y_pred=previsao1, y_true=df_test$Demanda_uni_equil)

#RMSLE
predicted_value = abs(previsao1) 
actual_value = abs(df_test$Demanda_uni_equil)

SLE = (log(predicted_value + 1) - log(actual_value+ 1))^2

RMSLE = sqrt(mean(SLE))

Score1 = 1/(1+exp(RMSLE))

# Creating a new dataframe with the results
result <- data.frame("modelo_v1", "all variables + no preprocessing", summary(modelo_v1)$r.squared, MAE1, MSE1, RMSE1, Score1)
names(result) <-c("Model", "Variables", "R-squared", "MAE", "MSE", "RMSE", "RMSLE")

View(result)

# Creating the model2 with all variables + normalized data
modelo_v2 <- lm(Demanda_uni_equil ~ ., data = df_train_normalized)

summary(modelo_v2)

previsao2 <- predict(modelo_v2, df_test_normalized)

MSE2 = MSE(y_pred=previsao2, y_true=df_test_normalized$Demanda_uni_equil)
MAE2 = MAE(y_pred=previsao2, y_true=df_test_normalized$Demanda_uni_equil)
RMSE2 = RMSE(y_pred=previsao2, y_true=df_test_normalized$Demanda_uni_equil)

#RMSLE
predicted_value = abs(previsao2) 
actual_value = abs(df_test_normalized$Demanda_uni_equil)

SLE = (log(predicted_value + 1) - log(actual_value+ 1))^2

RMSLE = sqrt(mean(SLE))

Score2 = 1/(1+exp(RMSLE))

# Creating a new dataframe with the results
result2 <- data.frame("modelo_v2", "all variables + normalized data", summary(modelo_v2)$r.squared, MAE2, MSE2, RMSE2, Score2)
names(result2) <-c("Model", "Variables", "R-squared", "MAE", "MSE", "RMSE", "RMSLE")
newresult <- rbind(result, result2)
View(newresult)

# Creating the model3 with top 3 variables and pre processing
modelo_v3 <- lm(Demanda_uni_equil ~ Venta_uni_hoy +
                  Dev_uni_proxima +
                  Dev_proxima, data = df_train_normalized)

previsao3 <- predict(modelo_v3, df_test_normalized)

MSE3 = MSE(y_pred=previsao3, y_true=df_test_normalized$Demanda_uni_equil)
MAE3 = MAE(y_pred=previsao3, y_true=df_test_normalized$Demanda_uni_equil)
RMSE3 = RMSE(y_pred=previsao3, y_true=df_test_normalized$Demanda_uni_equil)

#RMSLE
predicted_value3 = abs(previsao3) 
actual_value3 = abs(df_test$Demanda_uni_equil)

SLE3 = (log(predicted_value3 + 1) - log(actual_value3+ 1))^2
RMSLE3 = sqrt(mean(SLE3))
Score3 = 1/(1+exp(RMSLE3))

result3 <- data.frame("modelo_v3", "top 3 variables + normalized data", summary(modelo_v3)$r.squared, MAE3, MSE3, RMSE3, Score3)
View(newresult)
names(result3) <-c("Model", "Variables", "R-squared", "MAE", "MSE", "RMSE", "RMSLE")
newresult <- rbind(result, result2, result3)

# Creating the model4 with top 1 variables and pre processing
modelo_v4 <- lm(Demanda_uni_equil ~ Venta_uni_hoy, data = df_train_normalized)

previsao4 <- predict(modelo_v4, df_test_normalized)

MSE4 = MSE(y_pred=previsao4, y_true=df_test_normalized$Demanda_uni_equil)
MAE4 = MAE(y_pred=previsao4, y_true=df_test_normalized$Demanda_uni_equil)
RMSE4 = RMSE(y_pred=previsao4, y_true=df_test_normalized$Demanda_uni_equil)

#RMSLE
predicted_value4 = abs(previsao4) 
actual_value4 = abs(df_test$Demanda_uni_equil)

SLE4 = (log(predicted_value4 + 1) - log(actual_value4+ 1))^2
RMSLE4 = sqrt(mean(SLE4))
Score4 = 1/(1+exp(RMSLE4))

result4 <- data.frame("modelo_v4", "top 1 variable + normalized data", summary(modelo_v4)$r.squared, MAE4, MSE4, RMSE4, Score4)
names(result4) <- c("Model", "Variables", "R-squared", "MAE", "MSE", "RMSE", "RMSLE")
newresult <- rbind(result, result2, result3, result4)
View(newresult)

ggplot(data = newresult) +
  geom_point(mapping = aes(x = newresult$`Model`, y = newresult$`R-squared`))

ggplot(data = newresult) +
  geom_point(mapping = aes(x = newresult$`Model`, y = newresult$`MAE`))

ggplot(data = newresult) +
  geom_point(mapping = aes(x = newresult$`Model`, y = newresult$`RMSE`))