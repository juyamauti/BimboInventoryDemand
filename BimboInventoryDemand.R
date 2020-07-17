#Grupo Bimbo Inventory Demand

#loading necessary packages
library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
library(corrplot)

#loading datasets
df_cliente <- fread("cliente_tabla.csv", header = TRUE, sep = ",", encoding = "UTF-8")
View(df_cliente) #tem dados repetidos

df_produto <- fread("producto_tabla.csv", header = TRUE, sep = ",", encoding = "UTF-8")
View(df_produto)

df_town <- fread("town_state.csv", header = TRUE, sep = ",", encoding = "UTF-8")
View(df_town)

#loading train dataset
df_train <- fread("train.csv", header = TRUE, sep = ",", encoding = "UTF-8")
View(df_train)

#Since the train dataset is too big, we're going to get a 100.000 rows' sample (74180464 linhas)
df_sample <- sample_n(df_train, size = 100000)
View(df_sample)
rm(df_train)

#saving the sample into "AmostraBimbo.csv" so we don't have to load train dataset again
write.csv(df_sample, "AmostraBimbo.csv")

#reading the sample file
df_sample <- fread("AmostraBimbo.csv", header = TRUE, sep = ",", encoding = "UTF-8")
View(df_sample)

#Removing column #1 with row number
df_sample$V1 <- NULL

#convert df_sample to dataframe
class(df_sample)
df_sample <- as.data.frame(df_sample)

#EDA - análise exploratória
summary(df_sample)
str(df_sample)

#Checking "Semana" distribution
ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Semana), binwidth = 0.5)

#Checking "Agencia_ID" distribution
ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Agencia_ID), binwidth = 200)

#Checking "Canal_ID" distribution
ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Canal_ID), binwidth = 0.5)

#Other distributions
ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Venta_uni_hoy), binwidth = 5)

ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Venta_hoy), binwidth = 0.5)

ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Dev_uni_proxima), binwidth = 100)

ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Dev_proxima), binwidth = 100)

ggplot(data = df_sample) +
  geom_histogram(mapping = aes(x = Demanda_uni_equil), binwidth = 100)

#Checking for outliers
ggplot(data = df_sample, mapping = aes(y = Demanda_uni_equil)) +
  geom_boxplot()

ggplot(data = df_sample, mapping = aes(y = Venta_uni_hoy)) +
  geom_boxplot()

ggplot(data = df_sample, mapping = aes(y = Venta_hoy)) +
  geom_boxplot()

#Checking correlation between variables
col_num <- sapply(df_sample, is.numeric)
data_cor <- cor(df_sample[,col_num])

corrplot(data_cor, method = 'color')

ggplot(data = df_sample) +
  geom_point(mapping = aes(x = Agencia_ID, y = Demanda_uni_equil))

ggplot(data = df_sample) +
  geom_point(mapping = aes(x = Venta_uni_hoy, y = Demanda_uni_equil))

ggplot(data = df_sample) +
  geom_point(mapping = aes(x = Venta_hoy, y = Demanda_uni_equil))

#using dplyr to group/join data and get some insights

#Top 10 sum of "Demanda_uni_equil" by State
df_sample %>%
  inner_join(df_town, by = 'Agencia_ID') %>%
  select(State, Demanda_uni_equil) %>%
  group_by(State) %>%
  summarize(ave_Demanda = sum(Demanda_uni_equil)) %>%
  arrange(desc(ave_Demanda))

#Top 10 sum of "Demanda_uni_equil" by NombreCliente
df_sample %>%
  inner_join(df_cliente, by = 'Cliente_ID') %>%
  select(NombreCliente, Demanda_uni_equil) %>%
  group_by(NombreCliente) %>%
  summarize(ave_Demanda = sum(Demanda_uni_equil)) %>%
  arrange(desc(ave_Demanda))

#Top 10 sum of "Demanda_uni_equil" by NombreProducto
df_sample %>%
  inner_join(df_produto, by = 'Producto_ID') %>%
  select(NombreProducto, Demanda_uni_equil) %>%
  group_by(NombreProducto) %>%
  summarize(ave_Demanda = sum(Demanda_uni_equil)) %>%
  arrange(desc(ave_Demanda))

#Searching for distinct values in town
unique(df_town$State)

#Checking for NA values
sum(is.na(df_sample))

#Most important variables for the model using varImp
modelol_v1 <- train(Demanda_uni_equil ~ ., data = df_sample, method = "lm")
varImp(modelol_v1) 

# Venta_uni_hoy   1.000e+02
# Dev_uni_proxima 3.324e+00
# Dev_proxima     3.950e-01
# Ruta_SAK        7.288e-02
# Canal_ID        5.759e-02
# Venta_hoy       5.455e-02
# Cliente_ID      5.209e-02
# Semana          1.523e-02
# Producto_ID     1.601e-03
# Agencia_ID      0.000e+00

#Normalization
dados_z <- scale(df_sample[,-11]) #não normaliza a coluna target
df_sample <- as.data.frame(cbind(dados_z, df_sample$Demanda_uni_equil))
rm(dados_z)
colnames(df_sample)[11] <- "Demanda_uni_equil"

#Separating data into train/test
linha <- sample(1:nrow(df_sample), 0.7 * nrow(df_sample))
df_train <- df_sample[linha,]
df_test <- df_sample[-linha,]

dim(df_train)
dim(df_test)

#trying to create the model without any pre processing on the data
modelo <- lm(Demanda_uni_equil ~ Venta_uni_hoy 
             + Venta_hoy
             + Dev_uni_proxima
             + Dev_proxima, data = df_train)

previsao2 <- predict(modelo, df_test)

summary(modelo)

#R-squared: 0.99

#RMSLE
predicted_value = abs(previsao2) 
actual_value = abs(df_test$Demanda_uni_equil)

SLE = (log(predicted_value + 1) - log(actual_value+ 1))^2

RMSLE = sqrt(mean(SLE))

Score = 1/(1+exp(RMSLE))

Score
