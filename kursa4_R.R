
#install.packages("randomForest")
#install.packages("ggplot2")

library(randomForest)
library(ggplot2)

test_df <- read.csv("Test_Data_CSV.csv", sep =",")
test_df$Dust= as.factor(test_df$Dust)
train_df <- read.csv("Train_Data_CSV.csv", sep =",")
train_df$Dust= as.factor(train_df$Dust)


print(head(train_df))

print(summary(train_df))

print(str(train_df))

print(head(test_df))

print(summary(test_df))


print(str(test_df))

any_na <- any(is.na(train_df))
print(any_na)

any_na <- any(is.na(test_df))
print(any_na)


plot = ggplot(train_df, aes(x=Time, y=Differential_pressure, group=Data_No, color=factor(Data_No))) +
  geom_line(lwd = I(0.2), lty = 1)+
  labs(x="Время", y="Разность давления", title="Разность давления от времени") +
  scale_color_discrete(name="Номер эксперимента")
print(plot)

plot = ggplot(train_df, aes(x = Time, y = Flow_rate, group = Data_No, color = factor(Data_No)))+
  geom_line(lwd = I(0.2), lty = 1)+
  labs(x="Время", y="Скорость потока через фильтр", title="Скорость от времени") +
  scale_color_discrete(name="Номер эксперимента")
print(plot)

data111 = data.frame(v1 = names(table(train_df$Dust_feed)), v2 = as.numeric(table(train_df$Dust_feed)))
data111$v1 = as.character(round(as.numeric(data111$v1),digits = 2))

plot = ggplot(data111, aes(x = v1, y = v2)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Объем", y = "Число", 
       title = "Распределение объема") +
  theme_classic()
print(plot)


x_reshape <- function(df, columns, sequence_length) {
  data <- as.matrix(df[, columns])  
  num_elements <- nrow(data)
  d = lapply(1:(num_elements - sequence_length - 1), function(start) {
    stop <- start + sequence_length
    sequence_data <- data[start:stop, ]
  })
  d = as.data.frame(d)
  return(d)
}


get_x_slices <- function(df, feature_columns) {
  feature_list <- lapply(unique(df$Data_No), function(i) {
    data_subset <- df[df$Data_No == i, ]
    if (nrow(data_subset) > 124) {
      x_reshape(data_subset, feature_columns, 124)
    }
  })
  feature_array <- do.call(rbind, unlist(feature_list, recursive = FALSE))
  return(feature_array)
}

y_reshape <- function(df, sequence_length, columns = c('Differential_pressure')) {
  data <- as.matrix(df[, columns])
  num_elements <- nrow(data)
  data[(sequence_length + 1):num_elements, ]
}

get_y_slices <- function(df) {
  label_list <- lapply(unique(df$Data_No), function(i) {
    data_subset <- df[df$Data_No == i, ]
    if (nrow(data_subset) > 125) {
      y_reshape(data_subset, 125)
    }
  })
  
  label_array <- unlist(label_list)
  return(label_array)
  
}

evaluate <- function(y_true, y_pred) {
  mse <- mean((y_true - y_pred)^2)
  mae <- mean(abs(y_true - y_pred))
  r2 <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
  cat(" Среднеквадратичная ошибка: ", mse, '\n',
      "Средняя абсолютная ошибка: ", mae, '\n',
      "Коэффициент детерминации : ", r2, '\n')
}

feature_columns <- c('Differential_pressure')

X_train <- as.data.frame(get_x_slices(train_df,feature_columns))
X_test <- as.data.frame(get_x_slices(test_df, feature_columns))
y_train <- as.numeric(get_y_slices(train_df))
y_test <- as.numeric(get_y_slices(test_df))
X_train[] <- lapply(X_train, as.numeric)
X_test[] <- lapply(X_test, as.numeric)


rf = randomForest(X_train, y_train, 
                  ntree = 100, 
                  mtry = 8, 
                  nodesize = 5,
                  importance = TRUE)
print(rf)


y_rf_train <- as.numeric(predict(rf, X_train))
evaluate(y_train, y_rf_train)
ggplot() + 
  geom_line(aes(x = 1:length(y_rf_train), y = y_rf_train, color = "Предсказанная")) + 
  geom_line(aes(x = 1:length(y_train), y = y_train, color = "Реальная")) + 
  labs(x = "Время", y = "Значение", title = "Разница давления (обучающие данные)") + 
  theme_classic() + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("Предсказанная" = "red", "Реальная" = "blue"), name = "")


y_rf_test <- as.numeric(predict(rf, X_test))

evaluate(y_test, y_rf_test)

ggplot() + 
  geom_line(aes(x = 1:length(y_rf_test), y = y_rf_test, color = "Предсказанная")) + 
  geom_line(aes(x = 1:length(y_test), y = y_test, color = "Реальная")) + 
  labs(x = "Время", y = "Значение", title = "Разница давления (тестовые данные)") + 
  theme_classic() + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("Предсказанная" = "red", "Реальная" = "blue"), name = "")



y_pred = c()
y_true = c()

for(i in c(1:50)){
  df1 = subset(test_df, test_df$Data_No == i)
  y1 = df1$Differential_pressure
  y2 = df1$Differential_pressure
  while(y1[length(y1)] < 580){
    start = length(y1) - 124
    stop = length(y1)
    x1 = t(as.data.frame(y1[start:stop]))
    x1[] <- lapply(x1, as.numeric)
    y = as.numeric(predict(rf, x1))
    y1[length(y1)+1] = y
    y2[length(y2)+1] = NA
    print(y)
  }
  y_pred = c(y_pred, y1)
  y_true = c(y_true, y2)
}

rul <- function(df) {
  label_list <- lapply(unique(df$Data_No), function(i) {
    data_subset <- df[df$Data_No == i, ]
    if (nrow(data_subset) > 120) {
      data_subset$RUL[1]
    }
  })
  label_array <- unlist(label_list)
  return(label_array)
}

ind <- which(y_pred > 580)
rul_true = rul(test_df)
rul_pred = c(ind[1] / 10)
for (i in c(2:length(ind))){
  rul_pred[length(rul_pred)+1] = (ind[i]-ind[i-1]) / 10
}
df_final = data.frame(rul_true, rul_pred)

ggplot() + 
  geom_line(aes(x = 1:length(y_pred[(ind[2]+1):ind[3]]), y = y_pred[(ind[2]+1):ind[3]], color = "Предсказанная")) + 
  geom_line(aes(x = 1:length(y_true[(ind[2]+1):ind[3]]), y = y_true[(ind[2]+1):ind[3]], color = "Реальная")) + 
  labs(x = "Время", y = "Значение", title = "Разница давления (эксперимент 3)") + 
  theme_classic() + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("Предсказанная" = "red", "Реальная" = "blue"), name = "")

ggplot() + 
  geom_line(aes(x = 1:length(y_pred[(ind[39]+1):ind[40]]), y = y_pred[(ind[39]+1):ind[40]], color = "Предсказанная")) + 
  geom_line(aes(x = 1:length(y_true[(ind[39]+1):ind[40]]), y = y_true[(ind[39]+1):ind[40]], color = "Реальная")) + 
  labs(x = "Время", y = "Значение", title = "Разница давления (эксперимент 40)") + 
  theme_classic() + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("Предсказанная" = "red", "Реальная" = "blue"), name = "")


ggplot() + 
  geom_line(aes(x = 1:length(rul_pred), y = rul_pred, color = "Предсказанный")) + 
  geom_line(aes(x = 1:length(rul_true), y = rul_true, color = "Реальный")) +
  geom_point(aes(x = 1:length(rul_pred), y = rul_pred, color = "Предсказанный")) + 
  geom_point(aes(x = 1:length(rul_true), y = rul_true, color = "Реальный")) +
  labs(x = "Номер эксперимента", y = "Значение", title = "Оставшийся срок службы") + 
  theme_classic() + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("Предсказанный" = "red", "Реальный" = "blue"), name = "")

evaluate(rul_true, rul_pred)
