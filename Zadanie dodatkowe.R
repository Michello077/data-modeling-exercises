
# Michał Humiński, Dominik Lisiecki, Daria Plewa


# Biblioteki --------------------------------------------------------------



#install.packages('scorecard')
#install.packages('randomForest')
#install.packages('ggthemes')
library(scorecard)
library(randomForest)
library(dplyr)
library(ggthemes)

# Czesc_1 -----------------------------------------------------------------


# Dzielenie zbioru na zbior testowy oraz uczacy
dane <- split_df(daneZajecia, ratio = 0.75, seed = 66)
test <- data.frame(dane$test)
train <- data.frame(dane$train)


model <- glm(formula = Status ~ Score + CAD + atrialFibrillation + Stroke_TIA + hypertensionTreatmet + diabetes + lowGFR, family=binomial, data=train)
summary(model)


modelH <- glm(formula = StatusH ~ Score + CAD + atrialFibrillation + Stroke_TIA + hypertensionTreatmet + diabetes + lowGFR, family=binomial, data=train)
summary(model)


#Predykcja dla osob niehospitalizowanych
sur_pred <- predict(model,type = 'response',newdata = test)

y_pred <- ifelse(sur_pred>0.5,1,0)

test$surv <- y_pred

submit <- data.frame(test$patientId,test$Status)
names(submit)<-c("patientId","Survived")

write.csv(submit,file='przezycie_niehospitalizowanych.csv',row.names=FALSE)


#Predykcja dla osob hospitalizowanych
sur_pred <- predict(model,type = 'response',newdata = test)

y_pred <- ifelse(sur_pred>0.5,1,0)

test$surv <- y_pred

submit <- data.frame(test$patientId,test$StatusH)
names(submit)<-c("patientId","Survived")

write.csv(submit,file='przezycie_hospitalizowanych.csv',row.names=FALSE)



# Predykcja za pomoca biblioteki randomForest dla osob niehospitalizowanych

model <- randomForest(factor(Status) ~ Score + CAD + atrialFibrillation + Stroke_TIA + hypertensionTreatmet + diabetes + lowGFR, family=binomial, data = train)
importance <- importance(model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))


rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(importance))))


ggplot(rankImportance, aes(x = reorder(Variables, importance), 
                           y = importance, fill = importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()



prediction <- predict(model, test)


solution <- data.frame(patientId = test$patientId, Survived = prediction)


write.csv(solution, file = 'przezycie_niehospitalizowanych.csv.csv', row.names = F)


# Predykcja za pomoca biblioteki randomForest dla osob niehospitalizowanych

model <- randomForest(factor(StatusH) ~ Score + CAD + atrialFibrillation + Stroke_TIA + hypertensionTreatmet + diabetes + lowGFR, family=binomial, data = train)
importance <- importance(model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))


rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(importance))))


ggplot(rankImportance, aes(x = reorder(Variables, importance), 
                           y = importance, fill = importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()


prediction <- predict(model, test)


solution <- data.frame(patientId = test$patientId, Survived = prediction)


write.csv(solution, file = 'przezycie_niehospitalizowanych.csv.csv', row.names = F)




# Czesc_2 -----------------------------------------------------------------

pacjenci <- read.table("daneZajecia.csv", sep = ";", header = TRUE)
pacjenci_sel <- pacjenci[, c(1,2,17,18,19)]

#0: żyje 1:zgon/hospitalizowanu

colnames(pacjenci_sel) <- c("Status", "Hospitalizowany","Wiek", "BMI","GFR")

head(pacjenci_sel)
dim(pacjenci_sel)
number_row <- nrow(pacjenci_sel)

#1:3
train_sample <- pacjenci_sel[1164:number_row, ]
verification_sample <- pacjenci_sel[1:1165, ]
print(dim(train_sample))
print(dim(verification_sample))

reg_1 <- glm(Status ~ ., data = train_sample, family = binomial())
summary(reg_1)

reg_2 <- glm(Status ~ . -GFR, data = train_sample, family = binomial())
summary(reg_2)

prediction_reg <- predict(reg_2, newdata = verification_sample, type = "response")
summary(prediction_reg)

number_of_prediction <- length(prediction_reg)
prediction_vector <- rep(0, number_of_prediction)
prediction_vector[prediction_reg > 0.5] = 1
prediction_table_log <- table(True = verification_sample$Status, Predicted = prediction_vector)

log_err <- (sum(prediction_table_log) - sum(diag(prediction_table_log))) / sum(prediction_table_log) * 100
# 12.87554%


# roznymi kolorami zaznaczone sa osoby ktore nie mialy zawalu serca (na niebiesko) i mialy zawal serca (na czerwono)
plot(prediction_reg, pch = verification_sample$Status, col = prediction_vector + 1, xlab = "nr pacjenta", ylab = "prawdopodobienstwo")
legend("topleft", c("TN", "FN", "FP", "TP"), pch = c(0, 1, 0, 1), col = c(1, 1, 2, 2))
# znaczenie legendy:
# TN - osoby bez zawalu zaklasyfikowane jako osoby bez zawalu
# FN - osoby z zawalem zaklasyfikowane jako osoby bez zawalu
# FP - osoby bez zawalu zaklasyfikowane jako osoby z zawalem
# TP - osoby z zawalem zaklasyfikowane jako osoby z zawalem



# Uwagi_dodatkowe ---------------------------------------------------------

# Nie znalezlismy zaleznosci pomiedzy smiercia badz przezyciem pacjentow wzgledem stosowanych lekow
