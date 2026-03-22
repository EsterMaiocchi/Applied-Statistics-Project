setwd("C:/Users/morra/OneDrive - Politecnico di Milano/Applied statistics - Shared folder/Dati")

library(plm)

data <- read.table("HR_FB_pollutants.txt")
data <- subset(data, data$place_ID!="A")  #Solo outdoor!!
data[, -c(1,2,6)] = lapply(data[, -c(1,2,6)], scale)

Tempo = rep(1:30, times = 19)
data = cbind(data[, 1], Tempo, data[, 3], data[, 2], data[, 4:14])
colnames(data)[1:4] = c("subject_ID", "Tempo", "HR", "place_ID")

data$distance = ifelse(data$distance != 0, 1, 0)
data$distance = as.factor(data$distance)

data_rf <- pdata.frame(data, index = c("subject_ID", "Tempo"))

# Modello a effetti casuali
#In entrambe i casi nel summary non abbiamo gli effetti singoli/casuali

#Cerchiamo di definire una trasformazioni dei dati sensata


subject = unique(data$subject_ID)

#Per fare funzionare bisogna allargare al massimo la finestra dei plot (magari avere uno schermo wide)
x11()
par(mfrow=c(5,4))
for (i in subject) {
  plot(subset(data, data$subject_ID==i)$C02, exp(subset(data, data$subject_ID==i)$HR),  
       xlab = "CO2", ylab = "HR")
}

x11()
colors= rainbow(10)
data_first = data[1:300,]
plot(data_first$C02, data_first$HR, col = colors[as.factor(data_first$subject_ID)], type = "p", pch = 16)

colors= rainbow(9)
data_second = data[301:570,]
plot(data_second$C02, data_second$HR, col = colors[as.factor(data_second$subject_ID)], type = "p", pch = 16)


colors= rainbow(10)
data_first = data[1:300,]
plot(data_first$PM25, data_first$HR, col = colors[as.factor(data_first$subject_ID)], type = "p", pch = 16)

colors= rainbow(9)
data_second = data[301:570,]
plot(data_second$PM25, data_second$HR, col = colors[as.factor(data_second$subject_ID)], type = "p", pch = 16)


par(mfrow=c(2,10))
for (i in subject) {
  plot(subset(data, data$subject_ID==i)$PM25, exp(subset(data, data$subject_ID==i)$HR) )
}



#Pooled
pooled <- lm(HR ~ exp(C02) + T + exp(PM25)+ distance+ subject_ID + RH, data = data)
summary(pooled)


#yij = b0 + b1*x1_ij + .. + br*xr_ij + ui + error_ij
#i group, j observation within group; ui is r.v ~ 0, var   but the var is constant for every i
model_re <- plm(HR ~ C02 + distance + PM25, data = data_rf, model = "random")
summary(model_re)

#yij = b0 + b1*x1_ij + .. + br*xr_ij + ui + error_ij
#i group, j observation within group; ui is a number
model_fe <- plm(HR ~ exp(C02) + T + exp(PM25)+ distance, data = data_rf, model = "within")
summary(model_fe)

cov(data$PM25, data$C02)

fixed_effects <- fixef(model_fe)
dotchart(fixed_effects, main = "Effetti fissi per ogni soggetto", xlab = "Effetti fissi")

phtest(model_fe, model_re)

#Se il p-value è basso, scegli il modello a effetti fissi (FE). Questo suggerisce che il modello a effetti casuali 
#non è appropriato perché potrebbe essere influenzato dalla correlazione tra le variabili indipendenti e gli effetti casuali.

#Se il p-value è alto, puoi scegliere il modello a effetti casuali (RE). 
#Questo suggerisce che gli effetti casuali sono appropriati e che le variabili indipendenti non sono correlate con gli effetti individuali