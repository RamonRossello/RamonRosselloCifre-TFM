#carga de datos
getwd()
setwd("C:/Users/34682/Desktop/TFM/Posibles_datasets/vehicles/")
v<-read.csv('Australian vehicle Prices Clean.csv')

# Validación cruzada
set.seed(9)
train_ind <- sample(seq_len(nrow(v)), size =floor(0.9*nrow(v)))
v_train <- v[train_ind, ]
v_test <- v[-train_ind, ]
valid_cros_ind_1 <- sample(seq_len(nrow(v_train)), size =floor(0.5*nrow(v_train)))
v_train_aux1 <- v_train[valid_cros_ind_1, ]
v_train_aux2 <- v_train[-valid_cros_ind_1, ]
valid_cros_ind_2 <- sample(seq_len(nrow(v_train_aux1)), size =floor(0.5*nrow(v_train_aux1)))
valid_cros_ind_3 <- sample(seq_len(nrow(v_train_aux2)), size =floor(0.5*nrow(v_train_aux2)))
v_train_1 <- v_train_aux1[valid_cros_ind_2, ]
v_train_2 <- v_train_aux1[-valid_cros_ind_2, ]
v_train_3 <- v_train_aux2[valid_cros_ind_3, ]
v_train_4 <- v_train_aux2[-valid_cros_ind_3, ]
v_train_i1 <- rbind(v_train_2, v_train_3, v_train_4)
v_train_i2 <- rbind(v_train_1, v_train_3, v_train_4)
v_train_i3 <- rbind(v_train_1, v_train_2, v_train_4)
v_train_i4 <- rbind(v_train_1, v_train_2, v_train_3)

#Modelo nulo

mg0_1 <- glm(Price~ 1 ,family= gaussian(link= "identity"),v_train_i1)
pred_val <-predict(mg0_1, newdata = v_train_1, ty="response")
mg0_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg0_pr1_tr <- mean(abs(v_train_i1$Price - mg0_1$fitted.values))


mg0_2 <- glm(Price~ 1 ,family= gaussian(link= "identity"),v_train_i2)
pred_val <-predict(mg0_2, newdata = v_train_2, ty="response")
mg0_pr2_val <- mean(abs(v_train_2$Price - pred_val))
mg0_pr2_tr <- mean(abs(v_train_i2$Price - mg0_2$fitted.values))

mg0_3 <- glm(Price~ 1 ,family= gaussian(link= "identity"),v_train_i3)
pred_val <-predict(mg0_3, newdata = v_train_3, ty="response")
mg0_pr3_val <- mean(abs(v_train_3$Price - pred_val))
mg0_pr3_tr <- mean(abs(v_train_i3$Price - mg0_3$fitted.values))

mg0_4 <- glm(Price~ 1 ,family= gaussian(link= "identity"),v_train_i4)
pred_val <-predict(mg0_4, newdata = v_train_4, ty="response")
mg0_pr4_val <- mean(abs(v_train_4$Price - pred_val))
mg0_pr4_tr <- mean(abs(v_train_i4$Price - mg0_4$fitted.values))

#MAE_v
mg0_pr_val <- mean(c(mg0_pr1_val, mg0_pr2_val, mg0_pr3_val, mg0_pr4_val))
#MAE_train
mg0_pr_tr <- mean(c(mg0_pr1_tr, mg0_pr2_tr, mg0_pr3_tr, mg0_pr4_tr))


#Normal con función de enlace identidad

mg1_1 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "identity"),v_train_i1)
pred_val <-predict(mg1_1, newdata = v_train_1, ty="response")
mg1_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg1_pr1_tr <- mean(abs(v_train_i1$Price - mg1_1$fitted.values))


mg1_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "identity"),v_train_i2)
pred_val <-predict(mg1_2, newdata = v_train_2, ty="response")
mg1_pr2_val <- mean(abs(v_train_2$Price - pred_val))
mg1_pr2_tr <- mean(abs(v_train_i2$Price - mg1_2$fitted.values))

mg1_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "identity"),v_train_i3)
pred_val <-predict(mg1_3, newdata = v_train_3, ty="response")
mg1_pr3_val <- mean(abs(v_train_3$Price - pred_val))
mg1_pr3_tr <- mean(abs(v_train_i3$Price - mg1_3$fitted.values))

mg1_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "identity"),v_train_i4)
pred_val <-predict(mg1_4, newdata = v_train_4, ty="response")
mg1_pr4_val <- mean(abs(v_train_4$Price - pred_val))
mg1_pr4_tr <- mean(abs(v_train_i4$Price - mg1_4$fitted.values))

#MAE_v
mg1_pr_val <- mean(c(mg1_pr1_val, mg1_pr2_val, mg1_pr3_val, mg1_pr4_val))
#MAE_train
mg1_pr_tr <- mean(c(mg1_pr1_tr, mg1_pr2_tr, mg1_pr3_tr, mg1_pr4_tr))

#Normal con función de enlace log

mg2_1 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "log"),v_train_i1)
pred_val <-predict(mg2_1, newdata = v_train_1, ty="response")
mg2_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg2_pr1_tr <- mean(abs(v_train_i1$Price - mg2_1$fitted.values))


mg2_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "log"),v_train_i2)
pred_val <-predict(mg2_2, newdata = v_train_2, ty="response")
mg2_pr2_val <- mean(abs(v_train_2$Price - pred_val))
mg2_pr2_tr <- mean(abs(v_train_i2$Price - mg2_2$fitted.values))

mg2_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "log"),v_train_i3)
pred_val <-predict(mg2_3, newdata = v_train_3, ty="response")
mg2_pr3_val <- mean(abs(v_train_3$Price - pred_val))
mg2_pr3_tr <- mean(abs(v_train_i3$Price - mg2_3$fitted.values))

mg2_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "log"),v_train_i4)
pred_val <-predict(mg2_4, newdata = v_train_4, ty="response")
mg2_pr4_val <- mean(abs(v_train_4$Price - pred_val))
mg2_pr4_tr <- mean(abs(v_train_i4$Price - mg2_4$fitted.values))

#MAE_v
mg2_pr_val <- mean(c(mg2_pr1_val, mg2_pr2_val, mg2_pr3_val, mg2_pr4_val))
#MAE_train
mg2_pr_tr <- mean(c(mg2_pr1_tr, mg2_pr2_tr, mg2_pr3_tr, mg2_pr4_tr))

#Normal con función de enlace inversa

mg3_1 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "inverse"),v_train_i1)
pred_val <-predict(mg3_1, newdata = v_train_1, ty="response")
mg3_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg3_pr1_tr <- mean(abs(v_train_i1$Price - mg3_1$fitted.values))


mg3_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "inverse"),v_train_i2)
pred_val <-predict(mg3_2, newdata = v_train_2, ty="response")
mg3_pr2_val <- mean(abs(v_train_2$Price - pred_val))
mg3_pr2_tr <- mean(abs(v_train_i2$Price - mg3_2$fitted.values))

mg3_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "inverse"),v_train_i3)
pred_val <-predict(mg3_3, newdata = v_train_3, ty="response")
mg3_pr3_val <- mean(abs(v_train_3$Price - pred_val))
mg3_pr3_tr <- mean(abs(v_train_i3$Price - mg3_3$fitted.values))

mg3_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "inverse"),v_train_i4)
pred_val <-predict(mg3_4, newdata = v_train_4, ty="response")
mg3_pr4_val <- mean(abs(v_train_4$Price - pred_val))
mg3_pr4_tr <- mean(abs(v_train_i4$Price - mg3_4$fitted.values))

#MAE_v
mg3_pr_val <- mean(c(mg3_pr1_val, mg3_pr2_val, mg3_pr3_val, mg3_pr4_val))
#MAE_train
mg3_pr_tr <- mean(c(mg3_pr1_tr, mg3_pr2_tr, mg3_pr3_tr, mg3_pr4_tr))

#Gamma con función de enlace la identidad

mg4_1 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "identity"),v_train_i1)
pred_val <-predict(mg4_1, newdata = v_train_1, ty="response")
mg4_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg4_pr1_tr <- mean(abs(v_train_i1$Price - mg4_1$fitted.values))


mg4_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "identity"),v_train_i2)
pred_val <-predict(mg4_2, newdata = v_train_2, ty="response")
mg4_pr2_val <- mean(abs(v_train_2$Price - pred_val))
mg4_pr2_tr <- mean(abs(v_train_i2$Price - mg4_2$fitted.values))

mg4_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "identity"),v_train_i3)
pred_val <-predict(mg4_3, newdata = v_train_3, ty="response")
mg4_pr3_val <- mean(abs(v_train_3$Price - pred_val))
mg4_pr3_tr <- mean(abs(v_train_i3$Price - mg4_3$fitted.values))

mg4_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "identity"),v_train_i4)
pred_val <-predict(mg4_4, newdata = v_train_4, ty="response")
mg4_pr4_val <- mean(abs(v_train_4$Price - pred_val))
mg4_pr4_tr <- mean(abs(v_train_i4$Price - mg4_4$fitted.values))

#MAE_v
mg4_pr_val <- mean(c(mg4_pr1_val, mg4_pr2_val, mg4_pr3_val, mg4_pr4_val))
#MAE_train
mg4_pr_tr <- mean(c(mg4_pr1_tr, mg4_pr2_tr, mg4_pr3_tr, mg4_pr4_tr))

#Gamma con función de enlace el log

mg5_1 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "log"),v_train_i1)
pred_val <-predict(mg5_1, newdata = v_train_1, ty="response")
mg5_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg5_pr1_tr <- mean(abs(v_train_i1$Price - mg5_1$fitted.values))


mg5_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "log"),v_train_i2)
pred_val <-predict(mg5_2, newdata = v_train_2, ty="response")
mg5_pr2_val <- mean(abs(v_train_2$Price - pred_val))
mg5_pr2_tr <- mean(abs(v_train_i2$Price - mg5_2$fitted.values))

mg5_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "log"),v_train_i3)
pred_val <-predict(mg5_3, newdata = v_train_3, ty="response")
mg5_pr3_val <- mean(abs(v_train_3$Price - pred_val))
mg5_pr3_tr <- mean(abs(v_train_i3$Price - mg5_3$fitted.values))

mg5_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "log"),v_train_i4)
pred_val <-predict(mg5_4, newdata = v_train_4, ty="response")
mg5_pr4_val <- mean(abs(v_train_4$Price - pred_val))
mg5_pr4_tr <- mean(abs(v_train_i4$Price - mg5_4$fitted.values))

#MAE_v
mg5_pr_val <- mean(c(mg5_pr1_val, mg5_pr2_val, mg5_pr3_val, mg5_pr4_val))
#MAE_train
mg5_pr_tr <- mean(c(mg5_pr1_tr, mg5_pr2_tr, mg5_pr3_tr, mg5_pr4_tr))

#Gamma con función de enlace inversa

mg6_1 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "inverse"),v_train_i1)
pred_val <-predict(mg6_1, newdata = v_train_1, ty="response")
mg6_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg6_pr1_tr <- mean(abs(v_train_i1$Price - mg6_1$fitted.values))


mg6_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "inverse"),v_train_i2)
pred_val <-predict(mg6_2, newdata = v_train_2, ty="response")
mg6_pr2_val <- mean(abs(v_train_2$Price - pred_val))
mg6_pr2_tr <- mean(abs(v_train_i2$Price - mg6_2$fitted.values))

mg6_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "inverse"),v_train_i3)
pred_val <-predict(mg6_3, newdata = v_train_3, ty="response")
mg6_pr3_val <- mean(abs(v_train_3$Price - pred_val))
mg6_pr3_tr <- mean(abs(v_train_i3$Price - mg6_3$fitted.values))

mg6_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= Gamma(link= "inverse"),v_train_i4)
pred_val <-predict(mg6_4, newdata = v_train_4, ty="response")
mg6_pr4_val <- mean(abs(v_train_4$Price - pred_val))
mg6_pr4_tr <- mean(abs(v_train_i4$Price - mg6_4$fitted.values))

#MAE_v
mg6_pr_val <- mean(c(mg6_pr1_val, mg6_pr2_val, mg6_pr3_val, mg6_pr4_val))
#MAE_train
mg6_pr_tr <- mean(c(mg6_pr1_tr, mg6_pr2_tr, mg6_pr3_tr, mg6_pr4_tr))

#Inversa gaussiana con función de enlace identidad

mg7_1 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "identity"),v_train_i1)
pred_val <-predict(mg7_1, newdata = v_train_1, ty="response")
mg7_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg7_pr1_tr <- mean(abs(v_train_i1$Price - mg7_1$fitted.values))


mg7_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "identity"),v_train_i2)
pred_val <-predict(mg7_2, newdata = v_train_2, ty="response")
mg7_pr2_val <- mean(abs(v_train_2$Price - pred_val))
mg7_pr2_tr <- mean(abs(v_train_i2$Price - mg7_2$fitted.values))

mg7_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "identity"),v_train_i3)
pred_val <-predict(mg7_3, newdata = v_train_3, ty="response")
mg7_pr3_val <- mean(abs(v_train_3$Price - pred_val))
mg7_pr3_tr <- mean(abs(v_train_i3$Price - mg7_3$fitted.values))

mg7_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "identity"),v_train_i4)
pred_val <-predict(mg7_4, newdata = v_train_4, ty="response")
mg7_pr4_val <- mean(abs(v_train_4$Price - pred_val))
mg7_pr4_tr <- mean(abs(v_train_i4$Price - mg7_4$fitted.values))

#MAE_v
mg7_pr_val <- mean(c(mg7_pr1_val, mg7_pr2_val, mg7_pr3_val, mg7_pr4_val))
#MAE_train
mg7_pr_tr <- mean(c(mg7_pr1_tr, mg7_pr2_tr, mg7_pr3_tr, mg7_pr4_tr))

#Inversa gaussiana con función de enlace log

mg8_1 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "log"),v_train_i1)
pred_val <-predict(mg8_1, newdata = v_train_1, ty="response")
mg8_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg8_pr1_tr <- mean(abs(v_train_i1$Price - mg8_1$fitted.values))


mg8_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "log"),v_train_i2)
pred_val <-predict(mg8_2, newdata = v_train_2, ty="response")
mg8_pr2_val <- mean(abs(v_train_2$Price - pred_val))
mg8_pr2_tr <- mean(abs(v_train_i2$Price - mg8_2$fitted.values))

mg8_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "log"),v_train_i3)
pred_val <-predict(mg8_3, newdata = v_train_3, ty="response")
mg8_pr3_val <- mean(abs(v_train_3$Price - pred_val))
mg8_pr3_tr <- mean(abs(v_train_i3$Price - mg8_3$fitted.values))

mg8_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "log"),v_train_i4)
pred_val <-predict(mg8_4, newdata = v_train_4, ty="response")
mg8_pr4_val <- mean(abs(v_train_4$Price - pred_val))
mg8_pr4_tr <- mean(abs(v_train_i4$Price - mg8_4$fitted.values))

#MAE_v
mg8_pr_val <- mean(c(mg8_pr1_val, mg8_pr2_val, mg8_pr3_val, mg8_pr4_val))
#MAE_train
mg8_pr_tr <- mean(c(mg8_pr1_tr, mg8_pr2_tr, mg8_pr3_tr, mg8_pr4_tr))

#Inversa gaussiana con función de enlace inverse

mg9_1 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "inverse"),v_train_i1)
pred_val <-predict(mg9_1, newdata = v_train_1, ty="response")
mg9_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg9_pr1_tr <- mean(abs(v_train_i1$Price - mg9_1$fitted.values))


mg9_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "inverse"),v_train_i2)
pred_val <-predict(mg9_2, newdata = v_train_2, ty="response")
mg9_pr2_val <- mean(abs(v_train_2$Price - pred_val))
mg9_pr2_tr <- mean(abs(v_train_i2$Price - mg9_2$fitted.values))

mg9_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "inverse"),v_train_i3)
pred_val <-predict(mg9_3, newdata = v_train_3, ty="response")
mg9_pr3_val <- mean(abs(v_train_3$Price - pred_val))
mg9_pr3_tr <- mean(abs(v_train_i3$Price - mg9_3$fitted.values))

mg9_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "inverse"),v_train_i4)
pred_val <-predict(mg9_4, newdata = v_train_4, ty="response")
mg9_pr4_val <- mean(abs(v_train_4$Price - pred_val))
mg9_pr4_tr <- mean(abs(v_train_i4$Price - mg9_4$fitted.values))

#MAE_v
mg9_pr_val <- mean(c(mg9_pr1_val, mg9_pr2_val, mg9_pr3_val, mg9_pr4_val))
#MAE_train
mg9_pr_tr <- mean(c(mg9_pr1_tr, mg9_pr2_tr, mg9_pr3_tr, mg9_pr4_tr))

#Inversa gaussiana con función de enlace inversa cuadrática

mg10_1 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= inverse.gaussian(link= "1/mu^2"),v_train_i1)
pred_val <-predict(mg10_1, newdata = v_train_1, ty="response")
mg10_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg10_pr1_tr <- mean(abs(v_train_i1$Price - mg10_1$fitted.values))

#Poisson con función de enlace identidad

mg11_1 <- glm(as.integer(Price)~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= poisson(link= "identity"),v_train_i1)
pred_val <-predict(mg11_1, newdata = v_train_1, ty="response")
mg11_pr1_val <- mean(abs(v_train_1$Price - pred_val))
mg11_pr1_tr <- mean(abs(v_train_i1$Price - mg11_1$fitted.values))

#Poisson con función de enlace log

mg12_1 <- glm(as.integer(Price)~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= poisson(link= "log"),v_train_i1)
pred_val <-predict(mg12_1, newdata = v_train_1, ty="response")
mg12_pr1_val <- mean(abs(as.integer(v_train_1$Price) - round(pred_val,0)))
mg12_pr1_tr <- mean(abs(as.integer(v_train_i1$Price) - round(mg12_1$fitted.values,0)))


mg12_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= poisson(link= "log"),v_train_i2)
pred_val <-predict(mg12_2, newdata = v_train_2, ty="response")
mg12_pr2_val <- mean(abs(as.integer(v_train_2$Price) - round(pred_val,0)))
mg12_pr2_tr <- mean(abs(as.integer(v_train_i2$Price) - round(mg12_2$fitted.values,0)))

mg12_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= poisson(link= "log"),v_train_i3)
pred_val <-predict(mg12_3, newdata = v_train_3, ty="response")
mg12_pr3_val <- mean(abs(as.integer(v_train_3$Price) - round(pred_val,0)))
mg12_pr3_tr <- mean(abs(as.integer(v_train_i3$Price) - round(mg12_3$fitted.values,0)))

mg12_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= poisson(link= "log"),v_train_i4)
pred_val <-predict(mg12_4, newdata = v_train_4, ty="response")
mg12_pr4_val <- mean(abs(as.integer(v_train_4$Price) - round(pred_val,0)))
mg12_pr4_tr <- mean(abs(as.integer(v_train_i4$Price) - round(mg12_4$fitted.values,0)))

#MAE_v
mg12_pr_val <- mean(c(mg12_pr1_val, mg12_pr2_val, mg12_pr3_val, mg12_pr4_val))
#MAE_train
mg12_pr_tr <- mean(c(mg12_pr1_tr, mg12_pr2_tr, mg12_pr3_tr, mg12_pr4_tr))

#Poisson con función de enlace sqrt

mg13_1 <- glm(as.integer(Price)~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= poisson(link= "sqrt"),v_train_i1)
pred_val <-predict(mg13_1, newdata = v_train_1, ty="response")
mg13_pr1_val <- mean(abs(as.integer(v_train_1$Price) - round(pred_val,0)))
mg13_pr1_tr <- mean(abs(as.integer(v_train_i1$Price) - round(mg13_1$fitted.values,0)))


mg13_2 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= poisson(link= "sqrt"),v_train_i2)
pred_val <-predict(mg13_2, newdata = v_train_2, ty="response")
mg13_pr2_val <- mean(abs(as.integer(v_train_2$Price) - round(pred_val,0)))
mg13_pr2_tr <- mean(abs(as.integer(v_train_i2$Price) - round(mg13_2$fitted.values,0)))

mg13_3 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= poisson(link= "sqrt"),v_train_i3)
pred_val <-predict(mg13_3, newdata = v_train_3, ty="response")
mg13_pr3_val <- mean(abs(as.integer(v_train_3$Price) - round(pred_val,0)))
mg13_pr3_tr <- mean(abs(as.integer(v_train_i3$Price) - round(mg13_3$fitted.values,0)))

mg13_4 <- glm(Price~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= poisson(link= "sqrt"),v_train_i4)
pred_val <-predict(mg13_4, newdata = v_train_4, ty="response")
mg13_pr4_val <- mean(abs(as.integer(v_train_4$Price) - round(pred_val,0)))
mg13_pr4_tr <- mean(abs(as.integer(v_train_i4$Price) - round(mg13_4$fitted.values,0)))

#MAE_v
mg13_pr_val <- mean(c(mg13_pr1_val, mg13_pr2_val, mg13_pr3_val, mg13_pr4_val))
#MAE_train
mg13_pr_tr <- mean(c(mg13_pr1_tr, mg13_pr2_tr, mg13_pr3_tr, mg13_pr4_tr))

#modelo lineal final
mg_full <-glm(as.integer(Price)~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= poisson(link= "log"),v_train)
summary(mg_full)

# Optimización del modelo y validación
library(car)
library(MASS)
stepAIC(mg_full)
summary(v)
rC <- 1- (mg_full$deviance/mg_full$null.deviance) 
plot(mg_full)
pred_test <-round(predict(mg_full, newdata = v_test, ty="response"),0)
err_test <- mean(abs(as.integer(v_test$Price) - round(pred_test,0)))

# Ampliaciones

#Error en conjunto de test
#pred_test_in1 <- round((pred_test +2*sqrt(pred_test)),0)
#pred_test_in2 <- roUnd((pred_test -2*sqrt(pred_test)),0)
#aux <- data.frame(v_test$Price, pred_test, pred_test_in1, pred_test_in2)
#aux <- aux[order(aux$pred_test, decreasing = FALSE),]
plot(v_test$Price, pred_test, xlab = 'Precios del conjunto de test', ylab = 'Predicción del precio', main = 'Comportamiento del error de predicción del precio en el conjunto de test', ylim = c(0,140000))
abline(a=0, b=1, col=2, lw=3)

aux2 <- v_test[v_test$Price > 50000,]
pred_test_st <-round(predict(mg_full, newdata = aux2, ty="response"),0) 
err_test_st <- mean(abs(as.integer(aux2$Price) - round(pred_test_st,0)))


# Efectos de variables explicativas
exp(mg_full$coefficients)
summary(v_train)
summary(factor(v_train$DriveType))
summary(v_train[v_train$DriveType == 'Other',]$Price)
summary(v_train[v_train$DriveType == 'AWD',]$Price)
vb <-data.frame(Year = 2000, UsedOrNew = 'USED', Transmission = 'Manual', Engine = 1, FuelType = 'Unleaded', 
                DriveType = 'Front', FuelConsumption = 15, Kilometres = 350000, Cylinders = 3, 
                BodyType = 'SUV/Ute/Tray', Doors = 5, Seats = 2, DollarAustralian = 1.241, PriceIndex = 65.79)

vc <-data.frame(Year = 2023, UsedOrNew = 'NOT USED', Transmission = 'Automatic', Engine = 6, FuelType = 'Other', 
                DriveType = 'Other', FuelConsumption = 0, Kilometres = 1, Cylinders = 8, 
                BodyType = 'Other', Doors = 2, Seats = 8, DollarAustralian = 1.773, PriceIndex = 135.76)

predict(mg_full, newdata = vc,ty="response")
vc_l <- predict(mg_full, vc, ty="link")
exp(vc_l + ((predict(mg_full,newdata = vc, ty = "link",se.fit=T)$se.fit)* qnorm(0.975)))
exp(vc_l + ((predict(mg_full,newdata = vc, ty = "link",se.fit=T)$se.fit)* qnorm(0.025)))

predict(mg_full, newdata = vb,ty="response")
vb_l <- predict(mg_full, vb, ty="link")
exp(vb_l  + ((predict(mg_full,newdata = vb, ty = "link",se.fit=T)$se.fit)* qnorm(0.975)))
exp(vb_l  + ((predict(mg_full,newdata = vb, ty = "link",se.fit=T)$se.fit)* qnorm(0.025)))


# ANEXO
summary(v_train$Price)
aux_t <-predict(mg_full, newdata = v_train, ty="response")
max(aux_t)
min(aux_t)


summary(v_test$Price)
plot(aux$v_test.Price, aux$pred_test, xlab = 'Precios del conjunto de test', ylab = 'Predicción del precio', main = 'Modelo lineal generalizado de familia Poisson y función de enlace logaritmo', ylim = c(0,140000))
lines(aux$pred_test, pred_test_in1, col="green")
lines(aux$pred_test, pred_test_in2, col="red")

#Otros modelos vs conjunto test
mg_test <-glm(as.integer(Price)~ Year + UsedOrNew + Transmission + Engine + DriveType + FuelType  + FuelConsumption + Kilometres + Cylinders + BodyType + Doors + Seats + DollarAustralian + PriceIndex ,family= gaussian(link= "identity"),v_train)
1- (mg_test$deviance/mg_test$null.deviance) 
pred_test <-predict(mg_test, newdata = v_test, ty="response")
err_test <- mean(abs(v_test$Price - pred_test))



