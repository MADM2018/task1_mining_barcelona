---
  title: "Opiniones apps Play Store (kaggle) Taller1"
author: "Reinier Mujica"
date: "1 de marzo de 2019"
output: 
  html_document: 
  number_sections: yes
toc: yes
---
  
  ```{r echo = FALSE}
library(knitr)
opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE)
options(width = 100, dplyr.width = 100)
library(ggplot2)
theme_set(theme_light())
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
setwd('d:/MADM/Aplicacion de Mineria de Dades y tex industria del turisme/task1_mining_barcelona/')

#Librerias que ser?n utilizadas
library(stringr)       #Paquete para manipular la data
library(dplyr)         #Paquete para manipular datos
library(caTools)       #Paquete para subdividir la muestra
library(rpart)         #Paquete para el an?lisis de ?rboles"
library(rpart.plot)    #Paquete para el an?lisis de ?rboles"
library(randomForest)  #Paquete para el an?lisis de random forests"
library(mice)          #Contiene funciones para imputar valores perdidos
library(caret)         #Para calcular indicadores tales como R cuadrado
library(modelr)        #Provee funciones de ayuda para calcular estadisticos relacionados con modelos de regresion
library(broom)         #Crea data frame para mostrar estadisticos
library(ggplot2)       #Contine funciones para la visualizaci?n de datos en mapas

#Para usar los mapas de Google, necesitamos cargar la siguiente libreria, y luego instalar la versi?n de ggmap desde GitHub
library(devtools)
devtools::install_github("dkahle/ggmap")
devtools::install_github("hadley/ggplot2")
library(ggmap)

#Guardando la Api key de Google
register_google(key = "AIzaSyDBv1ye2NGm-83W4F6kXluSium5mQkLk_c")

#Verificamos que la clave se ha guardado correctamente
has_google_key()

#Cargamos los datos para el an?lisis
mll_data = read.csv("listings.csv", stringsAsFactors = TRUE, header = TRUE, sep = ",", encoding = "UTF-8")

#Quitamos espacios en blancos que puedan estar al incio y final de variables
str_trim(mll_data$neighbourhood_cleansed, side=c("both"))

#Remover el simbolo del d?lar de la variable precio
mll_data$price=as.numeric(gsub("\\$","",mll_data$price))

#Verificamos que la variable precios sea numerica
typeof(mll_data$price)

#Ahora, centremos la atencion en Barcelona y analicemos la ubicación y los precios

#Definiremos los datos que queremos representar en el mapa: La densidad de las propiedades en Airbnb
pm_data = mll_data[(mll_data$neighbourhood_cleansed=="el Raval"), ]

#Una alternativa ser?a usar Stamen (proveedor de mapas):

#Primero, obtenemos en mapa de Palma
MapStamen = qmap("barcelona", source = "stamen", zoom = 14, color = "bw")

#Ubiquemos la informaci?n de la ubicaci?n de las propiedades en el mapa
MapStamen + geom_point(data = pm_data, aes(x = longitude, y = latitude), colour = "blue", size = 2, alpha = 0.1)

#Otra opci?n es usar Google. Generemos un mapa de Palma usando Google Maps
MapGoogle=ggmap(
  ggmap = get_map(
    "Barcelona",
    zoom = 14, scale = "auto",
    maptype = "terrain",
    source = "google"),
  extent = "device",
  legend = "topright"
)

#Ubiquemos la informaci?n de la ubicaci?n de las propiedades en el mapa
MapGoogle + geom_point(data = pm_data, aes(x = longitude, y = latitude), colour = "blue", size = 2, alpha = 0.25)

#Podemos representar la informaci?n distinguiendo atributos de las propiedades: Por ejemplo, tipo de propiedad
MapStamen + geom_point(data = pm_data, aes(x = longitude, y = latitude), colour = "blue", size = 1, alpha = 1) +
  facet_wrap(~room_type)

MapGoogle + geom_point(data = pm_data, aes(x = longitude, y = latitude), colour = "blue", size = 1, alpha = 1) +
  facet_wrap(~room_type)

#Fijamos un indicador que muestre el precio 
pm_circle_size = 0.010 

#Creemos un mapa que muestre las zonas de Palma y las intensidades, seg?n el precio
MapStamen + geom_point(data = pm_data, aes(x = longitude, y = latitude), colour = "red", size = pm_data$price*pm_circle_size, alpha = 0.5)

MapGoogle + geom_point(data = pm_data, aes(x = longitude, y = latitude), colour = "red", size = pm_data$price*pm_circle_size, alpha = 0.5)

#Podemos usar un mapa para mostrar las intensidades, seg?n la ubicaci?n:
MapStamen +
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 size = 1, data = pm_data, geom = "polygon")

#Podemos extender el an?lisis para el caso de Mallorca
MapStamen_mll = qmap("mallorca", zoom = 10, color = "bw")

MapStamen_mll +
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 size = 2, data = mll_data, geom = "polygon")

#Fijamos un indicador que muestre el precio 
mll_circle_size = 0.005 

#Creemos un mapa que muestre las zonas de Palma y las intensidades, seg?n el precio
MapStamen_mll + geom_point(data = mll_data, aes(x = longitude, y = latitude), colour = "red", size = mll_data$price*mll_circle_size, alpha = 0.5)

#Descripci?n de los datos antes de ajustar un modelo de regresi?n lineal que prediga el precio

#Examinamos la variable precios
hist(mll_data$price)
mean(mll_data$price, na.rm = TRUE)

#El precio puede diferir dependiendo de la localidad. Podemos verificarlo a continuaci?n
tapply(mll_data$price, mll_data$neighbourhood_cleansed, mean, na.rm = TRUE)

#Alternativamente
aggregate(price ~ neighbourhood_cleansed, data=mll_data, FUN=mean)
table(mll_data$property_type)

#Podemos examinar el precio seg?n el tipo de propiedad
aggregate(price ~ property_type, data=mll_data, FUN=mean)

#Examinemos la asociacion entre precios y los evaluaciones de los consumidores
aggregate(price ~ review_scores_value, data=mll_data, FUN=mean)
cor(mll_data$price, mll_data$review_scores_value, use="pairwise.complete.obs")
cor(mll_data$price, mll_data$review_scores_rating, use="pairwise.complete.obs")
aggregate(price ~ review_scores_rating, data=mll_data, FUN=mean)

#Ajustemos un modelo lineal de regresi?n para predecir el precio

#Crearemos la muestra de entrenamiento y de prueba
spl = sample.split(mll_data$price, 0.8)
train = subset(mll_data, spl == TRUE)
test  = subset(mll_data, spl == FALSE)

mean(train$price, na.rm = TRUE)
mean(test$price, na.rm = TRUE)

model_lm = lm(price ~ bedrooms + review_scores_value + number_of_reviews + square_feet + longitude + latitude + cancellation_policy, data=train)
summary(model_lm)

model_lm$xlevels

#Calculemos el Error Cuadrado Medio
predict_lm = predict(model_lm, newdata = test)
MSE_lm = mean((predict_lm - test$price)^2, na.rm = TRUE)
MSE_lm

data.frame( R2 = R2(predict_lm, test$price, na.rm = TRUE),
            RMSE = RMSE(predict_lm, test$price, na.rm = TRUE),
            MAE = MAE(predict_lm, test$price, na.rm = TRUE))

#Examinemos la validaci?n cruzada

#Imputaremos los valores NA de las variables numericas. No imputaremos dichos valores en el caso de la longitud y latitud
mice_mod = mice(mll_data[, names(mll_data) %in% c("price", "bedrooms", "review_scores_value", "number_of_reviews", "square_feet" )], method = 'pmm', seed = 500)
mice_output = complete(mice_mod)

#Seleccionaremos los datos para el an?lisis, considerando la data con los valores imputados y las variables donde no se ha hecho tal imputacion
covar=subset(mll_data, select=c("longitude", "latitude", "cancellation_policy"))
data_reg=cbind(covar, mice_output)

#Definimos los parametros para la validacion cruzada
set.seed(123) 
train.control = trainControl(method = "cv", number = 10)

#Entrenemos un modelo de regresion lineal
model_vc_ols = train(price ~ bedrooms + review_scores_value + number_of_reviews + square_feet + longitude + latitude + cancellation_policy, data = data_reg, method = "lm", trControl = train.control)
print(model_vc_ols)

#Veamos si modelos alternativos pueden mejorar la prediccion. En primer lugar, probemos con una regresion de arbol
model_cart = rpart(price ~ bedrooms + review_scores_value + number_of_reviews + square_feet + longitude + latitude + cancellation_policy, data=train, method = "anova", na.action = na.omit)

#Calculemos el Error Cuadrado Medio
predict_cart = predict(model_cart, newdata = test)
MSE_cart = mean((predict_cart-test$price)^2, na.rm = TRUE)
MSE_cart

#Calculemos medidas alternativas, como el Error Absoluto Medio (MAE) o el R cuadrado o la Ra?z de la Desviaci?n Cuadr?tica Media (RMSE)
data.frame( R2 = R2(predict_cart, test$price, na.rm = TRUE),
            RMSE = RMSE(predict_cart, test$price, na.rm = TRUE),
            MAE = MAE(predict_cart, test$price, na.rm = TRUE))

#Consideremos la validacion cruzada
model_vc_tree = train(price ~ bedrooms + review_scores_value + number_of_reviews + square_feet + longitude + latitude + cancellation_policy, data = data_reg, method = "rpart", trControl = train.control)
print(model_vc_tree)

#Podemos dibujar el arbol
prp(model_cart)

#Ahora, probemos con un Random Forest Model
model_rf = randomForest(price ~ bedrooms + review_scores_value + number_of_reviews + square_feet + longitude + latitude + cancellation_policy, data=train, importance= TRUE, method = "anova", na.action = na.omit)
model_rf

predict_rf = predict(model_rf, ntree=4, newdata=test)
MSE_rf = mean((predict_rf-test$price)^2,na.rm = TRUE)
MSE_rf

#Calculemos medidas alternativas, como el Error Absoluto Medio (MAE) o el R cuadrado o la Ra?z de la Desviaci?n Cuadr?tica Media (RMSE)
predictions_rf = predict(model_rf, test)

data.frame( R2 = R2(predict_rf, test$price, na.rm = TRUE),
            RMSE = RMSE(predict_rf, test$price, na.rm = TRUE),
            MAE = MAE(predict_rf, test$price, na.rm = TRUE))

#Consideremos la validacion cruzada
model_vc_rf = train(price ~ bedrooms + review_scores_value + number_of_reviews + square_feet + longitude + latitude + cancellation_policy, data = data_reg, method = "rf", trControl = train.control)
print(model_vc_rf)

plot(model_rf)

#Grafico importancia de las variables
varImpPlot(model_rf, sort=TRUE, main="Importancia", n.var=5)

#Tabla con la importancia de las variables
var.imp=data.frame(importance(model_rf, type=2))
var.imp$Variables=row.names(var.imp)
var.imp[order(var.imp$IncNodePurity, decreasing = TRUE),]



