library(Hmisc)
library(Amelia) #para ver los valores vacios
library(caret)
library(ROCR)
library(DescTools) #pseudo r2
library(ggplot2)
library(vcd)
library(ggcorrplot)#correlacion
library(stargazer)#posile latex
library(xtable)
library(reshape2)
library(gclus)
library(hrbrthemes)
library(finalfit)#para plotear los or
set.seed(123)
#leemos la data del CSV
datos_en_bruto <- read.csv("C:/Users/Josafat/Desktop/creditcard.csv",sep=",",header=TRUE)
datos <- datos_en_bruto
#EDA
head(datos)
describe(datos)
sum(is.na(datos)) #probar esta
mapa <- missmap(datos, main = "Valores perdidos vs Observados")
#Aca plotear las variables(Histogramas o densidad) V1,V2,V3
#Datos Seleccionados

#limpiamos de los datos repetidos
datos <- unique(datos) 
datos$Time <- NULL
#Vemos que existen demasiados de una clase
table(datos$Class) 

#seleccionamos los datos de ambas clases para luego 
#generar un dataframe con la misma cantidad de
#clases
datos$Class <- factor(datos$Class)
#datos$Time <- NULL
datos_clase_0 <- datos[datos$Class=="0",]
datos_clase_1 <- datos[datos$Class=="1",]
#obtenemos un sample de la clase 0 y guardamos
#para hacer reproducible el modelo

#sample<- sample(1:nrow(datos_clase_0),nrow(datos_clase_1))
#saveRDS(sample, file="sample.Rda")
sample <- readRDS(file="sample.Rda")

#946 observaciones y 473 datos de la variable Clase
datos_finales <- rbind(sample,datos_clase_1)

#Correlacion
tmp <- datos_finales[,1:29]
data.corr <- abs(cor(tmp))
ggcorrplot::ggcorrplot(data.corr)

#distribucion de las variables
DATA <- datos_finales[,1:29]
data.r <- abs(cor(DATA))
melted <- melt(data.r)
data.col <- dmat.color(data.r)
data.o <- order.single(data.r)
#cpairs(DATA,data.o,panel.colors = data.col,gap=.5,main="")
ggplot(data=melted,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient(low="white", high="blue") +
  theme_ipsum()

#dividimos la data 70% train 30% test
line_divide <- createDataPartition(y =datos_finales$Class,p = 0.8, list = FALSE)

#datos fit del modelo y testear el modelo
datos_train <- datos_finales[line_divide,]
datos_test <- datos_finales[-line_divide,]

#MODELOS
#usando todas las variables el modelo no converge
model_1 <- glm(Class~.,data=datos_train,family = binomial(link="logit"))
#usando todas las variables menos Amount
model_2 <- glm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10
               +V11+V12+V13+V14+V15+V16+V17+V18+V19
               +V20+V21+V22+V23+V24+V25+V26+V27+V28
               ,data=datos_train,family=binomial(link ="logit"))
model_3 <- glm(Class~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10
               +V11+V12+V14+V16+V17+V18+V19+V20
               +V21+V22+V23+V25+V27+V28
               ,data=datos_train,family=binomial(link ="logit"))
#Nos quedamos con las variables significativas
#sig.var<- summary(model_1)$coeff[-1,4] <0.01
#names(sig.var)[sig.var == TRUE]

#summarys

#anova
anova(model_0,model_1,model_2,test="Chisq")

#Modelo 2 es el mejor porque en anova es significativo
#seudo r2,chisq,AIC

#plot curve


#testing al modelo_2
fitted.results <- predict(model_2,newdata=datos_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != datos_test$Class)
print(paste('Accuracy',1-misClasificError)) #precicion del modelo


p <- predict(model_2, newdata=datos_test, type="response")
pr <- prediction(p, datos_test$Class)
#prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prf <- performance(pr, "fpr","tpr")
tpr.prf <- prf@x.values[[1]]
fpr.prf <- prf@y.values[[1]]
#plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#FALSE POSITIVE RATE VS TRUE POSITIVE RATE
df <- data.frame(True_Positive_Rate = tpr.prf,False_positive_Rate =fpr.prf)
ggplot(aes(False_positive_Rate,True_Positive_Rate),data=df)+geom_line()

#MATRIZ DE CONFUSION
p <- predict(model_2, newdata=datos_test, type="response")
predicciones <- ifelse(test = p > 0.5, yes = 1, no = 0)
matriz_confusion <- table(datos_test$Class, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion
mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))
