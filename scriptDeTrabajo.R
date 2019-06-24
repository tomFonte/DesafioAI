setwd("E:/CrediTú")

# librería para el manejo de datos básicos
library(tidyverse)
# Nota: Las librerías serán llamadas a medida se vayan a necesitar.
# importación de datos
datos<-read.table("data_desafio_seguros",header=T, sep=',')

# Las columnas nombradas de acuerdo a lo indicado en el documento para tener una mejor
# comprensión de las variables que están siendo utilizadas
colnames(datos)<-c("ID","EstadoCtaCte","Duracion","HistCredit","Proposito","MontoCredito",
                   "AhorrosCta","TpoEstado","Tasa","Sex_Status","OtrosDeudores","TpoResid",
                   "Posesiones","Edad","OtrosPagosPendientes","FormaVive","NumCreditosBanco",
                   "TipoTrabajo","Cargas","TieneTelefono","Extranjero","Clasificacion")

# Prueba de replica de datos
length(datos$ID) == length(unique(datos$ID))

# ID es una referencia que contiene solo valores unicos. 
# Para efectos del analisis, será omitido, ya que a partir de este punto no aporta
datos<-datos%>%select(-ID)

# Reclasificación de la variable Clasificación. Sólo calibra para mejor comprensión 
# las etiquetas

datos<-datos%>%mutate(Class = factor(Clasificacion,levels = c(1,2),labels = c(0,1)))
datos<-datos[,-which(colnames(datos)=="Clasificacion")]

# Graficos para EDA
library(reshape2)
# Donut de Clasificacion
with(datos,round(table(as.numeric(Class))/nrow(datos)*100,2))%>%
  melt%>%
  ggplot(aes(x=2,y=value,fill=as.factor(Var1)))+
  geom_bar(width = 1,stat="identity",color='white')+
  coord_polar(theta = "y",start=0)+
  geom_text(aes(y=c(60,10),label=value),color='white')+
  theme_void()+xlim(0.5,2.5)+
  scale_fill_manual(values = c("#000080", "#808080"))

# Donut de Extranjero para ver si cumple con premisa de tasa de migración
with(datos,round(table(as.numeric(Extranjero))/nrow(datos)*100,2))%>%
  melt%>%
  ggplot(aes(x=2,y=value,fill=as.factor(Var1)))+
  geom_bar(width = 1,stat="identity",color='white')+
  coord_polar(theta = "y",start=0)+
  geom_text(aes(y=c(50,2.1),label=value),color='white')+
  theme_void()+xlim(0.5,2.5)+
  scale_fill_manual(values = c("#000080", "#808080"))

datos%>%ggplot(aes(x = Class))+geom_bar()
datos%>%ggplot(aes(x = EstadoCtaCte))+geom_bar(fill = '#000080')+facet_grid(.~Class)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())  

datos%>%ggplot(aes(x = HistCredit))+geom_bar()+facet_grid(.~Class)

# Graficos de dispersion para premisas
datos%>%ggplot(aes(x = Duracion, y = MontoCredito,col=Extranjero))+geom_point()+facet_grid(.~Class)
datos%>%ggplot(aes(x = Duracion, y = MontoCredito,col=TieneTelefono))+geom_point()+facet_grid(.~Class)

# Realizaré distintos métodos de Machine Learning para calcular un score Para ello, 
# necesitaré separar los datos entre entrenamiento y validación

set.seed(1234)
train<-sample(nrow(datos),0.7*nrow(datos),replace = FALSE)
trainSet<-datos[train,]
testSet<-datos[-train,]

## Regresión Logistica.
# Clasificador básico utilizado para este tipo de metodología.

logitreg<-glm(Class~.,data = trainSet, family=binomial)

preds<-predict(logitreg,type="response")
pred<-ROCR::prediction(preds,trainSet$Class)

AccTrain<-table(preds>=0.5,trainSet$Class)

# Creación de tabla para seleccion final del modelo
# En esta tabla se irán guardando los valores representativos de los modelos de ML.

Modelos<-data.frame(Modelo = "Logit",AUC = round(unlist(ROCR::performance(pred,"auc")@y.values)*100,2),
                    Type = "Train",TasaFP = round(AccTrain[1,2]/nrow(trainSet)*100,2),TasaErr = round((AccTrain[1,2]+AccTrain[2,1])/nrow(trainSet)*100,2),stringsAsFactors = F)

# Modelo Regresión Logistica, prueba de Validacion
predNewD<-predict(logitreg,testSet[,-21],type="response")
pred<-ROCR::prediction(predNewD,testSet$Class)

AccTest<-table(predNewD>=0.5,testSet$Class)

Modelos[nrow(Modelos)+1,]<-list("Logit",round(unlist(ROCR::performance(pred,"auc")@y.values)*100,2),
                                "Test",round(AccTest[1,2]/nrow(testSet)*100,2),round((AccTest[1,2]+AccTest[2,1])/nrow(testSet)*100,2))

# Se ajusta con respecto al método de seleccion stepwise. Este método lo replicaré
# cuando sea necesario.
stepMod<-step(logitreg,direction="both")

predStep<-predict(stepMod,type="response")
prediStep<-ROCR::prediction(predStep,trainSet$Class)

AccStep<-table(predStep>=0.5,trainSet$Class)

Modelos[nrow(Modelos)+1,]<-list("Logit-Step",round(unlist(ROCR::performance(prediStep,"auc")@y.values)*100,2),
                                "Train",round(AccStep[1,2]/nrow(trainSet)*100,2),round((AccStep[1,2]+AccStep[2,1])/nrow(trainSet)*100,2))

# Modelo Regresión Logistica Stepwise, prueba de Validacion
predNewD2<-predict(stepMod,testSet[,-21],type="response")
prediStep2<-ROCR::prediction(predNewD2,testSet$Class)
AccTest2<-table(predNewD2>=0.5,testSet$Class)

Modelos[nrow(Modelos)+1,]<-list("Logit-Step",round(unlist(ROCR::performance(prediStep2,"auc")@y.values)*100,2),
                                "Test",round(AccTest2[1,2]/nrow(testSet)*100,2),round((AccTest2[1,2]+AccTest2[2,1])/nrow(testSet)*100,2))

# Modelo Logistico con Interacciones
# Se hace mención de la alta tasa de Extranjeros y lo elevado de los precios 
# en el documento. Por esto, se sospecha que puede haber interacción entre las variables

### Las interacciones muestran la multiplicabilidad de las variables consideradas.
with(datos,interaction.plot(TieneTelefono,Extranjero,MontoCredito))
with(datos,interaction.plot(TieneTelefono,Sex_Status,MontoCredito))

# El modelo de interacciones a aplicar será el siguiente,,
#### Nota: por motivos de capacidad no pude ejecutar un modelo saturado con interacciones
#### elemento que hubiese sido ideal al momento de hacer la selección.
interModel<-glm(Class~Duracion+MontoCredito+Tasa+TpoResid+Edad+
                  NumCreditosBanco+Cargas+EstadoCtaCte+HistCredit+
                  Proposito+AhorrosCta+TpoEstado+Sex_Status+
                  OtrosDeudores+Posesiones+OtrosPagosPendientes+
                  FormaVive+TipoTrabajo+TieneTelefono*Extranjero
                ,data=trainSet,family=binomial) 


pred<-predict(interModel,type="response")
p3<-ROCR::prediction(pred,trainSet$Class)

AccStep<-table(pred>=0.5,trainSet$Class)

Modelos[nrow(Modelos)+1,]<-list("Logit-interaction",round(unlist(ROCR::performance(p3,"auc")@y.values)*100,2),
                                "Train",round(AccStep[1,2]/nrow(trainSet)*100,2),round((AccStep[1,2]+AccStep[2,1])/nrow(trainSet)*100,2))

# Ajuste de Validación

pred<-predict(interModel,testSet[,-21],type="response")
p3<-ROCR::prediction(pred,testSet$Class)

AccStep<-table(pred>=0.5,testSet$Class)

Modelos[nrow(Modelos)+1,]<-list("Logit-interaction",round(unlist(ROCR::performance(p3,"auc")@y.values)*100,2),
                                "Test",round(AccStep[1,2]/nrow(testSet)*100,2),round((AccStep[1,2]+AccStep[2,1])/nrow(testSet)*100,2))

# Stepwise en Interaccion
InterStep<-step(interModel,direction="both")
predInSt<-predict(InterStep,type="response")
predInStep<-ROCR::prediction(predInSt,trainSet$Class)

AccStep<-table(predInSt>=0.5,trainSet$Class)

Modelos[nrow(Modelos)+1,]<-list("Interaction-Step",round(unlist(ROCR::performance(predInStep,"auc")@y.values)*100,2),
                                "Train",round(AccStep[1,2]/nrow(trainSet)*100,2),round((AccStep[1,2]+AccStep[2,1])/nrow(trainSet)*100,2))

predInSt<-predict(InterStep,testSet[,-21],type="response")
predInStep<-ROCR::prediction(predInSt,testSet$Class)
table(predInSt>=0.5,testSet$Class)
AccStep<-table(predInSt>=0.5,testSet$Class)

Modelos[nrow(Modelos)+1,]<-list("Interaction-Step",round(unlist(ROCR::performance(predInStep,"auc")@y.values)*100,2),
                                "Test",round(AccStep[1,2]/nrow(testSet)*100,2),round((AccStep[1,2]+AccStep[2,1])/nrow(testSet)*100,2))

## Random Forest
### Otro modelo comunmente utilizado para hacer clasificación es el modelo de Random Forest
### Probaré su poder de clasificación dado la premisa de obtener alternativas que permitan tener
### menor cantidad de Falsos Positivos (clasificados como buenos cuando son malos)

library(randomForest)     
forestModel<-randomForest(Class~.,data = trainSet, importance = TRUE)

forPred<-predict(forestModel, trainSet[,-21],type="class")

importance(forestModel)

## Gracias a la matriz de confusion en el paquete Caret, podremos revisar el poder de clasificación
Confusion<-caret::confusionMatrix(data = forPred,reference = trainSet[,21])
Confusion$byClass[11]

# Debido a que este modelo no cuenta con Area bajo la curva como medida predictiva
# Solo atenderé a la precision de prediccion.
Modelos[nrow(Modelos)+1,]<-list("RandomForest",NA,
                                "Train",round((1-Confusion$byClass[11])*100,2),
                                round((1-Confusion$byClass[11])*100,2))

varImpPlot(forestModel,sort = T,n.var = 10)
forPred<-predict(forestModel, testSet[,-21],type="class")

Confusion<-caret::confusionMatrix(data = forPred,reference = testSet[,21])

Acc<-Confusion$table

Modelos[nrow(Modelos)+1,]<-list("RandomForest",NA,
                                "Test",round(Acc[1,2]/sum(Acc)*100,2),
                                round((Acc[1,2]+Acc[2,1])/sum(Acc)*100,2))

## SVM
# Un modelo bastante popular es el Support Vector Machine.

library(e1071)

# Suport vector machine
SVM<-svm(Class~.,data = trainSet)
svmPred<-predict(SVM, trainSet[,-21])
svmAcc<-table(svmPred,trainSet[,21])

Modelos[nrow(Modelos)+1,]<-list("SVM",NA,
                                "Train",round(svmAcc[1,2]/sum(svmAcc)*100,2),
                                round((svmAcc[1,2]+svmAcc[2,1])/sum(svmAcc)*100,2))

svmPred2<-predict(SVM, testSet[,-21])
svmAccT<-table(svmPred2,testSet[,21])

Modelos[nrow(Modelos)+1,]<-list("SVM",NA,
                                "Test",round(svmAccT[1,2]/sum(svmAccT)*100,2),
                                round((svmAccT[1,2]+svmAccT[2,1])/sum(svmAccT)*100,2))

# Adaboost
# Por último, dado que los modelos tipo "Ensemble" son muy populares en plataformas como Kaggle
# probaré el modelo de clasificación AdaBoost.
AdaModel<-train(trainSet[,-21],trainSet[,21],method = "ada")
summary(AdaModel)
AdaPredict<-predict(AdaModel)
Acc<-table(AdaPredict,trainSet[,21])

Modelos[nrow(Modelos)+1,]<-list("AdaBoost",NA,
                                "Train",round(Acc[1,2]/sum(Acc)*100,2),
                                round((Acc[1,2]+Acc[2,1])/sum(Acc)*100,2))
AdaPredict<-predict(AdaModel,testSet[,-21])
Acc<-table(AdaPredict,testSet[,21])

Modelos[nrow(Modelos)+1,]<-list("AdaBoost",NA,
                                "Test",round(Acc[1,2]/sum(Acc)*100,2),
                                round((Acc[1,2]+Acc[2,1])/sum(Acc)*100,2))


bestFP<-min(Modelos$TasaFP[Modelos$TasaFP>0 & Modelos$Type == "Test"])
Modelos[which(Modelos$TasaFP == bestFP),]

## Finalmente, el algoritmo con mejor tasa de falsos positivos es el modelo resultante
# de la metodología Step al modelo Logit. En este muchas variables no tienen todos sus 
# niveles significativos, pero por decisión se conservarán.

# Para finalizar, se calculará el precio del seguro segun indicado
score<-predict(stepMod,datos)
datos<-datos%>%mutate(score = predict(stepMod,datos,type="response"))
datos<-datos%>%mutate(p = 0.1*score)
datos<-datos%>%mutate(Precio = (3*(1+0.03+p)))

# Adicionalmente, cual es la tasa final de mal clasificacion. Para esto, se genera la 
# clase
datos<-datos%>%mutate(ModelClass = ifelse(score>=0.5,1,0))
# y se calcula el resultado de la clasificación
datos<-datos%>%mutate(missClass = ifelse(Class != ModelClass,3,Class))


# Tasa de Provision
with(datos,hist(score*MontoCredito))     

# Gráfico boxplot al Precio por Clasificación del Modelo
datos%>%
  ggplot(aes(y=Precio,x=as.factor(ModelClass)))+geom_boxplot(fill = c("#000080","#808080"))+
  scale_x_discrete(name = "Clasificación",breaks = c("0","1"),
                   labels = c("Buenos","Malos"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))

# Gráfico boxplot al Precio por Clasificación detallada
datos%>%unite("ClassPredit",c("Class","ModelClass"))%>%
  ggplot(aes(y=Precio,x=ClassPredit))+geom_boxplot(fill = c("#000080", "#808080","#CD534CFF","#008000"))+
  scale_x_discrete(name = "Clasificación",breaks = c("0_0","0_1","1_0","1_1"),
                   limits = c("0_0","1_1","0_1","1_0"),
                   labels = c("Buenos","Falsos Positivos","Falsos Negativos","Malos"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


# Gráfico univariado de distribución de variable MissClass
with(datos,round(table(Class,as.numeric(missClass))/nrow(datos)*100,2))%>%
  melt%>%subset(value>0)%>%unite("Class_Var",c("Class","Var2"))%>%
  ggplot(aes(x=2,y=value,fill=as.factor(Class_Var)))+
  geom_bar(width = 1,stat="identity",color='white')+
  coord_polar(theta = "y",start=0)+
  geom_text(aes(y=c(70,33,7,22),label=value),color='white')+
  theme_void()+xlim(0.5,2.5)+
  scale_fill_manual(values = c("#000080", "#CD534CFF","#808080","#008000"))
