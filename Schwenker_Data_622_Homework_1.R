
Use this table below as a template to report your findings:
  
  Algo	AUC	ACCURACY	TPR	FPR	TNR	FNR
LR	 	 	 	 	 	 
NB	 	 	 	 	 	 
kNN (k=3,k=5)	 	 	 	 	 	 

Rubric: 15%

Summarize and provide a explanatory commentary on the observed performance of these classifiers.

1) Help your client understand that you have selected an appropriate model that has the capacity to learn.

2) Help your client understand that when deployed your model is capable of generalizing.

So to get full credit you will build two tables like the one shown above

one on the models' ability OR capacity to learn

and the other one on the model's ability to generalize.

Rubric: 15% 

(What aspects of the data and or aspects of the algorithms, explain these performance differences)


THESE ARE WICKED-SCIENCE PROBLEMS AND THERE IS NO PRECISE ANSWER. Please base your

observations on common-sense,facts with appropriate citation. 

--------------------
  


run each model and gather performance measures 5 to 10 minutes each

organize and print table 15 minutes

summary and analysis 15 minutes 3 sentences per algorithm 

please submit the R script or your solution in text format

please submit the report in pdf format

You can also submit RMD.
library(e1071)
library(pROC)
library(caret)
data <-read.csv("https://raw.githubusercontent.com/aschwenker/data-622/main/Data_622_Homework_1_dataset.csv")
typeof(data)
class(data)
head(data)
nrow(data)
ncol(data)
dim(data)
names(data)

length(names(table(data$label))) ## if this is 2 then binary classification
print(ifelse(length(names(table(data$label)))==2,"Binary Classification",
             "MultiClass Classification"))
data<-data[complete.cases(data),]
data = na.omit(data)

data$X <- trimws(data$X)
data$Y<- trimws(data$Y)
unique_x <- unique(data$X)
length(unique_x)
unique_Y <- unique(data$Y)
length(unique_Y)
glm_model<-glm(label~.,data=data, family='binomial')

test_prob = predict(glm_model, newdata = data, type = "response",na.action = na.pass)
predtrclass<-ifelse(test_prob<0.5,0,1)
(trcfm<-caret::confusionMatrix(table(trdata[[14]],predtrclass)))

test_roc = roc(data$label ~ test_prob, plot = TRUE, print.auc = TRUE)
AUC = as.numeric(test_roc$auc)

cm<- confusionMatrix(test_prob)
install.packages('caret', dependencies = TRUE)

confusionMatrix( table(test_prob, data) )



Preds <- predict(glm_model, type = 'response')

auc(data$label, Preds)
# using s3 method for glm
auc(glmModel)


NBmodel<-naiveBayes(label~.,data=data)
nbtr.trpred<-predict(NBmodel,data[,-c(9)],type='raw')
nbtr.trclass<-unlist(apply(round(nbtr.trpred),1,which.max))-1
nbtr.trtbl<-table(data[[9]], nbtr.trclass)
tr.cfm<-caret::confusionMatrix(nbtr.trtbl)
tr.cfm