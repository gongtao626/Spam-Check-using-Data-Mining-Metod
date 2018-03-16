#Spamfree model building

#library
library(foreign)
library(RWeka)

#data collection
emailspam=read.arff("http://www.cis.utas.edu.au/iWeb3/~soyeonh/spambase.arff")

#convert spam to categorical data
emailspam$spam=factor(emailspam$spam)#factor

#data preprocessing and transformation
emailspam.scale<-cbind(scale(emailspam[-58]), emailspam[58])
apply(emailspam[-58], 2, sd)
#emailspam<-emailspam.scale
#The features selected by best first algorithm by WEKA software: 4 5 7 16 21 23 24 25 27 42 44 46 52 53 55
#emailspam<-cbind(emailspam[4],emailspam[5],emailspam[7],emailspam[16],emailspam[21],emailspam[23],emailspam[24],emailspam[25],emailspam[27],emailspam[42],emailspam[44],emailspam[46],emailspam[52],emailspam[53],emailspam[55],emailspam[58])

####
#Six model building and evaluation
####

#1 IBK:k-nearest neighbors classifier Aha & Kibler (1991).
emailspam_IBK=IBk(spam~.,data=emailspam)
#evaluate use 10-fold cross evaluation
eval_IBK<-evaluate_Weka_classifier(emailspam_IBK,numFolds=10,complexity=F,seed = 1, class = T)
#get the confusion matrix
cf_IBK<-eval_IBK[4]$confusionMatrix
confusion.matrix.rate<-prop.table(cf_IBK)*100
cf_IBK_rate<-round(confusion.matrix.rate, digit = 2)
diag.index <- cbind(1:2, 1:2)
error.IBK = sum(cf_IBK_rate)-sum(cf_IBK_rate[diag.index])
print(paste("IBK Error Rate= ", round(error.IBK, digit =2)))#9.21(57features)/8.87(15 best first features)

#2 SMO John C. Platt’s sequential minimal optimization algorithm for training a support vector classifier using polynomial or RBF kernels. 
emailspam_svm=SMO(spam~.,data=emailspam)
eval_svm<-evaluate_Weka_classifier(emailspam_svm,numFolds=10,complexity=F,seed = 1, class = T)
cf_svm<-eval_svm[4]$confusionMatrix
confusion.matrix.rate<-prop.table(cf_svm)*100
cf_svm_rate<-round(confusion.matrix.rate, digit = 2)
diag.index <- cbind(1:2, 1:2)
error.svm = sum(cf_svm_rate)-sum(cf_svm_rate[diag.index])
print(paste("svm Error Rate= ", round(error.svm, digit =2)))#9.58/13.52

#3 J48 C4.5 decision trees (Quinlan, 1993)
emailspam_J48=J48(spam~.,data=emailspam)
eval_j48<-evaluate_Weka_classifier(emailspam_J48,numFolds=10,complexity=F,seed = 1, class = T)
cf_j48<-eval_j48[4]$confusionMatrix
confusion.matrix.rate<-prop.table(cf_j48)*100
cf_j48_rate<-round(confusion.matrix.rate, digit = 2)
diag.index <- cbind(1:2, 1:2)
error.j48 = sum(cf_j48_rate)-sum(cf_j48_rate[diag.index])
print(paste("J48 Error Rate= ", round(error.j48, digit =2)))#7.02/7.31


#4 JRip propositional rule learner
emailspam_jrip=JRip(spam~.,data=emailspam)
eval_jrip<-evaluate_Weka_classifier(emailspam_jrip,numFolds=10,complexity=F,seed = 1, class = T)
cf_jrip<-eval_jrip[4]$confusionMatrix
confusion.matrix.rate<-prop.table(cf_jrip)*100
cf_jrip_rate<-round(confusion.matrix.rate, digit = 2)
diag.index <- cbind(1:2, 1:2)
error.jrip = sum(cf_jrip_rate)-sum(cf_jrip_rate[diag.index])
print(paste("JRip Error Rate= ", round(error.jrip, digit =2)))#7.17/7.96

#5 PART decision lists using the approach of Frank and Witten (1998)
emailspam_PART=PART(spam~.,data=emailspam)
eval_PART<-evaluate_Weka_classifier(emailspam_PART,numFolds=10,complexity=F,seed = 1, class = T)
cf_PART<-eval_PART[4]$confusionMatrix
confusion.matrix.rate<-prop.table(cf_PART)*100
cf_PART_rate<-round(confusion.matrix.rate, digit = 2)
diag.index <- cbind(1:2, 1:2)
error.PART = sum(cf_PART_rate)-sum(cf_PART_rate[diag.index])
print(paste("PART Error Rate= ", round(error.PART, digit =2)))#6.41/7.2

#6 AdaBoostM1 AdaBoost M1 method of Freund and Schapire (1996).
emailspam_AdaBoostM1 <- AdaBoostM1(spam~.,data=emailspam,control = Weka_control(W = "PART"))
eval_AdaBoostM1<-evaluate_Weka_classifier(emailspam_AdaBoostM1,numFolds=10,complexity=F,seed = 1, class = T)
cf_AdaBoostM1<-eval_AdaBoostM1[4]$confusionMatrix
confusion.matrix.rate<-prop.table(cf_AdaBoostM1)*100
cf_AdaBoostM1_rate<-round(confusion.matrix.rate, digit = 2)
diag.index <- cbind(1:2, 1:2)
error.AdaBoostM1 = sum(cf_AdaBoostM1_rate)-sum(cf_AdaBoostM1_rate[diag.index])
print(paste("AdaBoostM1 based on J48 Error Rate= ", round(error.AdaBoostM1, digit =2)))#4.39/6.67

