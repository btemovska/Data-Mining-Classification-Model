library(rpart)
library(caret)
library(rpart.plot)
library(dplyr)
library(data.tree)
library(caTools)
library(party)
library("RSQLite")

#Prepare Data
setwd("~/SQLite")
db<-dbConnect(SQLite(), dbname="DataMining.db")
dbListTables(db) 
df<-data.frame(dbGetQuery(db, 'select * from inventory_sales')) #[1] 942 rows 7 variables
str(df)

# narrow down to relevant columns
df<-select(df, Sale, Stock, Season, StockLevel) #[1] 942 rows 4 variables
head(df)
str(df)
df<-mutate(df, StockLevel=factor(StockLevel))  
str(df)

#Split data into training and test data sets
set.seed(1234)
g<-runif(nrow(df)) 
df<-df[order(g),]
head(df)

sample=sample.split(df$StockLevel, SplitRatio = 0.70)
train=subset(df, sample==TRUE)
test=subset(df, sample==FALSE)

#Tree with TRAIN
T1<-rpart(StockLevel~ ., data=train)
T1
prp(T1, main= "Tree_Train")
summary(T1) 
#Prediction with Train
P1<-predict(T1, train, type = 'class')
#Confusion Matrix with Train
confusionMatrix(P1, train$StockLevel) # Accuracy : 0.9818 
      #           Reference
      #Prediction high low
      #      high  112   5
      #      low     7 535

#Tree with TEST
T2<-rpart(StockLevel~ ., data=test)
T2
prp(T2, main= "Tree")
summary(T2)
#Prediction with Test
P2<-predict(T2, test, type = 'class')
#Confusion Matrix with Test
confusionMatrix(P2, test$StockLevel) # Accuracy : 0.9611     
      #           Reference
      #Prediction high low
      #     high   45   5
      #     low     6 227

par(mfrow=c(1,2))
prp(T1, main= "Tree_Train")
prp(T2, main= "Tree_Test")

# and it looks like the model approves of what we did as the test came at 96% and training at 98% accuracy
