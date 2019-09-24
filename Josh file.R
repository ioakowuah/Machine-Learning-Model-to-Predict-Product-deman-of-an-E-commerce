data <- read_csv("C:/Users/PRINCE/Downloads/Worksheet.csv")

data$Product_cat <- ifelse(data[,1]=="Beans (Oloyin) - Picked"| data[,1]=="Beans (Oloyin) - Unpicked"|data[,1]=="Aeroplane Basmati 5kg"|
                             data[,1]=="Ayoola Poundo Yam"| data[,1]=="Garri"|data[,1]=="Gino Thai Jasmine Rice"|data[,1]=="Golden Penny Semovita 1kg"|
                             data[,1]=="Golden Penny Semovita 2kg"|data[,1]=="Golden Penny Semovita 5kg"|data[,1]=="Golden Penny Semovita 10kg"|
                             data[,1]=="Honeywell Semolina 1kg"|data[,1]=="Honeywell Semolina 2kg"|data[,1]=="Honeywell Semolina 5kg"|data[,1]=="Honeywell Semolina 10kg"|
                             data[,1]=="Long Grain Rice"|data[,1]=="Long Runner Beans"|data[,1]=="Ofada Rice (Picked)"|data[,1]=="Ola Ola Poundo Yam"|data[,1]=="Short Grain Rice"|
                             data[,1]=="Sokoto (White) Beans"|data[,1]=="Tropical Sun Golden Sella Basmati Rice"|data[,1]=="Yam flour (Elubo)"|data[,1]=="Yellow Maize",
                           "staples",
                           ifelse(data[,1]=="Alfa Tomato Ketchup"|data[,1]=="Baby corn"|data[,1]=="Bama Mayonnaise (Small)"|data[,1]=="Bama Mayonnaise"|data[,1]=="Bamboo shoot"|
                                    data[,1]=="Coconut Milk"|data[,1]=="De Rica Tin Tomato"|data[,1]=="De Rica Tin Tomato (850g)"|data[,1]=="De Rica Tomato paste"|data[,1]=="Extreme Mayonnaise"|
                                    data[,1]=="Gino max cube (5g)"|data[,1]=="Gino Peppe & Onion mix"|data[,1]=="Gino Tin"|data[,1]=="Gino Tomato Paste Sachet"|
                                    data[,1]=="Ground Dried Pepper"|data[,1]=="Ground White Pepper"|data[,1]=="Heinz baked beans"|data[,1]=="Heinz Jollof Mix"|data[,1]=="Heinz Salad Cream"|data[,1]=="Heinz Tomato Paste"|
                                    data[,1]=="Heinz Tomato Ketchup"|data[,1]=="Knorr Cubes (Beef)"|data[,1]=="Knorr Chicken Powder"|data[,1]=="Knorr Chicken Cubes",
                                  "condiments",
                                  ifelse (data[,1]=="3.5L Mamador Oil"|data[,1]=="2.5L Mamador Oil"|data[,1]=="1.5L Mamador Oil"|data[,1]=="King's Oil 750ml"|data[,1]=="King's Oil 1L Sachet"|
                                            data[,1]=="King's Margarine 250g"|data[,1]=="5l King's Oil"|data[,1]=="3L King's Oil"|data[,1]=="2L King's Oil"|data[,1]=="25L King's Oil"|data[,1]=="1L King's Oil"|
                                            data[,1]=="10L King's Oil"|data[,1]=="Golden penny pure soya oil"|data[,1]=="Okomu Banga Oil 4l"|data[,1]=="Okomu Banga Oil 2l"|data[,1]=="Palm oil",
                                          "oils",
                                          ifelse (data[,1]=="Crayfish"|data[,1]=="Croaker Fish"|data[,1]=="Geisha"|data[,1]=="Hake Fish (Panla)"|data[,1]=="Tilapia fish"|data[,1]=="Titus Sardines"|
                                                    data[,1]=="Titus (Mackerel) Fish"|data[,1]=="Tuna Fish",
                                                  "Fish",
                                                  ifelse (data[,1]=="Whole (Full) Chicken"|data[,1]=="Chicken neck"|data[,1]=="Chicken Legs (Feet)"|data[,1]=="Chicken Laps"|
                                                            data[,1]=="Chicken heads"|data[,1]=="Chicken gizzard"|data[,1]=="Eggs- Mediumdata[,1]==Minu Chicken Sausage",
                                                          "Poultry","Others")))))

staple <- data[data$Product_cat == "staples",]
condiments <- data[data$Product_cat =="condiments",]
oils <- data[data$Product_cat =="oils",]
Fish <- data[data$Product_cat =="Fish",]
Poultry <- data[data$Product_cat =="Poultry",]
Others <- data[data$Product_cat =="Others",]

####Catergorizing data and creating month variables
wkdata<-rbind(staple,condiments,oils,Fish,Poultry,Others)
wkdata$month<- as.numeric(substr(wkdata$OrderTime, 4,5))
str(wkdata)

###Exploratory Analysis
table(wkdata$Product_cat,wkdata$month)
table(wkdata$Product_cat)

###Barplot
library(ggplot2)
ggplot(wkdata, aes(x=wkdata$Product_cat, y=wkdata$month, fill=wkdata$Product_cat)) +geom_bar(stat="identity")+theme_minimal()

###Subsetting without date
X<-wkdata[,4:5]

####Implementing Random Forest Classification
library(randomForest)
summary(wkdata)
##subsetting Product, month and Quantity
data1<-wkdata[,4:6]
table(data1$Product_cat)
data1$Product_cat<-factor(data1$Product_cat,levels = c('condiments','Fish','oils','Others','Poultry','staples'))
data1<-as.data.frame(as.factor(data1))
str(data1)
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)
# Create a Random Forest model with default parameters
model1 <- randomForest(Product_cat ~ ., data = TrainSet, importance = TRUE)
model1
model2 <- randomForest(Product_cat ~ ., data = TrainSet, ntree = 500, mtry = 2, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Product_cat)
# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$Product_cat)                    
table(predValid,ValidSet$Product_cat)
# To check important variables
importance(model2)        
varImpPlot(model2)
# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(Product_cat ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$Product_cat)
}

a







###Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss<-vector()
for (i in 1:10) wcss[i]<-sum(kmeans(X,i)$withins)
plot(1:10,type = "b")
set.seed(29)
kmeans<-kmeans(X,5,iter.max = 300, nstart = 10)
library(cluster)
clusplot(X,
         kmeans$cluster,
         lines=0,
         shade=TRUE,
         color=TRUE,
         labels=2,
         plotchar=FALSE,
         span=TRUE)