#Installing required packages
#install.packages("class")
#install.packages("ggplot2")
#install.packages("Amelia")
#install.packages("corrplot")
#install.packages("qwraps2")
#install.packages("table1")
#install.packages("gmodels")
#install.packages("sjPlot")
#install.packages("caret")
#install.packages("dplyr")
#install.packages("tree")
#install.packages("e1071")

library(class)
library(ggplot2)
library(Amelia)
library(corrplot)
library(qwraps2)
library(table1)
library(gmodels)
library(sjPlot)
library(caret)
library(dplyr)
library(tree)
library(e1071)


# Data Preparation and Manipulation
# Loading the "Churn" data
churn <- read.csv(file = "/Users/SagarKumbhare/Desktop/Data Mining and Visualization/churn.txt", stringsAsFactors=TRUE)

#Analyzing data
View(churn)

head(churn)
tail(churn)

str(churn)

summary(churn) 
sum.churn <- summary(churn$Churn) 
sum.churn

#Copying churn to new data frame
churn2 <- churn
churn2

#Finding missing values
missmap(churn) #No missing values

# Calculate proportion of churners
prop.churn <- sum(churn$Churn. == "True") / length(churn$Churn.)
prop.churn

#Calculating number of churned and non churned customers
churn.true <- filter(churn,churn$Churn.=="True.")
churn.true
dim(churn.true)

churn.false<- filter(churn,churn$Churn.=="False.")
churn.false
dim(churn.false)


# Bar chart to show proportions of churners and non churners
barplot(sum.churn,
        ylim = c(0, 3000),
        main = "Bar Graph of Churners and Non-Churners", col = "lightblue")
box(which = "plot", lty = "solid", col="black")


# Boxplot to see outliers or anomalies in minutes of churned customers
boxplot(churn$Day.Mins ~ churn$Churn., data = churn, col = "green",
        xlab = "Customer Churned",ylab = "Total Day mins")
boxplot(churn$Eve.Mins ~ churn$Churn., data = churn, col = "green",
        xlab = "Customer Churned",ylab = "Total Evening mins")
boxplot(churn$Night.Mins ~ churn$Churn., data = churn, col = "green",
        xlab = "Customer Churned",ylab = "Total Night mins")
boxplot(churn$Intl.Mins ~ churn$Churn., data = churn, col = "green",
        xlab = "Customer Churned",ylab = "Total Int mins")


# Boxplot to see outliers or anomalies in Calls of churned customers
boxplot(churn$Day.Calls ~ churn$Churn., data = churn, col = "blue",
        xlab = "Customer Churned",ylab = "Total Day Calls")
boxplot(churn$Eve.Calls ~ churn$Churn., data = churn, col = "blue",
        xlab = "Customer Churned",ylab = "Total Evening Calls")
boxplot(churn$Night.Calls ~ churn$Churn., data = churn, col = "blue",
        xlab = "Customer Churned",ylab = "Total Night Calls")
boxplot(churn$Intl.Calls ~ churn$Churn., data = churn, col = "blue",
        xlab = "Customer Churned",ylab = "Total Int Calls")

# Boxplot to see outliers or anomalies in Charges of churned customers
boxplot(churn$Day.Charge ~ churn$Churn., data = churn, col = "red",
        xlab = "Customer Churned",ylab = "Total Day Charges")
boxplot(churn$Eve.Charge ~ churn$Churn., data = churn, col = "red",
        xlab = "Customer Churned",ylab = "Total Evening Charges")
boxplot(churn$Night.Charge ~ churn$Churn., data = churn, col = "red",
        xlab = "Customer Churned",ylab = "Total Night Charges")
boxplot(churn$Intl.Charge ~ churn$Churn., data = churn, col = "red",
        xlab = "Customer Churned",ylab = "Total Int Charges")


# Boxplot to see outliers or anomalies in Account Length of churned customers
boxplot(churn$Account.Length ~ churn$Churn., data = churn, col = "green",
        xlab = "Customer Churned",ylab = "Account Length")


# Boxplot to see outliers or anomalies in Customer Service Calls of churned customers
boxplot(churn$CustServ.Calls ~ churn$Churn., data = churn, col = "blue",
        xlab = "Customer Churned",ylab = "Customer Service Calls")

# Boxplot to see outliers or anomalies in VMail Messages of churned customers
boxplot(churn$VMail.Message ~ churn$Churn., data = churn, col = "blue",
        xlab = "Customer Churned",ylab = "VMail Message")

#Finding the distribution of numerical variables
numeric.var <- sapply(churn, is.numeric) 

#Correlation between variables
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numeric Variables", method="number")


#Histograms of all numeric variables
#Using churn2 to avoid override
Account_Length <- hist(churn2$Account.Length)

#Days
Day_Mins <- hist(churn2$Day.Mins)
Day_Calls <- hist(churn2$Day.Calls)
Day_Charge <- hist(churn2$Day.Charge)

#Evening
Eve_Mins <- hist(churn2$Eve.Mins)
Eve_Calls <- hist(churn2$Eve.Calls)
Eve_Charge <- hist(churn2$Eve.Charge)

#Night
Night_Mins <- hist(churn2$Night.Mins)
Night_Calls <- hist(churn2$Night.Calls)
Night_Charge <- hist(churn2$Night.Charge)

#International
Intl_Mins <- hist(churn2$Intl.Mins)
Intl_Calls <- hist(churn2$Intl.Calls)
Intl_Charge <- hist(churn2$Intl.Charge)

#Customer Service Calls2
Cust_Serv_Calls <- hist(churn2$CustServ.Calls)

#Vmail Message
Vmail_Msg <- hist(churn2$VMail.Message)

#Log Transformation
#Account Length
logAccountLength <- log(churn2$Account.Length)
summary(logAccountLength)
hist(logAccountLength)

#Days
logDayMins <- log(churn2$Day.Mins)
summary(logDayMins)
hist(logDayMins)

logDayCalls <- log(churn2$Day.Calls)
summary(logDayCalls)
hist(logDayCalls)

logDayCharge <- log(churn2$Day.Charge)
summary(logDayCharge)
hist(logDayCharge)

#Evening
logEveMins <- log(churn2$Eve.Mins)
summary(logEveMins)
hist(logEveMins)

logEveCalls <- log(churn2$Eve.Calls)
summary(logEveCalls)
hist(logEveCalls)

logEveCharge <- log(churn2$Eve.Charge)
summary(logEveCharge)
hist(logEveCharge)

#Night
logNightMins <- log(churn2$Night.Mins)
summary(logNightMins)
hist(logNightMins)

logNightCalls <- log(churn2$Night.Calls)
summary(logNightCalls)
hist(logNightCalls)

logNightCharge <- log(churn2$Night.Charge)
summary(logNightCharge)
hist(logNightCharge)

#International 
logIntlMins <- log(churn2$Intl.Mins)
summary(logIntlMins)
hist(logIntlMins)

logIntlCalls <- log(churn2$Intl.Calls)
summary(logIntlCalls)
hist(logIntlCalls)

logIntlCharge <- log(churn2$Intl.Charge)
summary(logIntlCharge)
hist(logIntlCharge)

#Customer Service Calls
logCustServCalls <- log(churn2$CustServ.Calls)
summary(logCustServCalls)
hist(logCustServCalls)

#Vmail Message
logVmailMsg <- log(churn2$VMail.Message)
summary(logVmailMsg)
hist(logVmailMsg)

#Table of Mean, Median, Std Dev, Min, Max of numeric variables according to the States
table1::label(churn$Account.Length) <- "Account Length"
table1::label(churn$Day.Mins) <- "Day Mins"
table1::label(churn$Day.Calls) <- "Day Calls"
table1::label(churn$Day.Charge) <- "Day Charge"
table1::label(churn$Eve.Mins) <- "Eve Mins"
table1::label(churn$Eve.Calls) <- "Eve Calls"
table1::label(churn$Eve.Charge) <- "Eve Charge"
table1::label(churn$Night.Mins) <- "Night Mins"
table1::label(churn$Night.Calls) <- "Night Calls"
table1::label(churn$Night.Charge) <- "Night Charge"
table1::label(churn$Intl.Mins) <- "Intl Mins"
table1::label(churn$Intl.Calls) <- "Intl Calls"
table1::label(churn$Intl.Charge) <- "Intl Charge"
table1::label(churn$CustServ.Calls) <- "Cust Serv Calls"

table1::table1(~Account.Length+Day.Mins+Day.Calls+Day.Charge+
                 Eve.Mins+Eve.Calls+Eve.Charge+Night.Mins+
                 Night.Calls+Night.Charge+Intl.Mins+Intl.Calls+
                 Intl.Charge+CustServ.Calls|State,data = churn)




#Normalizing numeric data

#Selecting only numeric variables
churn_num <- select_if(churn, is.numeric)
churn_num

#Defining Min_Max normalization function
min_max_norm <- function(x){(x-min(x)) / (max(x)-min(x))}

#Applying Function
churn_min_max_norm <- as.data.frame(lapply(churn_num[1:16], min_max_norm))

head(churn_min_max_norm)

# Z-Score Normalization
churn_ZScore <- as.data.frame(scale(churn_num[1:16]))
head(churn_ZScore)

#Plotting Day Mins Vs Day Charge
ggplot(data=churn,aes(x=Day.Mins,y=Day.Charge))+
  geom_point(alpha=1/20)
  
#OR

attach(churn)
plot(Day.Mins,Day.Charge, main="Scatterplot Day Mins Vs Day Charge",
     xlab="Day_Mins", ylab = "Day_Charge")

#Scatter plot for Night Calls Vs Night Charges
ggplot(data=churn,aes(x=Night.Calls,y=Night.Charge))+
  geom_point()

#Scatter plot for Night Mins Vs Night Charges
ggplot(data=churn,aes(x=Night.Mins,y=Night.Charge))+
  geom_point(alpha=1/20)

#Scatter plot for International Calls Vs International Charges
ggplot(data=churn,aes(x=Intl.Calls,y=Intl.Charge))+
  geom_point(alpha=1/20)

#Scatter plot for International Mins Vs International Charges
ggplot(data=churn,aes(x=Intl.Mins,y=Intl.Charge))+
  geom_point(alpha=1/20)

# Distribution of categorical variables
#International Plan
Inter_plan <- ggplot(data=churn, aes(x=Int.l.Plan, group=Churn., fill=Churn.)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_light()
Inter_plan

#VMail Plan
Vmail_plan <- ggplot(data=churn, aes(x=VMail.Plan, group=Churn., fill=Churn.)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_light()
Vmail_plan

#Histogram of numeric variables with churn overlay
#Day
Day_Mins2<- ggplot(data=churn_num, aes(x = Day.Mins, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Day_Mins")+
  ylab("Churn")
  labs(fill="Churn")
Day_Mins2

Day_Calls2<- ggplot(data=churn_num, aes(x = Day.Calls, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Day_Calls")+
  ylab("Churn")
labs(fill="Churn")
Day_Calls2

Day_Charge2<- ggplot(data=churn_num, aes(x = Day.Charge, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Day_Charge")+
  ylab("Churn")
labs(fill="Churn")
Day_Charge2

#Evening
Eve_Mins2<- ggplot(data=churn_num, aes(x = Eve.Mins, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Eve_Mins")+
  ylab("Churn")
labs(fill="Churn")
Eve_Mins2

Eve_Calls2<- ggplot(data=churn_num, aes(x = Eve.Calls, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Eve_Calls")+
  ylab("Churn")
labs(fill="Churn")
Eve_Calls2

Eve_Charge2<- ggplot(data=churn_num, aes(x = Eve.Charge, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Eve_Charge")+
  ylab("Churn")
labs(fill="Churn")
Eve_Charge2

#Night
Night_Mins2<- ggplot(data=churn_num, aes(x = Night.Mins, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Night_Mins")+
  ylab("Churn")
labs(fill="Churn")
Night_Mins2

Night_Calls2<- ggplot(data=churn_num, aes(x = Night.Calls, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Night_Calls")+
  ylab("Churn")
labs(fill="Churn")
Night_Calls2

Night_Charge2<- ggplot(data=churn_num, aes(x = Night.Charge, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Night_Charge")+
  ylab("Churn")
labs(fill="Churn")
Night_Charge2

#International
Intl_Mins2<- ggplot(data=churn_num, aes(x = Intl.Mins, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Intl_Mins")+
  ylab("Churn")
labs(fill="Churn")
Intl_Mins2

Intl_Calls2<- ggplot(data=churn_num, aes(x = Intl.Calls, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Intl_Calls")+
  ylab("Churn")
labs(fill="Churn")
Intl_Calls2

Intl_Charge2<- ggplot(data=churn_num, aes(x = Intl.Charge, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Intl_Charge")+
  ylab("Churn")
labs(fill="Churn")
Intl_Charge2

#Account Length
Account_Length2<- ggplot(data=churn_num, aes(x = Account.Length, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Account_Length")+
  ylab("Churn")
labs(fill="Churn")
Account_Length2

#Customer Service Call
Cust_Serv_Call2<- ggplot(data=churn_num, aes(x = CustServ.Calls, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("Cust_Serv_Call")+
  ylab("Churn")
labs(fill="Churn")
Cust_Serv_Call2

#Vmail Message
VMail_Msg2<- ggplot(data=churn_num, aes(x = VMail.Message, fill = Churn.)) +
  geom_bar(width = 0.5)+
  xlab("VMail_Msg")+
  ylab("Churn")
labs(fill="Churn")
VMail_Msg2

#Scatter plot for two variables with churn overlay
#Night Calls and Night Charge
plot3 <- ggplot(churn,aes(Night.Calls,Night.Charge)) + 
  geom_point(aes(color=Churn.),size=3,alpha=0.6) +
  scale_color_manual(values = c("#E7B800","#00AFBB"))

plot3

# Night Mins and Night Charges
plot4 <- ggplot(churn,aes(Night.Mins,Night.Charge)) + 
  geom_point(aes(color=Churn.),size=3,alpha=0.6) +
  scale_color_manual(values = c("#E7B800","#00AFBB"))

plot4

#Intl Calls and Intl Charge
plot5 <- ggplot(churn,aes(Intl.Calls,Intl.Charge)) + 
  geom_point(aes(color=Churn.),size=3,alpha=0.6) +
  scale_color_manual(values = c("#E7B800","#FC4E07"))

plot5

#Day Calls and Day Charge
plot6 <- ggplot(churn,aes(Day.Calls,Day.Charge)) + 
  geom_point(aes(color=Churn.),size=3,alpha=0.6) +
  scale_color_manual(values = c("#FC4E07","#00AFBB"))

plot6

#Day Mins and Day Charge
plot7 <- ggplot(churn,aes(Day.Mins,Day.Charge)) + 
  geom_point(aes(color=Churn.),size=3,alpha=0.6) +
  scale_color_manual(values = c("#FC4E07","#00AFBB"))

plot7

#Evening Minutes and Evening Charge
plot8 <- ggplot(churn,aes(Eve.Calls,Eve.Charge)) + 
  geom_point(aes(color=Churn.),size=3,alpha=0.6) +
  scale_color_manual(values = c('#56B4E9',"#FC4E07"))

plot8

#K-NN model to predict churn

set.seed(1000)
knn_train <- train[c(3,7:20)]     
knn_test <- test[c(3,7:20)]      
knn_pred <- pred[c(3,7:20)]       
k <- sqrt(dim(knn_train)[1])         
k

# Training the model
dim(knn_train)

knn_model <- knn(train = knn_train, test = knn_test,cl = train$Churn., k=53)

summary(knn_train)

# Testing the model

p <- CrossTable(x = test$Churn.,y = knn_model,prop.chisq = FALSE )  
Result1 <- table(knn_model,test$Churn.)
Result1

Accuracy <- mean(knn_model==test$Churn.) 
Accuracy
specificity(Result1)                         

sensitivity(Result1)                      

# Decision Tree model to predict churn
attach(train)
set.seed(1000)
tree_model <- tree(Churn. ~.-State-Area.Code-Phone, data = train)
summary(tree_model)      

plot(tree_model)
text(tree_model)

## Testing the Model performance.
set.seed(1000)
predict_tree <- predict(tree_model, test,type = "class")
str(predict_tree)
confusionMatrix(predict_tree,test$Churn.)  


#Logistic Regression model to predict churn
#For 1000 values
set.seed(1000)
idx <- sample(seq(1, 3), size = nrow(churn), replace = TRUE, prob = c(.6, .2, .2))
train <- churn[idx == 1,]
test <-  churn[idx == 2,]
pred <-  churn[idx == 3,]
dim(train)
dim(test)
dim(pred)

attach(train)

set.seed(1000)

logit.model = glm(Churn. ~ Day.Mins + Day.Charge + Eve.Mins + 
                    Eve.Charge + Night.Mins + Night.Charge + Intl.Mins + 
                    Intl.Charge + CustServ.Calls, family = "binomial", data = train)


summary(logit.model)
coef(logit.model)

#For 1500 values
set.seed(1500)
idx <- sample(seq(1, 3), size = nrow(churn), replace = TRUE, prob = c(.6, .2, .2))
train2 <- churn[idx == 1,]
test2 <-  churn[idx == 2,]
pred2 <-  churn[idx == 3,]
dim(train2)
dim(test2)
dim(pred2)

attach(train2)

set.seed(1500)

logit.model2 = glm(Churn. ~ Day.Mins + Day.Charge + Eve.Mins + 
                    Eve.Charge + Night.Mins + Night.Charge + Intl.Mins + 
                    Intl.Charge + CustServ.Calls, family = "binomial", data = train2)


summary(logit.model2)
coef(logit.model2)

# Dropping Minutes columns


attach(train)

set.seed(1000)

logit.model3 = glm(Churn. ~ Account.Length + Day.Charge +  
                    Eve.Charge + Night.Charge +  
                    Intl.Charge + CustServ.Calls, family = "binomial", data = train)


summary(logit.model3)
coef(logit.model3)

#Analyzing the model
anova(logit.model3,test = "Chisq")

#Testing the model
probs = logit.model3 %>% predict(test, type = "response")          
contrasts(test$Churn.)
logit.pred = rep (" False." ,length(test$Churn.))      
logit.pred[probs > 0.5] = " True."                           
Result2 <- table(logit.pred, test$Churn.)  

Result2

MisClassError <- mean(logit.pred != test$Churn.)   
print(paste("Logistic Regression Model Accuracy =", 1-MisClassError))




















