# In this task we are going to predict the good quality houses prices in USA by applying 
#the decision tree method to classify the variables of our dataset and find out what is the 
#the most important factor in the houses prices.

#We starting by Setting our working directory
setwd("C:/Users/Moh/Downloads")

#Now we are importing our data to R by using read.csv code.
USA_Houses <- read.csv("kc_house_data.csv",header = T)


#Now we are inspecting our data
str(USA_Houses)
summary(USA_Houses)
names(USA_Houses)
head(USA_Houses)
tail(USA_Houses)


#Since some of the customers looking for new built houses especially been built within the last 10 years
#So we are going to filter our dataset and keep only the houses have been built after 2010.

#Now we going to make a new dataset calls New_Houses
New_Houses <- USA_Houses

#Change all the houses were built after 2010 to 10
New_Houses$yr_built[New_Houses$yr_built > c(2010)] <- 10

#By using subset function we are going to keep only houses were built after 2010
New_Houses <- subset(New_Houses,yr_built==10)

#Taking off all the not useful columns

New_Houses <- New_Houses[,c(3,4,5,6,7,8,12,13,14,17,18,19,20,21)]

#Now we going to classify the price column to be our target and make the 
#prices over 3 ranges 1 , 2 and 3.

New_Houses$price[New_Houses$price <= c(300000)] <- 1
New_Houses$price[(New_Houses$price > c(300000))&(New_Houses$price <= c(700000))] <- 2
New_Houses$price[New_Houses$price > c(700000)] <- 3


#On the next step we are going to make a new column factor calls price_pre
# this column will be our target for applying the decision tree classification.

New_Houses$price_pre <- as.factor(New_Houses$price)


#Now we will inspect the new data
str(New_Houses)
summary(New_Houses)
names(New_Houses)
head(New_Houses)
tail(New_Houses)

#Setting the seed to make sure we will not get different results
set.seed(4444)

#Now we dividing the variables into 2 samples 1 for training and 2 for validation
#and add the probability for both of them 80% for training ,20% for validation. 
Pred_New <- sample(2, nrow(New_Houses),replace = T,prob = c(0.8,0.2))
Pred_New



#With this step we going to appoint 1 for training and 2 for validation by making 
#a new data set calls train and validate.
train_new <- New_Houses[Pred_New==1,]
validate_new <- New_Houses[Pred_New==2,]


#Now we will apply the first method of classification by using the ctree function

#Now we going to insatll a package calls party so we can get another idea from the
#tree diagram.
install.packages("party")
library(party)



houses_tree_new <- ctree(price_pre ~ bedrooms + bathrooms + sqft_living   
                         + floors + grade + sqft_above + sqft_basement , train_new)

#Viewing our tree
houses_tree_new
plot(houses_tree_new)
plot(houses_tree_new,type="simple")

predict(houses_tree_new,validate_new)

predict(houses_tree_new,validate_new,type="prob")

# 

houses_tree_new <- ctree(price_pre ~ bedrooms + bathrooms + sqft_living   
                         + floors + grade + sqft_above +  sqft_basement , train_new, 
                         controls = ctree_control(mincriterion = 0.9,minsplit = 200))

#Viewing our tree
houses_tree_new
plot(houses_tree_new)
plot(houses_tree_new,type="simple")

#

Trai_New <- predict(houses_tree_new,train_new)

tab1_new <- table(predict=Trai_New , Actual = train_new$price_pre)
tab1_new
sum(diag(tab1_new))/sum(tab1_new)
1-sum(diag(tab1_new))/sum(tab1_new)

#

Vali_New <- predict(houses_tree_new,validate_new)

tab2_new <- table(predicted=Vali_New, Actual = validate_new$price_pre)
tab2_new
sum(diag(tab2_new))/sum(tab2_new)
1-sum(diag(tab2_new))/sum(tab2_new)

#Apply the second decision tree rpart method

#Install rpart and rpart.plot packages.
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

Tree_New <- rpart(price_pre ~ bedrooms + bathrooms + sqft_living
                  + floors + grade + sqft_above + sqft_basement , train_new)
rpart.plot(Tree_New,box.palette = "RdBu",shadow.col = "gray",nn=T)





############# Now we are going to predict the house prices when only the maximum conditions 

#Make a new data set calls houses_t, so we can make some change with keeping the original data as it is.
houses_t <- USA_Houses

#we will predict the houses prices when the maximum condition applys which is 5 

houses_t <- subset(houses_t,condition == 5)

#On the next step we are taking off some columns
#as they are not useful in our classification

houses_t$id= NULL
houses_t$date = NULL
houses_t$zipcode = NULL
houses_t$long= NULL
houses_t$lat= NULL
houses_t$condition=NULL


#Inspect the new dataset houses_t

str(houses_t)
summary(houses_t)
names(houses_t)
head(houses_t)
tail(houses_t)

#Now we going to classify the price column to be our target and make the 
#prices over 3 ranges 1 , 2 and 3.

houses_t$price[houses_t$price <= c(200000)] <- 1
houses_t$price[(houses_t$price > c(200000))&(houses_t$price <= c(700000))] <- 2
houses_t$price[houses_t$price > c(700000)] <- 3

#On the next step we are going to make a new column factor calls price_pre
# this column will be our target for applying the decision tree classification.

houses_t$price_pre <- as.factor(houses_t$price)

#Inspect our data after adding a new factor column

str(houses_t)
summary(houses_t)
names(houses_t)

#By setting a seed we can make sure that our outcome resulte will be fixed as much as we run
#the pred step.

set.seed(5555)

#Now we deviding the variables into 2 samples 1 for training and 2 for validation
#and add the probability for both of them 80% for training ,20% for validation. 
Pred <- sample(2, nrow(houses_t),replace = T,prob = c(0.8,0.2))
Pred

#With this step we going to appoint 1 for training and 2 for validation by making 
# 2 new data sets call train and validate.
train <- houses_t[Pred==1,]
validate <- houses_t[Pred==2,]



#By making a new data set we can apply the first decision tree method by using 
#rpart function
r.houses <- houses_t

r.houses <- rpart(price_pre ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors
                  + waterfront + view + grade + sqft_above + 
                   sqft_basement + yr_built + yr_renovated + sqft_living15 + sqft_lot15 , train)

#Now we are going to have a better idea of the result by using rpart.plot
rpart.plot(r.houses,box.palette = "RdBu",shadow.col = "gray",nn=T)


#Now we will apply the ctree function to make a classification on all of our
#data variables with keeping the price_pre our target.

houses_tree_full <- ctree(price_pre ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors
                            + waterfront + view + grade + sqft_above + 
                              sqft_basement + yr_built + yr_renovated + sqft_living15 + sqft_lot15 , train)

print(houses_tree_full)
plot(houses_tree_full)
plot(houses_tree_full,type="simple")

houses_tree_full <- ctree(price_pre ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors
+ waterfront + view + grade + sqft_above + sqft_basement + yr_built + 
  yr_renovated + sqft_living15 + sqft_lot15 , train,controls = ctree_control(mincriterion = 0.99,
                                                                             minsplit = 200))

#Now we will have an idea what is the most effective factor on predicting house price by printing the results

print(houses_tree_full)
plot(houses_tree_full)
plot(houses_tree_full,type="simple")

#Now we will train our dataset 
Tra_full <- predict(houses_tree_full,train)

tab1_full <- table(predict=Tra_full , Actual = train$price_pre)
tab1_full
sum(diag(tab1_full))/sum(tab1_full)
1-sum(diag(tab1_full))/sum(tab1_full)



#Now we will do validation on our dataset.
Vali_full <- predict(houses_tree_full,validate)
tab2_full <- table(predicted=Vali_full, Actual = validate$price_pre)
tab2_full
sum(diag(tab2_full))/sum(tab2_full)
1-sum(diag(tab2_full))/sum(tab2_full)



#Since the grade variable being the most effective variable on our target to classify the 
#observations into 1,2 and 3 so,now we will apply the ctree function to 
#make a classification on the most important variables with keeping the grade in.

houses_part <- ctree(price_pre ~ sqft_living +grade + bathrooms ,data =  train)


houses_part

plot(houses_part)

plot(houses_part,type="simple")


Tra_part <- predict(houses_part,train)

tab1_part <- table(predict=Tra_part , Actual = train$price_pre)
tab1_full
sum(diag(tab1_full))/sum(tab1_full)
1-sum(diag(tab1_full))/sum(tab1_full)


Vali_part <- predict(houses_part,validate)
tab2_part <- table(predict=Vali_part, Actual = validate$price_pre)
tab2_part
sum(diag(tab2_part))/sum(tab2_part)
1-sum(diag(tab2_part))/sum(tab2_part)

houses_part <- rpart(price_pre ~ sqft_living + grade + bathrooms ,data =  train)

rpart.plot(houses_part,box.palette = "RdBu",shadow.col = "gray",nn=T)


## END ##















# Now we will export our prepared data set into excel so we can import it into SAS Enterprise Miner
install.packages("writexl")
library(writexl)
write_xlsx(New_Houses,"price_prediction2010.xls")






