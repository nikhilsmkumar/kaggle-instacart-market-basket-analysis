setwd("C:/Users/NikhilS/Desktop/instakart")
#######INSTALL PACKAGES
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

#read the data
aisles=fread("aisles.csv")
departments=fread("departments.csv")
products=fread("products.csv")
order_products_prior=fread("order_products__prior.csv")
order_products_train=fread("order_products__train.csv")
orders=fread("orders.csv")

#convert the variables
aisles$aisle = as.factor(aisles$aisle)
departments$department = as.factor(departments$department)
orders$eval_set = as.factor(orders$eval_set)
products$product_name = as.factor(products$product_name)

# test data from orders 
test = orders[orders$eval_set=="test",]

#test data with prior users
products_new1 = left_join(products, aisles, by = "aisle_id")
products_new2 = left_join(products, departments, by = "department_id")
total_products = left_join(products_new1,products_new2, by = "product_id",copy=FALSE)
total_products = total_products[,c(1,2,3,4,9,5)]
colnames(total_products) = c("product_id","product_name","aisle_id", "department_id","department","aisle")
rm(products_new1, products_new2)

orders_train = left_join(order_products_train, orders, by = "order_id")
orders_prior = left_join(order_products_prior, orders, by = "order_id")

orders_train = left_join(orders_train, total_products,by = 'product_id')
orders_train = orders_train[,c(-11, -14, -15)]
orders_prior = left_join(orders_prior, total_products,by = 'product_id')
orders_prior = orders_prior[,c(-11, -14, -15)]

total_ordersset = rbind(orders_train,orders_prior)
head(total_ordersset,10)

test_history = total_ordersset[total_ordersset$user_id %in% test$user_id,]


#visualization data
order_products_priorvisz = left_join(order_products_prior, total_products, by = "product_id",copy=FALSE)

#How many items do people rebuy
ggplot(order_products_priorvisz,aes(x=reordered))+
  geom_histogram(stat='count',fill='blue')#seems like people reorder their previous purchased products

#best selling products
bestsellingproductsprior <- order_products_priorvisz %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count))
bestsellingproductsprior %>% 
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="blue")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+ggtitle("best selling products")
#looks people like to buy banana frequently

#best selling departments
bestsellingdepartsprior <- order_products_priorvisz %>% 
  group_by(department) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(departments,department_id,department),by="department") %>%
  arrange(desc(count))
bestsellingdepartsprior %>% 
  ggplot(aes(x=reorder(department,-count), y=count))+
  geom_bar(stat="identity",fill="blue")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+ggtitle("best selling departments")

#best selling aisles
bestsellingaislesprior <- order_products_priorvisz %>% 
  group_by(aisle) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(aisles,aisle_id,aisle),by="aisle") %>%
  arrange(desc(count))
bestsellingaislesprior %>% 
  ggplot(aes(x=reorder(aisle,-count), y=count))+
  geom_bar(stat="identity",fill="blue")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+ggtitle("best selling aisles")
#people like to buy fresh fruits frquently

#unique count in orders
unique_count = apply(orders,2,function(x)length(unique(x)))
unique_chart = as.data.frame(unique_count)
unique_chart = cbind(rownames(unique_chart),unique_chart)
colnames(unique_chart)[1] = "Freq"
#visualizing Count
ggplot(unique_chart, aes(x = (unique_chart$Freq), y = unique_chart$unique_count, fill = Freq)) +
  geom_bar(stat = "identity") + ggtitle("Visualizing Unique counts") + theme_bw() +
  labs(x = "Unique Count", y = "Number of variables") + theme(text = element_text(size = 15))

#visualizing eval_set in orders
ggplot(orders,aes(x=eval_set))+
  geom_histogram(stat = 'count',fill='blue')

#time of day people order
ggplot(orders,aes(x=order_hour_of_day))+
  geom_histogram(stat = 'count',fill='blue')
#people order between 9am to 8pm

#week people buy
ggplot(orders,aes(x=order_dow))+
  geom_histogram(stat = 'count',fill='blue')#mostly people order on sat and sunday

#order from days since prior order
ggplot(orders,aes(x=days_since_prior_order))+
  geom_histogram(stat='count',fill='blue')
#for every 9 and 30 days they order products
#missing values
sapply(orders, function(x) sum(is.na(x)))


#probability of user to reorder and product to reorder
products = inner_join(products, aisles, by = "aisle_id")
products = inner_join(products, departments, by = "department_id")

ordersusers = data.frame(orders$user_id,orders$order_id)
colnames(ordersusers) = c("user_id","order_id")
#add user_id  to order_products_train
order_products_train = left_join(order_products_train, ordersusers, by = "order_id")

order_products = inner_join(orders, order_products_prior,by= 'order_id')
order_products = order_products[order(order_products$user_id, order_products$order_number,order_products$product_id),]

#user probability
userpurchased = data.frame(table(order_products$user_id))
colnames(userpurchased) = c("user_id", "usertimesordered")
userreordered = aggregate(order_products$reordered, by=list(user_id=order_products$user_id), FUN=sum)
colnames(userreordered) = c("user_id", "userreordered")
user_prob = cbind(userpurchased, userreordered)
user_prob[,3] = NULL
user_prob$reorderchance = user_prob$userreordered/user_prob$usertimesordered
user_prob$user_id = as.integer(user_prob$user_id)

#product probability
productspurchased = data.frame(table(order_products$product_id))
colnames(productspurchased) = c("product_id", "prodtimesordered")
productsreordered = aggregate(order_products$reordered, by=list(product_id=order_products$product_id), FUN=sum)
colnames(productsreordered) = c("product_id", "prodreordered")
product_prob = cbind(productspurchased, productsreordered)
product_prob[,3] = NULL
product_prob$prodreorderchance = product_prob$prodreordered/product_prob$prodtimesordered
product_prob$product_id = as.integer(product_prob$product_id)

#order_products_train as training data to build model
trainingdata = order_products_train
trainingdata = left_join(trainingdata,user_prob,by = 'user_id')
trainingdata = left_join(trainingdata,product_prob,by = "product_id")
trainingdata$reordered = as.factor(trainingdata$reordered)
trainingdata$order_id = NULL

#divide the trainingdata into trainmodel and testmodel
trainmodel =trainingdata[sample(nrow(trainingdata), 900000, replace = F), ]
testmodel = trainingdata[!(1:nrow(trainingdata)) %in% as.numeric(row.names(trainmodel)), ]


#Model using C50 algorithm
library(C50)
library(caret)
ruleModel = C5.0(reordered ~ ., data = trainmodel)
summary(ruleModel)
plot(ruleModel)

#variable importance
varimportance = as.data.frame(varImp(ruleModel))
varimportance = cbind(rownames(varimportance),varimportance)
colnames(varimportance) = c("Varimportance", "importance")
rownames(varImp) = NULL
varimportance

#variable importance
ggplot(varimportance,aes(x=Varimportance, y=importance))+
  geom_bar(stat="identity",fill="blue")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+ggtitle("variable importance")

#predict using test data
test_pred = predict(ruleModel, testmodel[,-3], type = "class")

#confusion matrix
xtab = table(observed = testmodel[,3], predicted = test_pred)
confusionMatrix(xtab)

#using test_history
test_history = left_join(test_history,user_prob,by = 'user_id')
test_history = left_join(test_history,product_prob,by = "product_id")
test_history = test_history[,c(2,3,5,13,14,15,16,17,18)]
head(test_history)

#predicting with real test data
test_history_pred = predict(ruleModel, test_history,type = "class")
test_history$reordered = test_history_pred
test_history_new = test_history[,c(1,3,10)]
test = test[,c(2,1)]

finalprediction = left_join(test_history_new,test,by = "user_id")
table(finalprediction$reordered)
finalprediction = finalprediction[which(finalprediction$reordered == 1),]
table(finalprediction$reordered)
finalprediction = finalprediction[,c(4,1)]

#for transactions
finalprediction = split(finalprediction$product_id,finalprediction$order_id)
finalprediction = as.vector(finalprediction)
finalprediction = as.matrix(finalprediction)
finalprediction = as.data.frame(finalprediction)
finalprediction = data.frame(finalprediction)
finalprediction = cbind(rownames(finalprediction), finalprediction)
colnames(finalprediction)[1] = "order_id"
colnames(finalprediction)[2] = "product_id"
rownames(finalprediction) = NULL
finalprediction = as.matrix(finalprediction)
#writing file to submit to kaggle
write.csv(finalprediction,"submit.csv")



