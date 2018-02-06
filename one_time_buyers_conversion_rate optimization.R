install.packages("data.table")
library(data.table)
library(sqldf)
library(glmnet)
library(ggplot2)
###set up the data
path<-'/Users/Amber/Desktop/new life/bittiger'
wd <- setwd(path); customer_table <- fread("customer_table.csv",na.strings = c("unkown",'NA',"N/A","?"))
category_table <- fread('category_table.csv')
product_table<- fread("product_table.csv")
order_table<- fread("order_table.csv")

###check data
head(customer_table)
str(customer_table)
format(customer_table$customer_id, scientific=FALSE)
format(order_table$customer_id, scientific=FALSE)
format(order_table$product_id, scientific=FALSE)
format(order_table$order_id, scientific=FALSE)
format(product_table$product_id, scientific=FALSE)
names(customer_table)
customer_table$gender<-as.factor(customer_table$gender)


###select ones that purchase before 20161222
str(order_table$order_date)
buyers<-order_table[order_table$order_date<20161222 & order_table$order_amount>0]
format(customer_table$customer_id, scientific=FALSE)
format(order_table$customer_id, scientific=FALSE)
format(order_table$product_id, scientific=FALSE)
format(order_table$order_id, scientific=FALSE)
format(product_table$product_id, scientific=FALSE)
onetimebuyers<-sqldf("SELECT customer_id,order_date,order_id,order_amount
      FROM buyers
      GROUP BY 1
      WHERE order_date BETWEEN 20161223 AND 20160222
      HAVING count(1)=1
      ")
repurchasebuyers<-sqldf("SELECT customer_id,order_date,order_amount
                        FROM order_table
       WHERE order_date BETWEEN 20170222 AND 20170522 AND order_amount >0
                        GROUP BY 1")

targetbuyers<-sqldf("SELECT otb.customer_id as customer_id,
  CASE WHEN rb.customer_id IS NOT NULL THEN 1 ELSE 0 END as is_purchase
                     FROM onetimebuyers as otb
                     left join repurchasebuyers as rb
                     on rb.customer_id= otb.customer_id
                    ")
###take care of character types
str(targetbuyers)
modeltable<-sqldf('SELECT ct.*,tb.is_purchase
FROM customer_table ct
inner join targetbuyers tb
on tb.customer_id = ct.customer_id')

str(modeltable)
modeltable$gender<-as.numeric(modeltable$gender)
str(modeltable$gender)
logisticmodel<-glm(modeltable$is_purchase~.-country,data=modeltable,family = binomial)






