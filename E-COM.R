#Hellloooooo prof Jacek :) Welcome to my CLV in e-commerce project.
#At the beggining I wanted to point out that i did not use GLM family='binomial',
#Because when I looked for a topic I would be truly interested in - I found one after hours of searching.
#Than it turned out that "binomial" wouldn't work for it. I still hope that you'd take my project seriously and check it :)
#I really enjoyed making it + I wanted to make it the way that maybe you'd want to show other students :)

#BIREFLY EXPLAINED:
#This project analyzes Customer Lifetime Value (CLV) in e-commerce using a dataset 
#with 100,000 observations, focusing on understanding the relationships between CLV and factors 
#like revenue, units sold, discounts, and regional variations. The analysis employs 
#Generalized Linear Models (GLM) with different link functions and explores various approaches 
#to handling outliers, including both removal and capping methods. 
#My project's significance lies in its comprehensive comparison of modeling approaches 
#for CLV prediction in e-commerce, particularly in dealing with non-normal distribution 
#and heteroscedasticity in the data :)



#Let's start :)
install.packages("stats")
install.packages("graphics")
install.packages("ggplot2")
install.packages("dplyr")


#Not complicated from where I got the dataset huh? :)
kaggle <- read.csv("/Users/bartekgornicki/Desktop/STATYSTYKA PROJEKT/ecommerce_data.csv")
head(kaggle)
str(kaggle)
summary(kaggle)





colSums(is.na(kaggle))
#Oh LOL, I guess we have 0 missing values. How great isn't it?



clv_data <- aggregate(Revenue ~ Customer_ID, data = kaggle, sum)
colnames(clv_data)[2] <- "Customer_Lifetime_Value"
kaggle <- merge(kaggle, clv_data, by="Customer_ID", all.x=TRUE)
#Since Customer Lifetime Value (CLV) is not directly in the dataset, we have to estimate it as total rev / customer.





kaggle$Region <- as.factor(kaggle$Region)
#Simple conversion of Region to a factor.



#Now let's dive to something finally interesting which is EEEEDDDDAAAAAA.

hist(kaggle$Customer_Lifetime_Value, main="CLV Distribution", col="pink", breaks=30)

boxplot(kaggle$Customer_Lifetime_Value, main="CLV Boxplot")
#I saw that there are a lot of OLs, so we have 3 options: 
#1. Cap them to reduce their impact using "pmin"
#2. Remove them if they are "anomalies".
#3. Leave them.



#For the purpose of this project I've decided to leave them for now since there are 100 000 obs. 
#And we don't know if they will cause any problems or influence any correlations. If they will - we'll remove them or cap them.





cor(kaggle[, c("Customer_Lifetime_Value", "Units_Sold", "Discount_Applied", "Revenue")])
plot(kaggle$Revenue, kaggle$Customer_Lifetime_Value, main="Revenue vs CLV", col="purple")
plot(kaggle$Units_Sold, kaggle$Customer_Lifetime_Value, main="Units Sold vs CLV", col="black")
plot(kaggle$Discount_Applied, kaggle$Customer_Lifetime_Value, main="Discount vs CLV", col="yellow")
#There we can see 3 plots of correlations. I'd say that they didn't give us much information.
#Since there are 100 000 points and the plots are very dense.




qqnorm(kaggle$Customer_Lifetime_Value)
qqline(kaggle$Customer_Lifetime_Value, col="red")
#Checking Q-Q plot gave us info that the points deviate significantly from the red line, especially at the extremes (left and right).
#This suggests skewness and heavy tails, meaning Customer Lifetime Value (CLV) is not normally distributed.




test_model <- lm(Customer_Lifetime_Value ~ Revenue + Units_Sold + Discount_Applied + Region, data=kaggle)
plot(test_model$fitted.values, residuals(test_model), main="Homoscedasticity Check")
abline(h=0, col="red")
#Now we need to make a decision which model to choose. I'd stick with OLS or GLM
#Since they are the easiest ones for me beside LR.



#The plot above gave us information that the residuals appear to have a funnel shape, which suggests heteroscedasticity - not homo.
#Some points are spread out more at higher fitted values, which means the model might not be handling variance well.





#Let's stick with GLM than.
#Moment we've all been waiting for... OUR MODEL!!!


model_glm <- glm(Customer_Lifetime_Value ~ Revenue + Units_Sold + Discount_Applied + Region,
                 data=kaggle, family=Gamma(link="log"))
summary(model_glm)

#Hmm.. What can we see..:
#Median (0.05) close to 0, which is good.
#1st and 3rd Quartile (-0.4007 to 0.2585) are small, meaning most errors are close to 0.
#Min/Max (-3.01 to 1.74) indicate some outliers, but they are not extreme.



#Let's try and remove outliers and run GLM.
kaggle_clean_by_1percent <- kaggle[kaggle$Customer_Lifetime_Value < quantile(kaggle$Customer_Lifetime_Value, 0.99), ]
model_glm <- glm(Customer_Lifetime_Value ~ Revenue + Units_Sold + Discount_Applied + Region,
                 data=kaggle, family=Gamma(link="log"))
summary(model_glm)


#We can try to use different link function. (Both are datasets cleaned by dropping top 1%)

test_model_inv <- glm(Customer_Lifetime_Value ~ Revenue, 
                      family = Gamma(link = "inverse"), data = kaggle_clean_by_1percent)

test_model_identity <- glm(Customer_Lifetime_Value ~ Revenue, 
                           family = Gamma(link = "identity"), data = kaggle_clean_by_1percent)

AIC(model_glm, test_model_inv, test_model_identity)

#As from what I can see - identity link function performed slightly better.



#Here I cleaned the data not by "not using" the top 1% (which I assume are outliers)
#But rather the pmin method which prevents extreme values from overly influencing the model while still using all data points.
kaggle_clean_by_pmin <- kaggle
kaggle_clean_by_pmin$Customer_Lifetime_Value <- pmin(kaggle$Customer_Lifetime_Value, quantile(kaggle$Customer_Lifetime_Value, 0.99))


test_model_identity_with_pmin <- glm(Customer_Lifetime_Value ~ Revenue, 
                           family = Gamma(link = "identity"), data = kaggle_clean_by_pmin)
summary(test_model_identity_with_pmin)

#AIC: 1747896 - pmin tured out just a little bit worse than dropping 1%.




model_gaussian <- glm(Customer_Lifetime_Value ~ Revenue + Units_Sold + Discount_Applied + Region,
                      data=kaggle, family=gaussian(link="identity"))
plot(model_gaussian)

AIC(model_gaussian)
#Unfortunately worse than Gamma..