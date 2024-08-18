                                          ## Assignment 3 ##



#<a>#

library(ggplot2)
library(datasets)
data(iris)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(x = "Sepal Length", y = "Petal Length", title = "Scatterplot of Sepal Length and Petal Length")

# From the plot we can see that the Setosa species generally has a shorter sepal and petal length than the 
# Versicolor and Virginica species. The Versicolor Species has moderate and Virginica species has a longer 
# sepal and petal length in general. We can also see a postive correlation between sepal length and petal 
# length for all species. As the sepal length increases, the petal length tends to increase as well.



#<b>#

library(tidyverse)
df<-txhousing
df <- df[complete.cases(df),]
txhousing$month <- factor(txhousing$month, labels=c("Jan", "Feb", "Mar",
                                                    "Apr", "May", "Jun",
                                                    "Jul", "Aug", "Sep",
                                                    "Oct", "Nov", "Dec"))

# aggregate data by city and month
by_month <- group_by(txhousing, city, month) %>% 
  summarise(sales=mean(sales), volume=mean(volume),
            median=mean(median), listings=mean(listings),
            inventory=mean(inventory))
ggplot(by_month, aes(x=month, y=sales, group=city)) + 
  geom_line(data=filter(by_month)) 


# Sales by City
ggplot(txhousing, aes(x=date, y=sales, group=city)) +
  geom_boxplot()










#<c>#

library(tidyverse)
s <- titanic$Survived
s <- as.factor(s)
levels(s) <- c("Died", "Survived")
finalP<-ggplot(titanic,aes(s,Fare,color=Sex))+
geom_boxplot()+
labs(x = "")+
coord_flip()
finalP

