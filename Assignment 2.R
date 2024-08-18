

                                  ####    Assignment 2    ####


## <a> ##  

data(iris)
install.packages("ggplot2")
library(ggplot2)

# Creating side-by-side boxplots of continuous variables based on species
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Boxplot of Sepal.Length by Species") +
  xlab("Species") +
  ylab("Sepal.Length")

ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Boxplot of Petal.Length by Species") +
  xlab("Species") +
  ylab("Petal.Length")

# Creating a scatterplot of Sepal.Length and Petal.Length, coloring each plot by Species
plot(iris$Sepal.Length, iris$Petal.Length,
     xlab = "# ", ylab = "", col=iris$Species)
legend("bottomright", pch = 16, col = c(1,2,3),
       legend = c("Setosa", "Versicolor", "Virginica"))
#From the scatterplot, we can observe that the three species of iris (setosa, versicolor, and virginica)
#are fairly distinguishable based on their Sepal.Length and Petal.Length measurements. Setosa generally
#has shorter Sepal.Length and smaller Petal.Length compared to the other two species. Versicolor tends
#to have intermediate values for both Sepal.Length and Petal.Length. Virginica typically has longer
#Sepal.Length and larger Petal.Length compared to the other species.


## <b> ##

flip<-function(img){
  
  col.mat <- as.array(img[, ,1, ])
  
  dims <- dim(col.mat)
  rot <- array(0, dim = dims)
  
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      rot[i, j, ] <- col.mat[dims[1] - i+1, j, ]
    }
  }
  
  
  # Let's plot size by side
  par(mfrow = c(1,2))
  plot(dog)
  return(as.cimg(rot))
}
# Testing with dog.jpeg image in week 3 folder
img <- load.image("dog.jpeg")
rot<-flip(img)  
plot(rot)


## <c> ##

library(MASS)
ships
plot(ships$type, ships$incidents/ships$service,
     xlab = "# Type", ylab = "No. of incidents per month of service")

# Ship Type B has been in service for much longer compared to the other types of ships. Therefore it has
# a higher number of incidents simply because it has been in use for way longer than the other ships. We
# can check this by calculating the number of incidents per month of service for each ship type. From the
# given data we can see that ship type E is the least trustworthy ship. Therefore the hypothesis that ship
# type B is the least trustworthy ship as it has had the most number of accidents is false.


## <d> ##

library(rvest)
html<-read_html("https://stats.stackexchange.com/questions?tab=Votes")
title<-html %>% html_elements(".s-post-summary--content-title a") %>% html_text()
no_of_views<-html %>% html_elements(".is-supernova .s-post-summary--stats-item-number") %>% html_text()
no_of_ans<-html %>% html_elements(".has-answers .s-post-summary--stats-item-number") %>% html_text()
no_of_votes<-html %>% html_elements(".s-post-summary--stats-item__emphasized .s-post-summary--stats-item-number") %>% html_text()
df <- data.frame(title,no_of_views,no_of_ans,no_of_votes)
names(df) <- c('Title', 'Number of views', 'Number of answers', 'Number of votes')
df


## <e> ##

calculate_average_days <- function(n_simulations) {
  days_list <- vector("numeric", n_simulations)
  
  for (i in 1:n_simulations) {
    bottle <- rep(2, 100)  # Initializing a bottle with 100 whole tablets
    
    days <- 0
    while (TRUE) {
      days <- days + 1
      
      # Randomly selecting a tablet from the bottle
      selected_tablet <- sample(bottle, 1)
      
      if (selected_tablet == 1) {
        # If it's a half-tablet, break the loop
        break
      } else {
        # If it's a whole tablet, cutting it in half and putting the remaining half back in the bottle
        bottle <- c(bottle, 1)
        bottle <- bottle[-which(bottle == 2)[1]]
      }
    }
    
    days_list[i] <- days
  }
  
  average_days <- mean(days_list)
  return(average_days)
}

n_simulations <- 1000  # Number of simulations to run
average_days <- calculate_average_days(n_simulations)
average_days
