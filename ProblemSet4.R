#Lesson 4 Exercises
setwd("C:\\Users\\jeniferjones\\SkyDrive\\SlideRule_DS_Workshop\\EDA\\Two_Var")
library(datasets)
library(ggplot2)
library(dplyr)
#Your first task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.
ggplot(aes(x=x, y=price), data=diamonds)+ geom_point()
ggsave(filename="Question1.png")
dev.off()
#What are your observations?
#There is a positive trend as we price increases that look exponential 
#in nature, additionally there are a lot of concentrated areas with many 
#points as well as a few outliers. 

#What is the correlation between price and x?
cor.test(diamonds$x, diamonds$price, method = "pearson")
#What is the correlation between price and y?
cor.test(diamonds$y, diamonds$price, method = "pearson")
#What is the correlation between price and z?
cor.test(diamonds$z, diamonds$price, method = "pearson")

# Create a simple scatter plot of price vs depth.
ggplot(aes(x=depth, y=price), data=diamonds)+ geom_point()
ggsave(filename="Question4.png")
dev.off()
# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. 
ggplot(aes(x=depth, y=price), data=diamonds) + 
  geom_point(alpha = 1/100) + 
  scale_x_continuous(breaks = seq(40,80, by=2))
                                                      
ggsave(filename="Question5.png")
dev.off()
#What is the correlation between depth vs price?
cor.test(diamonds$depth, diamonds$price, method = "pearson")
#Since the value is -0.01 there is no meaningful correlation relationship between the variables. 

# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.
ggplot(aes(x=carat, y=price), data=diamonds) + 
  geom_point() + 
  xlim(0, quantile(diamonds$carat, 0.99)) + 
  ylim(0, quantile(diamonds$price, 0.99))
ggsave(filename="Question8.png")
dev.off()

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

# Create a new variable for volume in the diamonds data frame.
diamonds$volume<-diamonds$x * diamonds$y * diamonds$z

#Create scatterplot
ggplot(aes(x=volume, y=price), data=diamonds)+ geom_point()
ggsave(filename="Question9.png")
dev.off()

#Observations for scatterplot
#There are a few outliers which significantly skew the scale of the graph, 
#most of the points are concentrated towards the left of the x-axis toward the lower volume.
#However, it does look like there is a relationship between increasing volume and increasing price. 
#Additionally, there are some points on the 0 volume line which may indicate an issue within the dataset.

#What is the correlation of price and volume?
#Exclude diamonds that have a volumn of 0 or that are greater than or equal to 800
diamonds_correlation<-subset(diamonds, volume!=0 & volume <800)
cor.test(diamonds_correlation$volume, diamonds_correlation$price, method = "pearson")

#Correlation coefficient of 0.92 indicating a strong positive correlation.

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)

ggplot(aes(x=volume, y=price), data=diamonds_correlation) + 
  geom_point(alpha = 1/20) + 
  coord_cartesian(xlim=c(0,800), ylim=c(0,20000)) + 
  geom_smooth(method = "lm", color="green")
ggsave(filename="Question12.png")
dev.off()


#Do you think this would be a useful model to estimate
# the price of diamonds? Why or why not?
#Based on the correlation coefficient and the fit of the line I think this would be a 
#good fit for estimating the price of diamonds.  However, I would expect other factors
#such as the cut and clarity to also affect the price.  


# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

diamondsByClarity<-diamonds%>%
  group_by(clarity) %>%
  summarize(mean_price=mean(price), 
            median_price = quantile(price, 0.5), 
            min_price = min(price), 
            max_price = max(price),             
            n=n())%>%
  arrange(clarity)

#Had to use quantile instead of median due to an integer related error

#Code from udacity

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))


# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.
library(gridExtra)
Plot_1<-qplot(x=clarity, y=mean_price, data = diamonds_mp_by_clarity, geom="bar", stat="identity")
Plot_2<-qplot(x=color, y=mean_price, data = diamonds_mp_by_color, geom="bar", stat="identity")
grid.arrange(Plot_1, Plot_2, ncol=1)
ggsave(filename="Question14.png")
dev.off()
#What do you notice in each of the bar charts for mean price by clarity and mean price by color
#In the clarity graph the price decreases as we increase the clarity as well as decreasing with better color values.  

#Recommend to look by cut - see below
diamonds_by_cut <- group_by(diamonds, cut)
diamonds_mp_by_cut <- summarise(diamonds_by_cut, mean_price = mean(price))
Plot_3<-qplot(x=cut, y=mean_price, data = diamonds_mp_by_cut, geom="bar", stat="identity")
grid.arrange(Plot_1, Plot_2, Plot_3, ncol=1)
ggsave(filename="Question14a.png")
dev.off()




