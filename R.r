# Installing the required packages and importing the required Libraries to perform the given task
install.packages("tidyverse")
install.packages("data.table")
install.packages("dplyr")
install.packages("modeest")
library(modeest)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)

tidyr::who # Importing the who dataset

# 1.
who1 <- who %>% pivot_longer(
    cols = starts_with("new"),   # All the Columns that starts with "new" 
    names_to = "Key",            # New column named "Key" for all the columns gathered
    values_to = "Cases",         # New Column named "Cases" for all values
    values_drop_na = TRUE        # Drops rows having missing values
    )

# 2.
who1$Key <- str_replace(who1$Key,'newrel','new_rel') #Replaces the String "newrel" with "new_rel"
who2 <- who1

# 3.
who3 <- who2 %>% separate(Key, c("new", "type", "sexage"), sep = "_")

# 4.
who4 <- who3 %>% separate(sexage, into = c("sex", "age"), sep =1)

# 5.
head(who4, 5)  #Prints the first 5 rows from the dataset who4
tail(who4, 5)  #Prints the last 5 rows from the dataset who4

# 6.
# Create the csv file and saved it in my local directory
write.csv(who4,"who4.csv", row.names = FALSE) 

data("Nile") # loads the Nile data set in R

# 1.
Mean = mean(Nile)                    # Computes the mean value
paste("Mean = ", Mean)

Median = median(Nile)                # Computes the median value
paste("Median = ",Median)

Mode = mfv(Nile)                     # Computes the mode value
cat("Mode = ",Mode,"\n")  

Variance =  var(Nile)                # Computes the Variance
paste("Variance = ",round(Variance,2))

SD =  sd(Nile)                       # Computes the Standard Deviation
paste("Standard Deviation = ",round(SD, 2))

# 2.
Min = min(Nile)            # Computes the minimum value
Max = max(Nile)            # Computes the maximum value
Range = Max - Min          # Computes the range
paste("Min = ", Min)
paste("Max = ", Max)
paste("Range = ", Range)

# 3.
Interquartile_range = IQR(Nile)   # Calculating Interquartile range
Quartiles = quantile(Nile)        # Calculating Quartiles
paste("Interquartile Range = ",Interquartile_range)
paste("Quartiles = ")
print(Quartiles)

# 4.
hist(Nile, main="Histogram of Annual River Nile Volume at Aswan, 1871-1970", 
     xlab="River Flow (1e8 m^3)", ylab="Frequency", col="green")

# 5.
# creating Q-Q plot to compare the dataset to a theoretical normal distribution
qqnorm(Nile, main = 'Q-Q Plot for Normality', ylab="River Flow (1e8 m^3)") 
qqline(Nile, col = "red", lwd = 2)   # Adding reference line to the plot

x <- rnorm(100) # Generating vector of 100 values following a normal distribution

# Creating Q-Q plot to compare the dataset to a random values that follow a normal distribution
qqplot(x, Nile, main = 'Q-Q Plot for Normality', xlab = "Normal Distribution", ylab="River Flow (1e8 m^3)") 
qqline(Nile, col = "red", lwd = 2)

# 6.
plot(Nile,main = 'Time Series', ylab="River Flow (1e8 m^3)", type="b", col = "blue",pch = 21,
     bg = "red", cex = 1.5,lwd = 1 )


mpg = ggplot2::mpg

# 1.
options(repr.plot.width = 9, repr.plot.height =6.5) # changes the plot size

ggplot(data = mpg) + geom_point(mapping = aes(x = cty, y = hwy, color = manufacturer, size = 1)) +
labs(x = "City miles per gallon",y = "Highway miles per gallon", title = "MPG data",color = "Manufacturer") +
theme(plot.title = element_text(size = 20, color = "red"),legend.key.size = unit(0.8, 'cm'),
legend.title = element_text(size=18), legend.text = element_text(size=16), 
axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
axis.title = element_text(size = 16), axis.text = element_text(size = 14)) + 
guides(color = guide_legend(override.aes = list(size = 4)))

# 2.
mpg_displ <- ggplot(mpg, aes(displ,cty,color=class)) +
theme(legend.key.size = unit(0.8, 'cm'), legend.title = element_text(size=18),
legend.text = element_text(size=16), axis.title.x = element_text(size = 16), 
axis.title.y = element_text(size = 16), axis.title = element_text(size = 16), 
axis.text = element_text(size = 14), strip.text.x = element_text(size = 16, 
colour = "black", angle = 360)) + guides(color = guide_legend(override.aes = list(size = 4)))
mpg_displ + facet_wrap(~ class, , scales = "fixed") + 
geom_point() + labs(x = "Engine Displacement",y = "City miles per gallon")

# 3.
ggplot(data=mpg)+
geom_point(aes(x = displ , y = hwy,color = "hwy")) +
geom_point( aes(x = displ, y = cty,color = "cty")) + 
facet_grid(cyl ~ drv~ class, labeller = labeller(.rows = label_both) ) +
labs(x = "Engine Displacement",y = "Miles per gallon") +
theme(legend.key.size = unit(0.8, 'cm'),legend.title = element_text(size=18),
legend.text = element_text(size=16),axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16), axis.title = element_text(size = 16), 
axis.text = element_text(size = 12), 
strip.text.x = element_text(size = 11, colour = "black", angle = 360), 
strip.text.y = element_text(size = 14, colour = "black", angle = 270)) +
guides(color = guide_legend(override.aes = list(size = 4)))

