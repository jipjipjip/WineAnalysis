library("ggplot2")
library ("dplyr")
library("gridExtra")
library(GGally)
library("patchwork")

#centering titles on graphs
theme_update(plot.title = element_text(hjust = 0.5))

dataset <- read.csv("C:/Users/user/OneDrive/Documents/R Projects/Wine Quality/wineQualityReds.csv")

print(dataset,5)
head(dataset, 5)

summary (dataset)
glimpse(dataset)


#Turns quality column from integer to factor and ordered

dataset$quality <- factor(dataset$quality, ordered = T)

class(dataset$quality)

dataset$rating <- ifelse(dataset$quality <5, "bad", ifelse(dataset$quality<7, "average", "good"))

dataset$rating <- ordered(dataset$rating, levels = c("bad", "average", "good"))
summary(dataset)
class(dataset)
glimpse(dataset)

#plotting barchart for quality and rating
ggplot(dataset, aes(x = quality)) + geom_bar(width = 1, color = "black", fill = I('blue')) + ggtitle("Quality of Wine")

ggplot(dataset, aes(x = rating)) + geom_bar(width = 1, color = "black", fill = I('dark green')) + ggtitle("Wine Ratings")

#graphs for fixed acidity
#Use jittering over Point Overplotting is when one or more points are in the same place 
#(or close enough to the same place) that you can't look at the plot and tell how many points are there.
#Jittering is adding a small amount of random noise to data. 
#It is often used to spread out points that would otherwise be overplotted.
#It is only effective in the non-continuous data case where overplotted points typically are surrounded by whitespace

fixedacidity.boxplot <- ggplot(dataset, aes(x = 1, y = fixed.acidity)) + geom_jitter(alpha = 0.1) + geom_boxplot(alpha = 0.2, colour = "red") + scale_y_continuous(lim = c(4,16))

fixedacidity.histogram <- ggplot(dataset, aes(x = fixed.acidity)) + geom_histogram(binwidth = 1, color = 'black', fill = I('orange')) + scale_x_continuous(lim = c(4,16)) 

fixedacidity.boxplot + fixedacidity.histogram + plot_annotation(title = "Fixed Acidity")

volatileacidity.boxplot <- ggplot(dataset, aes(x = 1, y = volatile.acidity)) + geom_jitter(alpha = 0.1) + geom_boxplot(alpha = 0.2, colour = "red") + scale_y_continuous(lim = c(0,2))

volatileacidity.histogram <- ggplot(dataset, aes(x = volatile.acidity)) + geom_histogram(binwidth = 0.05, color = 'black', fill = I('orange')) + scale_x_continuous(lim = c(0,2)) 

volatileacidity.boxplot + volatileacidity.histogram + plot_annotation(title = "Volatile Acidity")

citricacid.boxplot <- ggplot(dataset, aes(x = 1, y = citric.acid)) + geom_jitter(alpha = 0.1) + geom_boxplot(alpha = 0.2, colour = "red") + scale_y_continuous(lim = c(0,1))

citricacid.histogram <- ggplot(dataset, aes(x = citric.acid)) + geom_histogram(binwidth = 0.05, color = 'black', fill = I('orange')) + scale_x_continuous(lim = c(0,1)) 

citricacid.boxplot + citricacid.histogram + plot_annotation(title = "Citric Acid")

residualsugar.boxplot <- ggplot(dataset, aes(x = 1, y = residual.sugar)) + geom_jitter(alpha = 0.1) + geom_boxplot(alpha = 0.2, colour = "red") + scale_y_continuous(lim = c(0,16))

residualsugar.histogram <- ggplot(dataset, aes(x = residual.sugar)) + geom_histogram(binwidth =1, color = 'black', fill = I('orange')) + scale_x_continuous(lim = c(0,16)) 

residualsugar.boxplot + residualsugar.histogram + plot_annotation(title = "Residual Sugar")

chlorides.boxplot <- ggplot(dataset, aes(x = 1, y = chlorides)) + geom_jitter(alpha = 0.1) + geom_boxplot(alpha = 0.2, colour = "red") + scale_y_continuous(lim = c(0,0.25))

chlorides.histogram <- ggplot(dataset, aes(x = chlorides)) + geom_histogram(binwidth = 0.01, color = 'black', fill = I('orange')) + scale_x_continuous(lim = c(0,0.25)) 

chlorides.boxplot + chlorides.histogram + plot_annotation(title = "Chlorides")

freesulfurdioxide.boxplot <- ggplot(dataset, aes(x = 1, y = free.sulfur.dioxide)) + geom_jitter(alpha = 0.1) + geom_boxplot(alpha = 0.2, colour = "red") + scale_y_continuous(lim = c(0,75))

freesulfurdioxide.histogram <- ggplot(dataset, aes(x = free.sulfur.dioxide)) + geom_histogram(binwidth =1, color = 'black', fill = I('orange')) + scale_x_continuous(lim = c(0,75)) 

freesulfurdioxide.boxplot + freesulfurdioxide.histogram + plot_annotation(title = "Free Sulfur Dioxide")

totalsulfurdioxide.boxplot <- ggplot(dataset, aes(x = 1, y = total.sulfur.dioxide)) + geom_jitter(alpha = 0.1) + geom_boxplot(alpha = 0.2, colour = "red") + scale_y_continuous(lim = c(0,300))

totalsulfurdioxide.histogram <- ggplot(dataset, aes(x = total.sulfur.dioxide)) + geom_histogram(binwidth =5, color = 'black', fill = I('orange')) + scale_x_continuous(lim = c(0,300)) 

totalsulfurdioxide.boxplot + totalsulfurdioxide.histogram + plot_annotation(title = "Total Sulfur Dioxide")

density.boxplot <- ggplot(dataset, aes(x = 1, y = density)) + geom_jitter(alpha = 0.1) + geom_boxplot(alpha = 0.2, colour = "red")

density.histogram <- ggplot(dataset, aes(x = density)) + geom_histogram(binwidth =0.001, color = 'black', fill = I('orange'))

density.boxplot + density.histogram + plot_annotation(title = "Density")

ph.boxplot <- ggplot(dataset, aes(x = 1, y = pH)) + geom_jitter(alpha = 0.1) + geom_boxplot(alpha = 0.2, colour = "red")

ph.histogram <- ggplot(dataset, aes(x = pH)) + geom_histogram(binwidth =0.05, color = 'black', fill = I('orange'))

ph.boxplot + ph.histogram + plot_annotation(title = "pH Levels")

sulphates.boxplot <- ggplot(dataset, aes(x = 1, y = sulphates)) + geom_jitter(alpha = 0.1) + geom_boxplot(alpha = 0.2, colour = "red")

sulphates.histogram <- ggplot(dataset, aes(x = sulphates)) + geom_histogram(binwidth =0.1, color = 'black', fill = I('orange'))

sulphates.boxplot + sulphates.histogram + plot_annotation(title = "Sulphates")
