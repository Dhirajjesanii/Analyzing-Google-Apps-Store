# load required libraries----------------------------------------

library(summarytools)
library(ggplot2)

#Load/Import Data---------------------------------------

Data = read.csv("C:/Users/Dell/Desktop/DATA/Google Play Store Apps Dataset/googleplaystore.csv")


#Check the Structure & Summary of Data---------------------------

str(Data)

summary(Data)

#Check summary with in detail(Like: Chart,Missing values, etc.)---------------------------------

view(dfSummary(Data))

# Category char to factor.-------------------------------------------------

Data$Category = as.factor(Data$Category)

# Reviews column chr to Int.-----------------------------------------------

Data$Reviews = as.integer(Data$Reviews)

# Rating Int to num.-------------------------------------------------------

Data$Rating = as.numeric(Data$Rating)

# Remove NA from Rating and Replace with 4.19.--------------------------------

Data$Rating = ifelse(is.na(Data$Rating),4.19,Data$Rating)

# Size column Chr To Factor.---------------------------------------------

Data$Size = as.factor(Data$Size)

# Size column Factor TO Numeric.-----------------------------------------------

Data$Size = as.numeric(Data$Size)

# Installs column chr TO factor.------------------------------------------

Data$Installs = as.factor(Data$Installs)

# Installs Column Factor To Numeric.-------------------------------------------

Data$Installs = as.numeric(Data$Installs)

# Type column 0 to "Zero"  & Chr to Factor.-------------------------------------------------

Data$Type = ifelse(Data$Type == 0,"Zero",Data$Type)
Data$Type = as.factor(Data$Type)

# Price column 0 TO Zero.-------------------------------------------------

Data$Price = as.factor(Data$Price)
Data$Price = as.numeric(Data$Price)
Data$Price = ifelse(Data$Price == 92,0,Data$Price)

# Content.Rating column chr TO Factor.------------------------------------

Data$Content.Rating = as.factor(Data$Content.Rating)

# Genres column Chr TO Factor.--------------------------------------------

Data$Genres = as.factor(Data$Genres)

# Which is max category(We can see there is more family category).---------------------------------------

ggplot(Data) + geom_histogram(aes(x = Category),fill = "Green",color = "Black",stat = "count") + labs(title = "max category") + coord_flip() 

# Category with High Rating.-----------------------------------------------

ggplot(Data) + geom_line(aes(x = Category,y = Rating)) + coord_flip()
ggplot(Data) + geom_bar(aes(x = Rating,fill = Category))

# Rating with category ----------------------------------------------------

ggplot(Data) + geom_histogram(aes(x = Rating,fill = Category))

# High Rating with Views.--------------------------------------------------

options(scipen = 100)# It will convert 10+e to 10000000
ggplot(Data) + geom_point(aes(x = Rating, y = Reviews)) + coord_flip() + labs(title = "Review based on Rating")

# Check Rating & Install(line).--------------------------------------------------

ggplot(Data) + geom_line(aes(x = Rating, y = Data$Installs))

# Check Install & Rating(point).-------------------------------------------

ggplot(Data) + geom_point(aes(x = Installs , y = Rating))

# Check Install & Rating(line).-------------------------------------------

ggplot(Data) + geom_line(aes(x = Installs , y = Rating))

# Rating (Simple Bar Chart).---------------------------------------------------------

ggplot(Data) + geom_bar(aes(x = Rating))

# Install (Simple Bar Chart).---------------------------------------------------------

ggplot(Data) + geom_bar(aes(x = Installs))

# Install & Rating(Bar chart With Fill method).---------------------------------------------------------

ggplot(Data) + geom_bar(aes(x = Rating, fill = Installs),position = "fill")

# Install & Rating (Point).---------------------------------------------------------

ggplot(Data) + geom_point(aes(x = Rating, y = Installs),color = "Black") + geom_smooth(aes(x = Rating ,y = Installs)) + labs(title = "Rating Based on Install")

# Split data into test & train.--------------------------------------------

Data$no = c(1:dim(Data)[1])#(We have to add new column for split data into test & train).---------------------------

Train = subset(Data,Data$no <= 8675)
Test = subset(Data,Data$no > 8675)

#(After split data we have to delete column which we added).------------------------------------------

Data = Data[-14]
Train = Train[-14]
Test = Test[-14]

#create model on Installs & Rating.------------------------------------------------------------

linearMethod = lm(Installs ~ Rating ,data = Train)
pred = predict(linearMethod, newdata = Test)

#Plot the predicted model.-----------------------------------------------------

ggplot(Test) + geom_histogram(aes(x = pred))
