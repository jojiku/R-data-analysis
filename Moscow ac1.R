# Importing all the stuff
install.packages("caTools")
install.packages('car')
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("mice")
install.packages("viridis")
install.packages("reshape2") 
install.packages("dplyr") 
install.packages("geomtextpath")
install.packages("RColorBrewer")
install.packages("ggtext")

library(readxl)
library(caTools)
library(car)
library(tidyverse)
library(caret)
library(lubridate)
library(ggplot2)
library(dplyr)

library(mice)

library(viridis)
library(reshape2)
library(geomtextpath)
library(jcolors)
library(RColorBrewer)
library(ggtext)

# Data preparation

data <- read_excel("C:/Users/USER/Desktop/KNU/Management data visualization/R projects/Moscow accidents/Refined_moscow_accidents.xlsx")
df = subset(data, select = -c(road,coordL,coordW))

# Checking and deleting NA's
sapply(df,function(x) sum(is.na(x)))

md.pattern(df)


df <- na.omit(df)

sapply(df,function(x) sum(is.na(x)))

# Preparation for making visualizations
df$month <- format(as.Date(df$date, format="%d.%m.%Y"),"%m")
df$month <- as.numeric(df$month)
df$NFatal <- as.numeric(df$NFatal)
df$MonthName <- month.name[df$month]
View(df)
md.pattern(df)
df$NInjured <- as.numeric(df$NInjured)
class(df$NInjured)

sapply(df,function(x) sum(is.na(x)))