############################### Clean, Output Function Loop #########################################

# This script is a sample for automating the cleaning of a large data set, then breaking out
# each brand into a CSV output with a summary of each metric

# Project Name: Clean, Output Function Loop

# Data source: sample data file

############################################################################################

# begin by setting the working directory for the files
setwd("C:/Users/*****/Documents/R")

#Load the packages and libraries, or d/l any using install.packages("???")
library(dplyr)
library(magrittr)
library(tidyr)
library(readr)
library(stringr)

# Begin by loading in the dataset using the appropriate method

#data1 <- file.choose() # Allows for manual selection from folder location, paired with below
#data <- read_csv(data1)  
data1 <- read_csv("disc_data2.csv") #loads in the CSV to a tibble, easier to work with
#data1 <- read.csv("file_name.tsv", sep = "\t", header = TRUE)
#data1 <- read.xls("file_name.xlsx")
#data1 <- XLGetRange(sheet = "sheet1", range = "A1:B21", header = TRUE) #only used when Excel workbook is open


#Open the data view
View(data1)

### Basic summary functions to understand the data
names(data1)
head(data1)
str(data1)
summary(data1)

# remove the NA columns from the data
data2 <- data1 %>% select(-`URL count`, -`URL 1`, -`URL 2`, -`URL 3`, -`URL 4`, -`URL 5`)
# validate the rows have been removed
str(data2)
str(data2$Brand)

#function for cleaning and exporting each brand summary states by property type
brandvec <- c("var1", "var2", "var3", "var4", "var5", "var6", "var7", "var8", "var9" )

# Run the cleaning and output function through the brand vector
for(i in brandvec) {
  sum_func(i)
}

### functions to clean data file and produce brand-by-brand csv Sum files

sum_func <- function(brand) {

# cleaning, summary, and output for 'var1', there are 4 places each variable needs to be input into each section of code
  
  if(brand == "var1") {
var1Data <- data2 %>% filter(Brand == "var1") %>%
  group_by(`Property Type`) %>% 
  summarise_each(funs(sum), `Users`, `Streamers`, `Users (Authenticated)`, `Streamers (Authenticated)`, `Users (Unauthenticated)`, `Streamers (Unauthenticated)`) %>%
  mutate(`Authenticated streams per User`= `Streamers (Authenticated)` / `Users (Authenticated)`) %>%
  mutate(`Unauthenticated streams per User`= `Streamers (Unauthenticated)` / `Users (Unauthenticated)`) %>%
write.csv("./output csv files/var1Data.csv") 
  }
  
  else if(brand == "var2") {
    var2Data <- data2 %>% filter(Brand == "var2") %>%
      group_by(`Property Type`) %>% 
      summarise_each(funs(sum), `Users`, `Streamers`, `Users (Authenticated)`, `Streamers (Authenticated)`, `Users (Unauthenticated)`, `Streamers (Unauthenticated)`) %>%
      mutate(`Authenticated streams per User`= `Streamers (Authenticated)` / `Users (Authenticated)`) %>%
      mutate(`Unauthenticated streams per User`= `Streamers (Unauthenticated)` / `Users (Unauthenticated)`) %>%
      write.csv("./output csv files/var2Data.csv")
  }
  
  else if(brand == "var3") {
    var3Data <- data2 %>% filter(Brand == "var3") %>%
      group_by(`Property Type`) %>% 
      summarise_each(funs(sum), `Users`, `Streamers`, `Users (Authenticated)`, `Streamers (Authenticated)`, `Users (Unauthenticated)`, `Streamers (Unauthenticated)`) %>%
      mutate(`Authenticated streams per User`= `Streamers (Authenticated)` / `Users (Authenticated)`) %>%
      mutate(`Unauthenticated streams per User`= `Streamers (Unauthenticated)` / `Users (Unauthenticated)`) %>%
      write.csv("./output csv files/var3Data.csv")
  }
  
  else if(brand == "var4") {
    var4Data <- data2 %>% filter(Brand == "var4") %>%
      group_by(`Property Type`) %>% 
      summarise_each(funs(sum), `Users`, `Streamers`, `Users (Authenticated)`, `Streamers (Authenticated)`, `Users (Unauthenticated)`, `Streamers (Unauthenticated)`) %>%
      mutate(`Authenticated streams per User`= `Streamers (Authenticated)` / `Users (Authenticated)`) %>%
      mutate(`Unauthenticated streams per User`= `Streamers (Unauthenticated)` / `Users (Unauthenticated)`) %>%
      write.csv("./output csv files/var4Data.csv")
  }
  
  else if(brand == "var5") {
    var5Data <- data2 %>% filter(Brand == "var5") %>%
      group_by(`Property Type`) %>% 
      summarise_each(funs(sum), `Users`, `Streamers`, `Users (Authenticated)`, `Streamers (Authenticated)`, `Users (Unauthenticated)`, `Streamers (Unauthenticated)`) %>%
      mutate(`Authenticated streams per User`= `Streamers (Authenticated)` / `Users (Authenticated)`) %>%
      mutate(`Unauthenticated streams per User`= `Streamers (Unauthenticated)` / `Users (Unauthenticated)`) %>%
      write.csv("./output csv files/var5Data.csv")
  }
  
  else if(brand == "var6") {
    var6Data <- data2 %>% filter(Brand == "var6") %>%
      group_by(`Property Type`) %>% 
      summarise_each(funs(sum), `Users`, `Streamers`, `Users (Authenticated)`, `Streamers (Authenticated)`, `Users (Unauthenticated)`, `Streamers (Unauthenticated)`) %>%
      mutate(`Authenticated streams per User`= `Streamers (Authenticated)` / `Users (Authenticated)`) %>%
      mutate(`Unauthenticated streams per User`= `Streamers (Unauthenticated)` / `Users (Unauthenticated)`) %>%
      write.csv("./output csv files/var6Data.csv")
  }
  
  else if(brand == "var7") {
    var7Data <- data2 %>% filter(Brand == "var7") %>%
      group_by(`Property Type`) %>% 
      summarise_each(funs(sum), `Users`, `Streamers`, `Users (Authenticated)`, `Streamers (Authenticated)`, `Users (Unauthenticated)`, `Streamers (Unauthenticated)`) %>%
      mutate(`Authenticated streams per User`= `Streamers (Authenticated)` / `Users (Authenticated)`) %>%
      mutate(`Unauthenticated streams per User`= `Streamers (Unauthenticated)` / `Users (Unauthenticated)`) %>%
      write.csv("./output csv files/var7Data.csv")
  }
  
  else if(brand == "var8") {
    var8Data <- data2 %>% filter(Brand == "var8") %>%
      group_by(`Property Type`) %>% 
      summarise_each(funs(sum), `Users`, `Streamers`, `Users (Authenticated)`, `Streamers (Authenticated)`, `Users (Unauthenticated)`, `Streamers (Unauthenticated)`) %>%
      mutate(`Authenticated streams per User`= `Streamers (Authenticated)` / `Users (Authenticated)`) %>%
      mutate(`Unauthenticated streams per User`= `Streamers (Unauthenticated)` / `Users (Unauthenticated)`) %>%
      write.csv("./output csv files/var8Data.csv")
  }
  
  else if(brand == "var9") {
    var9Data <- data2 %>% filter(Brand == "var9") %>%
      group_by(`Property Type`) %>% 
      summarise_each(funs(sum), `Users`, `Streamers`, `Users (Authenticated)`, `Streamers (Authenticated)`, `Users (Unauthenticated)`, `Streamers (Unauthenticated)`) %>%
      mutate(`Authenticated streams per User`= `Streamers (Authenticated)` / `Users (Authenticated)`) %>%
      mutate(`Unauthenticated streams per User`= `Streamers (Unauthenticated)` / `Users (Unauthenticated)`) %>%
      write.csv("./output csv files/var9Data.csv")
  }
  
  else {return("this brand is not on the list")}
}  




