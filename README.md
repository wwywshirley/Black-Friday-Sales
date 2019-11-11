# Black-Friday-Sales
Our dataset is comprised of sales transactions captured at a retail store on “Black Fridays.” It has 550,069 rows and 12 columns. Our primary goal is to predict purchase amount using the data features, thus using the dataset to form a regression problem. Customers will be split into two classes (i.e. the Big Spenders and Small Buyers; aka High/Low spending classes). We expect to improve our understanding of customer behavior from an examination of multiple shopping experiences and to build a report that can highlight our engineering skills for future employers.

# Code
## Basic Data Exploration

Group Dataset by Unique User_ID. NOTE: Product_ID, are not included in this grouping . This is deliberate because they have too many categories
```
df_unique_Users <- df %>% group_by(User_ID) %>% 
  summarise(Gender = Gender[1], 
            Age = Age[1], 
            Occupation = Occupation[1], 
            City_Category = City_Category[1], 
            Stay_In_Current_City_Years = Stay_In_Current_City_Years[1], 
            Marital_Status = Marital_Status[1], 
            Purchase = sum(Purchase), 
            Items_Count = n())
#View Data
dim(df) #Checks dimentionality of our dataset. Thankfully, we don't have a high dimensionality issue. 537577 
        # rows & 12 columns
dim(df_unique_Users) #Number of Unique Users = 5891   Number of columns = 9

head(df, 20) # A quick look at the first 20 rows of our dataset
head(df_unique_Users, 20)

str(df) #
str(df_unique_Users)

plot_str(df) # To create a Network Graph for our data set
plot_str(df_unique_Users)
```
![](images/Picture2.jpg)
