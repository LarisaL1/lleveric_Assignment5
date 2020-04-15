library(readr)
library(cluster)
Cereals <- read_csv("Downloads/Assignment 5/Cereals.csv")


head(Cereals)
data <- Cereals [,-(1:3)]
summary(data)

#remove records with missing measurements
df <- data
df <- na.omit(df)
head(df)

df <- scale(df)
head(df)

# Using agnes () function II
df <- data

# compute with agnes and with different linkage methods
hc_single <- agnes (df, method = "single")
hc_complete <- agnes (df, method = "complete")
hc_average <- agnes (df, method = "average")
hc_ward <- agnes (df, method = "ward")

# Compare Agglomerative coefficients
print (hc_single$ac)
print (hc_complete$ac)
print (hc_average$ac)
print (hc_ward$ac)

# plot dendrogram
plot (hc_ward)
rect.hclust(hc_ward, k=3, border = 1:3)

members <- cutree(hc_ward, k = 3) # cut 3 clusters

centers <- aggregate( . ~ members, data = df, FUN = mean)
centers

#based on these results, Cluster #2 is the healthiest: it has the lowest number of calories, fat, sodium, carbohydrates and sugars.


#Trying with a modified dataset
data2 <- data[-c(1,3,5,9,13,20,25,26,28,33,45,46,57,59),]
summary(data2)

#remove records with missing measurements
df <- data2
df <- na.omit(df)
head(df)

df <- scale(df)
head(df)

# Using agnes () function II
df <- data2

# compute with agnes and with different linkage methods
hc_single <- agnes (df, method = "single")
hc_complete <- agnes (df, method = "complete")
hc_average <- agnes (df, method = "average")
hc_ward <- agnes (df, method = "ward")

# Compare Agglomerative coefficients
print (hc_single$ac)
print (hc_complete$ac)
print (hc_average$ac)
print (hc_ward$ac)

#once again, the Ward model is the best

# plot dendrogram
plot (hc_ward)
rect.hclust(hc_ward, k=3, border = 1:3)

members <- cutree(hc_ward, k = 3) # cut 3 clusters

centers <- aggregate( . ~ members, data = df, FUN = mean)
centers

#the results show that Cluster #2 is the healthiest (just like in the previous dataset), with the lowest numbers for calories, fat, and carbohydrates
