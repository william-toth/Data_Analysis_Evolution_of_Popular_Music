# Name: William Toth
# Assignment: Final Project
#-------------------------------------------------

library(readr)
df <- read_csv("Downloads/Music_Evolution_Data.csv")

#Exploratory Analysis

head (df)

dim (df)

#[1] 17094    41

summary (df)

#-------------------------------------------------------------

#Converting cluster variable to factor

df$cluster <- as.factor (df$cluster)
table (df$cluster)

#   1    2    3    4    5    6    7    8    9   10   11   12   13 
#1318 1185 1308 1068  973 1158 1366 1133 1973 1294 1219 1545 1554 

#checking cluster variable by decade

cluster_freq_by_decade <- table (df$decade, df$cluster)

barplot (cluster_freq_by_decade, 
         beside = TRUE,
         #main = "Cluster Distribution by Decade",
         col = c("red", "purple", "blue", "darkblue", "darkred"),
         legend = c("60s", "70s", "80s", "90s", "00s"),
         xlab = "Cluster Number",
         ylab = "Number of Times in BBH100 by Decade",
         border = TRUE) 



#--------------------------------------------------------------

#Histogram + Distribution for every cluster style over time

install.packages ("ggplot2")
library (ggplot2)

#Cluster 1

cluster_1_df <- subset (df, df$cluster == 1, select = year)

ggplot (cluster_1_df, aes(x = year)) +
          geom_histogram(aes(y=..density..),
          binwidth = 1,
          colour = "black", fill = "white") +
        geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 2

cluster_2_df <- subset (df, df$cluster == 2, select = year)

ggplot (cluster_2_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 3

cluster_3_df <- subset (df, df$cluster == 3, select = year)

ggplot (cluster_3_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 4

cluster_4_df <- subset (df, df$cluster == 4, select = year)

ggplot (cluster_4_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 5

cluster_5_df <- subset (df, df$cluster == 5, select = year)

ggplot (cluster_5_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 6

cluster_6_df <- subset (df, df$cluster == 6, select = year)

ggplot (cluster_6_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 7

cluster_7_df <- subset (df, df$cluster == 7, select = year)

ggplot (cluster_7_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 8

cluster_8_df <- subset (df, df$cluster == 8, select = year)

ggplot (cluster_8_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 9

cluster_9_df <- subset (df, df$cluster == 9, select = year)

ggplot (cluster_9_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 10

cluster_10_df <- subset (df, df$cluster == 10, select = year)

ggplot (cluster_10_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 11

cluster_11_df <- subset (df, df$cluster == 11, select = year)

ggplot (cluster_11_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 12

cluster_12_df <- subset (df, df$cluster == 12, select = year)

ggplot (cluster_12_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#Cluster 13

cluster_13_df <- subset (df, df$cluster == 13, select = year)

ggplot (cluster_13_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#--------------------------------------------------------------

#Artist Popularity

artist_popularity_df <- subset (df, select = c(artist_name_clean, year))
artist_popularity_df$artist_name_clean <- as.factor (artist_popularity_df$artist_name_clean)
most_popular <- sort (table (artist_popularity_df$artist_name_clean), decreasing = TRUE)
most_popular[1:30]


beach_boys_df <- subset (df, df$artist_name_clean == "BEACHBOYS", select = year)

ggplot (beach_boys_df, aes(x = year)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 2,
                 colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

#--------------------------------------------------------------

#H1 topic over time

plot (df$hTopic_01~df$year,
      xlab = "Year",
      ylab = "H1 Topic Value (Dominant 7ths)")

h1_df <- aggregate (df$hTopic_01~df$year, FUN = mean)
plot (h1_df$`df$hTopic_01`~h1_df$`df$year`,
      xlab = "Year",
      ylab = "Average Yearly H1 Topic Value")

h5_df <- aggregate (df$hTopic_05~df$year, FUN = mean)
plot (h5_df$`df$hTopic_05`~h5_df$`df$year`,
      xlab = "Year",
      ylab = "Average Yearly H5 Topic Value",
      main = "Avg 'No Chords' (H5) Use in BBH100 Over Time")

h8_df <- aggregate (df$hTopic_08~df$year, FUN = mean)
plot (h8_df$`df$hTopic_08`~h8_df$`df$year`,
      xlab = "Year",
      ylab = "Average Yearly H8 Topic Value",
      main = "Avg 'Major Chords' (H8) Use in BBH100 Over Time")

t4_df <- aggregate (df$tTopic_04~df$year, FUN = mean)
plot (t4_df$`df$tTopic_04`~t4_df$`df$year`,
      xlab = "Year",
      ylab = "Average Yearly T4 Topic Value",
      main = "Avg 'Piano, Orchestra, Harmonic' (T4) Use Over Time")

t5_df <- aggregate (df$tTopic_05~df$year, FUN = mean)
plot (t5_df$`df$tTopic_05`~t5_df$`df$year`,
      xlab = "Year",
      ylab = "Average Yearly T5 Topic Value",
      main = "Avg 'Guitar, Loud, Energertic' (T5) Use Over Time")


t7_df <- aggregate (df$tTopic_07~df$year, FUN = mean)
plot (t7_df$`df$tTopic_07`~t7_df$`df$year`)

t8_df <- aggregate (df$tTopic_08~df$year, FUN = mean)
plot (t8_df$`df$tTopic_08`~t8_df$`df$year`)

#-------------------------------------------------------------

#Random Forest Approach

#install.packages ("randomForest")

library (randomForest)

rf_df <- subset (df, select = c(era, hTopic_01:tTopic_08))
rf_df$era <- as.factor (rf_df$era)

n <- nrow(rf_df)
ntrain <- round(n*0.6)
set.seed (123)

tindex <- sample (n, ntrain)

train_rf_df <- rf_df[tindex, ]
test_rf_df <- rf_df[-tindex, ]

rf <- randomForest (era~., data = train_rf_df, importance = TRUE)
print (rf)

#Call:
#  randomForest(formula = era ~ ., data = train_rf_df, importance = TRUE) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 4
#
#OOB estimate of  error rate: 42.01%
#Confusion matrix:
#  1    2   3    4 class.error
#1 79  915  11   56   0.9255419
#2 67 4004 107  544   0.1520542
#3  2  848 177  391   0.8751763
#4  5 1255 108 1687   0.4477905

prediction <- predict (rf, newdata = test_rf_df, type = "class")

table (prediction, test_rf_df$era)

#prediction    1    2    3    4
#         1   63   32    0    5
#         2  597 2649  593  883
#         3    6   66  123   62
#         4   43  367  259 1090

misclassification_error_rate <- sum(test_rf_df$era != prediction) / 
  nrow(test_rf_df)*100
misclassification_error_rate

#[1] 42.60018

varImpPlot(rf)


