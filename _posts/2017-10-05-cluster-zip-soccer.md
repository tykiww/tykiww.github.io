---
layout: post
title: "Unzipping and Clustering FIFA data"
tags: [Unzip, zip, soccer, futbol, football, cluster, algorithm, K-means]
comments: TRUE
---

I finally took the time to look at a soccer dataset!

Growing up, the sport I lived day by day for was soccer. There was nothing that was more exciting for me (at that time) than to be outside and playing. Unfortunately, priorities shift and other things get in the way. 

_a couple of months ago.._
![](https://github.com/tykiww/imgbucket/blob/master/img/soccer/fut1.jpg)

Today, we're going to visit two things. 

1. How to unzip files in R. 
2. How to do a simple cluster analysis.

I'm actually not the biggest fan of watching soccer nor am I a big FIFA of guy. Kinda weird since I love to play. Anyways, one of the first things I wanted to do once I built this page was to do a soccer analysis. Unfortunately, I found out that finding real player information is very difficult. Not much I can do about that but luckily, [Kaggle](https://kaggle.com) has some nice compilations of data that you can always search through. 

The dataset we are looking at can be loaded from my github repository (The link is in the code). This dataset has 53 columns and 17,000 rows. It's a big dataset, but not sure how big Big Data is these days. These columns include variables such as player ratings, teams, countries, and skill aspects that influence their overall score. It is a near-complete FIFA player stats set.

Let's start with our usual, required packages. The newest packages we are exposed to this time will be the `cluster` and `fpc`. The function we use, `kmeans`, is luckily already in our R studio, but those two are there to help us map out which groups are truly different. I'll show you how to work them when we get there. Let's first find out how to download zipped files.

```r
library(rvest)
library(cluster) 
library(fpc)
```

I've recently come across too many problems when loading straight datasets. Especially large ones were compressing becomes standard. An easy solution is to just open it separately in your machine and read the file from your working directly, but I think it's always fun to find new ways to read data.

The steps are simple. We have to specify a `tempfile()` which specifies an empty frame we will fill up while using the `download.file` and `unzip` functions. Just make sure to specify the correct name of the file you are trying to unzip! Sometimes you will need to open it an make sure which one to grab, but overall it speeds up the process of having to download things.

```r
url <- "http://tykiww.github.io/assets/FullData.csv.zip"

tt <- tempfile()
download.file(url,tt, mode="wb")
unzip(tt, "FullData.csv")
data <- read.csv("FullData.csv", header=T)
glimpse(data)
```

    ## Observations: 17,588
    ## Variables: 53
    ## $ Name               <fct> Cristiano Ronaldo, Lionel Messi, Neymar...
    ## $ Nationality        <fct> Portugal, Argentina, Brazil, Uruguay, G...
    ## $ National_Position  <fct> LS, RW, LW, LS, GK, GK, LS, RS, , GK, R...
    ## $ National_Kit       <dbl> 7, 10, 10, 9, 1, 1, 9, 11, NA, 1, 17, 1...
    ## $ Club               <fct> Real Madrid, FC Barcelona, FC Barcelona...

Simple huh? I've used this method countless times to grab zip files. Very very useful.

Now here's my regular spring (dataset) cleaning. Utah weather is weird. Some days feel like spring, but then an hour later, it snows like the dandruff off a dirty child. Can't do much about that!

I learned for the first time that you have to put logical operators (in this case `|`) within the quotations in order for it to search multiple values. I decided to play around with it to select specific column names. The rest of the steps are commented out for convenience.

```r
# Select Variables of interest
# Take out National position, Goalkeepers, GK Traits,
# National Kit, Contract Expiry, Birthdate, Skill Moves, 
pattern1 <- "National_Position|National_Kit|Club_Position|Contract_Expiry|Club_Joining|Club_Kit|Club_Joining"
pattern2 <- "Birth_Date|Weak_Foot|Skill_Moves|GK_Positioning|GK_Diving|GK_Kicking|GK_Handling|GK_Reflexes"
try1 <- data[,!grepl(pattern1,colnames(data))] # had to do it twice to save space laterally.
try2 <- try1[,!grepl(pattern2,colnames(try1))]
try3 <- try2[!try2$Preffered_Position=="GK",]
sum(is.na(try3)) # check to see if there were any other nas? 
# 0 means none, we're okay!

# remove kg, cm
try3$Height <- sub(" cm","",try3$Height)
try3$Height<- as.numeric(try3$Height)
try3$Weight <- sub(" kg","",try3$Weight)
try3$Weight <- as.numeric(try3$Weight)
try3$Name <- as.character(try3$Name)

# clear old info
fut <- try3
rm(try3,try1,data) # get rid of old data to save space.
pos1 <- sapply(fut,is.numeric) # only numeric
pos2 <- sapply(fut,is.factor) # only factor
numfut <- fut[,pos1] # numeric values
facfut <- fut[,c(4,pos2)]  # categorical values
```

I separated the values into numeric and categorical values. We'll only use the numerical ones though! It would be rather fun to figure out a logistic regression to see how the most important values affect the overall player rating. 

We'll start now by looking at the correlation values between all the numerical indicators. Good thing this is a video game, so most of the categorical data has no effect on the overall raw score of the player. If you don't believe me, you can run a `tapply(fut$Rating,fut$Preffered_Foot,summary)` and you'll notice that the distributions between each group doesn't vary much. This specific one is for left footed vs right footed players.

First, we can find the correlation values between Ranking and the other variables simply by using the `cor()` function. I decided to plot it two different ways. I only did this because they were visually pleasing to some. I am more a fan of the `barplot()`, but I have seen examples of the line plot. 

```r
# correlations
correls <- cor(numfut)[1,][-1]

# Plot correlations 1
barplot(correls, las = 2, col = "light blue") +
text(5,.715,"*", col = "red") +
text(12,.82,"*", col = "red") +
text(17,.7,"*", col = "red") +
text(19.3,.7,"*", col = "red")

# Plot correlations 2

plot(correls, col = "blue", pch = 19, type = "l", 
     las = 2, xaxt = "n", xlab = "", lwd = 2.25) +
axis(1,at=1:33, las = 2, labels = names(correls)) +
points(11,max(correls), col = "red", pch = 19) + 
text(14,.82,"reactions", col = "red")
```

![](https://raw.githubusercontent.com/tykiww/imgbucket/master/img/soccer/fut2.png)

We see that the highest few variables were **Reactions**, **Ball Control**, **Composure**, and **Short Pass**.

![](https://raw.githubusercontent.com/tykiww/imgbucket/master/img/soccer/fut3.png)

To be honest, I don't really see the practical application of the line plot when we already have a frequency graph. There really isn't a 'point' of connecting the 'points' together if they aren't related to each other. My opinion is that it is improper to do something like this. I guess you can use whichever one your employer seems to like more.

Now that we have our top characteristics, let's figure out (using these influential values) what category of players we are most interested in. We also can figure out, what type of player is amongst the best groups. We will use a K means cluster algorithm to figure that out!

We'll begin by setting the seed and pulling out our desired data.

```r
# find the variables of interest.
pattern1 <- "Rating|Reactions|Ball_Control|Short_Pass|Composure|Long_Pass|Shot_Power"
foot <- fut[,grepl(pattern1,colnames(fut))]
set.seed(15) # because 15 is my favorite number
```

The K means algorithm has 3 main arguments.

1. nstart

2. iter.max

3. K

These arguments are very important in determining how our learning algorithm will run. 

The nstart option attempts multiple initial configurations and reports on the best one. For example, adding nstart = 25 will generate 25 initial random centroids and choose the best one for the algorithm. For ours, at least 25 random sets are chosen. 

iterations are done for convergence. This means that we really don't really need large amounts of iterations if we are dealing with large sets of data. With large data and a good machine, the largest dataset that gives useful results only takes a maximum of 2 minutes until the data fully converges. I would recommend that you don't make too many iterations unless your data is too small. Then again, if we lack information, what are we trying to look for in the first place?

K is the number of groups we want to specify. Our goal here is to maximize the total variance explained and sum of squares. One way for us to do this is by using what is called the "elbow" method. 

The elbow method plots the percentage of variance explained (y) by the number of clusters (x). There will be an angular shape that looks like an elbow, and you choose that point. Rather subjective if you look at it that way.. Mathematically, we find an optimal tradeoff point where the marginal cost and benefit seem to overlap. Just at the point where we start seeing less increase in percentage of variance (R^2) explained. It would make sense that the more clusters you make, the less you will be able to explain due to making more distinct separations between the data. 

```r
#Elbow Method for finding the optimal number of clusters
# Compute and plot wss for k = 2 to k = 15.
range <- 1:15
elbow <- sapply(range, function(k) kmeans(foot, k, nstart=25, iter.max = 20 )$tot.withinss)
```

While running this code, you will see some warnings. Depending on what you set the range as, count the number of warnings. If this exceeds the length of your range, maybe we need to switch to a different algorithm. The number on the left indicates the last few times the function ran, so as long as your 'elbow' value is before the warnings, you should be okay. Usually, the bigger the number of clusters, the harder it is to converge with such limited iterations. Let's plot the total within-cluster SS.

```r
plot(range, elbow,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-cluster SS")
```

![](https://raw.githubusercontent.com/tykiww/imgbucket/master/img/soccer/fut4.png)

Great! The plot seems to show that around 3 is the optimum tradeoff value. Now we can specify and run our model and plot out the values. The `cusplot()` function comes from the `cluster` library.

```r
# Our confirmed Model
fitted <- kmeans(foot,3,nstart =25, iter.max = 10)
clusplot(foot, fitted$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
```

![](https://raw.githubusercontent.com/tykiww/imgbucket/master/img/soccer/fut5.png)

Now as we take a look at this plot, we observe a huge blob between 3 distinct groups. It seems that we have so much data, that it converges on top of each other. That's not a problem though. We can see that the data was plotted between 2 principle components. This is great as we notice that 78.85% of the variability was explained by using 2 of these components. Within these x-y values we have 3 groups that separate our 'blob'. If we want to see the groups separated within rankings, we can see how our players were separated into Top, middle, and worst players. Let's take a look at how these were exactly segmented in a table

```r
# table of values distinguishing where our value lies
table(fut$Rating,fitted$cluster)
```

![](https://raw.githubusercontent.com/tykiww/imgbucket/master/img/soccer/fut6.png)

Now I'm not going to print out this whole table as it is a little too large for this post. You can just take a look at this snippet. Now obviously, when playing our game we want to maximize the rating of our players so we have, practically, the best team to play with. Unfortunately, we know that not every team has all the best players, so let's take a look at "what is the lowest rating that I could settle with so I still have a high-average rated team". We can see that the second group captures most of the highest rated players and starts to converge until we hit about 78 on the rating scale. Therefore, we can say that as long as our players are above 78, we have a pretty solid team of 'top players'.

Here is another diagram of the clusters separating the groups using the `plotcluster` function taken from the `fpc` library (Centroid Plot against 2 discriminant functions)

```r
plotcluster(foot, fitted$cluster)
```

![](https://raw.githubusercontent.com/tykiww/imgbucket/master/img/soccer/fut7.png)

We see that our values are along a range from 40-99, so the values are closely arranged together. This lets see how our data has been separated into 3 different groups along that range.

Of course, this wasn't the best way to perform the analysis, since the outcome is rather obvious, but I have been able to gain really good insight on how to work with cluster algorithms. I want to see if I can get better at understanding the mechanics and start working with more categorical variables and recognize trends. I guess this is an area I know that I definitely want to improve in and would love to find some mentoring in. 

At least I know what types of players to settle for if I was to ever pick up the remote. I don't even remember the last time I played that game. I really want to go out and play for real now.
