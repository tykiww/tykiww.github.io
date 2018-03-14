---
layout: post
title: "Let's just run a regression 笑"
fb-img: https://tykiww.github.io/img/reg/reg1.png
tags: [Regression, multiple regression, base R, Predictive analytics, Global Warming, Greenhouse gasses]
comments: true
---

Let's just run a regression.笑

Today is time for the most common data analysis technique: Regression. Some people like to call it Multiple Regression, but my guess is that people are just saying that to feel good about themselves doing something difficult. Honestly, whenever I get a dataset, the first thing I do is just run a regression on the data. It is an invaluable tool that can be surprisingly subjective depending on what you are really looking for.

In search for more "relevant" larger datasets to run on regression, I finally found one. I did most of the cleaning and scraping off of the NASA website, but I won't be showing it here. I will write an entry sometime soon using both `rvest` and `selenium` packages. Model creation is really arbitrary depending on what you want to predict.. Not everything goes quite to plan as you would expect. 

For this analysis, I decided to take a look at climate change. Poor Polar Bear..

![](https://www.theblackvault.com/documentarchive/wp-content/uploads/2015/03/8845010-stop-global-warming.jpg)

For a moment, I thought to myself. Maybe I will end the debate on climate change once and for all! Yet, that is probably not going to happen. I'll enjoy this thoroughly. This test will check to see if (H<sub>o</sub>) there really is a significant difference in greenhouse gas effect. We will look at 5 different types of greenhouse gasses (co2, methane, n2o, sf6, hcfc) and compare these individual values.

Citation of the data will be at the bottom!
If you want to follow along, download the datasets in this [repository](https://github.com/tykiww/projectpage/tree/master/datasets/Climate%20data).

Just some background on how the data was collected for validity.

Temperature: Temp data was collected by NASA. This comprises an estimate of the global surface temperatures using current data. The method of collection was performed at sampling stations gathering long run land-ocean temperatures as high as 12,000 kilometers.

Greenhouse Gasses: Data was collected at the Maunaloa observatory. Since each chemical compound is a different size, each are expressed in various sizes. This will not affect overall regression estimates and standard errors.

pp = parts per
m = million
b = billion
t = trillion

- co2: ppm
- methane: ppm
- n2o: ppb
- sf6: ppb
- hcfc: ppt

Let's begin by installing our libraries. I will be using `plotly` again just to give you a different taste of some of my visualizations. It also comes in extremely handy in detecting outliers and leverage points. The other new libraries that appear here is the `car` and `outliers` package. The car stands for "Companion to Applied Regression". Many of the functions I run will be tested for outliers and checking irregularities in my datasets (collinearity and such).

```r
library("car")
library("outliers")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("plotly")
```

Let's move on to downloading the data. For me, it is quite simple because most everything is in my cluttered desktop, but if you are trying to follow along, click [here](https://github.com/tykiww/projectpage/tree/master/datasets/Climate%20data) to find the datasets for each item. I uploaded them onto my public repository for easy use. These are each in csv text files you can just open up anywhere.

We will have a total of 6 dataframes to load. 1 response variable for temperature, and 5 explanatory variables for each greenhouse gas. I first modified the temperature data to fit the rest of the other sets where I can just have temp, month, and year. There were some weird quirks with the tables where month 13 was the cumulative of the year, so I decided to remove those and create my own months.

```r
# Temperature
nasatemp <- read.csv("Nasa Temp")
nasatemp <- nasatemp[,-1]

# code to reshape to each row is a month
nasatemp <- nasatemp[,1:13]
dim(nasatemp) # check dimensions.
newtemps <- data.frame(temp=matrix(t(nasatemp[2:13]),ncol=1))
newtemps$month<-rep(1:12)
newtemps$year<-rep(1880:2017,each=12)

# Filter to 1969 and after
newtemp <- subset(newtemps,newtemps$year>=1969)
```

Let's now take a look at all the other gasses! For this process, I went through and merged just the methane and co2 to set a standard for how I wanted my data to look. I guess I could have done it all at once, but I was wondering what type of structure was best to hold the dataframe.

```r
# CO2
maunaloa1 <- read.csv("CO2")
maunaloa1 <- maunaloa1[,-1]
# Methane
maunaloa2 <- read.csv("Methane")
maunaloa2 <- maunaloa2[,-1]
# merge datasets and clean up
climate <- merge(merge(newtemp,maunaloa1,by=c("year","month"), all.x=TRUE),maunaloa2,by=c("year","month"),all.x=TRUE)
climate <- subset(climate,!is.na(where.x) & !is.na(where.y))
climate <- climate[,c("year","month","temp","co2","methane")]

# Add other 3 gasses.

# N20
maunaloa3 <- read.csv("N2O")
maunaloa3 <- maunaloa3[,2:4]
# Hydrochloroflurocarbon 
maunaloa4 <- read.csv("HCFC")
maunaloa4 <- maunaloa4[,2:4]
# sulfer hexaflouride (SF6) 
maunaloa5 <- read.csv("SF6")
maunaloa5 <- maunaloa5[,2:4]

# merge additional datasets and clean up
climate <- merge(climate,maunaloa3,by=c("year","month"), all.x=TRUE)
climate <- subset(climate,!is.na(n2o))
climate <- merge(climate,maunaloa4,by=c("year","month"), all.x=TRUE)
climate <- subset(climate,!is.na(hcfc))
climate <- merge(climate,maunaloa5,by=c("year","month"), all.x=TRUE)
climate <- subset(climate,!is.na(sf6))
#with so many observations removed for missing data, renumber the rows
rownames(climate) <- 1:dim(climate)[1]
# delete old datasets now that we have our latest climate data.
rm(maunaloa1,maunaloa2,maunaloa3,maunaloa4,maunaloa5,nasatemp,newtemps,newtemp)

glimpse(climate)
```

Now that we're done, we can see that we have a complete dataset with 8 variables and 189 observations. Hopefully that's enough for our model!

Now first things first, we need to check the mean, standard deviation, and correlation for each of the values compared with temperature. We notice that the highest spread is shown by the methane data, but it seems to have a higher correlation than that of co2. This makes sense as average methane detection was about 5 times that of n2o. Overall, it seems that the data is well correlated with reasonable variance.

```r
sd <- sapply(climate,sd)[-c(1:3)]
mean <- sapply(climate,mean)[-c(1:3)]
cor <- cor(climate)[3,-c(1:3)]
cbind(mean,sd,cor) # cbind all 3 summary statistics.
```
    ##                mean        sd       cor
    ## co2      386.164339 11.020916 0.5998160
    ## methane 1809.901164 27.517525 0.6287320
    ## n2o      322.522275  4.308157 0.6209823
    ## hcfc      18.326032  3.912656 0.5199473
    ## sf6        6.675138  1.401520 0.6175104


Here are some plots of the data. And data checking p-values for overall normality of the data. Of course, I don't need to be doing every step here, but it is nice to have a good reference. The p-values indicate that each of the data are roughly normal. Nothing to worry too much about.

```r
pvals <- c()
for (i in 3:8) {
  pvals[i]<- shapiro.test(climate[,i])$p.value
}
pvals[3:8] # p-values from Shapiro Wilk test. All below alpha = 0.05
# [1] 3.670646e-05 4.334441e-04 6.554122e-05 8.794333e-06 2.735278e-10 1.997994e-06

a <- qplot(y=temp,x=co2,data=climate) + geom_smooth(method='lm', formula=y~x)
b <- qplot(y=temp,x=methane,data=climate) + geom_smooth(method='lm', formula=y~x)
c <- qplot(y=temp,x=n2o,data=climate) + geom_smooth(method='lm', formula=y~x)
d <- qplot(y=temp,x=hcfc,data=climate) + geom_smooth(method='lm', formula=y~x)
e <- qplot(y=temp,x=sf6,data=climate) + geom_smooth(method='lm', formula=y~x)
grid.arrange(a,b,c,d,e,ncol = 2)
```

![](https://tykiww.github.io/img/reg/reg1.png)

The plots confirm the summary statistics. Now the model.

The model we will fit will look like this:

model: temp = ß<sub>o</sub> + ß<sub>1</sub>(co2) + ß<sub>2</sub>(methane) + ß<sub>3</sub>(n20) + ß<sub>4</sub>(methane) + ß<sub>5</sub>(sf6) + e, e~N(0,sd^2)

```r
out.climate <- lm(temp~co2+methane+n2o+hcfc+sf6,data=climate) # fit the model
summary(out.climate)
```

    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -4.251e+03  1.251e+03  -3.399 0.000829 ***
    ## co2          1.710e-02  3.799e-01   0.045 0.964142    
    ## methane      1.240e-01  7.255e-02   1.710 0.088995 .  
    ## n2o          1.341e+01  4.105e+00   3.266 0.001304 ** 
    ## hcfc        -5.072e+00  9.745e-01  -5.205 5.18e-07 ***
    ## sf6         -2.207e+01  1.260e+01  -1.752 0.081527 .  
    ## Multiple R-squared:  0.4989,	Adjusted R-squared:  0.4852 
    ## F-statistic: 36.44 on 5 and 183 DF,  p-value: < 2.2e-16
    
Each beta value corresponds to the correlation coefficient of each variable just like in the simple linear case. ß<sub>o</sub> will indicate what would happen if none of the greenhouse gasses had any effect at all and serves as the intercept. The p-value of investigating a difference is < 0.0001 and we have an R^2 of 0.4852. Yet there's something weird about the data. hcfc and sf6, if included seem to bring temperature down for each value of x. This doesn't sound right, unless there are such things as greenhouse gasses.

```r
out.climate1 <- lm(temp~hcfc,data=climate) # Check simple linear cases.
summary(out.climate1)$coefficients

out.climate2 <- lm(temp~sf6,data=climate)
summary(out.climate2)$coefficients
```

    ##              Estimate Std. Error  t value     Pr(>|t|)
    ## (Intercept) 21.354611  5.2206259 4.090431 6.393161e-05
    ## hcfc         2.319244  0.2786281 8.323797 1.757765e-14
    ##              Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept) 12.528030  4.8848496  2.564671 1.111219e-02
    ## sf6          7.689596  0.7162619 10.735733 2.973586e-21

If we compare the model to one that does not include co2, methane, and n2o, we seem to find that this is the opposite (the beta hat values are positive!).

Let's create the model and plot each of the values before doing anything to investigate anything fishy in the model. 

```r
vif(out.climate) # Checking, then plot for collinearity
plot(~co2 + methane + n2o + hcfc + sf6,data=climate)
```

    ##       co2    methane        n2o       hcfc        sf6 
    ## 21.018098   4.778693 375.061627  17.433294 373.859809 

![](https://tykiww.github.io/img/reg/reg2.png)


Unfortunately, there is high collinearity with this data. The visual shows a tight correlation of values within each other. At first glance, it may seem like this isn't that big of a deal, but this actually makes analysis difficult to perform. In a matrix algebra standpoint, we are looking to find orthogonal data where values that can be predicted at any combination of values of each explanatory variable. In layman's terms, this correlation is bad. We are searching for data that looks more like the top 4 boxes in the left-top corner. Data that is spread out lets us calculate every possible scenario rather than just tightly correlated variables like hcfc and sf6.

The `vif()` gives the generalized variance inflation factors for linear models and gives us the numerical values of the collinearity. Just as we saw in the plot, we can tell that n20, hcfc, and sf6 have very high values of collinearity. The cutoff for good data is usually 10. 

We'll start by refitting the model without each high collinearity value one at a time and comparing the model estimation power. *I'll skip this part*, but it is rather simple. All we need to do is to take out the variable that has the highest collinearity and check to see if the model predicts better without that particular greenhouse gas. If that doesn't work, shuffle around which variables are most important and impactful. Transforming the data may also be an option as well. Also check to see that the percentage of error estimation (R^2) is still high for our model. There needs to be a good optimization of useful information and high prediction. 

```r
out.clim <- lm(temp~co2+methane,data=climate)
summary(out.clim)
```

    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -571.96359   74.88111  -7.638 1.13e-12 ***
    ## co2            0.37840    0.16701   2.266   0.0246 *  
    ## methane        0.27057    0.06689   4.045 7.66e-05 ***
    ## Multiple R-squared:  0.4115,	Adjusted R-squared:  0.4052 
    ## F-statistic: 65.04 on 2 and 186 DF,  p-value: < 2.2e-16

Unfortunately, after a while of manipulating data, I had to take out n2o, sf6, and hcfc. This was originally due to the high collinearity of sf6 and n2o. Afterwards, I toyed with keeping hcfc for co2, so I went in and tried different combinations with and without co2 and hcfc. I realized that the model prediction R^2 was a lot better keeping co2 in the data and removing hcfc. I also realized that co2 is more the parameter of interest as co2, water vapor, and methane were the most abundant greenhouse gasses in the atmosphere. If I had a choice, I would have loved to include n2o, but every time I left that in, the collinearity of the data changes the estimate values. As I'm going for maximum prediction, I will leave my model to look like this:

model: temp = ß<sub>o</sub> + ß<sub>1</sub>(co2) + ß<sub>2</sub>(methane) + e, e~N(0,sd^2)

:Now that we have the model, let's get down to clearing out any influential observations or outliers.

Influential observations are easy to pick out (abnormal values in the x direction). All we need to do is use the `lm.influence()` function and `cooks.distance()` luckily the names are intuitive. We will now proceed to compare the individual points with the leverage cutoff values

Given p as the number of variables and n as the number of total rows:
- leverage cutoffs that show how influential: 2*(p+1)/n
- Cook's Distance cutoffs that show how influential: 4/(n-(p+1))


```r
# new climate dataset with desired variables
climates <- climate[,1:5]
p <- 2
n <- nrow(climates)

# leverage outputs
leverage.out <- lm.influence(out.clim)$hat
# Compute cook's distance for every one of our data points
cd.out <- cooks.distance(out.clim)

# influential points in datasets.
climates[2 * (p + 1) / n < leverage.out,]
climates[4 / (n - (p+1) ) < cd.out,]
```
![](https://tykiww.github.io/img/reg/reg3.png)
![](https://tykiww.github.io/img/reg/reg4.png)



Now that we know which points are influential, let's check whether or not these are good or bad points. These can be seen in our plots. Usually, I would just hover over the points using `ggplotly()` to investigate whether these points are truly influential.

```r
# Check points with plots. Use ggplotly to look at individual points.
a <- qplot(y=temp,x=co2,data=climates) + geom_smooth(method='lm', formula=y~x) + geom_point(size = 2, aes(377.37,24), col = "red") + geom_text( aes(377.37,20),label = "co2 influential")

b <- qplot(y=temp,x=methane,data=climates) + geom_smooth(method='lm', formula=y~x) +   geom_point(size = 2, aes(1774.59,24), col = "red") + 
geom_point(size = 2, aes(1762.14,70), col = "red") + 
geom_text( aes(1774.59,20),label = "methane influential") + 
geom_text( aes(1770,74),label = "methane influential")

grid.arrange(a,b)
# row 54 seems to be the only classified "bad" influential point from our co2 data. 
# rows 74, 54 seem to be the only classified "bad" influential point from our methane data.

# remove rows 54 and 74 from climates
climates <- climates[-c(54,74),]
out.clims <- lm(temp~co2+methane,data=climates)
summary(out.clims)
```

![](https://tykiww.github.io/img/reg/reg5.png)

    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -580.73168   75.05219  -7.738 6.73e-13 ***
    ## co2            0.35590    0.16437   2.165   0.0317 *  
    ## methane        0.28014    0.06675   4.197 4.23e-05 ***
    ## Multiple R-squared:  0.4236,	Adjusted R-squared:  0.4173 
    ## F-statistic: 66.88 on 2 and 182 DF,  p-value: < 2.2e-16

Not bad. Many points can be influential in the x direction, but they seem to be strongly supporting the prediction of the data. The R^2 value increased a little with co2 a little less significant. Yet we are still good to go. After we remove those values, we can check out the outliers. 

First, we are going to calculate the R studentized residuals outside 2 standard deviations. Any points that fall outside are subject to investigation using our `ggplotly()` interactive graph.

```r
t <- qnorm(0.975)
climates[abs(rstudent(out.clims)) > t,]
ha <- rstudent(out.clims)[7]
2*(1-pnorm(abs(ha))) # row 7 is barely an outlier. [0.04871981]
# We want to leave about 5% in the dataset. For our data to be 95%.
```

![](https://tykiww.github.io/img/reg/reg6.png)

```r
# Check points with plots. Use ggplotly to look at individual points.
a <- qplot(y=temp,x=co2,data=climates) + geom_smooth(method='lm', formula=y~x)  + geom_point(size = 2, aes(373.92,90), col = "red") + 
geom_point(size = 2, aes(383.26,94), col = "red") + 
geom_point(size = 2, aes(385.53,23), col = "red") +
geom_text( aes(373.92,87),label = "co2 outlier") +
geom_text( aes(383.26,90),label = "co2 outlier") +
geom_text( aes(385.53,30),label = "co2 outlier")

b <- qplot(y=temp,x=methane,data=climates) + geom_smooth(method='lm', formula=y~x) +
geom_point(size = 2, aes(1786.15,90), col = "red") + 
geom_point(size = 2, aes(1799.19,94), col = "red") + 
geom_point(size = 2, aes(1809.46,23), col = "red") +
geom_text( aes(1786.15,87),label = "methane outlier") +
geom_text( aes(1799.19,99),label = "methane outlier") +
geom_text( aes(1809.46,28),label = "methane outlier")


grid.arrange(a,b)

# remove rows 87, 32, 76
climates <- climates[-c(87,32,76),]
out.clims <- lm(temp~co2+methane,data=climates) # Last time refitting model!
summary(out.clims)
```

![](https://tykiww.github.io/img/reg/reg7.png)

There is one peculiar thing I did with taking out the outliers. This is just my style, but I decided to leave in the outlier plots that were very recent (ie. 10 years). This may be an important thing to do especially when you are uncertain about future values. We never know, that maybe times will change in the next 10 years where methane and co2 become even more dangerous for the environment. 

Overall, here is the summary of the output.

    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -580.40258   74.05789  -7.837 3.96e-13 ***
    ## co2            0.39814    0.16239   2.452   0.0152 *  
    ## methane        0.27085    0.06596   4.106 6.11e-05 ***
    ## Multiple R-squared:  0.4438,	Adjusted R-squared:  0.4376 
    ## F-statistic: 71.43 on 2 and 179 DF,  p-value: < 2.2e-16


It seems like we have a model that predicts about 40 percent of the variance. Our beta hat values also seem to significantly show that these estimates do indeed reject the case of no effect. So, for the data we have currently, it really does support that greenhouse gasses have an effect on global warming. Now, whether global warming is a seasonal trend or not will have to be predicted with a time series, but for what we know now it is safe to say that the gasses we put into the air do have a role to play.

The whole process can arduous and long. It really is up to how well you hope to predict and use the information. I like to see how careful I can be and think about the most relevant information for our needs. Of course, with larger sets and larger data, it become very difficult to fit a huge linear regression. Much of the data would be littered with collinearity problems and make it difficult when more information is added.

I don't think I will be doing a full step by step regression anymore. Probably will do a logistical regression, but I will most likely move out of the realm of hypothesis tests and into more probability models.








-------------

#Climate Data:
Retrieved by Below: 12/20/2017
- Monthly Global Mean Land-Ocean Temperature Data (1880-Present)
* http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.txt

- Greenhouse gasses from Mauna Loa Observatory, Hawaii:
* Monthly mean atmospheric CO2 concentration expressed as micromol per mole and abbreviated as ppm
* ftp://aftp.cmdl.noaa.gov/data/trace_gases/co2/flask/surface/co2_mlo_surface-flask_1_ccgg_month.txt
* Monthly mean atmospheric CH4 concentration expressed as micromol per mole and abbreviated as ppm
* ftp://aftp.cmdl.noaa.gov/data/trace_gases/ch4/flask/surface/ch4_mlo_surface-flask_1_ccgg_month.txt
* Monthly Atmospheric N20 Concentration expressed as micromol per mole and abbreviated as ppb
* ftp://aftp.cmdl.noaa.gov/data/hats/n2o/insituGCs/CATS/monthly/mlo_N2O_MM.dat
* Monthly Atmospheric Hydrochloroflurocarbon Concentration expressed as micromol per mole and abbreviated as ppb
* ftp://aftp.cmdl.noaa.gov/data/hats/hcfcs/hcfc142b/insituGCs/CATS/monthly/mlo_HCFC142b_MM.dat
* Monthly atmospheric sulfer hexaflouride (SF6) Concentration expressed as micromol per mole and abbreviated as ppt
* ftp://aftp.cmdl.noaa.gov/data/hats/sf6/insituGCs/CATS/monthly/mlo_SF6_MM.dat








