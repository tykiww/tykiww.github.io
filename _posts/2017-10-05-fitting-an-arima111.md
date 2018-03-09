---
layout: post
title: "Fitting a simple timeseries: Arima (1,1,1)"
fb-img: https://tykiww.github.io/img/arima111/yosemite.png
tags: [asta, arima, base R, vizualization]
---


How many visitors can we expect to see in Yosemite National park for the next 5 years? One of my most desired go-tos has always been Yosemite National Park. I’ve never been! It's just so beautiful.. Maybe one day soon!

![](https://cdn.shopify.com/s/files/1/0272/4781/files/summer-sunset-over-half-dome-from-glacier-point-yosemite.jpg?1089)

My first entry is a simple timeseries model of an ARIMA(1,1,1). The Arima model, also known as the Box Jenkins method is the most general class of autoregressive model for forecasting a time series. Simply, the 1,1,1 stands for (last period's change,year to year change, moving average). These details can be fine tuned according to how the data looks, but as a general guideline, the ARIMA(1,1,1) is beneficial and accurate for most cases. If you are curious, visit this [link](https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials).

That's enough of the stats lessons. Let's actually try fitting the model to the Yosemite data!

For the annual visitors data, I was able to download a dataset from the National Park's service [STATS](https://irma.nps.gov/Stats/SSRSReports/Park%20Specific%20Reports/Annual%20Park%20Recreation%20Visitation%20(1904%20-%20Last%20Calendar%20Year)?Park=YOSE) website. The information spans from the inception of the park up until the last calendar year, and since it's only the beginning of 2018, the data runs until 2016. Might be fun to see how accurate these next predictions will be once the 2017 data comes out!

For a more in-depth analysis, I would hunt for park event details but for today we are just focusing on the simple Rbase graphs and the statistical model. Normally, I would scrape the information using the rvest package yet the data was so small, I just downloaded the csv and copied it in through a simple read.table.

``` r								
Yos <- read.table(header = TRUE, text = '
Year	RecreationVisitors	TotalRecreationVisitors
1906	5,414	190,404,561
1907	7,102	190,404,561
1908	8,850	190,404,561
1909	13,182	190,404,561
...
2014	3,882,642	190,404,561
2015	4,150,217	190,404,561
2016	5,028,868	190,404,561'
)
```
The `tail()` shows a quick look at what we just read in. Now, to perform an analysis, these strings need to be numeric and we can plot the data to take a look.

``` r												
Yos$RecreationVisitors <- as.numeric(gsub(",","",Yos$RecreationVisitors)) #Gsub all the commas with no spaces.
Yos$RecreationVisitors <- Yos$RecreationVisitors/10^6

plot(RecreationVisitors~Year,
     data=Yos,type="b", 
     ylab = "Yosemite Annual Visitors (In millions)")
```

![](https://tykiww.github.io/img/arima111/yos1.png)

From this data, we can see the multiplicative curvature of the graph. This is difficult to fit an ARIMA model (or any model) as the analysis uses additive data to predict future values. Fortunately, this is a simple fix. By performing a log transformation, we can see how the data becomes more readable.

``` r											
Yos$lnvisitors <- log(Yos$RecreationVisitors) #Log transformation
plot(lnvisitors~Year,
     data=Yos,
     type="b", 
     ylab = "Yosemite Log Annual Visitors (In millions)")
abline(v=1941, col="red")
abline(v=1946, col="red")
```

![](https://tykiww.github.io/img/arima111/yos2.png)

How simple! The graph appears to be additive from 1930s to present, but has a dip between 1940-1950. Looking closely, Non-constant mean changes occur from 1941 to 1946. The dip seen from 1940-1946 roughly fits the time period of world war 2. This may suggest that wartime may have changed opportunities or interest of individuals. Some other possible causes for the curvature from 1920s to 1946 may be due to an increase in exposure, interest, and transportation. Good to think about.

Yet, this makes the data unusable! So we will need to subset the data to after 1945. By subsetting, we notice a more additive model that suggests less if not any changes in behavior or policy from year to year. With this subsetted data, we can see a more constant mean change that reflects future Yosemite tourism demand.

```r							
Yos[Yos$Year==1945,] #Subset to after 1945
Yos46 <- Yos[-(1:40),]

#plot again to check for additive model
plot(lnvisitors~Year,
     data=Yos46,
     type="b",
     ylab= "Yosemite Log Annual Visitors (In millions)")								
```

![](https://tykiww.github.io/img/arima111/yos3.png)

Everything looks great! now that we have an additive model subsetted to the recent past, we can continue with the forecasting.

Now let's take look at the arima model. You might need to install the asta package. The asta package was created by a professor from the University of Pittsburg, and information on time series analysis is on his [website](http://www.stat.pitt.edu/stoffer/tsa4/index.html) The `sarima()` model links all of the past datapoints and gives us parameter estimates. At first you are going to see a list of graphics that show the residual for the 1,1,1 model. We're going to ignore this as it has no relevance to our question.

The output ar1, ma1, and constant are the names for phi, epsilon, and mu. This information tells us the parameter estimate mu, and the standard errors. 

The more interesting and applicable portion is done using the function `sarima.for()`. This is prediction element. You can see in Yos46.future how **_easy_** it is to fit the arima model. The first element denoting the log visitors, n.ahead being the number of years to forecast, and 1,1,1 coming from the arima function. Immediately, a graphic appears with the prediction values and 95 percent confidence estimates. Of course, these are logged values so we need to make sure to finish by creating an unlogged graph that meets publication quality!

``` r
library("astsa")

Yos46.out <- sarima(Yos46$lnvisitors,1,1,1)
Yos46.out$ttable[1:3,1]      #Find Parameter values

# Compute predictions for the next 5 years by fitting in an ARIMA(1,1,1) and make a graph
LogYosemiteVisitors <- Yos46$lnvisitors

Yos46.future <- sarima.for(LogYosemiteVisitors,n.ahead=5,1,1,1)
abline(v=71, col = "blue")   #up to 2016


exp(Yos46.future$pred)       #The unlogged prediction values for the next 5 years.
Yos46.yhat <- exp(Yos46.future$pred)
```

Parameter estimates (ar1, ma1, mu)

    ##      ar1      ma1 constant 
    ##  -0.6511   0.5499   0.0291 

Prediction estimate values    

    ## Time Series:
    ## Start = 72 
    ## End = 76 
    ## Frequency = 1 
    ## [1] 5.096657 5.301227 5.421584 5.606032 5.755369
 
![](https://tykiww.github.io/img/arima111/yos4.png)
 
Now all we need to do is represent the forecast values and the 95% confidence estimates along with a really **_pretty_** looking graph. In my honest opinion, base R doesn't have the prettiest graphics. My go-to is ggplot, but for the sake of functionality, I made it very simple. I'll show you some cool tricks with ggplot pretty soon!

``` r
#Above gives us prediction and standard error, so compute 95% prediction intervals
t <- qnorm(0.975)
Yos46.L <- Yos46.future$pred - t* Yos46.future$se
Yos46.U <- Yos46.future$pred + t* Yos46.future$se

#un-transform the log data back to normal

Yos46.yhat.L <- exp(Yos46.L)
Yos46.yhat.U <- exp(Yos46.U)
cbind("parameter" = Yos46.yhat,"lower" = Yos46.yhat.L,"upper" = Yos46.yhat.U)


#The *pretty* not so pretty graph

plot(RecreationVisitors~Year,data=Yos,type="o",                        #Data up until the current year
     ylab="Yearly Visitors (in millions)",
     xlim=c(1980,2021),
     ylim=c(0,7),
     main = "Yosemite National Park Annual Visitors")
lines(2017:2021,Yos46.yhat.L,col="blue",lty=2)                         #lower bounds
lines(2017:2021,Yos46.yhat.U,col="blue",lty=2)                         #upper bounds
polygon(c(2017:2021,rev(2017:2021)),c(Yos46.yhat.L,rev(Yos46.yhat.U)), #Filling in the lines between the upper
        col = "light gray",                                            #and lower bounds in the prediction.
        border = NA)
lines(2017:2021,Yos46.yhat,col="red",type="b",pch=5)
lines(2017:2021,Yos46.yhat,col="red",type="b",pch=3)
```
Table of the unlogged 95% parameter estimates and CI.

    ## Time Series:
    ## Start = 72 
    ## End = 76 
    ## Frequency = 1 
    ##    parameter    lower    upper
    ## 72  5.096657 4.472109 5.808425
    ## 73  5.301227 4.446741 6.319912
    ## 74  5.421584 4.366934 6.730940
    ## 75  5.606032 4.376368 7.181205
    ## 76  5.755369 4.362865 7.592320

![](https://tykiww.github.io/img/arima111/yos5.png)
 
As you can see, we now have both a table with predictions and a good looking graph of those future values. Pretty simple huh? This type of analysis is very handy for forcasting future performance whether it be for marketing trends, or tourism details. **_Yet there is always a catch!_** I probably should have told you at the very beginning, but this type of modelling is not useful for seasonal trends which oscillate during different periods (ie. mapping climate change, airport delay times... I will be doing a different seasonal arima example next.) 

Some other things to watch out for may include (Arima Model assumptions):
1.There are no known/suspected predictor variables
2.There are no deterministic time trends of the form 1,2,3,...,t
3.There are no one time anomalies (The model parameters are constant over time)
4.The error process is homoscedastic.

Since arima is a type of regression using past values (values predicted from year to year), sharp dips and spikes may interfere with the model. This means that arima does not take into consideration any procedural changes, anomalies, or other predictor values that may have actually contributed to any skips in convergence (ie. park closes down, vertical integration of a new financial firm, or even a blizzard that tears through a potato farm).

Regardless, we've done it! The Arima model still continues to be one of the most standard procedures to forecast future values to this day. Maybe because time happens to be amongst the strongest predicting elements of nature.
Although it is not the best possible model (do we even know what the best model even is?), it is almost like those gloves where one-size fits all.
