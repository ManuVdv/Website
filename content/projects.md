---
description: projects
slug: projects
thumbnail: images/tn.png
title: Projects
---

--------------------

## Project 3: Statistical analysis on the Dow Jones Index

**1. Calculating losses with historical data**

```{r}
install.packages("qrmdata") 
library(qrmdata)
```

Read data of Dow Jones index constituents, see https://finance.yahoo.com/q/cp?s=%5EDJI
```{r}
data(DJ_const)
names(DJ_const)
```


portfolio and shares
```{r}
portfolio <- c("BA","IBM","KO") # ,"JNJ"
lambda <- c(1000,1000,1000) # 
S <- DJ_const[,portfolio]
logS <- log(S)
tt <- nrow(logS)
dim(diff(logS))
#Risk factor changes
#we will not take the first difference because it is useless
X <- diff(logS)[2:tt,]
```

current value of the portfolio and weights (current time is end time series: tt)
```{r}
Vtt <- sum(lambda * S[tt,]) # 427890 
weights <- lambda * S[tt,]/Vtt
```


function to build historical losses for this portfolio:
```{r}
loss.sim <- function(Xval, proportion, value){
  # arguments:
  # Xval ... matrix or vector of d risk-factor changes 
  # proportion ... row vector of d weights representing the proportion invested in each stock 
  # value ... scalar representing the value of the portfolio
  
  
  #if you don't have a matrix its a bit harder
  if (is.matrix(Xval)){
    n <- dim(Xval)[1]
    prod <- (exp(Xval)-1) %*% t(weights) #you can also do this with a for loop
    #log returns thats why we use exp
    total <- rowSums(prod)
  } else {
    n <- length(Xval)
    prod <- weights * (exp(Xval)-1)
    total <- sum(prod)
  }
  
  loss <- -value * total
  return(loss)
}

Loss.sim <- loss.sim(X, weights, Vtt)
summary(Loss.sim)
hist(Loss.sim, breaks = 20)

# with historical losses we can estimate the expected value of L_{tt+1}
meanL <- mean(Loss.sim, na.rm = TRUE)
```
![loss.sim](/images/loss.sim.jpeg)

**2. Calculating linear losses and mean and variance**

```{r}
linearised loss 
losslin.sim <- function(Xval, proportion, value){
  # arguments:
  # Xval ... matrix or vector of d risk-factor changes 
  # proportion ... row vector of d weights representing the proportion invested in each stock 
  # value ... scalar representing the value of the portfolio
  
  if (is.matrix(Xval)){
    n <- dim(Xval)[1]
    prod <- (Xval) %*% t(weights)
    total <- rowSums(prod)
  } else {
    n <- length(Xval)
    prod <- weights * Xval
    total <- sum(prod)
  }
  
  loss <- -value * total
  return(loss)
}


Losslin.sim <- losslin.sim(X, weights, Vtt)
hist(Losslin.sim, breaks = 20)

# mean and variance of loss lin is
# E(Llin) = Vtt * weights' E(X) 
# Var(Llin) = Vtt^2 * weights * sigma * weights
# easy to compute when you have the variances of the risk factor changes
# mean and variance of historical risk factor changes ~ normal
n <- dim(X)[1]
muX.hat <- colMeans(X, na.rm = TRUE)
sigmaX.hat <- var(X, na.rm = TRUE)  
meanLosslin <- -Vtt * sum(weights*muX.hat) 
varLosslin <- Vtt^2 *(weights %*% sigmaX.hat %*% t(weights))
```

**3. VaR and ES for our simulated loss with historical data**

```{r}
alpha <- c(seq(0.1,0.8,0.1), seq(0.9,1,0.01))
VaR.hs <- quantile(Loss.sim, alpha, na.rm = TRUE)
#this quantile function is for sample quantile 
#you need to remove the NAs! that's why we use na.rm
#for 7
ES.hs <- rep(0, length(alpha))
for(i in 1:length(alpha)) {
  values <- Loss.sim[Loss.sim > VaR.hs[i]]
  ES.hs[i] <- mean(values, na.rm = TRUE)
}

ran <- range(VaR.hs, ES.hs, na.rm = TRUE)
plot(alpha, VaR.hs, type = "l", ylim = ran, xlab = expression(alpha),
     ylab = expression("estimated VaR"[alpha],"~ES"[alpha])) # true ES_alpha
lines(alpha, ES.hs , type = "l", col = "maroon3") # ES_alpha estimate
legend("topleft", bty = "n", y.intersp = 1.2, lty = rep(1, 2),
       col = c("black", "maroon3"),
       legend = c(expression(VaR[alpha]),
                  expression(ES[alpha])))

# or build it
# n <- length(Loss.sim)
# L.order <- sort(Loss.sim)
# Varhat <- L.order[ceiling(n*alpha)] 
# plot(alpha, Varhat)

```
plot the histogram of the losses and highlight the VaR and ES 

![alpha](/images/alpha.jpeg)

**4. For losses that are Normal distributed, compute the VaR and ES for different values of alpha. Do the same for a t distribution**

```{r}
alpha <- c(0.90,0.95,0.975,0.99,0.995,0.999,0.9999,0.99999,0.999999)
```

Standard deviation (daily)
```{r}
sigma <- 0.2*10000/sqrt(250)
```

Degrees of freedom when using the Student t distribution
```{r}
t.dof <- 4
```
VaR based on the normal distribution:
As p is a vector of quantiles, plugging it into the inverse normal 
quantile function results in a vector of VaRs

```{r}
VaR.normal <- qnorm(alpha, sd=sigma)
```
VaR using student t:
standarized
```{r}
VaR.t4 <- qt(alpha, t.dof) * sqrt((t.dof-2)/t.dof) * sigma
```


---------------------------


## Project 2: Analysing AirBnb listings in Istanbul

Authors: Manu Vanderveeren, Shreya Salot, Fabio Bodenmann, Grace Feng, Jad El Temsah and Riccardo Luca Broggi

**1. Loading all the packages which we will use**

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r echo=FALSE}
# below we load all the packages that we will need for our analysis
library(tidyverse)
library(vroom)
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
library(tidyquant)
library(GGally)
```
Getting the data
```{r CACHE=TRUE}

listingsRAW <- vroom("http://data.insideairbnb.com/turkey/marmara/istanbul/2020-06-28/data/listings.csv.gz")

```
**2. Understanding the data**

```{r}
#We only select the relevant columns
listings <- listingsRAW %>% 
  select(price, cleaning_fee, extra_people, property_type, room_type, number_of_reviews, review_scores_rating, longitude, latitude, neighbourhood, minimum_nights, guests_included, bathrooms, bedrooms, beds, host_is_superhost, is_location_exact, neighbourhood_cleansed, cancellation_policy)

```

##Exploratory Data Analysis (EDA)

“The goal during EDA is to develop an understanding of the data. The easiest way to do this is to use questions as tools to guide the investigation… EDA is fundamentally a creative process. And like most creative processes, the key to asking quality questions is to generate a large quantity of questions.”

## Looking at the raw values

```{r}
#glimpse shows us all the different columns
glimpse(listings)

```

## Computing summary statistics of the variables of interest, or finding NAs

```{r}
listings <- listings %>% 
  
  # we use parse_number to remove the (dollar) signs in front of the cells in the columns (price, cleaning_fee and extra_people) so the values are just plain numbers that we can work with
  mutate(price = parse_number(price), 
         cleaning_fee = parse_number(cleaning_fee),
         extra_people = parse_number(extra_people)) 

skim(listings)

```
  
##Handling missing values (NAs)

Use skimr::skim() function to view a summary of the cleaning_fee data. This is also stored as a character, so you have to turn it into a number, as discussed earlier.

How many observations have missing values for cleaning_fee?
What do you think is the most likely reason for the missing observations of cleaning_fee? In other words, what does a missing value of cleaning_fee indicate?
cleaning_fee is an example of data that is missing not at random, since there is a specific pattern/explanation to the missing data.

Fill in the code below to impute the missing values of cleaning_fee with an appropriate numeric value. Then use skimr::skim() function to confirm that there are no longer any missing values of cleaning_fee.
  
```{r}

skim(listings$cleaning_fee)

listings <- listings %>%
  mutate(cleaning_fee = case_when(
    is.na(cleaning_fee) ~ 0, 
    TRUE ~ cleaning_fee
  ))

skim(listings$cleaning_fee)

#There are 13660 missing values for the variable cleaning_fee. Comparing it to other variables, we can see that review_scores_rating has a very similar missing value proportion. This suggests that listings with no reviews (most likely new listings) do not ask for a cleaning_fee to attract new customers. Accordingly, we set the NAs to a value of 0.

```
  
Next, we look at the variable property_type. We can use the count function to determine how many categories there are their frequency. What are the top 4 most common property types? What proportion of the total listings do they make up?

Since the vast majority of the observations in the data are one of the top four or five property types, we would like to create a simplified version of property_type variable that has 5 categories: the top four categories and Other. Fill in the code below to create prop_type_simplified.


```{r}

# Arranging the property types to view the top property types
listings %>% 
  count(property_type) %>% 
  mutate(proportion = n/sum(n)) %>% 
  arrange(desc(proportion))
  
```

```{r}

# we make a new column where we assign all other property types than the 4 most common ones to the category other
listings <- listings %>%
  mutate(prop_type_simplified = case_when(
    property_type %in% c("Apartment","Serviced apartment", "House","Boutique hotel") ~ property_type, 
    TRUE ~ "Other"
  ))
# we count the number of properties per property type and we arrange them from most common to least common
listings %>%
  count(property_type, prop_type_simplified) %>%
  arrange(desc(n)) 
  
```

Airbnb is most commonly used for travel purposes, i.e., as an alternative to traditional hotels. We only want to include listings in our regression analysis that are intended for travel purposes:

What are the most common values for the variable minimum_nights?

```{r}

listings %>% 
  count(minimum_nights) %>% 
  mutate(proportion = n/sum(n)) %>% 
  arrange(desc(proportion))
  
```

The most common value for minimum nights are 1, 2 and 3. 1 night accounts for 56%, 2 nights for 19% and 3 nights for 11% of the values. 


Is there any value among the common values that stands out?

A 1 night minimum is by far the most common value for this variable. However, there are also several occasions when the listings require a 1 month minimum stay. These are unlikely for travel purposes, rather for short-term  housing solutions. Among the most common three values, no values are unusual. 


What is the likely intended purpose for Airbnb listings with this seemingly unusual value for minimum_nights?

Filter the airbnb data so that it only includes observations with minimum_nights <= 4


Generating a visual representation of the airbnb listings in Istanbul with leaflet

```{r}
library(leaflet)
# here we load the package leaflet which we need in order to plot all the properties on an interactive map. The code below takes an interactive map  of Istanbul from OpenStreetMap.Mapnik and plots all the listings were the minimum number of nights is equal to or below 4.
leaflet(data = filter(listings, minimum_nights <= 4)) %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(lng = ~longitude, 
                   lat = ~latitude, 
                   radius = 1, 
                   fillColor = "red", 
                   fillOpacity = 0.4, 
                   label = ~property_type)

```
![Map](/images/Rplot.jpeg)


```{r}
# here we calculate the price for a 4 night stay, of course only for the properties where the minimun stay is below 5 nights.
listings <- listings %>% 
  filter(minimum_nights <= 4) %>% 
  mutate(price_4_nights = cleaning_fee + 4 * (price + extra_people * if_else(guests_included < 2, 1, 0)))


```
Use histograms or density plots to examine the distributions of price_4_nights and log(price_4_nights). Which variable should you use for the regression model? Why?


```{r}
# this code produces a density plot with price for 4 nights on the x-axis, we can clearly see that the graph is heavily skewed to the right
 listings %>% ggplot(aes(x=price_4_nights)) + 
  geom_density() 
# as there is a large difference in prices between prices for a 4 night stay, we replaced the price on the x-axis with a log price, this gives us a very interesting result as the graph looks normally distributed.
 listings %>% ggplot(aes(x=log(price_4_nights))) + 
  geom_density() 

```
![Map](/images/P4N.jpeg)

As the diagrams show, it is better to use the log of the variable price_4_nights in the regression model as it more clearly demarks the normal distribution of the prices, and therefore affords much more predictive power to our model

We now fit a regression model called model1 with the following explanatory variables: prop_type_simplified, number_of_reviews, and review_scores_rating.

```{r}
# Here we run a regression model of the log of the price for 4 nights with the number_of_reviews, prop_type_simplified and the review_scores_rating
model1 <-  lm(log(price_4_nights) ~ number_of_reviews + prop_type_simplified + review_scores_rating, data=listings) 
summary(model1)

```

Interpret the coefficient review_scores_rating in terms of price_4_nights
What the model suggests is that the number of reviews has significant explanatory power on the price for four nights, as we see a very small value for the P coefficient that would explain the price in terms of the null assumption. We  find instead an extremely low value, which therefore rejects the null hypethsis and tells us that the number of reviews is a significant predictor, as confirmed by the  three stars next to the coefficient row

Interpret the coefficient of prop_type_simplified in terms of price_4_nights.
The prop_type_simplified variable, on the other hand, is not as easy to interpret: what appears in fact is that only when the simplified property typeis a Servicd Apartment  or a Other category  does it reliably predict the  price for 4 nights, while in the case it is a  house or boutique hotel it does not: this is based on the P values seeni in the last column as explained earlier.
A  possible explanationc oould be that the  sheer number of houses, the most common category as seen earlier in the analysis,  confound the signal it could give. The boutique hotel, on the other hand we would expect to be more preedictive, however it appears it really is not.


```{r}
# Here we run a regression model of the log of the price for 4 nights with room_type, number_of_reviews, prop_type_simplified and the review_scores_rating
model2 <-  lm(log(price_4_nights) ~ room_type + number_of_reviews + prop_type_simplified + review_scores_rating, data=listings) 
summary(model2)

```

What we see when including in our regression the room_type variable is that this in fact holds much greater predictive power, especially when combined with the boutique hotel variable: with this new regression in fact we see that the Boutique Hotel property type has a very low P value, which combined with the room type variable helps us model much better the price for four nights.


Are the number of bathrooms, bedrooms, beds, or size of the house (accomodates) significant predictors of price_4_nights?

```{r}
# here we clean the data by setting all the NA values to 0, so that we can run the regression  ???? TODO
l3 <- listings %>% 
  mutate(bathrooms = case_when(
    is.na(bathrooms) | is.nan(bathrooms) | bathrooms==Inf | bathrooms==-Inf ~ as.integer(0), 
    TRUE ~ as.integer(bathrooms)),
  bedrooms = case_when(
    is.na(bedrooms) | is.nan(bedrooms) | bedrooms==Inf | bedrooms==-Inf  ~ 0, 
    TRUE ~ bedrooms),
  beds = case_when(
    is.na(beds) | is.nan(beds) | beds==Inf | beds==-Inf  ~ 0, 
    TRUE ~ beds)) %>%
  filter(price_4_nights>0)
# Here we run a regression model of the log of the price for 4 nights with beds, bathrooms and bedrooms
model3 <- lm(log(price_4_nights) ~ beds + bathrooms + bedrooms, data=l3)
summary(model3)

```

This regression confirms that the number of bedrooms, bathroom and beds are all relevant predictors of the price of a property, as their p values are extremely small

Do superhosts (host_is_superhost) command a pricing premium, after controlling for other variables?

Most owners advertise the exact location of their listing (is_location_exact == TRUE), while a non-trivial proportion don’t. After controlling for other variables, is a listing’s exact location a significant predictor of price_4_nights?

For all cities, there are 3 variables that relate to neighbourhoods: neighbourhood, neighbourhood_cleansed, and neighbourhood_group_cleansed. There are typically more than 20 neighbourhoods in each city, and it wouldn’t make sense to include them all in the model. Use city knowledge, or ask someone with city knowledge, and see whether you can group neighbourhoods together so the majority of listings falls in fewer (5-6 max) geographical areas. You would thus need to create a new categorical variabale neighbourhood_simplified and determine whether location is a predictor of price_4_nights

```{r}
#Mapping neighborhoods into groups for Istanbul thanks to Sena Salman's help, Group 1 is the most posh areas, 2 are average and 3 more in the outskirts

listings_w_area <- listings %>% 
  filter(price_4_nights>0)%>% 
  mutate(neighbourhood_custom_areas = case_when(  

neighbourhood_cleansed == "Uskudar" | neighbourhood_cleansed == "Besiktas" | neighbourhood_cleansed == "Beyoglu" | neighbourhood_cleansed == "Sisli" | neighbourhood_cleansed == "Adalar" | neighbourhood_cleansed == "Fatih" | neighbourhood_cleansed == "Atasehir" | neighbourhood_cleansed == "Sariyer" | neighbourhood_cleansed == "Kadikoy" | neighbourhood_cleansed == "Bakirkoy" ~ 1,

  neighbourhood_cleansed == "Kagithane" | neighbourhood_cleansed == "Esenyurt" | neighbourhood_cleansed == "Maltepe" | neighbourhood_cleansed == "Bayrampasa" | neighbourhood_cleansed == "Buyukcekmece" | neighbourhood_cleansed == "Sultanbeyli" | neighbourhood_cleansed == "Gaziosmanpasa" | neighbourhood_cleansed == "Zeytinburnu" | neighbourhood_cleansed == "Gungoren"  | neighbourhood_cleansed == "Bahcelievler" | neighbourhood_cleansed == "Eyup" | neighbourhood_cleansed == "Umraniye" | neighbourhood_cleansed == "Cekmekoy" | neighbourhood_cleansed == "Esenler" | neighbourhood_cleansed == "Sancaktepe" | neighbourhood_cleansed == "Kucukcekmece" | neighbourhood_cleansed == "Sultangazi" | neighbourhood_cleansed == "Bagcilar" ~ 2,

  neighbourhood_cleansed == "Sile" | neighbourhood_cleansed == "Tuzla" | neighbourhood_cleansed == "Pendik" | neighbourhood_cleansed == "Arnavutkoy" | neighbourhood_cleansed == "Avcilar" | neighbourhood_cleansed == "Beylikduzu" | neighbourhood_cleansed == "Kartal" | neighbourhood_cleansed == "Beykoz" | neighbourhood_cleansed == "Catalca" | neighbourhood_cleansed == "Silivri" | neighbourhood_cleansed == "Basaksehir" ~ 3)) 

# Here we run a regression model of the log of the price for 4 nights with the neighbourhood_custom_areas
model4 <- lm(log(price_4_nights) ~ neighbourhood_custom_areas, data=listings_w_area)
summary(model4)

```

Having mappes the different neighborhoods to select groups based on their "presitge", we can see that this information holds significant predicting power on the price of a 4 night stay, as shown by the extremely low P value fitted

What is the effect of cancellation_policy on price_4_nights, after we control for other variables?

```{r}
# Here we run a regression model of the log of the price for 4 nights with cancellation_policy, bathrooms, bedrooms, room_type, number_of_reviews, prop_type_simplified and review_scores_rating
model3 <- lm(log(price_4_nights + 0.0001) ~ cancellation_policy + bathrooms + bedrooms + room_type + number_of_reviews + prop_type_simplified + review_scores_rating, data=l3)
summary(model3)
```

----------------------

## Project 1: Identifying prostate cancer

Authors: Manu Vanderveeren & Laura Bogaert

**1. Understanding the data**

This prostate dataset wants to predict a score called “Cscore” which describes the progression
of prostate cancer based on multiple variables such as size of the prostate, age of patient
among others.
After uploading the data, we checked the different dimensions, possible missing data, and
names of all columns. 

```{r}
> dim(prostate)
[1] 97 8
> sum(is.na(prostate))
[1] 0
> names(prostate)
[1] "Cscore" "lcavol" "lweight" "age" "lbph" "svi" "lcp" "lpsa"
```
We found that our data has 97 data poins and 8 columns called “Cscore”, “lcavol”, “lweight”, “
age”, “lbph, "svi", "lcp", "lpsa" with no missing data.
To better understand the several variables, We plotted them individually to Cscore.

![Cscore](/images/Cscore.jpg)

Evaluating the graphs, we find three items that require further investigation.
Firstly, we see that svi’s x-values are all 0 or 1. This suggests the variable is categorial. Secondly, both lbph and lcp seem to have a big group of data points with the same highly negative value. This value will be further discussed underneath.
Thirdly, there seems to be some outliers that might influence the predictions.
 
Overall, all variables have different ranges. If this is not taken into account, some variables might have a higher weight on the predictions. In this case, however, it should not pose a problem as the glm function in R automatically standardizes the data.

We make svi categorial and find that 76 data points do not have ‘svi’ and 21 do. The variable is now ready to be used for predictions
0 1
76 21

Looking at the variables lbph and lcp, we find that the most negative point is -1.36 with almost half the data having this x-value for both variables. We assume this means that lbph and lcp are some kind of marker that is not always detectable in the blood and hence are given this value in case the marker is not found. Given we do not have more information about the variables, we leave them as they are.

```{r}
>	min(prostate$lbph) [1] -1.386294
>	min(prostate$lcp) [1] -1.386294
>	sum(prostate$lbph <= -1.3862) [1] 43
>	sum(prostate$lcp <= -1.3862) [1] 45
```
Finally, checking for the outliers seen on the graphs above, we looked for data points that were 3 times the interquartile range above the upper quartile or bellow the lower quartile.

```{r}
[[1]]
[1] 373.0657 216.9818

[[2]]
numeric(0)

[[3]]
[1] 6.10758

[[4]]
numeric(0)

[[5]]
numeric(0)

[[6]]
[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

[[7]]
numeric(0)

[[8]]
numeric(0)
```
We found that Cscore and lweight had outliers in this category while cvi (list 6) cannot have outliers as it is a categorial variable. We decided, however, to keep the outliers in the data set due to a lack of understanding of how these variables were measured and whether the outliers were a typo or real possible number.

Finally, to understand the variables, we performed a correlation plot to see how the variables are relating to Cscore and each other. It is important to check for possible collinearity as Lasso can specifically be used to counter this problem.

![svi](/images/svi.jpg)

Looking at the correlation matrix, we see that svi, lcp, lpsa and lcavol are the four variables most highly correlated to Cscore. These variables will probably predict Cscore the best. However, we see that these same four variables are also rather highly correlated to each other meaning that there is some multicollinearity.

The presence of this phenomenon can have a negative impact on our analysis as a whole and specifically the variance. Therefore, we will have to use a shrinkage technique such as Lasso to select an appropriate subset of our variables.

**2. LASSO model**

In order to run a lasso model, we created a matrix x and vector y and split up our data in test and train data for a 80-20 split. Next, we ran a lasso model on our training data and plotted the coefficients of each variable against the ℓ1-norm of the whole coefficient vector as λ varies. We observe that the lasso first results a model that contains only lpsa. Then lcp and svi enter almost simultaneously. Next is lweight and lcavol before the remaining variables enter the model.

![L1norm](/images/L1norm.png)

Next, we need to find the optimal lambda for the model. We use cross validation to find the optimal lambda. Underneath, you can see the value of lambda plotted against the MSE of the training data.

![MSE](/images/MSE.png)

As seen on the graph above, the lambda for the smallest MSE (minlambda) on the training data returns 3 non-zero variables while the lambda within one SE (lambda1se) returns a model with one non-zero variable.
We compared the MSE on the test data using both lambdas and found that the minlambda gave the best test MSE. (see underneath)

```{r}
>	MSE.minlambda [1] 639.5722

>	MSE.lambda1se [1] 793.1503
```
Next, we ran the lasso model on the whole dataset using the minlambda and found the coefficients of all variables.

```{r}
>	lasso.coef
(Intercept)	lcavol	lweight	age	lbph	svi	lcp lpsa
-26.046484	0.000000	0.000000	0.000000	0.000000	17.677799	3.628364 23.814752
```
Finally, we see that Lasso gives us a 3 non-zero variables model with coefficients 23.81, 17.68, and 3.63 of lpsa, svi, and lcp, respectively. The intercept of the model is -26.05 meaning that every person starts off with a negative score. This then increases the higher the lcp and lpse. Whenever a person has “1” for svi, its score will go up by 17.68.
Comparing the final Lasso model to variables that correlated highly with Cscore or not, the model does not seem too surprising; all variables that correlated highly with Cscore are still in the model, except for lcavol which was forced to zero by Lasso.
The reason for this will be discussed further in the next chapter.

**3. Does "Lcacol" correspond to how wll it can predict Cscore?**

When looking at the correlation matrix, the correlation between lcavol and Cscore was rather high meaning that the variable could explain Cscore well. In contrast, the coefficient of lcavol was reduced to zero in the Lasso model. Therefore, the coefficient does not correspond to how well the variable originally seemed to be able to predict Cscore.
This can be explained by the correlation between the lcavol and the other variables that are not zero in the Lasso model. Lasso is specifically used when the dataset has multiple variables that correlate. It penalizes and forces coefficients of variables to zero when there are too many that do not add much to the model and might cause a high variance.
As the other variables such as lpsa explained Cscore a bit better, Lasso chose to keep those and reduce lcavol’s coefficient to zero.

**4. Model with Non-linear effects**

Next, we wanted to fit and compare different models with different polynomials for the variab les that Lasso gave us. The problem was that there were many possible polynomials and comparing all of them manually would take a long time. We therefore found a function called “polywog” which not only compares different polynomials using cross validation but also includes interactions between the different variables. Additionally, the function deletes variables that are exactly the same (e.g. svi^2 is deleted because svi is categorial and 1^2 equals 1) before it uses Lasso to prevent overfitting. More about this function can be found h ere: https://cran.r-project.org/web/packages/polywog/polywog.pdf.

We fitted the model on the same training data as used for Lasso with the three variables given by the previous model. For each degree of polynomial, the function returns the lambda involved and error rate. (see below)

```{r}
>	cv1$results
	degree lambda.min	cv.err
[1,]	1 7.9974302 1659.6030
[2,]	2 0.6287708 913.3407
[3,]	3 0.1690634 670.3135
[4,]	4 18.6840829 716.2160

```
We see that the third polynomial gives the lowest error rate. The polywog function automatically chooses the correct degree and can be used to predict the MSE of the test data.
The test MSE gives 277.28, which is significantly lower than Lasso’s test MSE of 639.57.

```{r}
>	MSE.polywog.3v [1] 277.2717
```

Although we previously said we would only use the variables given by the original Lasso model, we wanted to check if the model would generate better results when lcavol was added back.

```{r}
>	cv2$results
	degree lambda.min	cv.err
[1,]	1 0.07561102 1606.8289
[2,]	2 3.96883460 827.1445
[3,]	3 5.58414921 799.1701
[4,]	4 86.34432040 959.1398

>	MSE.polywog.4v [1] 677.2405
```

As seen above, this was not the case. Both the error of the training data and MSE of the test data of the model with now 4 variables (polywog.4v) generated a higher error than the polywog model with 3 variables (polywog.3v).
We therefore conclude that the polywog model with 3 variables is the best model with appropriate non-linear effects with a MSE test of 277.28 compared to 677.24 of the polywog.4v and 639.57 of the Lasso model.
Finally, it would be interesting to look at the coefficients generated polywog.3v model.

```{r}
>	cv1$polywog.fit$coefficients
(Intercept)	svi	lcp	lpsa	svi.lcp
0.147715677	-144.665137258	0.000000000	4.775317200	179.383763409
svi.lpsa	lcp^2	lcp.lpsa	lpsa^2	svi.lcp^2
0.001381793	6.342430677	0.000000000	-4.010743051	-17.063534984
svi.lcp.lpsa	svi.lpsa^2	lcp^3	lcp^2.lpsa	lcp.lpsa^2
-46.019161095	11.851694891	0.661667904	-1.218838544	1.025148043
lpsa^3				
1.980807662				
```
As mentioned above, the function deleted svi^2 because svi would generate the exact same result. Multiple interaction variables between all three variables or a combination are added to the model too (e.g. lcp.lpsa, svi.lcp.lpsa) making for a very strong model.

**5. Conclusion**

This project tried to generate the best possible model to predict Cscore, which describes the progression of prostate cancer, based on multiple variables.
Overall, we created two main models polywog and Lasso model. The latter generated coefficients for three variables: svi, lcp, and lpsa. The polywog model generated coefficients for these three variables as well as for newly created variables which were interactions between these three variables as well as polynomials of them.
The models generated a test MSE of 639.57 and 277.28, respectively.

---------------------------

