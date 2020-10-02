---
description: projects
slug: projects
thumbnail: images/tn.png
title: Projects
---

---------------------------

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

