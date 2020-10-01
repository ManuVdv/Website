---
description: projects
slug: projects
thumbnail: images/tn.png
title: Projects
---

## Project 1: Identifying prostate cancer

**1. Understanding the data**

This prostate dataset wants to predict a score called “Cscore” which describes the progression
of prostate cancer based on multiple variables such as size of the prostate, age of patient
among others.
After uploading the data, I checked the different dimensions, possible missing data, and
names of all columns. 

```{r}
> dim(prostate)
[1] 97 8
> sum(is.na(prostate))
[1] 0
> names(prostate)
[1] "Cscore" "lcavol" "lweight" "age" "lbph" "svi" "lcp" "lpsa"
```
I found that our data has 97 data poins and 8 columns called “Cscore”, “lcavol”, “lweight”, “
age”, “lbph, "svi", "lcp", "lpsa" with no missing data.
To better understand the several variables, I plotted them individually to Cscore.

![Cscore](/Users/vande/Documents/Website/themes/hugo-coder-portfolio/static/images/Cscore.jpg)
![Cscore](Users/vande/Documents/Website/themes/hugo-coder-portfolio/static/images/Cscore.jpg)
![Cscore](static/images/Cscore.jpg)
![Cscore](images/Cscore.jpg)

...
