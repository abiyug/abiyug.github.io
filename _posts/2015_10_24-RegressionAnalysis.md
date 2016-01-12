---
title: "ToothGrowth Data Analysis"
author: "Abiyu Giday"
date: "October 24, 2015"
output:
  html_document:
    toc: yes
  pdf_document:
    fig_caption: yes
    toc: yes
---

Here's a figure: 
```{r figs, echo=FALSE, fig.width=4,fig.height=3, fig.cap="\\label{fig:myfig}plotting example"}
plot(runif(100, 0.0, 1.0),type="l")
```

Take a look at Figure \ref{fig:myfig} for an example of plotting in R.


author: "[figure1](figure1)"

```{r xtable, results="asis"}
n <- 100
x <- rnorm(n)
y <- 2*x + rnorm(n)
out <- lm(y ~ x)
library(xtable)
tab <- xtable(summary(out)$coef, digits=c(0, 2, 2, 1, 2))
print(tab, type="html")
```

```{r kable, echo=FALSE}
library(knitr)
kable(head(mtcars))
```

```{r pander}
n <- 100
x <- rnorm(n)
y <- 2*x + rnorm(n)
out <- lm(y ~ x)
library(pander)
panderOptions("digits", 2)
pander(out)
```
##Overview
On this paper we will look at a "ToothGrowth" dataset that contains three variables and sixtyobservations. The experiment was performed on 10 huinea pigs witch pigs given 3 dose of Vitamin C (0.5, 1, and 2 mg) with each of two delivery methods (orange juice or ascorbic acid). We will begin with explorator analysis of the dataa, followed by generating model's that fit the combined data as well as fit model for each type of Vitamin C in the data. We will then look at the corelation $r^2$ that measures the strenght or weakness of the predictor, and calculate  95% confidence interval comparing the tooth growth to dose. Ultimately we will try and answer the question which Vitamin C source has a better tooth growth rate when applied equally.
 
##Basic Exploratory Analyses
Here we are going to tidy the data using a dplyr package. We will group the data since there are two sets of groups (OJ and VC), and we will average growth per dose intervals, which are given every 0.5 mg upto 2.mg. We will then draw a scatter plot, segregating the plot by vitamin C type. We will map a smooth line between the points in the graph.

```{r,warning=FALSE, message=FALSE }
library(dplyr)
#dim(ToothGrowth)
#str(ToothGrowth)
#View(ToothGrowth)
 
# data prepration
tothdf <- ToothGrowth  %>% group_by(supp, dose)  %>% summarise_each(funs(mean))
tothdf$supp <- as.factor(sapply(tothdf$supp, tolower))
tothdf
summary(tothdf)
```

```{r, figure1, warning=FALSE, message=FALSE}
library(ggplot2)
#exploratory plots and observation
qplot(dose, len, data = tothdf, colour = supp, main="Scatter plot for Does vs Tooth Length", xlab = "Dose in mg", ylab = "Tooth Growth", size = len, shape = supp, geom = c("point", "smooth"))
 ```

###Basic Summary of the data
From figure 1 and the summarized data, we see that on the average vitamin C obtained from Orange(OJ) at 0.5mg and 1.0mg cause a tooth to grow at a faster rate than ascorbic acid (VJ). For example, taking 0.5 mg of OJ results in 13.23 vs 7.98 for ascorbic acid. That is significant 66% more growth when does comes from OJ. At 1mg dose the gap narrows, but vitamin C from orange juice is 35% more effective for toth growth than VC. However, if both orange juice and ascorbic acid are given at does 2.0mg, tooth growth at the same rate.


```{r, echo=FALSE, warning=FALSE, message=FALSE}
#Filter Supplement type (VC or OJ)
oj.dose <- ToothGrowth  %>% filter(supp == "OJ")
vc.dose <- ToothGrowth  %>% filter(supp == "VC")

summary(tothdf)
summary(oj.dose$len)
summary(vc.dose$len)
```

##Confidence interval or Hypothesis test to compare tooth growth by supp and dose
Here we will generate three linear models. First for the combined data (toth.lm), and the other two (oj.lm and vc.lm) for grouped data segregated by vitamin C types. We will then dervie the regression line for each, 

```{r, echo=FALSE, warning=FALSE, message=FALSE}
toth.lm <- lm(tothdf$len ~ tothdf$dose)
oj.lm <- lm(oj.dose$len ~ oj.dose$dose)
vc.lm <- lm(vc.dose$len ~ vc.dose$dose)

#the equations for each
toth.lm
oj.lm
vc.lm
```

**Overall toth growth regression line is**

$Y = \beta_0 + \beta_1 x + \epsilon$

$\hat{y} = 7.423 + 9.764 x$
 
Regression line equation just for OJ

$\hat{y} = 11.55 + 7.811 x$

Regression line equation just for VC

$\hat{y} = 3.295 + 11.716$ 

```{r, warning=FALSE, message=FALSE}
summary(toth.lm)
```

The squared corelation $r^2$ for the over all fitted model is 0.6443, so we can say 64.43% of the tooth growth in the data can be attributed to the dose levels of Vitamin C. Vitamin C here a combination of both orange juice and ascorbic acid. We can also see that since the P value for dose is  1.23 x $10^{-14}$ , whhic is less than 0.05, the regressor for the equation (dose) is fairly significant. 

**Here we will demonstrate 95% confidence interval** 
We will use the critical value from the regression model summary and calculate the 95% confidence interval for total vitamin c dose, which in this case will be the slope $\beta_1$.

critical value equation is B +/- t*SE

$t = qt(0.975, 58)$

$t*SE = 2.001717 * 0.9525 = 1.906636$

$\hat{y} = 7.423 + 9.764 x$

Therefor the 95% confidence interval for the dose interval is:  

$\hat{B} = 9.7636 + 1.906636,  9.7636 - 1.906636$ 

$\hat{B} = (11.67, 7.8)$


**The summary for the fitted models for OJ and VC are as folloows.** 
```{r, warning=FALSE, message=FALSE}
summary(oj.lm)
summary(vc.lm)
```

##Over all Observation

The squared corelation $r^2$ measures how strong and/or weak a predictor is to the outcome. When comapring the $r^2$ for orange juice (OJ) dose vs ascorbic acid (VC), we see that at 80.28%, OJ is a much stronger predictor than VC, which has a 56.26% value for $r^2$.  Also, we can see that OJ has a highter slope at 1.302 vs 1.079 for VC. That is that the rate of tooth grows at 30% faster rate when getting vitamin C from orange juice than it is from getting it from asorbic acid. If vitamin C does from OJ is given at 0.5, 1.0 and 1.5 mg 10 guinea pigs tooth grew at a faster rate, however at 2.0 mg dose, both OJ and VC cause the same level of tooth growth on the guine peauges. 


