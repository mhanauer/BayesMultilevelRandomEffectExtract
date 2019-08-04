---
title: "Creating simulated random effects data for two and three level models"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Here is the model that I am trying to recreate

Level 1: There is the intercept that varies for each person over time.  Then there is the slope for time that varies for each person over time.  Finally there is the error term that is unique for each data point.

$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}Time_{ij} + e_{ij}}~~~ (1.1)$$


Level 2 Intercept: Here the intercept is broken down into the constant plus the effect of the intervention, which is at level 2 in the intercept because it does not vary over time only by person and the error term which varies by person. 

$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + \gamma_{01}Intervention_{j} + u_{0j}} ~~~ (1.2)$$


Then there is level the two slope which has the constant effect, plus the slope for the intervention for each person, plus a random error term that unique to each person.  

$$ Level~2~Slope~Time:~~~{\beta_{1j} = \gamma_{10} + \gamma_{11}Intervention_{j} + u_{1j}} ~~~ (1.3)$$

Then we have the mixed model, which has all the components combined
$$Mixed~model: ~~~{y_{ij} =   (\gamma_{00}+ \gamma_{01}Intervention_{j} + u_{0j}) + (\gamma_{10}}+\gamma_{11}*Intervention_{j} +u_{1j})*Time_{ij} + e_{ij} $$

Library packages 
```{r}
library(HLMdiag)
library(ggplot2)
library(lme4)
#library(lmtest)
library(sjstats)
library(MASS)
library(semTools)
library(MuMIn)
```



I am basing this example on the example below and extending it by adding an intervention variable: http://m-clark.github.io/docs/sem/latent-growth-curves.html

I am creating a data set with 500 total people across 4-time points (ranging from 0 to 3) totaling 2,000 data points.  
I then create the number of subjects, which are 500 people replicated four times each.


I am assuming I have an outcome that is normally distributed and in standard normal form.     

Then I am setting the intercept to .5, a slope for the variable time to .25, and a slope for the intervention variable to .25.

Then I am creating the random effects for the intercept and time, because each person gets a unique intercept and a unique slope for time.  

I am also creating a slope for the interaction effect between time and intervention, which is also .25
```{r}
power_matt_two = function(){
n = 200
timepoints = 6
time = timepoints-1
time = rep(0:time, times=n)
subject = rep(1:n, each=timepoints)
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), n)
intervention = rep(intervention, each = timepoints)
  intercept = .5
slopeT = .25
slopeI = .25
slopeTI = .25
randomEffectsCorr = matrix(c(1,.2,.2, 1), ncol = 2)
randomEffectsCorr

randomEffects = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)
dim(randomEffects)
colnames(randomEffects) = c("Int", "SlopeT")
dim(randomEffects)
sigma = .05
y1 = (intercept + randomEffects$Int[subject])+(slopeT + randomEffects$SlopeT[subject])*time + slopeI*intervention + slopeTI*time*intervention+ rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, intervention, y1)
model1 = lmer(y1 ~ time*intervention + (time|subject), data = d)
model1_summary = summary(model1)
model1_summary$coefficients[,1]
}

```
Get random effects, but does not seem possible to do tests with freq need bayes
https://mc-stan.org/users/documentation/case-studies/tutorial_rstanarm.html
http://www.tqmp.org/RegularArticles/vol14-2/p099/p099.pdf
```{r}
sum_model1 =  summary(model1)
sum_model1$coefficients
ranef(model1)
library(rstanarm)
d$intervention
M1_stanlmer <- stan_lmer(y1 ~ intervention + (1 | subject), data = d,seed = 349)
library(tidyr)
library(broom)
summaryTwoLevelModelPerson <- tidy(M1_stanlmer, intervals=TRUE, prob=.95,
parameters = "varying")
summaryTwoLevelModelPerson
head(M1_stanlmer$coefficients)

draws <- as.data.frame(M1_stanlmer)
dim(draws)
head(draws)
median(draws$`b[(Intercept) subject:1]`)
summaryTwoLevelModelPerson
### Difference between them
diff_sub1_sub2 = draws$`b[(Intercept) subject:1]`-draws$`b[(Intercept) subject:2]`
median(diff_sub1_sub2)
sd(diff_sub1_sub2)
quantile(diff_sub1_sub2, c(.025, .95))
```

