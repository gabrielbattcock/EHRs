## Survival analysis 1 
library(ggplot2)
library(tidyverse)


#Practical 1
```{r}

library(ggplot2)
library(tidyverse)
pbc <- read_csv("../Practical datasets-20230224/pbcbase_2021.csv")
```

```{r}
head(pbc)
dead_number <- sum(pbc$d == 1)
earliest_in <- min(pbc$datein)
eariest_out <- min(pbc$dateout)

summary(pbc)
```



### Dates in R  
```{r}
pbc$datein=as.Date(pbc$datein,"%d%b%Y")
pbc$dateout=as.Date(pbc$dateout,"%d%b%Y")

pbc$daysinstudy <- pbc$dateout-pbc$datein
```

Discuss: What do you think is the appropriate time origin in the PBC
study? How is time measured relative to the time origin?

The date in for individual is appropriate so that the time is measured from entry date in the study

```{r, echo = FALSE}
library(survival)

Surv(time=pbc$time,event=pbc$d)
```
The positive sign in the `Surv()` function represent that the individuals are alive.

Another way of specifying the Surv() function is:
```{r}
Surv(time=as.numeric(pbc$dateout),event=pbc$d,origin=as.numeric(pbc$datein))#calculates as number so is in days
```

## Whitehall data


```{r}
whl <- read_csv("../Practical datasets-20230224/whitehall.csv")
head(whl)
```
change from string to date

```{r}
whl$timein=as.Date(whl$timein,"%d%b%Y")
whl$timeout=as.Date(whl$timeout,"%d%b%Y")
whl$timebth=as.Date(whl$timebth,"%d%b%Y")

whl_timeout <- max(whl$timeout)
sum(whl$timeout == whl_timeout)

```
The study must have ended on that day

```{r}
whl$timein=as.numeric(whl$timein)
whl$timeout=as.numeric(whl$timeout)
whl$timebth=as.numeric(whl$timebth)
Surv(time=whl$timeout,event=whl$chd,origin=whl$timein)
Surv(time=whl$timeout,event=whl$chd,origin=whl$timebth)
Surv(time=whl$timein,time2=whl$timeout,event=whl$chd,origin=whl$timebth)
Surv(time=whl$timein/365.25,time2=whl$timeout/365.25,event=whl$chd,
origin=whl$timebth/365.25)
```


## Section 2 - derivation of MLE

The maximum likliehood of the exponential distribution $L(\lambda) = \lambda \exp{}


```{r}
surv <- read_csv("../Practical datasets-20230224/surv_data_practical1.csv")
head(surv)

lambda = 1/mean(surv$survtimes)
```
### Different packages for estimating Max likelihood
```{r}


exp.model = survreg(Surv(survtimes)~1,dist="exponential",data=surv)
summary(exp.model)
exp(-1.53)

```

```{r}
library(flexsurv)
exp.model2 = flexsurvreg(Surv(survtimes)~1,dist="exponential",data=surv)
exp.model2
```


## Section 3

The Weibull distribution function: $h(t) = \lambda \kappa t^{\kappa - 1}$ .

```{r}
wei.haz<-function(x,lambda,kappa){lambda*kappa*x^(kappa-1)}
curve(wei.haz(x,2,1.23),xlab="Time",ylab="Hazard function")
```

The log logistic function: $h(t) = \frac{e^\theta}


# Practical 2

```{r}
library(survminer)
library(survival)
```

## Part A - Primary Bilary Cirrhosis data

```{r}
n_placebo <- sum(pbc$treat == 1)
n_active <- sum(pbc$treat == 2)

median_event <- median(pbc)
```
Creating a Kaplan-Meier plot

```{r}
pbc.km <- survfit(Surv(time,d)~treat,data=pbc)

ggsurvplot(pbc.km, data = pbc, conf.int = T)

```

To complete the survival probability

For week time = 1.572: 
```{r}
survival1 <- function(d, n, prev){prev*(1-d/n)}
t_1.572 <- survival1(0, 19, 0.7656)
t_1.687 <- survival(1, 18, t_1.572)
t_1.725 <- survival(1, 17, t_1.687)
t_2.182 <- survival(1, 16, t_1.725)
t_2.201 <- survival(1, 15, t_2.182)
```

Using a log rank test we will compare the survival curves in the treatment group.

```{r}
survdiff(Surv(time,d)~treat, data = pbc)
```

Cumulative hazard fucntions in the two treatment groups. 

```{r}
pbc.km1 <- survfit(Surv(time,d)~1,data=subset(pbc,pbc$treat==1))
pbc.km2 <- survfit(Surv(time,d)~1,data=subset(pbc,pbc$treat==2))
cumhaz.1<-cumsum(pbc.km1$n.event/pbc.km1$n.risk)
cumhaz.2<-cumsum(pbc.km2$n.event/pbc.km2$n.risk)
plot(pbc.km1$time,cumhaz.1,type="s",col="red",xlab="Time",ylab="Cumulative hazard")
lines(pbc.km2$time,cumhaz.2,type="s",col="black")
```
```{r}
ggsurvplot(pbc.km,conf.int=T,mark.time=F,
xlab="Time", ylab="Survivor function",fun="cumhaz")
```
## Part B

```{r}
whl <- read_csv("../Practical datasets-20230224/whitehall.csv")\
head(whl)
```

Refit the data to calculate time 

```{r}
whl$atrisk1 <- whl$timeout - whl$timein
whl$atRisk2 <- whl$timeout - whl$timebth

whl.km1 <- survfit(Surv(atrisk1,chd)~sbpgrp, data = whl)
ggsurvplot(whl.km1, data = whl, conf.int = F, xlab ="Time")

whl.km2 <- survfit(Surv(atRisk2, chd)~sbpgrp, data = whl)
ggsurvplot(whl.km2, data = whl, conf.int = F, xlab ="Time")
```




