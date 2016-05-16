# Chapter6_part2_assignment
Stacey Harmer  
May 15, 2016  
Statistical Rethinking, Ch 6.3. and 6.4
6M1, 6M5, 6M6
6J1: explore how the code in Code Block 6.16 works.  Explain what is happening in each line.

##### 6M1. Write down and compare the definitions of AIC, DIC, and WAIC. Which of these criteria
##### is most general? Which assumptions are required to transform a more general criterion into a less
##### general one?

AIC is approximation of predictive accuracy, and is only reliable when: 
1.  priors flat, or ovewhelmed by likelihood
2. posterior distribution approximately multivariate Gaussian
3. same size N much greater than number of parameters k.

DIC = Deviance Information Criterion.  only 2 and 3 of the above are assumed.  

WAIC = Widely Applicable Information Criterion.  no assumptions about shape of posterior

The different assumptions are about the shape of the posterior distribution and whether priors flat.


#####  6M5. Provide an informal explanation of why informative priors reduce overfitting.

When you approach the problem with the assumption that you know something about the parameters,
this reduces the possibility that you'll get outputs that diverge widely from your understanding
of reality.

#####  6M6. Provide an informal (?) explanation of why overly informative priors result in underfitting.

If you assume you know too much about the paramaters, your model isn't free to really 
consider all the data in your dataset.

#####  6J1: explore how the code in Code Block 6.16 works.  Explain what is happening in each line.


```r
## first load data that will be needed to explain code block
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```
## rstan (Version 2.9.0-3, packaged: 2016-02-11 15:54:41 UTC, GitRev: 05c3d0058b6a)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## rstan_options(auto_write = TRUE)
## options(mc.cores = parallel::detectCores())
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.58)
```

```r
data(cars)

head(cars)
```

```
##   speed dist
## 1     4    2
## 2     4   10
## 3     7    4
## 4     7   22
## 5     8   16
## 6     9   10
```

```r
m <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,30)
  ) , data=cars )
post <- extract.samples(m,n=1000)

n_samples <- 1000   
#  set a range of sample numbers

# sapply - apply a function repeatedly across a dataframe or other dataset
ll <- sapply( 1:n_samples ,  # will apply the function across all 1000 samples in our range
              function(s) {  # s - will progressvely put numbers 1 to 1000 in this spot
                mu <- post$a[s] + post$b[s]*cars$speed 
                ## post is a dataframe, columns a, b, sigma.  samples from posterior distribution of model fit to cars
                ## mu is a model that is being fit for each row found in object post
                ## cars$speed are real data
                dnorm( cars$dist , mu , post$sigma[s] , log=TRUE )
                ## and here its making a normal distribution using informaiton generated as above
                # mu is 1000 rows while cars is only 50.  but works (?)
              } )
```
