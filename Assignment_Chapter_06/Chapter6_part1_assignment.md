# Ch6_part1
Stacey Harmer  
May 8, 2016  

Read sections 6.1 and 6.2
Problems 6E1, 6E2, 6E3, 6E4


6E1. State the three motivating criteria that define information entropy. Try to express each in your
own words.

###### 1.  measure of uncertainty should be continuous (avoid large step-wise changes in estimate)
###### 2.  the greater the number of possible events, the larger your uncertainty should be
###### 3.  to describe events that can be combined, add the respective measures of uncertainty


6E2. Suppose a coin is weighted such that, when it is tossed and lands on a table, it comes up heads
70% of the time. What is the entropy of this coin?


```r
## probabilty of head is 0.7, of tail is 0.3
prob <- c(0.7, 0.3)
-sum(prob*log(prob))
```

```
## [1] 0.6108643
```

6E3. Suppose a four-sided die is loaded such that, when tossed onto a table, it shows “1” 20%, “2”
25%, ”3” 25%, and ”4” 30% of the time. What is the entropy of this die?


```r
prob.4die <- c(0.2, 0.25, 0.25, 0.3)
-sum(prob.4die*log(prob.4die))
```

```
## [1] 1.376227
```


6E4. Suppose another four-sided die is loaded such that it never shows “4”. The other three sides
show equally often. What is the entropy of this die?


```r
prob.4die.2 <- c(0.333, 0.333, 0.333)
-sum(prob.4die.2*log(prob.4die.2))
```

```
## [1] 1.098513
```
