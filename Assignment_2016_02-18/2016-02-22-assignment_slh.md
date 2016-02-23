---
output:
  html_document:
    keep_md: yes
---
# Statistical Rethinking Chapter 2

Name: Stacey Harmer

## 2E1:   2) (Pr(rain±Monday)  (note that my pipes key is broken)


## 2E2:    the probability that it is Monday given that it is raining

## 2E3: 1)  Pr(Monday±rain) 

## 2E4: In a literal sense, the phrase 'the probability of water is 0.7' is meaningless. In the globe example in the text, my finger would or would not rest on water - my data points are binary (0 and 1), not fractions. 

#### now, finish medium (m1, m2, m5, m6, m7) abd try hard problems

### 2M1: Compute and plot the grid approximate posterior distribution for the below, assuming uniform prior

1) W W W 

# define grid; only 3 data points
p_grid <- seq( from=0 , to=1 , length.out=3 ) 
# define prior; this is uniform (3 points, all = 1)
prior <- rep( 1 , 3 )
# compute likelihood at each value in grid; 3 W in 3 tosses
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab="probability of water", ylab= "posterior probability")
mtext("3 points")

2) W W W L
# define grid; only 4 data points
p_grid <- seq( from=0 , to=1 , length.out=4 ) 
# define prior; this is uniform (3 points, all = 1)
prior <- rep( 1 , 4 )
# compute likelihood at each value in grid; 3 W in 3 tosses
likelihood <- dbinom( 3 , size=4 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab="probability of water", ylab= "posterior probability")
mtext("4 points")

3) L W W L W W W 
# define grid; only 7 data points
p_grid <- seq( from=0 , to=1 , length.out=7 ) 
# define prior; this is uniform (3 points, all = 1)
prior <- rep( 1 , 7 )
# compute likelihood at each value in grid; 3 W in 3 tosses
likelihood <- dbinom( 5 , size=7 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab="probability of water", ylab= "posterior probability")
mtext("7 points")


### 2M2: Now assume a prior for p that is equal to zero when p < 0:5 and is a positive constant when p > or = 0:5. Again compute and plot the grid approximate posterior distribution for each of the sets of observations in the problem just above.

prior <- ifelse(p_grid <0.5, 0, 1)

1) W W W 

# define grid; only 3 data points
p_grid <- seq( from=0 , to=1 , length.out=3 ) 
# define prior; this is uniform (3 points, all = 1)
prior <- ifelse(p_grid <0.5, 0, 1)
# compute likelihood at each value in grid; 3 W in 3 tosses
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab="probability of water", ylab= "posterior probability")
mtext("3 points")

2) W W W L
# define grid; only 4 data points
p_grid <- seq( from=0 , to=1 , length.out=4 ) 
# define prior; this is uniform (3 points, all = 1)
prior <- ifelse(p_grid <0.5, 0, 1)
# compute likelihood at each value in grid; 3 W in 3 tosses
likelihood <- dbinom( 3 , size=4 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab="probability of water", ylab= "posterior probability")
mtext("4 points")

3) L W W L W W W 
# define grid; only 7 data points
p_grid <- seq( from=0 , to=1 , length.out=7 ) 
# define prior; this is uniform (3 points, all = 1)
prior <- ifelse(p_grid <0.5, 0, 1)
# compute likelihood at each value in grid; 3 W in 3 tosses
likelihood <- dbinom( 5 , size=7 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab="probability of water", ylab= "posterior probability")
mtext("7 points")


## 2M3:  globes: earth and mars.  pw for earth = 07; pw for mars = 0. one toss of the globe = L observation.  what is the posterior probabilty that the globe tossed was the earth? that is, solve for Pr(earth±land) 
#(answer is 0.23)

#Pr(p±w) = probability earth, given land.  so p = earth, w = land
#posterior = (0.3 * 0.5)/((1 + .3)/2) = 0.23

## 2M4:  6 possible sides of cards, 3 white and 3 black.  So the first ring of the garden has 6 paths, that lead to:  B  B  B  W W W.    Two of these black sides have black on the other face of the card (that is, they are the two sides of hte same card).  The third black card has white on the other side.  So the second ring of the garden is:  B  B  W  B W  W.    Thus 2 of the 3 choices  leading from the first side = black yield second side also = black.

Sharon had nice solution.  ways double black card can produce a black side = 2
prior prob that double black card chosen = 1/3  so prob that 2nd side black if 1st side black = 2 * 1/3 = 2/3

## is this smaller
# and this bigger?
#### and this even smaller?

Emily points out that Github doesn't like .Rmd files.  
Could upload both .Rmd and .md versions of the homework.


### 2M5:Now suppose there are four cards: B/B, B/W, W/W, and another B/B. Again suppose a card is drawn from the bag and a black side appears face up. Again calculate the probability that the other side is black.

8 card sides - 5 of 8 are black.  1st ring of garden: 5 paths lead to B.  Of these 5 paths, 4 will lead to a 2nd side being black.  so if 1st side is black, the probability is 4/5 that the second side is also black.

### 2M6:  3 cards (B/B, B/W, and W/W), but not equal likelihood of pulling each one out.   for each time you choose B/B, you'd chose 2 B/W and 3 W/W.  Now, assume you pull out a card and find the first side is B.  the chance of hte second side being B is now 0.5.  show this.

Garden:  I'll approximate the differences in likelihood of choosing cards by changing the effective number of cards in the mix.  
1 B/B, 2 B/W, and 3 W/W cards.  So 12 sides total: 8 white, 4 black.
1st ring of garden: if 1st side is B, could either be from B/B, B/W, or B/W (4 sides). of these 4 choices, 2 will be B on the second side (original card was B/B) and two will be W on the second side.  So overall probability = 2/4 = 0.5

### 2M7:  3 cards (B/B, B/W, and W/W).  You draw first card, find upwards side is B. You draw second card, find upwards side is W.  The probability that the back of the first card is also B is now 0.75.  Show this. (Hint: Treat this like the sequence of globe tosses, counting all the ways to see each observation, for each possible first card.)

The first card must be B/B or B/W.  4 sides, 3/4 ways to be B on first side.
The second card cannot be B/B; it must be W/W or B/W.  4 sides, 3/4 ways to be W on first side.  the probability that this card is W/B (independnet of card 1) = 1/3; chance that it is W/W is 2/3
The third card might be W/W, B/W, or B/B.

SO considering only cards 1 and 2, they could be:  card 1: B/B or W/B; and card 2: W/W or B/W.   
All possible ways to get the observed order:
 card 1       card 2        card 3
1) B/B  (x2)      W/W        B/W
2) B/B  (x2)      B/W        W/W
3) B/W  (x1)      W/W        B/B

So it seems like there should be a 5/6 chance of the second side of the first card being 'B'  (?)
## ASK Julin about this.

### 2H1:  2 species of panda bear; equally common.  Species A: twins 10% of time; species B has twins 20% of the time.  Unknown female had twins.  what is probability next birth will be twins?
I can guess that the answer must be between 10 and 20%, and will likely be closer to the latter.

species: A and B.  pt for A = .1; pt for B = .2. one birth  = T observation.  what is the posterior probabilty that species was B? that is, solve for Pr(B ± twins) 
equal likelihood mother was A or B

Pr(p±t) = probability B, given twins.  so p = earth, w = land
posterior = (0.2 * 0.5)/((.1 + .2)/2) = 0.66 probability that the mom was species B

So I guess that likelihood that the next birth will be twins would be:
.66 * .2 =  13% likelihood  (that result seems wrong to me)



