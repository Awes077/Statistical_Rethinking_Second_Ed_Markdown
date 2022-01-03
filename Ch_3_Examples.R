library(rethinking)

P_grid <- seq(from=0, to=1, length=1000) #testing 1000 values from 0 1
prior <- rep(1, 1000) # all values have equal prior, so uniform prior
likelihood <- dbinom(6, size=9, prob=P_grid) 
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)

#sample the posterior here
samples <- sample(P_grid, prob=posterior, size=1e4, replace=T)
plot(samples)
dens(samples)

#now we want to look at the cumulative posterior probability for values below 0.5. So basically what is the posterior
#probability of the globe having less than half water (more or less)

#straight from our grid approximation of the posterior
sum(posterior[P_grid<0.5])

#more general approach where we summarize our samples from the posterior - equivalent to asking questions about results of
#a Markov chain etc.

sum(samples<0.5)/1e4

#now let's look between 0.5 and 0.75 - so posterior prob over a defined interval

sum(samples>0.5 &samples<0.75)/1e4

#now let's say we want the bottom 80th percentile, so what is the upper boundary on the lower 80% of our sample

quantile(samples, 0.8)


#now do the same thing, but identify the INTERVAL that contains 80% of our probability mass. So looking for an upper
#and lower bound on the interval that contains 80% of our samples

quantile(samples, c(0.1,0.9))

#here we look at the central 50% CI for the parameter value. The issue here is we assume centralty - which is to say
# we assume each tail is less informative. When our true parameter value is close to a min or max, or if we just have
#a strong skew in general, then the central CI will deviate strongly from the best.
PI(samples, 0.5)


#The highest posterior density interval addresses this issue. Here we look for the narrowest interval that still gives
#us a posterior density of 50% - so we want to maximize the density within in interval. In this case we get similar
#answers but if we were to follow the example in the book itself, we'd see that when p is closer to 1, then the PI
#above is quite wrong, while this would get us much closer to the true value for p. 
HPDI(samples,0.5)



#globe tossing limited data model

p_grid <- seq(from=0 , to=1 , length.out=1000)

prior <- rep(1,1000)

likelihood <- dbinom( 3 , size=3 , prob=p_grid )

posterior <- likelihood * prior

posterior <- posterior / sum(posterior)

samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )

#here we apply the absolute value of the distance between our point estimate (d, or decision) and the entirety of the
#posterior. By using the absolute value, rather than the quadratic distance (squared error), we end up with the 
#median of the posterior as our point estimate. If we used the quadratic distance, we'd get the mean. thus different
#loss functions lead to different point estimate decisions. But the one we settle on may differ depending on context.
#E.g. in the book, mcelreath uses hurricane wind speed and evacuation orders as an example where we would be more
#concerned with underpredicting wind speed and thus when our prediction is below the true speed we increase 'loss' or
#'cost' much faster. When overpredicting, where actual wind speed is lower than our model output, we have slower loss
#'#as this is less dangerous overall.

loss <- sapply( p_grid , function(d) sum( posterior*abs( d - p_grid ) ) )

p_grid[which.min(loss)]

#Now talking about using models to simulate data. Start with general simulation of a binomial process

#so in our globe tossing example, if we fix p @ 0.7, do two tosses, we get two waters (at least when I first ran it).
#try it again and this changes. But v low probability of getting 0 water.
rbinom( 1 , size=2 , prob=0.7 )


#now let's simulate more than 2 tosses, we'll do 9 tosses, 10,000 times, and plot the number of times we got water

dummy_w <- rbinom( 1e5 , size=9 , prob=0.7 ) 

simplehist(dummy_w , xlab= "dummy water count")



#An interesting point at the end of this section about INDEPENDENCE of samples. In the globe tossing example, its a question
#of how much do we really rotate the globe when we toss it? does it spin and rotate or just spin? Since water and land
#are spatially distributed and non-uniform, when we toss from water we may end up on water again simply because of how
#the toss itself was made. In the same way, we may switch from land to water and back to land becasue of the toss. 
#In essense, it's similar to spatial autocorrelation in ecological systems. he lays out a simple way to check if our
#data may be biased in this way. Simulate from a binomial (since this is a binomial process) and see how frequently in
#10,000 samples of n=9 do we get runs of water followed by water or land followed by water followed by land? In his example
#which I don't think there's code in the book for but there is in the notebooks available for on git or wherever, he
#finds that while all-water runs are similar b/w truly independent simulations and observations, the other run is not.
#this susggests non-independence of the data, and thus that our predictions from a binomial model may be off because of
#underlying spatial nonindependence. Wild.






#PRACTICE QS AND MODEL

p_grid <- seq( from=0 , to=1 , length.out=1000)

prior < - rep( 1 , 1000)

likelihood <- dbinom( 6 , size=9 , prob=p_grid )

posterior <- likelihood * prior

posterior <- posterior / sum(posterior)

set.seed(100)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )




#how much post prob lies below 0.2 DIVIDE BY SAMPLE SIZE DUH
sum(samples<0.2)/1e4

#how much post prob lies above 0.8
sum(samples>0.8)/1e4

sum(samples>0.2 & samples<0.8)/1e4


quantile(samples, 0.2)
quantile(samples, 0.8)

HPDI(samples, 0.66)
PI(samples,0.66)



#new model with new output - 15 tosses with 8 dubs

p_grid <- seq(from=0, to=1, length.out=1000)
prior <- c(rep(0,500), rep(1, 500))
likelihood <- dbinom(8, 15, prob=p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)

samples <- sample(p_grid, prob=posterior, size=1e4, replace=T)

HPDI(samples, 0.9)

#now I need to a posterior predictive check but no idea how to go about that

w <- rbinom(1e4, size =15, prob=samples)
simplehist(w)


sum(w==8)/1e4


w_69_nice <- rbinom(1e4, size=9, prob=samples)
sum(w_69_nice==6)/1e4

# HARD PROBLEMS - Build on frequency of male vs female in first and second births

data(homeworkch3)

#Generate our grid approximation of the posterior
p_grid <- seq(from=0, to =1, length.out=1000)
prior <- rep(1,1000)
boys_total <- sum(birth1+birth2)
likelihood <- dbinom(boys_total, 200,prob = p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)

#figure out which parameter value for p maximizes our posterior probability

plot(posterior~p_grid)

#So about 0.55?
p_grid[which.max(posterior)]

#Now we want to sample the posterior (much more realistic in most cases) and then find pHPDIs for
#the 50%, 89% and 97%.

samples <- sample(p_grid, prob = posterior, 1e4, replace=T)

HPDI(samples, 0.5)
HPDI(samples, 0.89)
HPDI(samples, 0.97)


#Now we want to compare our data to the frequncy of boys in 10,000 samples of 200 births. So here we are really
#testing the assumption of whether or not sex and birth order are independent I think.

tot_sims <- rbinom(1e4, 200, prob=samples)
dens(sims)

first_sims <- rbinom(1e4, 100, prob=samples)
sum(birth1)


#okay now we want to see if we get better prediction by stratifying based on birth order. So do males tend to follow a
#female? Something along those lines. So we pull only situations where a boy followed a girl.


first_girl_indices <- which(birth1==0)

second_boy_indices <- which(birth2[first_girl_indices]==1)
length(second_boy_indices)


second_son_sims <- rbinom(1e4, 39, prob=samples)
dens(sims)

#okay so what we see is that while it does kind of predict well for the TOTAL data, it doesn't predict well for our
#subsamples. Birth1 has fewer boys than predicted by 10,000 simulations, by a good amount. There are many more second boys
#when controlling for birth order than would be predicted by our pooled data model. So I think I get some idea now of
#how we would stratify to fit different models to each group, on some level. Right?



