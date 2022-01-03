

#Chapter 4 walkthrough



#setion 4.1.1 - Normal by addition

#simulating the coin toss and step example for explaining the normal distribution.

#16 coin flips, steps between -1 and 1, 1000 people doing it. Always normal.
pos <- replicate(1000, sum(runif(16, -1,1)))

hist(pos)


#section 4.1.2 - Normaly by multiplication

#looking at how multiplicative effects rather than additive effects 


#example here is multiplicative effects of genes on growth rate. So we sample 12 growth rates from 1 to 1.1, where 1 
#indictes no growth and 1.1 indicates a 10% increase. We then multiply these loci together. We replicate 10000 times and
#plot, and find rates are roughly nomrally distributed.
growth <- replicate(10000, prod(1+runif(12,0,0.1)))
dens(growth, norm.comp = T)


#now let's test with larger effects.

big <- replicate(10000, prod(1+runif(12, 0,0.5)))
dens(big, norm.comp = T)

#AHHH so it's not normally distributed now punk! This makes sense though, based on McElreath's explanation. We see that
#since effects are larger now, multiplying them deviates from an additive process much more strongly. Thus we lose the
#normally distributed pattern. But I fucking bet if we log transform this shit it's normal again ayyy??


#Section 4.1.3





