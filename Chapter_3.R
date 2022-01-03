

library(rethinking)

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )


PI( samples , prob=0.5 )

HPDI( samples , prob=0.5 )

p_grid[ which.max(posterior) ]

chainmode( samples , adj=0.01 )

mean( samples )
median( samples )

sum( posterior*abs( 0.5 - p_grid ) )