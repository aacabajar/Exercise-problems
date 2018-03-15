
set = c(2,4,4,9)

iter_num = 1000
ave_mean = c()
for ( i in 1:iter_num){
  
  ave_mean[i] = mean(sample(set, 4, replace=TRUE))
}

stat_density(aes(ave_mean))
qplot(ave_mean, geom="density")
