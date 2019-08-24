library(data.frame)
#test
library(ggplot2)

permute <- function(vec){ #permuting function
  combn.win = expand.grid(1:2, 1:(length(vec)/2))
  smp = sample(vec, length(vec))
  mat_n  = t(matrix(smp, nrow=2, ncol=length(vec)/2))
  return(mat_n) 
}

iter_num = 100
dat_tbl = data.frame(outcome=integer(), Freq=integer())
for (i in 1:iter_num){
outcome = c()  
n_trials = 1
    while (n_trials <= 100) {
    mat_n <- matrix(c(1:8,16:9), ncol=2)
      while (nrow(mat_n) >= 1){  
          Pr = c()
          win = c() 
          for ( i in 1:nrow(mat_n)){
          # i = 1
          x = mat_n[i,1]
          y = mat_n[i,2]
          Pr[i] = 0.55 + 0.02*(x-y)
          win[i] = ifelse(rbinom(1,1,Pr[i])==1, mat_n[i,1], mat_n[i,2])
          }
          
          mat_n = permute(win)
          
      } 
    outcome[n_trials] = win
    n_trials =   n_trials + 1
      
    }
dat_tbl <- rbind(dat_tbl, as.data.frame(table(outcome)))

}  
dat_tbl <- as.data.table(dat_tbl)
ggplot(dat_tbl, aes(Freq, color=outcome)) +
  geom_density()


