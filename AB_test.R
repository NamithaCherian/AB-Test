# Variables that fully define A/B test results

t <- 20 #Total number of subjects, i.e a+b
a <- 10 #Number of subjects in group A
b <- 10 #Number of subjects in group B
a_yes <- 7 #yes count in group A
b_yes <- 4 #yes count in group B

#Computing A/B test results

t_yes <- a_yes +  b_yes #Total number of YES
t_no <- t - t_yes       #Total number of NO
a_yes_pc <- (a_yes / a)*100 #Percentage of YES in geoup A
b_yes_pc <- (b_yes / b)*100 #Percentage of YES in geoup B

#Experiment Statistic- Percentage difference on YES in A and B; shows how different the tep groups are

ab_yes_pc <- a_yes_pc - b_yes_pc

cat('Observed YES percentage : A:', a_yes_pc, ' B:', b_yes_pc, ' A-B:', ab_yes_pc, 
    '\nTotal Counts: YES:', t_yes, ' NO:',t_no)

set.seed(0)
bag=c(rep(1,t_yes), rep(0,t_no)) #Adding all results into one bad

p=100
perm_result <- rep(0,p)

for (i in 1:p){
  bag <- sample(bag)
  a_res <- bag[1:a]
  b_res <- bag[(a+1) : (a+b)]
  ab_res <-  100*(sum(a_res)/a) - 100*(sum(b_res)/b)
  perm_result[i] <- ab_res
  
}

perm_result
table(perm_result)
hist(perm_result, main='Density distribution of Null Hypothesis', xlab='Difference between A and B', ylab='Density') #Visualisation
abline(v=c(30, -30), col='blue')

#Number of time the Null Hypothesis produces results more extreme than the experiment
extreme_count =   sum(abs(perm_result) >= ab_yes_pc)
  
#Probability of Null Hypothesis produces results more extreme than the experiment
extreme_ratio = extreme_count/p

cat(
  'Number of Permutations: ', p,
  '\nNo of times Null Hypothesis produces results more extreme: ', extreme_count,
  '\np- value:', extreme_ratio
)

#As the p-value is 0.35 we can conclude that the results of the A/B test is not statistically significant






