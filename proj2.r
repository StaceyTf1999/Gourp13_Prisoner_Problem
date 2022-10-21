# Group 13: Tongfei Li (s2328507),  Likang Xu (s2295871), Yifan Jin (s2434130)
# github: 
# brief description of what each team member contributed to the project, 
# and roughly what proportion of the work was undertaken by each team member

# Team member contributions:
# Likang Xu: Contributing to the main structure of the whole task. Undertaking most of 
# the coding as well as comments on question 5. 
# Tongfei Li: Completing the rest of the coding and improve some, making comments 
# on the 3 strategy and question 1, and advancing the process. 
# Yifan Jin: Participation in the discussion of the code framework, especially the 
# fourth and fifth question and comments on overall description and half of the questions.

# The whole structure would be displayed here first. Firstly, we analyse each strategy 
# and write 3 functions of which the parameters are n (the number of opened boxes), 
# card_num (the numbers on the cards in the first to the 2n boxes), Prisoner (the number
# of each prisoner). In the first two strategy, if the card number equals Prisoner,
# then the function will return true and the last observed card number would be 
# the box number next open. Otherwise, it will return false. As for the third strategy, 
# if prisoner’s number is not appeared in the cards number in the selected boxes, 
# then the function will return false.
# Then, we make a function: select.strategy() that can generate 3 different strategies.
# Specifically, when we input “1”, which means we would choose strategy one, 
# then only strategy_1() function would run. 
# After that, we Write a function Pone to estimate the probability of a single prisoner 
# succeeding in finding their number based on arguments n, k, strategy and nreps.
# Similarly,the function Pall is to estimate the probability of all prisoners finding their number, 
# which means they succeed in completing the task, using the same parameters as 
# Pone, except for k. After obtaining the two function above, we generate function success_prob, 
# which can show the individual and joint success probabilities under each strategy.
# Surprisingly, the joint success probability of strategy 1 is close to 30%, while
# that of the other two strategies is approximately 0.
# As for the function dloop which would estimate the occurrence probability of loop length 
# from 1 to 2n by simulation. To write dloop, we form an nreps×2n null matrix “a”. 
# for the j-th row in “a”, the i-th element will count the number of occurrences of loop 
# with length i in the j-th simulation. 
# Finally, we make a plot that shows probabilities of loop occurrence length from 1 to 100, 
# where x denotes the loop length and y denotes the occurrence probability. Additionally,
# the probability that there is no loop longer than 50 is also close to 30%. 






# First strategy: The prisoner first opened the box with his own number 
# written on it, and the number on the card in the box was k. If k is not their 
# prisoner number, they go to box k, open it and repeat the process until they 
# find a card with their number on it. But criminals can open at most n boxes.

# Define function for strategy 1:
# n: the number of opened boxes;
# card_num: represents the numbers on the cards in the first to the 2n boxes; 
# i.e., here card_num[1] represents the card number in the first box. 
# Prisoner: represents the prisoner's number.

# next_box is firstly equal to the prisoner's number since according to 
# strategy 1, the prisoner would open the box whose number is the same as their 
# number. Then, determine whether the number k in this box is equal to the 
# criminal's number --- if equal, then return T and break the i loop, which 
# means the prisoner successfully find the card with their own number; if not 
# equal, then open the next box which number is k, and determine whether the new # card number in box k is equal to the prisoner's number. Repeat this process 
# until opens all n boxes. If this prisoner could not find the card with their 
# number after opening n boxes, then return F which means this prisoner fail to 
# escape.


strategy_1 <- function(n, card_num, Prisoner){
  next_box <- Prisoner # The first opened boxed number is the prisoner number
  for (i in 1:n){
    if (card_num[next_box] == Prisoner){
      return(T) #if card number = Prisoner, return True
      break
    } else next_box = card_num[next_box] 
    #last observed card number would be the box number next open
  }
  return(F) #if could not find prisoner number after open n boxes, return False
}


# Second strategy: As strategy 1, but starting from a randomly selected box

# Here, the next_box starts from randomly selected a box. The sample() function 
# would help us to do this. 
strategy_2 <- function(n, card_num, Prisoner){
  # randomly select a box, the following is the same as strategy one
  next_box <- sample(c(1:(2*n)), size = 1)
  for (i in 1:n){
    if (card_num[next_box] == Prisoner){
      return(T)
      break
    } else next_box = card_num[next_box]
  }
  return(F)
}

# Third strategy: Randomly open n boxes.

# Here prisoner_select() give out the selected boxes number. When sampling the 
# number, replace = FALSE is needed since prisoner would not choose two same 
# boxes. If prisoner’s number is not appeared in the card_num[prisoner_select] 
# (all the cards number in the selected boxes), then return F since the prisoner 
# fails to escape. 
strategy_3 <- function(n, card_num, Prisoner){
  prisoner_select = sample(c(1:(2*n)), size = n, replace = FALSE)
  # whether he/she got his card
  if (Prisoner %in% card_num[prisoner_select]){
    return(T)
  }
  return(F)
}

# select.strategy() take strategy as input which can take 3 inputs (1, 2, 3) which 
# represent 3 different strategies. When input “1”, which means we would choose 
# strategy one, then only strategy_1() function would run. 
select.strategy <- function(strategy){
  if (strategy == 1){
    strategy = strategy_1
  } 
  else if (strategy == 2){
    strategy = strategy_2
  }
  else {
    strategy = strategy_3
  }
  return(strategy)
}

# Pone
# n: the number of opened boxes;
# k: the card number in the first box the prisoner opened; 
# strategy: which can take 3 inputs (1, 2, 3); 
# nreps: the number of replicate simulations; which means the prisoner would 
# enter the room nreps times. Each time, the cards in each box would be different, 
# so the prisoner would success or fail in each simulation. 
# 
# In the Pone function, we firstly use strategy = select.strategy(strategy) to 
# select one of the three strategies. Then define correct = 0 at first, which 
# would be the number of time that a prisoner success in nreps times simulation. 
# Then the j loop means each simulation from 1 to nreps. Each time, card_num 
# would be randomly sample given which the Prisoner would return the prisoner’s 
# number which is equal to the box number in which we firstly observe card with k 
# on it. If the strategy(n, card_num, Prisoner) give out a True, then correct 
# would add one. Otherwise, nothing would happen. Finally, return the correct
# /nreps would give us the final probability of successful simulations within the 
# nreps times simulation. 
Pone <- function(n, k, strategy, nreps){
  strategy = select.strategy(strategy)
  correct = 0
  for (j in 1:nreps){
    card_num <- sample(c(1:(2*n)), size = 2*n, replace = FALSE)
    Prisoner <- which(card_num == k)
    
    if (strategy(n, card_num, Prisoner)){
      correct = correct + 1
    }
  }
  return(correct/nreps)
}

# Pall
# Define the function Pall that estimates joint success probabilities:
# Given a strategy, we use sample() to randomly generate the card number in each box, 
# which is shown as a 2n vector below. Then judge whether a prison succeed 
# in a jth experiments. If he does not succeed, we will jump out of the loop 
# so that 'success' remains the same as the one in previous loop. Regard it 
# as a success if all the prisoners succeed in an experiment. Finally, 
# return a 2n array which is the joint success probabilities in nrep times experiments.


Pall <- function(n, strategy, nreps){
  strategy = select.strategy(strategy)
  correct = 0
  for (j in 1:nreps){
    card_num <- sample(c(1:(2*n)), size = 2*n, replace = FALSE)
    success = 0
    for (k in 1:(2*n)) {
      Prisoner <- which(card_num == k)
      if (strategy(n, card_num, Prisoner)){
        success = success + 1
      } else {break}
    }
    # success counts the number of success prisoners in the j experiment
    if (success == 2*n){
      correct = correct + 1
    }
  }
  return(correct/nreps)
}



# The function success_prob, of which parameters are n and nreps, demonstrates both 
# the individual and joint success probabilities under the 3 strategies above.

success_prob <- function(n, nreps) {
  cat('Simulated', {nreps}, 'times with', {n}, 'prisoners\n\n')
  for (strategy in 1:3) {
    cat('Strategy ', {strategy}, ':\n', sep = '')
    cat('P(individual success) = ', {Pone(n, 1, strategy, nreps)}, '\n' ,sep = '')
    cat('P(joint success) = ', {Pall(n, strategy, nreps)}, '\n\n' ,sep = '')
  }
}


# While using strategy 1, prisoners are selecting cards in a loop, which the 
# loops start from the card number of the k’s box (k is prisoner number), ends up 
# with card number k. Hence if all the loops in the 2n boxes length less than n, 
# all the prisoners will success. We constructed a function dloop(), to estimate 
# the occurrence probability of loop length from 1 to 2n by simulation. Here is 
# how it works: 
# dloop() has two parameters: “n” defines the number of prisoners given by 2, and 
# “nreps” implies the number of replicate simulations. The main idea is: firstly 
# form an nreps×2n null matrix “a”, for the j-th row in “a”, the i-th element 
# will count the number of occurrences of loop with length i in the j-th 
# simulation. 
# 
# To achieve the goal, in every simulation:
# 1)	using sample() to reshuffle the cards and open the first box, add the card 
# number to a vector called loop. 
# 2)	Then judge whether the card number equal to 1, if the answer is no, open 
# next box indexed by the card number, and add the new card number to the vector 
# loop. 
# 3)	Repeat section 2) until the card number in the final box is 1, if we got to 
# this point, count the occurrence the loop in “a”, ignore the boxes appeared in 
# the loop by setting the card number in loop to 0, select the first box with non
# -zero card number, and redefine the loop to be a null set. 
# 4)	Repeat sections 2) and 3) except in section 2) we firstly judge whether the 
# card number equal to the index of the first box with non-zero card number. This 
# section ends when all the card numbers are 0. Note that we put this judgement 
# at first in the if-else structure for efficiency. 
# 5)	Repeat section 1) to 4) nreps times then we got the needed matrix “a”.

# 6)	To return the occurrence probabilities, we constructed a function 
# prob_non_zero(), given the matrix “a”, it will generate a zero array with 
# length 2n (number of columns in “a”), then for each column in “a” (which counts 
# the appearances of each loop in every simulation), we calculate the number of 
# non-zero elements given by the number of rows, which gives the occurrence 
# probability of each loop, and write it into the 2n array, and return the final 
# array.

prob_non_zero <- function(a){
  non_zero = rep(0,ncol(a))
  for (k in 1:ncol(a)) {
    # the number of non-zero elements given by the number of rows
    non_zero[k] = (length(which(a[,k]!=0)))/nrow(a)
  }
  return(non_zero)
}

dloop <- function(n, nreps){
  # a can store the occurrence of loops
  a = array(0,dim=c(nreps,2*n))
  for (j in 1:nreps) {
    # reshuffle the cards
    card_num <- sample(c(1:(2*n)), size = 2*n, replace = FALSE)
    # open the first box
    next_box <- card_num[1]
    # add the card number to the loop
    loop <- c(next_box)
    # find all the loops
    for (i in 1:(2*n)) {
      # this actually happens only when we find all the loop(s)
      if (sum(card_num)==0){
        break
      }
      # which(card_num!=0)[1] means the index the first non-zero element
      else if (card_num[next_box] != which(card_num!=0)[1]){ 
        loop = append(loop, card_num[next_box])
        next_box = card_num[next_box]
      } 
      # if we got to this point, it means we find a loop
      else { 
        loop = append(loop, card_num[next_box])
        # count the occurrence the loop in “a”
        a[j,length(loop)] = a[j,length(loop)] + 1
        # setting the card number in loop to 0
        card_num[card_num %in% loop] = 0 
        # the first box with non-zero card number
        next_box <- which(card_num!=0)[1]
        # redefine the loop
        loop <- c()
      }
    }
  }
  return(prob_non_zero(a))
}


# Estimate the individual and joint success probabilities under each strategy 
# for n = 5 and for n = 50.

success_prob(5, 10000)
success_prob(50, 10000)

# write!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

print('Intuitively, as in question 3, the probability of each prisoner succeeds in 
      completing the challenge is 0.5. Given n = 50, as each prisoner’s choice 
      is independent, the probability of 100 prisoners succeeding at the same time 
      is 0.5^100, which is almost approximately zero! Once we  come up with the 
      right strategy, the probability of success will surprisingly increase to 30%.')


# Estimate the probability of each loop length from 1 to 2n when n = 50 and nreps = 10000. 

prob = dloop(50,10000)

# Draw the plot that shows probabilities of loop occurrence length from 1 to 2n (n = 50), 
# where x denotes the loop length and y denotes the occurrence probability.

barplot(prob, col='yellow', main='Probabilities of loop occurrence length
        from 1 to 2n (n = 50)',xlab='loop length (from 1 to 2n)',
        ylab='Occurrence probability',ylim=c(0,0.7))


# probability that there is no loop longer than 50 

cat('P(no loop longer than 50) = ', {1-sum(prob[51:100])}, '\n\n' ,sep = '')







