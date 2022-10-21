# Group 13: Likang Xu (s2295871), Tongfei Li (s2328507), Yifan Jin (s2434130)

# Github: https://github.com/StaceyTf1999/Group13_Prisoner_Problem.git

# Team member contributions:
# Likang Xu: Contributing to the main structure of the code
# Tongfei Li: Contributing to the rest of the code, and modified
# Yifan Jin: Giving insights to the code framework, and wrote comments
# Everyone contributed roughly equally


# Three strategies were tested by simulation to solve the prisoner puzzle. 
### First strategy: The prisoner first opened the box with his own number 
# written on it, and the number on the card in the box was k. If k is not their 
# prisoner number, they go to box k, open it and repeat the process until they 
# find a card with their number on it. But criminals can open at most n boxes.
### Second strategy: As strategy 1, but starting from a randomly selected box.
### Third strategy: Randomly open n boxes.

# Firstly, we analysed each strategy and write 3 functions (strategy_1(), 
# strategy_2(), strategy_3()). For each strategy, if the prisoner managed to find
# his number, the function will return true otherwise false. Then, we made a 
# function: select.strategy() select between the three strategies given an input
# in (1, 2, 3). Specifically, when we input “1”, which means we would choose 
# strategy one.

# After this, we wrote a function Pone() to estimate the probability of a single 
# prisoner succeeding in finding their number. Similarly, the function Pall() is
# to estimate the probability of all prisoners finding their number, which means
# they succeed in completing the task. After obtaining the two functions above, 
# we generated function success_prob(), which can show the individual and joint
# success probabilities under each strategy. 

# While using strategy 1, prisoners are selecting cards in a loop, which the 
# loops start from the card number of the k’s box (k is prisoner number), ends up 
# with card number k. Hence if all the loops in the 2n boxes length less than n, 
# all the prisoners will success. The function dloop() which would estimate the
# occurrence probabilities of loops length from 1 to 2n by simulation. 

# We then used the success_prob() function to estimate the individual and joint
# success probabilities under each strategy. Surprisingly, the joint success 
# probability of strategy 1 is close to 30%, while that of the other two strategies
# is approximately 0. 

# Next, we make a plot that shows probabilities of loop occurrence length from 1
# to 100 by 10000 times of simulation, where x denotes the loop length and y denotes
# the occurrence probability. Additionally, the probability that there is no loop
# longer than 50 is also close to 30%. 


# the arguments are the same for strategy 1, 2, 3
# n: the number of opened boxes;
# card_num (array) is the card numbers in the boxes; 
# Prisoner represents the prisoner's number.

# strategy 1
# For i in 1:n, the prisoner would open the box whose number is the same as their
# number. Then, determine whether the number k in this box is equal to the
# criminal's number --- if equal, then return T and break the i-th loop, which
# means the prisoner success; if not equal, then open the next box which number
# is k, and determine whether the prisoner find his number. Repeat this process
# until the prisoner success or opens all n boxes and fails (return F)
strategy_1 <- function(n, card_num, Prisoner){
  next_box <- Prisoner # The first opened boxed number is the prisoner number
  for (i in 1:n){
    if (card_num[next_box] == Prisoner){
      return(T) #if card number = Prisoner, return True
      break
    } else next_box = card_num[next_box] 
    #last observed card number would be the box number opens next
  }
  return(F) #if could not find prisoner number after open n boxes, return False
}


# strategy 2
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


# strategy 3
# Here prisoner_select() give out the n randomly selected boxes. If prisoner’s
# number appeared in the card_num[prisoner_select] (all the cards number in the
# selected boxes), then return T, otherwise return F.
strategy_3 <- function(n, card_num, Prisoner){
  # replace = FALSE is needed since one cannot choose two same boxes
  prisoner_select = sample(c(1:(2*n)), size = n, replace = FALSE)
  # whether he/she got his card
  if (Prisoner %in% card_num[prisoner_select]){
    return(T)
  }
  return(F)
}


# select.strategy() take strategy as input which can take inputs 1, 2, 3, which 
# represent 3 different strategies. When input “1”, which means we would choose 
# strategy one, then strategy_1() function would be selected. 
select.strategy <- function(strategy){
  if (strategy == 1){
    strategy = strategy_1
  } 
  else if (strategy == 2){
    strategy = strategy_2
  }
  else if (strategy == 3){ 
    strategy = strategy_3
  }
  return(strategy)
}


# Pone
# n: the number of opened boxes;
# k: the card number in the first box the prisoner opened; 
# strategy: which can take 3 inputs (1, 2, 3); 
# nreps: the number of replicate simulations
# Firstly using select.strategy(strategy) to select one of the three strategies.
# Then define correct = 0 to count the number prisoner successes in nreps times 
# of simulation. Each time, card_num would be randomly sample, Prisoner would 
# return the prisoner’s number given k. If the strategy(n, card_num, Prisoner) 
# give out a True, then correct would add one. Finally, return the correct/nreps
# as the probability of success. 
Pone <- function(n, k, strategy, nreps){
  # select one strategy
  strategy = select.strategy(strategy)
  # used for count the number of prisoner success in nreps times
  correct = 0
  # simulate nreps times
  for (j in 1:nreps){
    # shuffle the boxes
    card_num <- sample(c(1:(2*n)), size = 2*n, replace = FALSE)
    # find prisoner number given k
    Prisoner <- which(card_num == k)
    # count if the prisoner success
    if (strategy(n, card_num, Prisoner)){
      correct = correct + 1
    }
  }
  # return the probability of success
  return(correct/nreps)
}


# Pall
# the arguments are the same as Pone() except k
# Given a strategy, in the j-th simulation, we use sample() to randomly generate
# the card number in each box. Then using “success” to count the times when a 
# prisoner wins the game, if one fails, move to next simulation. If all prisoners
# find their number (success==2*n), counts using “correct”. Finally, the function
# would return the probability of joint success (correct/nreps).
Pall <- function(n, strategy, nreps){
  #  select one strategy
  strategy = select.strategy(strategy)
  # used for count the times when prisoner win the game
  correct = 0
  # simulate nreps times
  for (j in 1:nreps){
    # shuffle the boxes
    card_num <- sample(c(1:(2*n)), size = 2*n, replace = FALSE)
    # counts the number of success prisoners in the j experiment
    success = 0
    for (k in 1:(2*n)) {
      Prisoner <- which(card_num == k) # find prisoner number
      # count the number of success prisoners
      if (strategy(n, card_num, Prisoner)){
        success = success + 1
      } else {break} # if one prisoner fails, all would fail
    }
    # if all prisoner find their card, count
    if (success == 2*n){
      correct = correct + 1
    }
  }
  # return the probability of joint success
  return(correct/nreps)
}


# The function success_prob() would print the individual and joint success 
# probabilities under the 3 strategies given n, nreps defined above. 
success_prob <- function(n, nreps) {
  cat('Simulated', {nreps}, 'times with', {2*n}, 'prisoners\n\n')
  for (strategy in 1:3) {
    cat('Strategy ', {strategy}, ':\n', sep = '')
    cat('P(individual success) = ', {Pone(n, 1, strategy, nreps)}, '\n' ,sep = '')
    cat('P(joint success) = ', {Pall(n, strategy, nreps)}, '\n\n' ,sep = '')
  }
}


# Estimate the occurrence probabilities of loops length from 1 to 2n by simulation
# The main idea is: form an nreps×2n matrix “a”, for the j-th row in “a”, the 
# i-th element will count the number of occurrences of loop with length i in the 
# j-th simulation. Then using the function prob_non_zero() to calculate the 
# occurrence probability of each loop.


# prob_non_zero
# given the matrix “a”, it will generate a zero array with length the number of 
# columns in “a”, then for each column in “a” (which counts the appearances of each
# loop in every simulation), we calculate the number of non-zero elements (means
# the loop occurs at least once) given by the number of rows, which gives the 
# occurrence probability of each loop, and write it into the 2n array, and return
# the final array.
prob_non_zero <- function(a){
  non_zero = rep(0,ncol(a)) # to store the probabilities
  # for each column in a
  for (k in 1:ncol(a)) {
    # the number of non-zero elements given by the number of rows
    non_zero[k] = (length(which(a[,k]!=0)))/nrow(a)
  }
  return(non_zero)
}


# dloop() (the two arguments, n, nreps defined above)
# To find the matrix "a", in every simulation:
# 1)	using sample() to shuffle the cards and open the first box, add the card 
# number to a vector called loop. 
# 2)	Then judge whether the card number equal to 1, if the answer is no, open 
# next box indexed by the card number, and add the new card number to the vector 
# loop. 
# 3)	Repeat section 2) until the card number in the final box is 1, if we got to 
# this point, count the occurrence of the loop in “a”, ignore the boxes appeared in 
# the loop by setting the card number in loop to 0, select the first box with non
# -zero card number, and redefine the loop to be a null set. 
# 4)	Repeat sections 2) and 3) except in section 2) we firstly judge whether the 
# card number equal to the index of the first box with non-zero card number. This 
# section ends when all the card numbers are 0. Note that we put this judgement 
# at first in the if-else structure for efficiency. 

# 5)	Repeat section 1) to 4) nreps times then we got the needed matrix “a”.
# 6)	Using prob_non_zero() to return the occurrence probability of each loop
dloop <- function(n, nreps){
  # a can store the occurrence of loops
  a = array(0,dim=c(nreps,2*n))
  for (j in 1:nreps) {
    # shuffle the cards
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
        # add the card number to the loop
        loop = append(loop, card_num[next_box])
        # open next box
        next_box = card_num[next_box]
      } 
      # if we got to this point, it means we find a loop
      else { 
        loop = append(loop, card_num[next_box])
        # count the occurrence the loop in “a”
        a[j,length(loop)] = a[j,length(loop)] + 1
        # setting the card number in loop to 0
        card_num[card_num %in% loop] = 0 
        # open the first box with non-zero card number
        next_box <- which(card_num!=0)[1]
        # redefine the loop
        loop <- c()
      }
    }
  }
  return(prob_non_zero(a))
}


# Estimate the individual and joint success probabilities under each strategy 
# for n = 5 and for n = 50, and nreps = 10000.
success_prob(5, 10000)
success_prob(50, 10000)
# Intuitively, the probability of each prisoner succeeds in completing the 
# challenge is 0.5. Given n = 50, as each prisoner’s choice is independent, the 
# probability of 100 prisoners succeeding at the same time is 0.5^100, which is
# almost approximately zero! Once we come up with the right strategy, the 
# probability of success will surprisingly increase to 30%.


# Estimate the probability of loop length from 1 to 2n when n = 50, nreps = 10000. 
prob = dloop(50,10000)


# Draw the plot that shows probabilities of loop occurrence length from 1 to 2n, 
# where x denotes the loop length and y denotes the occurrence probability.
barplot(prob, col='yellow', main='Probabilities of loop occurrence length
        from 1 to 2n (n = 50)',xlab='loop length (from 1 to 2n)',
        ylab='Occurrence probability',ylim=c(0,0.7))


# probability that there is no loop longer than 50 
cat('P(no loop longer than 50) = ', {1-sum(prob[51:100])}, '\n\n' ,sep = '')







