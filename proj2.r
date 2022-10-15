# Group 13: Tongfei Li (s2328507),  Likang Xu (s2295871), Yifan Jin (s2434130)
# github: 
# brief description of what each team member contributed to the project, 
# and roughly what proportion of the work was undertaken by each team member
#

strategy_1 <- function(n, card_num, Prisoner){
  next_box <- Prisoner
  for (i in 1:n){
    if (card_num[next_box] == Prisoner){
      return(T)
      break
    } else next_box = card_num[next_box]
  }
  return(F)
}


strategy_2 <- function(n, card_num, Prisoner){
  # randomly select a box
  next_box <- sample(c(1:(2*n)), size = 1)
  for (i in 1:n){
    if (card_num[next_box] == Prisoner){
      return(T)
      break
    } else next_box = card_num[next_box]
  }
  return(F)
}

strategy_3 <- function(n, card_num, Prisoner){
  prisoner_select = sample(c(1:(2*n)), size = n, replace = FALSE)
  # whether he/she got his card
  if (Prisoner %in% card_num[prisoner_select]){
    return(T)
  }
  return(F)
}

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


#

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



# 

success_prob <- function(n, nreps) {
  cat('Simulated', {nreps}, 'times with', {n}, 'prisoners\n\n')
  for (strategy in 1:3) {
    cat('Strategy ', {strategy}, ':\n', sep = '')
    cat('P(individual success) = ', {Pone(n, 1, strategy, nreps)}, '\n' ,sep = '')
    cat('P(joint success) = ', {Pall(n, strategy, nreps)}, '\n\n' ,sep = '')
  }
}


# Likang write this part





prob_non_zero <- function(a){
  non_zero = rep(0,ncol(a))
  for (k in 1:ncol(a)) {
    non_zero[k] = (length(which(a[,k]!=0)))/nrow(a)
  }
  return(non_zero)
}




dloop <- function(n, nreps){
  # explain what is a 
  a = array(0,dim=c(nreps,2*n))
  for (j in 1:nreps) {
    card_num <- sample(c(1:(2*n)), size = 2*n, replace = FALSE)
    next_box <- card_num[1]
    loop <- c(next_box)
    for (i in 1:(2*n)) {
      # this actually happens only when we find all the loop(s), but we put this at the first for efficiency
      if (sum(card_num)==0){
        break
      }
      else if (card_num[next_box] != which(card_num!=0)[1]){ # card_num[which(card_num!=0)[1] means the index the first non-zero element
        # can't use loop[i+1] = '', since in this cycle, i will increase
        loop = append(loop, card_num[next_box])
        next_box = card_num[next_box]
      } 
      else { # if we got to this point, it means we find a loop, hence we drop the loop from card_num, and add one to a in the position as the length of the loop, after which we set the next_box = the first element of the new card_num, as well as redefine the loop
        loop = append(loop, card_num[next_box])
        card_num[card_num %in% loop] = 0
        next_box <- which(card_num!=0)[1]
        a[j,length(loop)] = a[j,length(loop)] + 1
        loop <- c()
        # cat('card_num',{card_num}, '\nloop', {loop},'\n')
      }
    }
  }
  return(prob_non_zero(a))
}






prob = dloop(5,100)


success_prob(5, 10000)
success_prob(50, 10000)

# write!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

print('Include in your comments brief remarks on what is surprising about the results.')

#  probability that there is no loop longer than 50

prob = dloop(50,10000)

barplot(prob, col='yellow', main='Probalities of loop occurrence length
        from 1 to 2n')


1-sum(prob[51:100])






