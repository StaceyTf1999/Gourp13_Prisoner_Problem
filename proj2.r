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



# if too slow, change the loop
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


dloop <- function(n, nreps){
  # explain what is a 
  a = rep(0,2*n)
  for (j in 1:nreps) {
    card_num <- sample(c(1:(2*n)), size = 2*n, replace = FALSE)
    next_box <- card_num[1]
    loop <- c(next_box)
    for (i in 1:(2*n)) {
      # this actually happens only when we find all the loop(s), but we put this at the first for efficiency
      if (sum(card_num)==0){
        break
      }
      else if (card_num[next_box] != card_num[which(card_num!=0)][1]){ # card_num[which(card_num!=0)[1] means the index the first non-zero element
        # can't use loop[i+1] = '', since in this cycle, i will increase
        loop = append(loop, card_num[next_box])
        next_box = card_num[next_box]
      } else { # if we got to this point, it means we find a loop, hence we drop the loop from card_num, and add one to a in the position as the length of the loop, after which we set the next_box = the first element of the new card_num, as well as redefine the loop
        # card_num_text = card_num
        # next_box <- card_num[!card_num %in% loop][1]
        card_num[card_num %in% loop] = 0
        next_box <- card_num[which(card_num!=0)][1]
        a[length(loop)] = a[length(loop)] + 1
        loop <- c(next_box)
      }
    }
  }
  return(a/nreps)
}

#  probability that there is no loop longer than 50

prob = dloop(50,10000)

1-sum(prob[51:100])






