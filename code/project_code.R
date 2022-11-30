# Remove everything from environment
rm(list = ls(all = TRUE)); graphics.off() ; gc()

# Libraries
library(readr)
library(tidyr)
library(tibble)
library(stringr)
library(dbplyr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(purrr)
library(magrittr)

# Read data
path_root = "."
path_data=file.path(path_root, "data")
df <- read.csv(file.path(path_data, "DataMAB.csv"))

## Oracle's optimal decisions
# best arm to pull when discount = 100%
disc_100 <- df %>%
  select(-X) %>%
  colSums()
max(disc_100)
which.max(disc_100)
# best arm to pull when discount = 99.9%
df_999 <- df
for(i in 1:nrow(df_999)) {      
  df_999[i,] <- df_999[i,] *0.999**df$X[i]
}
df_999 <- df_999 %>% 
  select(-X) %>%
  colSums()
max(df_999)
which.max(df_999)
# best arm to pull when discount = 99%
df_99 <- df
for(i in 1:nrow(df_99)) {      
  df_99[i,] <- df_99[i,] *0.99**df$X[i]
}
df_99 <- df_99 %>% 
  select(-X) %>%
  colSums()
max(df_99)
which.max(df_99)


## Hoeffding's inequality
# sample mean from first 100 pulls of first arm
sample_mean = sum(df[1:100,2])/100

# set exp(-2*n*L**2) == 0.05, such that the Probability(True Reward>UCB of sample mean + L) < 0.05
L <- sqrt(log(0.05)/(-2*100)) # in base r, log() is = ln() with base e. log base  10 is log10().

# UCB with confidence 95%
UCB_95 <- sample_mean + L
UCB_95


## Random pull

# Reward function 
randompull_get_reward <- function(data, N, discount_factor) {
  number_of_pulls = integer(24) #initial vector of pulls
  number_of_rewards = integer(24) #initial vector of pulls with rewards
  number_of_no_rewards = integer(24) #initial vector of pulls with no rewards
  ads_selected = integer(0)
  total_reward = 0
  for(n in 1:N) {
    ad = sample(1:24, 1) #choose a random arm
    number_of_pulls[ad] = number_of_pulls[ad] + 1
    ads_selected = append(ads_selected, ad)
    reward = data[number_of_pulls[ad], ad+1]
    if (reward == 1) {
      number_of_rewards[ad] = number_of_rewards[ad] + 1
    } else {
      number_of_no_rewards[ad] = number_of_no_rewards[ad] + 1
    }
    total_reward = total_reward + reward*discount_factor**n
  }
  
  return(total_reward)
}

# Average reward function
randompull_find_average_reward <- function(nloops, data, N, discount_factor) {
  sum_rewards = 0
  for(i in 1:nloops) {
    sum_rewards = sum_rewards + randompull_get_reward(data, N, discount_factor)
  }
  average_reward = sum_rewards / nloops
  return(average_reward)
}


## Epsilon-Greedy

epsilon = 0.2
# Reward function 
epsilon_get_reward <- function(epsilon, data, N, discount_factor) {
  number_of_pulls = integer(24) #initial vector of pulls
  number_of_rewards = integer(24) #initial vector of pulls with rewards
  number_of_no_rewards = integer(24) #initial vector of pulls with no rewards
  sum_of_discounted_rewards = integer(24) #initial vector of sum of discounted rewards to calc exp mean
  ads_selected = integer(0)
  total_reward = 0
  for(n in 1:N) {
    p = runif(1) 
    if (sum(number_of_pulls)==0 | p < epsilon) {
      ad = sample(1:24, 1) #choose a random arm on first pull or 0.2 of the time
    }
    else {
      ad = which.max(sum_of_discounted_rewards/number_of_pulls) #choose the best arm
    }
    number_of_pulls[ad] = number_of_pulls[ad] + 1
    ads_selected = append(ads_selected, ad)
    reward = data[number_of_pulls[ad], ad+1]
    sum_of_discounted_rewards[ad] = sum_of_discounted_rewards[ad] + reward*discount_factor**n #this is needed for exp mean
    if (reward == 1) {
      number_of_rewards[ad] = number_of_rewards[ad] + 1
    } else {
      number_of_no_rewards[ad] = number_of_no_rewards[ad] + 1
    }
    total_reward = total_reward + reward*discount_factor**n
  }
  
  return(total_reward)
}

# Average reward function 
epsilon_find_average_reward <- function(nloops, epsilon, data, N, discount_factor) {
  sum_rewards = 0
  for(i in 1:nloops) {
    sum_rewards = sum_rewards + epsilon_get_reward(epsilon, data, N, discount_factor)
  }
  average_reward = sum_rewards / nloops
  return(average_reward)
}

start.time <- Sys.time()

epsilon_find_average_reward(100, epsilon, df, 10000, 1)
epsilon_find_average_reward(100, epsilon, df, 10000, 0.999)
epsilon_find_average_reward(100, epsilon, df, 10000, 0.99)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


## Changing the epsilon
# range of epsilon from 0.01 to 0.4
epsilon <- c(0.4,0.3,0.2,0.1,0.05,0.01)

start.time <- Sys.time()

for (ep in epsilon){
  print(ep)
  print(epsilon_find_average_reward(100, ep, df, 10000, 1))
  print(epsilon_find_average_reward(100, ep, df, 10000, 0.999))
  print(epsilon_find_average_reward(100, ep, df, 10000, 0.99))
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## Modifying Epsilon-Greedy to be time variate

epsilon = 0.2
# Reward function 
epsilon_get_reward_time_variant <- function(epsilon, data, N, discount_factor,explore_limit) {
  number_of_pulls = integer(24) #initial vector of pulls
  number_of_rewards = integer(24) #initial vector of pulls with rewards
  number_of_no_rewards = integer(24) #initial vector of pulls with no rewards
  sum_of_discounted_rewards = integer(24) #initial vector of sum of discounted rewards to calc exp mean
  ads_selected = integer(0)
  total_reward = 0
  
  # allows for exploration in the first 1000 loops
  for(n in 1:explore_limit) {
    p = runif(1) 
    if (sum(number_of_pulls)==0 | p < epsilon) {
      ad = sample(1:24, 1) #choose a random arm on first pull or 0.2 of the time
    }
    else {
      ad = which.max(sum_of_discounted_rewards/number_of_pulls) #choose the best arm
    }
    number_of_pulls[ad] = number_of_pulls[ad] + 1
    ads_selected = append(ads_selected, ad)
    reward = data[number_of_pulls[ad], ad+1]
    sum_of_discounted_rewards[ad] = sum_of_discounted_rewards[ad] + reward*discount_factor**n #this is needed for exp mean
    if (reward == 1) {
      number_of_rewards[ad] = number_of_rewards[ad] + 1
    } else {
      number_of_no_rewards[ad] = number_of_no_rewards[ad] + 1
    }
    total_reward = total_reward + reward*discount_factor**n
  }
  
  # after the first 1000 loops, only pull the best arm!
  for(n in (explore_limit+1):N) {
    
    ad = which.max(sum_of_discounted_rewards/number_of_pulls) #choose the best arm
    
    number_of_pulls[ad] = number_of_pulls[ad] + 1
    ads_selected = append(ads_selected, ad)
    reward = data[number_of_pulls[ad], ad+1]
    sum_of_discounted_rewards[ad] = sum_of_discounted_rewards[ad] + reward*discount_factor**n #this is needed for exp mean
    if (reward == 1) {
      number_of_rewards[ad] = number_of_rewards[ad] + 1
    } else {
      number_of_no_rewards[ad] = number_of_no_rewards[ad] + 1
    }
    total_reward = total_reward + reward*discount_factor**n
  }
  
  
  return(total_reward)
}

# Average reward function 
epsilon_time_find_average_reward <- function(nloops, epsilon, data, N, discount_factor,explore_limit) {
  sum_rewards = 0
  for(i in 1:nloops) {
    sum_rewards = sum_rewards + epsilon_get_reward_time_variant(epsilon, data, N, discount_factor,explore_limit)
  }
  average_reward = sum_rewards / nloops
  return(average_reward)
}

epsilon <- 0.2
explore_limit <- c(1000,500,250)

start.time <- Sys.time()

for (limit in explore_limit){
  print(limit)
  print(epsilon_time_find_average_reward(100, epsilon, df, 10000, 1,limit))
  print(epsilon_time_find_average_reward(100, epsilon, df, 10000, 0.999,limit))
  print(epsilon_time_find_average_reward(100, epsilon, df, 10000, 0.99,limit))
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## UCB1

# Reward function
ucb1_get_reward <- function(data, N, discount) {
  rewards_table <- data %>% select(-X)
  
  # Initialising experimental means by pulling each arm once (N=1 to 24)
  ads_selected = seq(1,24)
  times_selected = rep(1,24)
  rewards_by_ads = as.list(rewards_table[1,])
  total_reward = do.call(sum,rewards_by_ads)
  
  
  # For each subsequent round, choose ad with the maximum UCB
  for (n in 25:N){
    ad = 0
    max_upper_bound = 0
    
    for (i in 1:24){
      xbar = as.numeric(rewards_by_ads[i])/times_selected[i]
      l = sqrt(2 *log(n)/times_selected[i])
      ucb = xbar + l
      
      if (ucb == max_upper_bound){
        ad = sample(c(ad,i),1)
        max_upper_bound = ucb
      }
      
      if (ucb > max_upper_bound){
        ad = i
        max_upper_bound = ucb
      }
      
    }
    
    ads_selected = append(ads_selected,ad)
    times_selected[ad] = times_selected[ad] + 1
    
    
    reward = rewards_table[times_selected[ad], ad]
    discounted_reward = reward*(discount^n)
    rewards_by_ads[ad] = as.numeric(rewards_by_ads[ad]) + discounted_reward
    total_reward = total_reward +  discounted_reward
    
  }
  return (total_reward)
}

# Average reward function
ucb1_find_average_reward <- function(nloops, data, N, discount) {
  sum_rewards = 0
  for(i in 1:nloops) {
    sum_rewards = sum_rewards + ucb1_get_reward(data, N, discount)
  }
  average_reward = sum_rewards / nloops
  return(average_reward)
}

start.time <- Sys.time()

ucb1_find_average_reward(100,df,10000,1)
ucb1_find_average_reward(100,df,10000,0.999)
ucb1_find_average_reward(100,df,10000,0.99)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#Apply discount factor to the exploratory term
# Reward function
ucb1_get_reward_modified <- function(data, N, discount) {
  rewards_table <- data %>% select(-X)
  
  # Initialising experimental means by pulling each arm once (N=1 to 24)
  ads_selected = seq(1,24)
  times_selected = rep(1,24)
  rewards_by_ads = as.list(rewards_table[1,])
  total_reward = do.call(sum,rewards_by_ads)
  
  
  # For each subsequent round, choose ad with the maximum UCB
  for (n in 25:N){
    ad = 0
    max_upper_bound = 0
    
    for (i in 1:24){
      xbar = as.numeric(rewards_by_ads[i])/times_selected[i]
      l = (discount^n)*sqrt(2 *log(n)/times_selected[i])
      ucb = xbar + l
      
      if (ucb == max_upper_bound){
        ad = sample(c(ad,i),1)
        max_upper_bound = ucb
      }
      
      if (ucb > max_upper_bound){
        ad = i
        max_upper_bound = ucb
      }
      
    }
    
    ads_selected = append(ads_selected,ad)
    times_selected[ad] = times_selected[ad] + 1
    
    
    reward = rewards_table[times_selected[ad], ad]
    discounted_reward = reward*(discount^n)
    rewards_by_ads[ad] = as.numeric(rewards_by_ads[ad]) + discounted_reward
    total_reward = total_reward +  discounted_reward
    
  }
  return (total_reward)
}

# Average reward function
ucb1_find_average_reward_modified <- function(nloops, data, N, discount) {
  sum_rewards = 0
  for(i in 1:nloops) {
    sum_rewards = sum_rewards + ucb1_get_reward_modified(data, N, discount)
  }
  average_reward = sum_rewards / nloops
  return(average_reward)
}

start.time <- Sys.time()

ucb1_find_average_reward_modified(100,df,10000,1)
ucb1_find_average_reward_modified(100,df,10000,0.999)
ucb1_find_average_reward_modified(100,df,10000,0.99)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## Thompson Sampling

# Reward function
thompson_get_reward <- function(data, N, discount_factor) {
  number_of_pulls = integer(24) #initial vector of pulls
  number_of_rewards = integer(24) #initial vector of pulls with rewards
  number_of_no_rewards = integer(24) #initial vector of pulls with no rewards
  ads_selected = integer(0)
  total_reward = 0
  for(n in 1:N) {
    ad = 0
    max_random = 0
    for(i in 1:24) {
      random_beta = rbeta(1, number_of_rewards[i] + 1, number_of_no_rewards[i] + 1)
      if(random_beta > max_random) {
        max_random = random_beta
        ad = i
      }
    }
    number_of_pulls[ad] = number_of_pulls[ad] + 1
    ads_selected = append(ads_selected, ad)
    reward = data[number_of_pulls[ad], ad+1]
    if (reward == 1) {
      number_of_rewards[ad] = number_of_rewards[ad] + 1
    } else {
      number_of_no_rewards[ad] = number_of_no_rewards[ad] + 1
    }
    total_reward = total_reward + reward*discount_factor**n
  }
  return(total_reward)
}

# Average reward function
thompson_find_average_reward <- function(nloops, data, N, discount_factor) {
  sum_rewards = 0
  for(i in 1:nloops) {
    sum_rewards = sum_rewards + thompson_get_reward(data, N, discount_factor)
  }
  average_reward = sum_rewards / nloops
  return(average_reward)
}

start.time <- Sys.time()

thompson_find_average_reward(100, df, 10000, 1)
thompson_find_average_reward(100, df, 10000, 0.999)
thompson_find_average_reward(100, df, 10000, 0.99)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#Modifying thompson sampling to account for discount factor
# Reward function
thompson_get_reward_modified <- function(data, N, discount_factor) {
  number_of_pulls = integer(24) #initial vector of pulls
  number_of_rewards = integer(24) #initial vector of pulls with rewards
  number_of_no_rewards = integer(24) #initial vector of pulls with no rewards
  ads_selected = integer(0)
  total_reward = 0
  for(n in 1:N) {
    ad = 0
    max_random = 0
    for(i in 1:24) {
      random_beta = rbeta(1, (number_of_rewards[i] + 1)/(discount_factor**n), 
                          (number_of_no_rewards[i] + 1)/(discount_factor**n))
      if(random_beta > max_random) {
        max_random = random_beta
        ad = i
      }
    }
    number_of_pulls[ad] = number_of_pulls[ad] + 1
    ads_selected = append(ads_selected, ad)
    reward = data[number_of_pulls[ad], ad+1]
    if (reward == 1) {
      number_of_rewards[ad] = number_of_rewards[ad] + 1
    } else {
      number_of_no_rewards[ad] = number_of_no_rewards[ad] + 1
    }
    total_reward = total_reward + reward*discount_factor**n
  }
  return(total_reward)
}

# Average reward function
thompson_find_average_reward_modified <- function(nloops, data, N, discount_factor) {
  sum_rewards = 0
  for(i in 1:nloops) {
    sum_rewards = sum_rewards + thompson_get_reward(data, N, discount_factor)
  }
  average_reward = sum_rewards / nloops
  return(average_reward)
}

start.time <- Sys.time()

thompson_find_average_reward_modified(100, df, 10000, 1)
thompson_find_average_reward_modified(100, df, 10000, 0.999)
thompson_find_average_reward_modified(100, df, 10000, 0.99)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
