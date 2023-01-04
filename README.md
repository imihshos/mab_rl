# Multi-Armed Bandit

In this project, we explored the effect of the time value of money on the exploitation and exploration tradeoff. ϵ-greedy, UCB1 and Thompson Sampling were used and their performances were evaluated for different discount factors (100%, 99.9% and 99%).

This project is motivated by a case study of Obama's fundraising campaign (https://www.optimizely.com/insights/blog/how-obama-raised-60-million-by-running-a-simple-experiment/). 

We simulated 10,000 rounds of pulls 100 times for each algorithm and took the average to obtain the following results results. 

### Theoretical Maximum Reward

| Discount Factor| Best Ad        | Reward  |
|:-------------: |:-------------: | :-----: |
| 100%           | 17             | 2453    |
| 99.9%          | 17             | 259.75  |
| 99%            | 17             | 26.47   |

### ϵ-greedy

| Discount Factor| Reward         | Regret  |
|:-------------: |:-------------: | :-----: |
| 100%           | 2248.18        | 204.82  |
| 99.9%          | 210.35         | 49.4    |
| 99%            | 16.19          | 10.28   |


### UCB1

| Discount Factor| Reward         | Regret  |
|:-------------: |:-------------: | :-----: |
| 100%           | 1850           | 603     |
| 99.9%          | 154.30         | 105.45  |
| 99%            | 15.82          | 10.65   |


### Thompson Sampling

| Discount Factor| Reward         | Regret  |
|:-------------: |:-------------: | :-----: |
| 100%           | 2240.08        | 212.92  |
| 99.9%          | 182.32         | 77.43   |
| 99%            | 15.92          | 10.55   |

### Modifications
When a discount factor is introduced, the value of exploration decreases because later rewards hold less weight. Therefore, we want to modify the algorithms such that the amount of exploration is lower.
* UCB1: Apply the same discount factor to the exploratory term in the UCB equation
* Thompson Sampling: Divide the Beta parameters in the posterior distribution by the same discount factor
