# Simulating Dunning-Kruger Data #

library(ggplot2)
library(dplyr)
library(tidyr)

sim_dk <- function(n = 1000, bias = 0, sig_o = 1, sig_s = 1) {
  x = rnorm(n=n) # create true ability score
  o = x + rnorm(n=n, sd = sig_o) # create observed ability
  s = x + bias + rnorm(n=n, sd = sig_o)
  q_vals = quantile(o, c(0.25,0.5,0.75))
  q = case_when(o <= q_vals[1] ~ 1,
                o > q_vals[1] & o <= q_vals[2] ~ 2,
                o > q_vals[2] & o <= q_vals[3] ~ 3,
                o > q_vals[3] ~ 4)
  
  return(data.frame(x, o, s, q))
}

make_qplot <- function(data) {
  plt <- data %>%
    group_by(q) %>%
    summarize(Actual = mean(o),
              Perceived = mean(s)) %>%
    pivot_longer(-q, values_to = "val", names_to = "Type") %>%
    ggplot(., aes(x=q, y=val))+
    geom_line(aes(color=Type), size = 1.5)+
    geom_point(aes(color=Type, shape=Type), size = 4)+
    theme_minimal()+
    labs(x = "Quartile", y = "Score",
         title = "Better Than Average Effect Simulation")+
    theme(legend.position="bottom", 
          plot.title = element_text(hjust=0.5))
  return(plt)
}

# the best simulation
sim5 <- sim_dk(bias = 0.75, sig_o = 1.1)
plot5 <- make_qplot(sim5)
plot5
