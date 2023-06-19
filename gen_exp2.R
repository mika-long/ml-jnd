# code for generating experiment 2 in the Hoffmann paper 

library(nullabor)
library(ggplot2)
library(dplyr)

d <- 0.4# size of the shift between distributions 
n1 <- 15 # size of the fitst group of points 
r <- 1/3 # ratio between n1 and n2 
n2 <- r * n1 

for (d in c(0.4, 0.6, 0.8, 1.0, 1.2)){
  for (n1 in c(15, 45, 135)){
    for (r in c(1/3, 2/3, 3/3)){ 
      n2 <- r * n1 
      # generate data for 3 replications 
      for (i in c(1, 2, 3)){
        data1 <- rexp(n1, 1)
        data2 <- rexp(n2, 1/(d + 1)) 
        result <- t.test(data1, data2)
        # print("p_value")
        print(result$p.value)
      }
    }
  }
}

# code for saving pictures 
ggsave(paste("./images",rep, "/pr/", offset, "/", ss, "/", iter, "_1.png", sep=""), width=480, height=480, units="px")


# combined data 
c_df <- data.frame(
  Value = c(data1, data2),
  Group = factor(c(rep("1", n1), rep("2", n2)))
)

# permutate data based on the lineup method 
d <- lineup(null_permute("Value"), c_df)
attr(d, "pos") # position of actual graph 
ggplot(data=d, aes(x=Value, y=Group, color=Group)) + geom_point() + facet_wrap(~ .sample)

# boxplot 
ggplot(c_df, aes(x = Value, y = Group, fill = Group, color = Group)) +
  geom_boxplot(alpha=0.5) +
  labs(x = "Sample", y = "Value") +
  xlab("") + 
  ylab("") + 
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# density plot 
ggplot(c_df, aes(x = Value, color = Group, fill = Group)) +
  geom_density(alpha=0.5) + 
  ylab("") + 
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# histogram 
ggplot(c_df, aes(x = Value, y = Group, color = Group)) +
  geom_histogram(alpha=0.5, stat="bin", bins = 10) +
  labs(x = "Sample", y = "Value") +
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# dotplot 
ggplot(c_df, aes(x = Value, y = Group, color = Group)) +
  geom_jitter(alpha=0.5, height = 0.05) + 
  ylab("") + 
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

