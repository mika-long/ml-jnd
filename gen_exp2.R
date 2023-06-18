# code for generating experiment 2 in the Hoffmann paper 

library(nullabor)
library(ggplot2)

d <- 0.4 # size of the shift between distributions 
n1 <- 15 # size of the fitst group of points 
r <- 1/3 # ratio between n1 and n2 
n2 <- r * n1 

# generate data
data1 <- rexp(n1, 1)
data2 <- rexp(n2, 1/(d + 1))

# p-value calculation 
t.test(data1, data2)

# combined data 
c_df <- data.frame(
  Value = c(data1, data2),
  Group = factor(c(rep("1", n1), rep("2", n2)))
)


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

