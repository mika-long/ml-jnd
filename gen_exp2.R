# code for generating experiment 2 in the Hoffmann paper 
# execute this once to obtain all the stimuli 
library(nullabor)
library(ggplot2)
library(dplyr)

 
gen_den <- function(data) {
  ggplot(data, aes(x = Value, color = Group, 
                   fill = Group)) + ylab("") + 
    geom_density(alpha=0.5) + 
    theme(legend.position = "none",
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
}
# function for generating histogram  
gen_hist <- function(data){
  ggplot(data, aes(x = Value, fill = Group)) +
    geom_histogram(alpha=0.5, stat = "bin", bins = 10) +
    ylab("") + 
    theme(legend.position = "none", 
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
}
# function for generating box plot 
gen_box <- function(data){
  ggplot(data, aes(x = Value, y = Group, 
                   fill = Group, color = Group)) +
    geom_boxplot(alpha=0.5) + ylab("") + 
    theme(legend.position = "none", 
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
}
# function for generating dot plot 
gen_dot <- function(data){
  ggplot(data, aes(x = Value, y = Group, 
                   color = Group)) +
    geom_jitter(alpha=0.5, height = 0.05) + 
    ylab("") + 
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
}
# general function 
gen_graph <- function(data, type) {
  switch(type,
         den = gen_den(data), 
         box = gen_box(data), 
         hist = gen_hist(data), 
         dot = gen_dot(data)
         )
}
# function for saving graph 
save_graph <- function(type, i, graph, tf){
  if (tf == TRUE){
    ggsave(paste("./images/d", d, "n", n1, "r", round(r, 2), "rep", rep, "/", type, "/", i, "_1.png", sep=""), width=480, height=480, units="px", plot=graph)
  } else {
    ggsave(paste("./images/d", d, "n", n1, "r", round(r, 2), "rep", rep, "/", type, "/", i, "_0.png", sep=""), width=480, height=480, units="px", plot=graph)
  }
}

# Heavy lifting for generating plots 
for (d in c(0.4, 0.6, 0.8, 1.0, 1.2)){
  for (n1 in c(15, 45, 135)){
    for (r in c(1/3, 2/3, 3/3)){ 
      n2 <- r * n1 
      for (rep in seq(1, 3)){ # generate data for 3 replications
        data1 <- rexp(n1, 1)
        data2 <- rexp(n2, 1/(d + 1)) 
        # combined dataframe 
        c_df <- data.frame(
          Value = c(data1, data2),
          Group = factor(c(rep("1", n1), rep("2", n2)))
        )
        # lineup data 
        df <- lineup(null_permute("Value"), c_df)
        # real data position 
        df_pos <- attr(df, "pos")
        # splitting it up 
        df_split <- df %>% group_split(.sample)
        for (i in unique(df$.sample)){
          df_i <- df_split[[i]] 
          p_den <- gen_graph(df_i, "den")
          p_box <- gen_graph(df_i, "box")
          p_hist <- gen_graph(df_i, "hist")
          p_dot <- gen_graph(df_i, "dot")
  
          if (i == df_pos){
            save_graph("den", i, p_den, TRUE)
            save_graph("box", i, p_box, TRUE)
            save_graph("hist", i, p_hist, TRUE)
            save_graph("dot", i, p_dot, TRUE)
          } else if (i != df_pos) {
            save_graph("den", i, p_den, FALSE)
            save_graph("box", i, p_box, FALSE)
            save_graph("hist", i, p_hist, FALSE)
            save_graph("dot", i, p_dot, FALSE)
          }
        }
       
      }
    }
  }
}