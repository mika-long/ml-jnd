knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(tidyverse)
#library(nullabor)
library(dplyr)
library(plotly)
library(gridExtra)
#library(patchwork)
#setwd("~/Downloads/")

wind <- read.csv("SEAwindData.csv", TRUE, sep = ",", na.strings = TRUE)

start = 51
end = 100


#polar with reference
for(rep in start:end) {
  for(ss in c(0.02, 0.04, 0.06, 0.08, 0.1)) { #what percent of data do we want to keep
    x <- wind %>% sample_frac(ss)
    w <- x %>% group_by(direction, difference) %>% summarize(value = n())
    for(offset in seq(0, 270, 90)) {
      realnumber <- sample(1:20, 1)
      p <- w %>% mutate(direction, direction = ifelse(direction >= (360-offset), direction - (360-offset), direction + offset)) #%>% ggplot(aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
      q <- ggplot(p, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
      
      for(iter in 1:20) {
        if(iter==realnumber) {
          q <- ggplot(p, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          
          g <- q + geom_bar(position="fill", stat="identity", width=10.3) + 
            scale_fill_brewer(palette='PuBu', direction=-1) + labs(fill="Minutes between Wheel Events") + 
            scale_x_continuous(breaks=seq(0,360,45), labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N'))  + 
            geom_hline(yintercept = 0.5, colour="white") + coord_polar() + geom_vline(xintercept=seq(0,350,10), color="white", lwd=0.1) + 
            theme(legend.position="none", axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.margin = margin(0,0,0,0), axis.line = element_blank())
          ggsave(paste("./images",rep, "/pr/", offset, "/", ss, "/", iter, "_1.png", sep=""), width=480, height=480, units="px")
          
        } else {
          tempx <- transform(x, difference=sample(difference))
          tempw <- tempx %>% group_by(direction, difference) %>% summarize(value = n())
          summary(tempw)
          tempp <- tempw %>% mutate(direction, direction = ifelse(direction >= (360-offset), direction - (360-offset), direction + offset)) #%>% ggplot(aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          tempq <- ggplot(tempp, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          
          g <- tempq + geom_bar(position="fill", stat="identity", width=10.3) + 
            scale_fill_brewer(palette='PuBu', direction=-1) + labs(fill="Minutes between Wheel Events") + 
            scale_x_continuous(breaks=seq(0,360,45), labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N'))  + 
            geom_hline(yintercept = 0.5, colour="white") + coord_polar() + geom_vline(xintercept=seq(0,350,10), color="white", lwd=0.1) + 
            theme(legend.position="none", axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.margin = margin(0,0,0,0), axis.line = element_blank())
          ggsave(paste("./images",rep, "/pr/", offset, "/", ss, "/", iter, "_0.png", sep=""), width=480, height=480, units="px")
          
        }
      }
    }
  }
  print(rep)
}

#polar no reference
for(rep in start:end) {
  for(ss in c(0.02, 0.04, 0.06, 0.08, 0.1)) { #what percent of data do we want to keep
    x <- wind %>% sample_frac(ss)
    w <- x %>% group_by(direction, difference) %>% summarize(value = n())
    for(offset in seq(0, 270, 90)) {
      realnumber <- sample(1:20, 1)
      p <- w %>% mutate(direction, direction = ifelse(direction >= (360-offset), direction - (360-offset), direction + offset)) #%>% ggplot(aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
      q <- ggplot(p, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
      
      for(iter in 1:20) {
        if(iter==realnumber) {
          q <- ggplot(p, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          
          g <- q + geom_bar(position="fill", stat="identity", width=10.3) + 
            scale_fill_brewer(palette='PuBu', direction=-1) + 
            labs(fill="Minutes between Wheel Events") + scale_x_continuous(breaks=seq(0,360,45), labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N')) + 
            coord_polar() + geom_vline(xintercept=seq(0,350,10), color="white", lwd=0.1) + 
            theme(legend.position="none", axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.margin = margin(0,0,0,0), axis.line = element_blank())
          ggsave(paste("./images",rep, "/pnr/", offset, "/", ss, "/", iter, "_1.png", sep=""), width=480, height=480, units="px")
          
        } else {
          tempx <- transform(x, difference=sample(difference))
          tempw <- tempx %>% group_by(direction, difference) %>% summarize(value = n())
          summary(tempw)
          tempp <- tempw %>% mutate(direction, direction = ifelse(direction >= (360-offset), direction - (360-offset), direction + offset)) #%>% ggplot(aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          tempq <- ggplot(tempp, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          
          g <- tempq + geom_bar(position="fill", stat="identity", width=10.3) + 
            scale_fill_brewer(palette='PuBu', direction=-1) + labs(fill="Minutes between Wheel Events") + 
            scale_x_continuous(breaks=seq(0,360,45), labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N')) + coord_polar() + 
            geom_vline(xintercept=seq(0,350,10), color="white", lwd=0.1) + 
            theme(legend.position="none", axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.margin = margin(0,0,0,0), axis.line = element_blank())
          ggsave(paste("./images",rep, "/pnr/", offset, "/", ss, "/", iter, "_0.png", sep=""), width=480, height=480, units="px")
          
        }
      }
    }
  }
  print(rep)
}

#bar with reference
for(rep in start:end) {
  for(ss in c(0.02, 0.04, 0.06, 0.08, 0.1)) { #what percent of data do we want to keep
    x <- wind %>% sample_frac(ss)
    w <- x %>% group_by(direction, difference) %>% summarize(value = n())
    for(offset in seq(0, 270, 90)) {
      realnumber <- sample(1:20, 1)
      p <- w %>% mutate(direction, direction = ifelse(direction >= (360-offset), direction - (360-offset), direction + offset)) #%>% ggplot(aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
      q <- ggplot(p, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
      
      for(iter in 1:20) {
        if(iter==realnumber) {
          q <- ggplot(p, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          
          g <- q + geom_bar(position="fill", stat="identity", width=10.3) + 
            scale_fill_brewer(palette='PuBu', direction=-1) + labs(fill="Minutes between Wheel Events") + 
            scale_x_continuous(breaks=seq(0,360,45), labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N')) + 
            geom_vline(xintercept=seq(0,350,10), color="white", lwd=0.1) + geom_hline(yintercept = 0.5, colour="white") + 
            theme(legend.position="none", axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.margin = margin(0,0,0,0), axis.line = element_blank())
          ggsave(paste("./images",rep, "/br/", offset, "/", ss, "/", iter, "_1.png", sep=""), width=480, height=480, units="px")
          
        } else {
          tempx <- transform(x, difference=sample(difference))
          tempw <- tempx %>% group_by(direction, difference) %>% summarize(value = n())
          summary(tempw)
          tempp <- tempw %>% mutate(direction, direction = ifelse(direction >= (360-offset), direction - (360-offset), direction + offset)) #%>% ggplot(aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          tempq <- ggplot(tempp, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          
          g <- tempq + geom_bar(position="fill", stat="identity", width=10.3) + 
            scale_fill_brewer(palette='PuBu', direction=-1) + labs(fill="Minutes between Wheel Events") + 
            scale_x_continuous(breaks=seq(0,360,45), labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N')) + 
            geom_vline(xintercept=seq(0,350,10), color="white", lwd=0.1) + geom_hline(yintercept = 0.5, colour="white") + 
            theme(legend.position="none", axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.margin = margin(0,0,0,0), axis.line = element_blank())
          ggsave(paste("./images",rep, "/br/", offset, "/", ss, "/", iter, "_0.png", sep=""), width=480, height=480, units="px")
          
        }
      }
    }
  }
  print(rep)
}

#bar no reference
for(rep in start:end) {
  for(ss in c(0.02, 0.04, 0.06, 0.08, 0.1)) { #what percent of data do we want to keep
    x <- wind %>% sample_frac(ss)
    w <- x %>% group_by(direction, difference) %>% summarize(value = n())
    for(offset in seq(0, 270, 90)) {
      realnumber <- sample(1:20, 1)
      p <- w %>% mutate(direction, direction = ifelse(direction >= (360-offset), direction - (360-offset), direction + offset)) #%>% ggplot(aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
      q <- ggplot(p, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
      
      for(iter in 1:20) {
        if(iter==realnumber) {
          q <- ggplot(p, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          
          g <- q + geom_bar(position="fill", stat="identity", width=10.3) + 
            scale_fill_brewer(palette='PuBu', direction=-1) + labs(fill="Minutes between Wheel Events") + 
            scale_x_continuous(breaks=seq(0,360,45), minor_breaks=seq(0, 360, 10), labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N')) + 
            geom_vline(xintercept=seq(0,350,10), color="white", lwd=0.1) + 
            theme(legend.position="none", axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.margin = margin(0,0,0,0), axis.line = element_blank())
          ggsave(paste("./images",rep, "/bnr/", offset, "/", ss, "/", iter, "_1.png", sep=""), width=480, height=480, units="px")
          
        } else {
          tempx <- transform(x, difference=sample(difference))
          tempw <- tempx %>% group_by(direction, difference) %>% summarize(value = n())
          summary(tempw)
          tempp <- tempw %>% mutate(direction, direction = ifelse(direction >= (360-offset), direction - (360-offset), direction + offset)) #%>% ggplot(aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          tempq <- ggplot(tempp, aes(fill=forcats::fct_rev(factor(difference)), y=value, x=direction))
          
          g <- tempq + geom_bar(position="fill", stat="identity", width=10.3) + 
            scale_fill_brewer(palette='PuBu', direction=-1) + labs(fill="Minutes between Wheel Events") + 
            scale_x_continuous(breaks=seq(0,360,45), minor_breaks=seq(0, 360, 10), labels = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'N')) + 
            geom_vline(xintercept=seq(0,350,10), color="white", lwd=0.1) + 
            theme(legend.position="none", axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.margin = margin(0,0,0,0), axis.line = element_blank())
          ggsave(paste("./images",rep, "/bnr/", offset, "/", ss, "/", iter, "_0.png", sep=""), width=480, height=480, units="px")
          
        }
      }
    }
  }
  print(rep)
}
