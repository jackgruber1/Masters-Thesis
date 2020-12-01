#Jeancarlo Velez
#4/22/2020
#Master's Thesis Network Through Time Analysis 

#load packages
library(igraph)
library(ggplot2)
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(scales)
library(stargazer)

#clean workspace
rm(list = ls())

#-------------------------------------------------------------#

####----Upload Network Data for Countyries----####

network_data <- read.csv("Thru Time Data Alt.csv", header = T) #upload Data

colnames(network_data)[1] <- "year" #correct first name glitch 


#####----Facet Wraps for Each Network Measure-----####

#Outdegree

outdegree_evol <- ggplot(network_data, aes(x = year, y = outdegree)) + #data
  geom_line(size = 1, aes(color = country)) + #line
  facet_wrap( ~ country, ncol = 2) + #facet wrap
  theme_light() + 
  scale_color_brewer(palette = "Reds") 
  #labs(title="Outdegree Throughout the Years", x="Year",
       #y="Outdegree") # Labels

outdegree_evol <- outdegree_evol + theme(plot.title=element_text(size=12, face="bold"), 
                                 axis.text.x=element_text(size=10), 
                                 axis.text.y=element_text(size=10),
                                 axis.title.x=element_text(size=12),
                                 axis.title.y=element_text(size=12)) 

outdegree_evol

ggsave("Outdegree Thru Years.png", outdegree_evol) #save

#Indegree

indegree_evol <- ggplot(network_data, aes(x = year, y = indegree)) + #data
  geom_line(size = 1, aes(color = country)) + #line
  facet_wrap( ~ country, ncol = 2) + #facet wrap
  theme_light() + 
  scale_color_brewer(palette = "Reds") 
  #labs(title="Indegree Throughout the Years", x="Year",
       #@y="Indegree") # Labels

indegree_evol <- indegree_evol + theme(plot.title=element_text(size=12, face="bold"), 
                                         axis.text.x=element_text(size=10), 
                                         axis.text.y=element_text(size=10),
                                         axis.title.x=element_text(size=12),
                                         axis.title.y=element_text(size=12)) 

indegree_evol

ggsave("Indegree Thru Years.png", indegree_evol) #save

#Hub Score

hub_evol <- ggplot(network_data, aes(x = year, y = hub)) + #data
  geom_line(size = 1, aes(color = country)) + #line
  facet_wrap( ~ country, ncol = 2) + #facet wrap
  theme_light() + 
  scale_color_brewer(palette = "Reds") 
  #labs(title="Hub Score Throughout the Years", x="Year",
      # y="Hub Score") # Labels

hub_evol <- hub_evol + theme(plot.title=element_text(size=12, face="bold"), 
                                         axis.text.x=element_text(size=10), 
                                         axis.text.y=element_text(size=10),
                                         axis.title.x=element_text(size=12),
                                         axis.title.y=element_text(size=12)) 

hub_evol

ggsave("Hub Score Thru Years.png", hub_evol) #save

#Authority Score

aut_evol <- ggplot(network_data, aes(x = year, y = authority)) + #data
  geom_line(size = 1, aes(color = country)) + #line
  facet_wrap( ~ country, ncol = 2) + #facet wrap
  theme_light() + 
  scale_color_brewer(palette = "Reds") 
  #labs(title="Authority Score Throughout the Years", x="Year",
       #y="Authority Score") # Labels

aut_evol <- aut_evol + theme(plot.title=element_text(size=12, face="bold"), 
                             axis.text.x=element_text(size=10), 
                             axis.text.y=element_text(size=10),
                             axis.title.x=element_text(size=12),
                             axis.title.y=element_text(size=12)) 

aut_evol

ggsave("Authority Thru Years.png") #save

#GDP

gdp_evol <- ggplot(network_data, aes(x = year, y = gdp)) + #data
  geom_line(size = 1, aes(color = country)) + #line
  facet_wrap( ~ country, ncol = 2) + #facet wrap
  theme_light() + 
  scale_color_brewer(palette = "Reds")  
  #labs(title="Gross Domestic Product Throughout the Years", x="Year",
       #y="Gross Domestic Product") # Labels

gdp_evol <- gdp_evol + theme(plot.title=element_text(size=12, face="bold"), 
                             axis.text.x=element_text(size=10), 
                             axis.text.y=element_text(size=10),
                             axis.title.x=element_text(size=12),
                             axis.title.y=element_text(size=12)) 

gdp_evol

ggsave("GDP Thru Years.png", gdp_evol) #save

# Betweeness

bet_evol <- ggplot(network_data, aes(x = year, y = betweeness)) + #data
  geom_line(size = 1, aes(color = country)) + #line
  facet_wrap( ~ country, ncol = 2) + #facet wrap
  theme_light() + 
  scale_color_brewer(palette = "Reds")  
#labs(title="Gross Domestic Product Throughout the Years", x="Year",
#y="Gross Domestic Product") # Labels

bet_evol <- bet_evol + theme(plot.title=element_text(size=12, face="bold"), 
                             axis.text.x=element_text(size=10), 
                             axis.text.y=element_text(size=10),
                             axis.title.x=element_text(size=12),
                             axis.title.y=element_text(size=12)) 

bet_evol

ggsave("GDP Thru Years.png", gdp_evol) #save

####----Upload Data for Worldwide Measures----####

world_data <- read.csv("World Data.csv", header = T) #upload Data

colnames(network_data)[1] <- "year" #correct first name glitch 


#####----Facet Wraps for Each Network Measure-----####

#GDP

gdp <- ggplot(world_data, aes(x = year, y = gdp)) + #data
  geom_line(size = 1, color = "Light Blue") + #line
  theme_light() 
  #labs(title="Gross Domestic Product Throughout the Years", x="Year",
       #y="Gross Domestic Product") # Labels

gdp <- gdp + theme(plot.title=element_text(size=12, face="bold"), 
                             axis.text.x=element_text(size=10), 
                             axis.text.y=element_text(size=10),
                             axis.title.x=element_text(size=12),
                             axis.title.y=element_text(size=12)) 

gdp

ggsave("GDP Thru Years.png", gdp)

#Density

den <- ggplot(world_data, aes(x = year, y = density)) + #data
  geom_line(size = 1, color = "Light Blue") + #line
  theme_light() 
  #labs(title="Network Density Throughout the Years", x="Year",
      # y="Network Density") # Labels

den <- den + theme(plot.title=element_text(size=12, face="bold"), 
                   axis.text.x=element_text(size=10), 
                   axis.text.y=element_text(size=10),
                   axis.title.x=element_text(size=12),
                   axis.title.y=element_text(size=12)) 

den

ggsave("Density Thru Years.png", den)

#Assortativity by GDP

as_gdp <- ggplot(world_data, aes(x = year, y = assortativity_gdp)) + #data
  geom_line(size = 1, color = "Light Blue") + #line
  theme_light()  
  #labs(title="Assortativity by Income Throughout the Years", x="Year",
       #y="Network Assortativity") # Labels

as_gdp <- as_gdp + theme(plot.title=element_text(size=12, face="bold"), 
                   axis.text.x=element_text(size=10), 
                   axis.text.y=element_text(size=10),
                   axis.title.x=element_text(size=12),
                   axis.title.y=element_text(size=12)) 

as_gdp

ggsave("GDP Assortativity Thru Years.png", as_gdp)


#Assortativity by OECD Membership

as_oecd <- ggplot(world_data, aes(x = year, y = assortativity_oecd)) + #data
  geom_line(size = 1, color = "Light Blue") + #line
  theme_light()  
  #labs(title="Assortativity by OECD Membership Throughout the Years", x="Year",
       #y="Network Assortativity") # Labels

as_oecd <- as_oecd + theme(plot.title=element_text(size=12, face="bold"), 
                         axis.text.x=element_text(size=10), 
                         axis.text.y=element_text(size=10),
                         axis.title.x=element_text(size=12),
                         axis.title.y=element_text(size=12)) 

as_oecd

ggsave("OECD Assortativity Thru Years.png")


#Assortativity by Geography

as_region <- ggplot(world_data, aes(x = year, y = assortativity_region)) + #data
  geom_line(size = 1, color = "Light Blue") + #line
  theme_light()  
  #labs(title="Assortativity by World Region Throughout the Years", x="Year",
       #y="Network Assortativity") # Labels

as_region<- as_region + theme(plot.title=element_text(size=12, face="bold"), 
                           axis.text.x=element_text(size=10), 
                           axis.text.y=element_text(size=10),
                           axis.title.x=element_text(size=12),
                           axis.title.y=element_text(size=12)) 

as_region

ggsave("Region Assortativity Thru Years.png")
