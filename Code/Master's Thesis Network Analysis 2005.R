#Jeancarlo Velez
#4/23/2020
#Master's Thesis Netowork Analysis 2005 Analysis  

#load packages
library(igraph)
library(ggplot2)
library(tidyverse)
library(statnet)
library(readxl)
library(RColorBrewer)
library(scales)
library(stargazer)

#setting up workspace 
rm(list = ls())
#------------------------------------------------------------------------------------------------

#####----- Section 1: Forming the Trade Network -------

set.seed(1)

# 1.1 - Upload adjacency matrix to R

tech_2005 <- read.csv("2005_TechMatrix.csv", header=T, row.names = 1) #upload 2005 technology matrix 

matrix_2005 <- as.matrix(tech_2005) #coerce into matrix 

# 1.2 - Form Directed graph object 

degree = igraph::degree
delete.vertices = igraph::delete.vertices


network_2005 <- graph_from_adjacency_matrix(matrix_2005, mode = "directed", diag = F) 

network_2005 <- delete.vertices(network_2005, which(degree(network_2005)==0)) # delete isolated vertices

fruch_layout = layout_with_fr(network_2005) # layout 

# 1.3 - Plot graph to test 

plot(network_2005, layout = fruch_layout) 


#####----- Section 2: Uploading Other Attributes -------

# 2.1 Uploading dataframe with alt attributes (GDP, Continent, OECD, WTO)

attributes <- read.csv("Country_Attributes_2005.csv", header=T) 

colnames(attributes)[1] <- "Country" #Correct name mistake on first column. 

attributes$GDP <- as.numeric(attributes$GDP)

attributes$Scaled_GDP <- rescale(attributes$GDP, c(7, 25)) #Scaled GDP for graphs - CHANGE FOR OTHER SCRIPT

str(attributes) 

# 2.2 - Incorporating these vertex attributes into Network 

V(network_2005)$GDP=attributes[match(V(network_2005)$name, attributes$Country), 
                               "Scaled_GDP"] 

V(network_2005)$True_GDP=attributes[match(V(network_2005)$name, attributes$Country), 
                                    "GDP"] 

V(network_2005)$Region=factor(attributes[match(V(network_2005)$name, attributes$Country),
                                         "Region"]) 

V(network_2005)$Income = factor(attributes[match(V(network_2005)$name, attributes$Country), 
                                           "Income_Group"]) 

V(network_2005)$OECD = factor(attributes[match(V(network_2005)$name, attributes$Country),
                                         "OECD_Membership"]) 


#####----- Section 3: Cleaning Up the Network -------

#   3.1.1 - COLOR - Assigning colors to vertices according to region

colorscheme <- brewer.pal(7, "Spectral") 

V(network_2005)$color=colorscheme[as.numeric(V(network_2005)$Region)] # One color per region


#   3.1.2 - Plot of network colored coded by region and sized by GDP

set.seed(1)

png(filename="2005 World Trade Network.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

plot(network_2005, vertex.frame.color = NA, vertex.size = V(network_2005)$GDP,
     vertex.label.cex = .7, edge.arrow.size = .1, edge.width = 2,
     layout = fruch_layout)

dev.off()


# 3.2 - ALT - Color coded according to OECD membership

svg("2005 World Trade Network OECD.svg")

colorscheme_OECD <- brewer.pal(3, "Paired")

V(network_2005)$color=colorscheme_OECD[as.numeric(V(network_2005)$OECD)] # Color according to OECD membership


plot(network_2005, vertex.frame.color = NA, vertex.size = V(network_2005)$GDP,
     vertex.label.cex = .7, edge.arrow.size = .1, edge.width = 2,
     layout = fruch_layout, main = "2005 World Trade Network")

legend("bottomleft",legend = c("Non-OECD", "OECD"), 
       pch = 21, pt.bg = brewer.pal(3, "Paired"), cex = 0.6)

dev.off()

#####----- Section 4: Centrality Measures -------

betweenness=igraph::betweenness 

# 4.1 Network Wide Metrics: 

edge_density(network_2005) #print 

diameter(network_2005, directed = T) #print 

diam <- get.diameter(network_2005, directed = T)

# 4.2 Degree: 

#   4.2.1 - Calculating degrees 

in_degree <- degree(network_2005, mode = "in") 

out_degree <- degree(network_2005, mode = "out") 

all_degree <- degree(network_2005, mode = "all") #

#   4.2.2 Plot with size proportional to Out-Degree 

png(filename="2005 Outdegree.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

# Change color to region again
colorscheme <- brewer.pal(7, "Spectral") 

V(network_2005)$color=colorscheme[as.numeric(V(network_2005)$Region)] # One color per region

plot(network_2005, vertex.frame.color = NA, vertex.size = out_degree,
     vertex.label.cex = .7, edge.arrow.size = .1, edge.width = 2, layout = fruch_layout)

dev.off()

#   4.2.2 Plot with size proportional to In-Degree 

png(filename="2005 Indegree.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

plot(network_2005, vertex.frame.color = NA, vertex.size = in_degree,
     vertex.label.cex = .7, edge.arrow.size = .1, edge.width = 2, layout = fruch_layout)

dev.off()


# 4.3 Betweeness Centrality:

bet_cent <- betweenness(network_2005, directed = T) 

#   4.3.2 - Plot with size proportional to betweeness centrality 

png(filename="2005 Betweeness.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

plot(network_2005, vertex.frame.color = NA, vertex.size = rescale(bet_cent, c(7, 30)),
     vertex.label.cex = .7, edge.arrow.size = .1, edge.width = 2, layout = fruch_layout)

dev.off()


#   ALT - Undirected betweeness Centrality

# Create Undirected Network for Community Detection 

undirected_network_2005 <- graph_from_adjacency_matrix(matrix_2005,
                                                       mode = "undirected", diag = F) # Undirected Network


undirected_network_2005 <- delete.vertices(undirected_network_2005,
                                           which(degree(undirected_network_2005)==0)) #delete isolated nodes

undirected_bet_cent <- betweenness(undirected_network_2005, directed = F)

undirected_fruch_layout <- layout_with_fr(undirected_network_2005)

# Color

colorscheme <- brewer.pal(7, "Spectral") 

V(undirected_network_2005)$color=colorscheme[as.numeric(V(network_2005)$Region)] # One color per region

# Plot network 

png(filename="2005 Undirected Betweeness.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

plot(undirected_network_2005, vertex.size = undirected_bet_cent/20, vertex.frame.color = NA,
     vertex.label.cex = .7, edge.arrow.size = .1, edge.width = 2, layout = undirected_fruch_layout)

dev.off()

# 4.4 Eigenvector Centrality:

#   4.4.1 Directed Eigenvector Centrality (Katz/Alpha Centrality)

katz <- alpha.centrality(network_2005, nodes= V(network_2005), alpha = .3)

plot(network_2005, vertex.frame.color = NA, vertex.size = katz*5,
     vertex.label.cex = .7, edge.arrow.size = .1, edge.width = 2, layout = fruch_layout,
     main = "2005 World Trade Network sized by Katz Centrality")

legend("bottomleft",legend = c("East Asia", "Europe", "Latin America", "North Africa", "North America", "Rest of the World", "South Asia"), 
       pch = 21,pt.bg = brewer.pal(7, "Spectral"), cex = .6)

#   4.4.2 Undirected Eigenvector Centrality 

undirected_eigen <- eigen_centrality(undirected_network_2005, scale = T) 
#eigenvector. It is a list. Must extract vector for plotting. eigen$vector 

plot(undirected_network_2005, vertex.frame.color = NA, vertex.size = undirected_eigen$vector*20,
     vertex.label.cex = .7, edge.arrow.size = .1, edge.width = 2, layout = fruch_layout)


#####----- Section 5: Community Detection and Assortativity -------

# 5.1 - Community Detection using betweeness algorithm

#community detection. Cannot get it to work for directed graph

png(filename="2005 Community.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

bet_community <- edge.betweenness.community(undirected_network_2005) 

plot(bet_community, undirected_network_2005, layout = fruch_layout) #plot communities 

dev.off()

# 5.2 - Assortativity 

#   5.2.1 - Assortativty by region

assortativity_nominal(network_2005, V(network_2005)$Region) 

#   5.2.2 - Assortativity by GDP 

assortativity(network_2005, V(network_2005)$GDP)

assortativity_nominal(network_2005, V(network_2005)$Income)

#   5.2.3 - Assortativity by OECD Membership

assortativity_nominal(network_2005, V(network_2005)$OECD)


#####----- Section 6: Hubs and Authorities ----------

# 6.1 Hubs  

png(filename="2005 Hub.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

hubs <- hub_score(network_2005, weights = NA)$vector

plot(network_2005, vertex.frame.color = NA, vertex.size = hubs*20,
     vertex.label.cex = .7, edge.arrow.size = .1, edge.width = 2, layout = fruch_layout)

dev.off()

# 6.2 Authorities

png(filename="2005 Authority.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

authorities <- authority_score(network_2005, weights = NA)$vector 

plot(network_2005, vertex.frame.color = NA, vertex.size = authorities*20,
     vertex.label.cex = .7, edge.arrow.size = .1, edge.width = 2, layout = fruch_layout)

dev.off()


#####----- Section 7: Centrality vs. GDP Plots----------

# 7.1 Assembling a dataset of node-level measures 

Country <- V(network_2005)$name 
Country_GDP <- V(network_2005)$True_GDP 
Country_Scaled_GDP <- V(network_2005)$GDP
Country_Region <-attributes[match(Country, attributes$Country), 
                            "Region"]
com_membership <- factor(membership(bet_community))

net_data_2005 <- tibble(year = 2005,
                        Country = Country, 
                        Country.GDP = Country_GDP,
                        Region = Country_Region,
                        Country.Scaled.GDP = Country_Scaled_GDP,
                        in.degree = in_degree, 
                        out.degree = out_degree, 
                        degree = all_degree,
                        betweeness = bet_cent,
                        eigenvector.cent = undirected_eigen$vector,  
                        katz.cent = katz, 
                        hub.level = hubs, 
                        authority.level = authorities,
                        community = com_membership)


net_data_2005$Country.Scaled.GDP <- rescale(net_data_2005$Country.Scaled.GDP, c(0,13)) 
#RESCALE GDP FOR EACH YEAR. IMPORTANT. This rescales to trillions.

write.csv(net_data_2005, "Network Data 2005.csv")

#####------ Section 8 - Linear Models  --------######

attach(net_data_2005)

# 8.1 Outdegree ~ GDP 

svg("Outdegree 2005 Reg Diagnostics.svg")

outdegree_lm = lm(log(Country.GDP) ~ out.degree)

summary(outdegree_lm) #results 

par(mfrow = c(2,2)) #splits the diagnostic plot functions for lm 
plot(outdegree_lm) 

dev.off()

# Normal Plot to check 

par(mfrow = c(1,1))
plot(out.degree, log(Country.GDP)) 

# 8.2 Indegree ~ GDP 

indegree_lm = lm(log(Country.GDP) ~ in.degree) #USED LOG to deal with outliers

summary(indegree_lm) #results 

svg("Indegree 2005 Reg Diagnostics.svg")

par(mfrow = c(2,2)) #splits the diagnostic plot functions for lm 
plot(indegree_lm) 

dev.off()

#check

par(mfrow = c(1,1))
plot(in.degree, log(Country.GDP)) 


# 8.4 Katz Centrality ~ GDP

katz_lm = lm(log(Country.GDP) ~ katz.cent)

summary(katz_lm)

svg("Katz reg 2005 diagnostics.svg")

par(mfrow = c(2,2))
plot(katz_lm)

dev.off()

par(mfrow = c(1,1))
plot(katz.cent, log(Country.GDP)) 

# 8.5 Betwenness Centrality ~ GDP

bet_lm = lm(log(Country.GDP) ~ betweeness)

summary(bet_lm)

svg("Betweness reg 2005 diagnostics.svg")

par(mfrow = c(2,2))
plot(bet_lm)

dev.off()

par(mfrow = c(1,1))
plot(betweeness, log(Country.GDP)) 


# 8.6 Hub ~ GDP

hub_lm = lm(log(Country.GDP) ~ hub.level)

summary(hub_lm)

svg("Hub 2005 reg diagnostics.svg")

par(mfrow = c(2,2))
plot(hub_lm)

dev.off()

par(mfrow = c(1,1))
plot(hub.level, log(Country.GDP)) 


# 8.7 Authority ~ GDP

aut_lm = lm(log(Country.GDP) ~ authority.level)

summary(aut_lm)

svg("authority reg 2005 diagnostics.svg")

par(mfrow = c(2,2))
plot(aut_lm)

dev.off()

par(mfrow = c(1,1))
plot(authority.level, Country.GDP)  

# 8.8 Exporting Regression Output into an HTML File 

stargazer(outdegree_lm, indegree_lm, bet_lm, katz_lm, hub_lm, aut_lm, type="html",
          dep.var.labels=c("Log(Gross Domestic Product)"),
          covariate.labels=c("Outdegree","Indegree","Betweeness Centrality",
                             "Katz Centrality","Hub Centrality", "Authority Centrality"),
          out="models.htm")


#####------ Section 9 - GGPlots --------######

#9.1 Out Degree and GDP

options(scipen = 999) ### Turn off scientific notation 

OutDegree_v_GDP <- ggplot(net_data_2005, aes(x = out.degree, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme  
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Out Degree & GDP for World Trade Network 2005",
       x="Out Degree", y="Gross Domestic Product") # Labels

OutDegree_v_GDP_F <- OutDegree_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                             axis.text.x=element_text(size=10), 
                                             axis.text.y=element_text(size=10),
                                             axis.title.x=element_text(size=20),
                                             axis.title.y=element_text(size=20)) 

OutDegree_v_GDP_F # Print plot

ggsave("Outdegree GDP Plot 2005.svg", OutDegree_v_GDP_F, device = svg,
       height = 5, width = 10) #save


# 9.2 In Degree and GDP

InDegree_v_GDP <- ggplot(net_data_2005, aes(x = in.degree, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="In Degree & GDP for World Trade Network 2005",
       x="In Degree", y="Gross Domestic Product") # Labels

InDegree_v_GDP_F <- InDegree_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                           axis.text.x=element_text(size=10), 
                                           axis.text.y=element_text(size=10),
                                           axis.title.x=element_text(size=20),
                                           axis.title.y=element_text(size=20)) 

InDegree_v_GDP_F  # print plot


ggsave("Indegree GDP Plot 2005.svg", InDegree_v_GDP_F, device = svg,
       height = 5, width = 10) #save

# 9.3 Katz Centrality and GDP 

Katz_v_GDP <- ggplot(net_data_2005, aes(x = katz, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Katz Centrality & GDP for World Trade Network 2005",
       x="Katz Centrality", y="Gross Domestic Product") # Labels

Katz_v_GDP_F <- Katz_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                   axis.text.x=element_text(size=10), 
                                   axis.text.y=element_text(size=10),
                                   axis.title.x=element_text(size=20),
                                   axis.title.y=element_text(size=20)) 

Katz_v_GDP_F  # Print Plot


ggsave("Katz GDP Plot 2004.svg", Katz_v_GDP_F, device = svg,
       height = 5, width = 10) #save

# 9.4 Eigenvector Centrality and GDP


Eigen_v_GDP <- ggplot(net_data_2005, aes(x = eigenvector.cent, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Eigenvector Centrality & GDP for World Trade Network 2005",
       x="Eigenvector Centrality", y="Gross Domestic Product") # Labels

Eigen_v_GDP_F <- Eigen_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                     axis.text.x=element_text(size=10), 
                                     axis.text.y=element_text(size=10),
                                     axis.title.x=element_text(size=20),
                                     axis.title.y=element_text(size=20)) 

Eigen_v_GDP_F #Print 


# 9.5 Betwenness Centrality and GDP 

Bet_v_GDP <- ggplot(net_data_2005, aes(x = betweeness, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Betweeness Centrality & GDP for World Trade Network 2005", 
       x="Betweeness Centrality", y="Gross Domestic Product") # Labels

Bet_v_GDP_F <- Bet_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                 axis.text.x=element_text(size=10), 
                                 axis.text.y=element_text(size=10),
                                 axis.title.x=element_text(size=20),
                                 axis.title.y=element_text(size=20)) 

Bet_v_GDP_F # Print 

ggsave("Betweenesss GDP Plot 2005.svg", Bet_v_GDP_F, device = svg,
       height = 5, width = 10) #save


# 9.6 Hub Centrality and GDP 

Hub_v_GDP <- ggplot(net_data_2005, aes(x = hub.level, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Hub Centrality & GDP for World Trade Network 2005", 
       x="Hub Centrality", y="Gross Domestic Product") # Labels

Hub_v_GDP_F <- Hub_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                 axis.text.x=element_text(size=10), 
                                 axis.text.y=element_text(size=10),
                                 axis.title.x=element_text(size=20),
                                 axis.title.y=element_text(size=20)) 

Hub_v_GDP_F #Print 


ggsave("Hub GDP Plot 2005.svg", Hub_v_GDP_F, device = svg,
       height = 5, width = 10) #save

# 9.7 Authority Centrality and GDP 

Aut_v_GDP <- ggplot(net_data_2005, aes(x = authority.level, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Authority Centrality & GDP for World Trade Network 2005", 
       x="Authority Centrality", y="Gross Domestic Product") # Labels

Aut_v_GDP_F <- Aut_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                 axis.text.x=element_text(size=10), 
                                 axis.text.y=element_text(size=10),
                                 axis.title.x=element_text(size=20),
                                 axis.title.y=element_text(size=20)) 

Aut_v_GDP_F # Print 

ggsave("Authority GDP Plot 2005.svg", Aut_v_GDP_F, device = svg,
       height = 5, width = 10) #save

#####---- Section 10 - Other Regression GGPlots-----####

#10.1 Out Degree and GDP

options(scipen = 999) ### Turn off scientific notation 

OutDegree_v_GDP <- ggplot(net_data_2005, aes(x = out.degree, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=out.degree, y=log(Country.GDP))) + # Regression Line
  theme_light() + # Theme  
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Out-Degree & GDP Regression for World Trade Network 2005", 
       x="Out-Degree", y="Log(Gross Domestic Product)") # Labels

OutDegree_v_GDP_F <- OutDegree_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                             axis.text.x=element_text(size=10), 
                                             axis.text.y=element_text(size=10),
                                             axis.title.x=element_text(size=20),
                                             axis.title.y=element_text(size=20)) 

OutDegree_v_GDP_F # Print plot

ggsave("Outdegree GDP Reg 2005.svg", OutDegree_v_GDP_F, device = svg,
       height = 5, width = 10) #save

# 10.2 In Degree and GDP

InDegree_v_GDP <- ggplot(net_data_2005, aes(x = in.degree, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=in.degree, y=log(Country.GDP))) + #Regression Line
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="In-Degree & GDP Regression for World Trade Network 2005", 
       x="In-Degree", y="Log(Gross Domestic Product)") # Labels

InDegree_v_GDP_F <- InDegree_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                           axis.text.x=element_text(size=10), 
                                           axis.text.y=element_text(size=10),
                                           axis.title.x=element_text(size=20),
                                           axis.title.y=element_text(size=20)) 

InDegree_v_GDP_F  # print plot

ggsave("Indegree GDP Reg 2005.svg", InDegree_v_GDP_F, device = svg,
       height = 5, width = 10) #save


# 10.3 Katz Centrality and GDP 

Katz_v_GDP <- ggplot(net_data_2005, aes(x = katz, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=katz, y=log(Country.GDP))) + # Regression Line
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Katz Centrality & GDP Regression for World Trade Network 2005", x="Katz Centrality",
       y="Log(Gross Domestic Product)") # Labels

Katz_v_GDP_F <- Katz_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                   axis.text.x=element_text(size=10), 
                                   axis.text.y=element_text(size=10),
                                   axis.title.x=element_text(size=20),
                                   axis.title.y=element_text(size=20)) 

Katz_v_GDP_F  # Print Plot

ggsave("Katz GDP Reg 2005.svg", Katz_v_GDP_F, device = svg,
       height = 5, width = 10) #save

# 10.4 Betwenness Centrality and GDP 

Bet_v_GDP <- ggplot(net_data_2005, aes(x = betweeness, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=betweeness, y=log(Country.GDP))) + # Regression Line
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Betweeness Centrality & GDP for World Trade Network 2005", x="Betweeness Centrality",
       y="log(Gross Domestic Product)") # Labels

Bet_v_GDP_F <- Bet_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                 axis.text.x=element_text(size=10), 
                                 axis.text.y=element_text(size=10),
                                 axis.title.x=element_text(size=20),
                                 axis.title.y=element_text(size=20)) 

Bet_v_GDP_F # Print 

ggsave("Betweeness GDP Reg 2005.svg", Bet_v_GDP_F, device = svg,
       height = 5, width = 10) #save

# 10.5 Hub Centrality and GDP 

Hub_v_GDP <- ggplot(net_data_2005, aes(x = hub.level, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=hub.level, y=log(Country.GDP))) + # Regression Line
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Hub Centrality & GDP Regression for World Trade Network 2005", x="Hub Centrality",
       y="log(Gross Domestic Product)") # Labels

Hub_v_GDP_F <- Hub_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                 axis.text.x=element_text(size=10), 
                                 axis.text.y=element_text(size=10),
                                 axis.title.x=element_text(size=20),
                                 axis.title.y=element_text(size=20)) 

Hub_v_GDP_F #Print 

ggsave("Hub GDP Reg 2005.svg", Hub_v_GDP_F, device = svg,
       height = 5, width = 10) #save


# 10.6 Authority Centrality and GDP 

Aut_v_GDP <- ggplot(net_data_2005, aes(x = authority.level, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=authority.level, y=log(Country.GDP))) + # Regression Line
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(title="Authority Centrality & GDP Regression for World Trade Network 2005", 
       x="Authority Centrality", y="log(Gross Domestic Product)") # Labels

Aut_v_GDP_F <- Aut_v_GDP + theme(plot.title=element_text(size=20, face="bold"), 
                                 axis.text.x=element_text(size=10), 
                                 axis.text.y=element_text(size=10),
                                 axis.title.x=element_text(size=20),
                                 axis.title.y=element_text(size=20)) 

Aut_v_GDP_F # Print 

ggsave("Athority GDP Reg 2005.svg", Aut_v_GDP_F, device = svg,
       height = 5, width = 10) #save


######------Section 11 - GGPlot Histograms--------#####

# 11.1 - In-degree Histogram

In_Degree_Hist <- ggplot(data=net_data_2005, aes(in.degree)) + 
  geom_histogram(breaks=seq(0, 10, by = 1), col="black", aes(fill=..count..)) + 
  labs(x = "In-Degree", y = "Number of Countries") + 
  xlim(c(0,10)) + 
  ylim(c(0,20)) +
  guides(fill = FALSE)

In_Degree_Hist <- In_Degree_Hist + theme(plot.title=element_text(size=20, face="bold"), 
                                         axis.text.x=element_text(size=10), 
                                         axis.text.y=element_text(size=10),
                                         axis.title.x=element_text(size=20),
                                         axis.title.y=element_text(size=20)) 

In_Degree_Hist #print 

ggsave("Indegree Hist 2005.png", In_Degree_Hist, device = png,
       height = 5, width = 10) #save

# 11.2 - Out-degree Histogram 

Out_Degree_Hist <- ggplot(data=net_data_2005, aes(out.degree)) + 
  geom_histogram(breaks=seq(0, 30, by = 1), col="black", aes(fill=..count..)) + 
  guides(fill = FALSE) + 
  labs(x = "Out-Degree", y = "Number of Countries") + 
  xlim(c(0,30)) + 
  ylim(c(0,5))

Out_Degree_Hist <- Out_Degree_Hist + theme(plot.title=element_text(size=20, face="bold"), 
                                           axis.text.x=element_text(size=10), 
                                           axis.text.y=element_text(size=10),
                                           axis.title.x=element_text(size=20),
                                           axis.title.y=element_text(size=20))

Out_Degree_Hist #print 

ggsave("Outdegree Hist 2005.svg", Out_Degree_Hist, device = svg,
       height = 5, width = 10) #save


# 11.3 - Katz Centrality Histogram

Katz_Degree_Hist <- ggplot(data=net_data_2005, aes(katz.cent)) + 
  geom_histogram(breaks=seq(0, 30, by = 1), col="black", aes(fill=..count..)) + 
  guides(fill = FALSE) + 
  labs(x = "Katz Centrality", y = "Number of Countries") + 
  xlim(c(0,10)) + 
  ylim(c(0,20))

Katz_Degree_Hist <- Katz_Degree_Hist + theme(plot.title=element_text(size=20, face="bold"), 
                                             axis.text.x=element_text(size=10), 
                                             axis.text.y=element_text(size=10),
                                             axis.title.x=element_text(size=20),
                                             axis.title.y=element_text(size=20))

Katz_Degree_Hist #print

ggsave("Katz Hist 2005.svg", Katz_Degree_Hist, device = svg,
       height = 5, width = 10) #save


# 11.4 - Authority Histogram 

Aut_Degree_Hist <- ggplot(data=net_data_2005, aes(authority.level)) + 
  geom_histogram(breaks=seq(0, 1, by = .1), col="black", aes(fill=..count..)) + 
  guides(fill = FALSE) + 
  labs(x = "Authority Index", y = "Number of Countries") + 
  xlim(c(0,1)) + 
  ylim(c(0,20))

Aut_Degree_Hist <- Aut_Degree_Hist + theme(plot.title=element_text(size=20, face="bold"), 
                                           axis.text.x=element_text(size=10), 
                                           axis.text.y=element_text(size=10),
                                           axis.title.x=element_text(size=20),
                                           axis.title.y=element_text(size=20))

Aut_Degree_Hist #print

ggsave("Authority Hist 2005.svg", Aut_Degree_Hist, device = svg,
       height = 5, width = 10) #save


# 11.5 - Hub Histogram 

Hub_Degree_Hist <- ggplot(data=net_data_2005, aes(hub.level)) + 
  geom_histogram(breaks=seq(0, 1, by = .1), col="black", aes(fill=..count..)) + 
  guides(fill = FALSE) + 
  labs(x = "Hub Index", y = "Number of Countries") + 
  xlim(c(0,1)) + 
  ylim(c(0,5))


Hub_Degree_Hist <- Hub_Degree_Hist + theme(plot.title=element_text(size=20, face="bold"), 
                                           axis.text.x=element_text(size=10), 
                                           axis.text.y=element_text(size=10),
                                           axis.title.x=element_text(size=20),
                                           axis.title.y=element_text(size=20))

Hub_Degree_Hist #print

ggsave("Hub Hist 2005.svg", Hub_Degree_Hist, device = svg,
       height = 5, width = 10) #save

