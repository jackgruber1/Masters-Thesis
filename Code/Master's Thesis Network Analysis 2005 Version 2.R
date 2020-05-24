#Jeancarlo Velez
#3/15/2020
#Master's Thesis Netowork Analysis 

#preliminaries
#installing the network packages the packages 
#install.packages(c("igraph", "bipartite", "asnipe", 
# "assortnet", "ggplot2", "ggmap", "rnetcarto", "ecodist",
# "igraphdata", "statnet", "RColorBrewer", "tidyverse"))

#load packages
library(igraph)
library(ggplot2)
library(tidyverse)
library(statnet)
library(readxl)
library(RColorBrewer)
library(scales)
library(stargazer)
library(Cairo)

#setting up workspace
rm(list = ls())
setwd('C:/Users/jcarl/OneDrive/Desktop/Working Space 2005') #change if need be
#------------------------------------------------------------------------------------------------

#####----- Section 1: Forming the Trade Network -------

set.seed(1)

# 1.1 - Upload adjacency matrix to R

tech <- read.csv("2005_TechMatrix.csv", header=T, row.names = 1) #upload 2005 technology matrix 

matrix <- as.matrix(tech) #coerce into matrix 

# 1.2 - Form Directed graph object 

degree = igraph::degree
delete.vertices = igraph::delete.vertices

net <- graph_from_adjacency_matrix(matrix, mode = "directed", diag = F) 

net <- delete.vertices(net, which(degree(net)==0)) # delete isolated vertices

fruch_layout = layout_with_fr(net) # layout 

# 1.3 - Plot graph to test 

plot(net, layout = fruch_layout) 


#####----- Section 2: Uploading Other Attributes -------

# 2.1 Uploading dataframe with alt attributes (GDP, Continent, OECD, WTO)

attributes <- read.csv("Country_Attributes_2005.csv", header=T) 

colnames(attributes)[1] <- "Country" #Correct name mistake on first column. 

attributes$Scaled_GDP <- rescale(attributes$GDP, c(7,25)) #Scaled GDP for graphs - CHANGE FOR OTHER SCRIPT

str(attributes) 

# 2.2 - Incorporating these vertex attributes into Network 

V(net)$GDP=attributes[match(V(net)$name, attributes$Country), 
                      "Scaled_GDP"] 

V(net)$True_GDP=attributes[match(V(net)$name, attributes$Country), 
                           "GDP"] 

V(net)$Region=factor(attributes[match(V(net)$name, attributes$Country),
                                "Region"]) 

V(net)$Income = factor(attributes[match(V(net)$name, attributes$Country), 
                                  "Income_Group"]) 

V(net)$OECD = factor(attributes[match(V(net)$name, attributes$Country),
                                "OECD_Membership"]) 


#####----- Section 3: Cleaning Up the Network -------

#   3.1.1 - COLOR - Assigning colors to vertices according to region

colorscheme <- brewer.pal(7, "Spectral") 

V(net)$color=colorscheme[as.numeric(V(net)$Region)] # One color per region


#   3.1.2 - Plot of network colored coded by region and sized by GDP

set.seed(1)

png(filename="2005 World Trade Network.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

# Size by GDP/ Divide by high number to make the countries visible.
#All years will be divided by this number so that they are comparable
plot(net, 
     vertex.frame.color = NA, 
     vertex.size = V(net)$True_GDP/10^11.65, 
     vertex.label.cex = .7,
     edge.arrow.size = 0.2, 
     edge.width = 2,
     layout = fruch_layout,
     margin = -0.1)

dev.off()

# Plotting a legend separately

png(filename="Region Legend.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

plot(NULL)

legend("topleft", 
       legend =c("East Asia", "Europe", "Latin America", "North Africa", 
                 "North America", "Rest of the World", "South Asia"), 
       pch=21, pt.cex=1, cex=1.1, 
       bty='n', 
       pt.bg = brewer.pal(7, "Spectral"))

mtext("Region", at=0.1, cex=1.5)

dev.off()

# 3.2 - ALT - Color coded according to OECD membership

svg("2005 World Trade Network OECD.svg")

colorscheme_OECD <- brewer.pal(3, "Paired")

V(net)$color=colorscheme_OECD[as.numeric(V(net)$OECD)] # Color according to OECD membership


plot(net,
     vertex.frame.color = NA,
     vertex.size = V(net)$True_GDP/10^11.65,
     vertex.label.cex = .7, 
     edge.arrow.size = 0.2, 
     edge.width = 2,
     layout = fruch_layout, 
     main = "2005 World Trade Network",
     margin = -0.1)

legend("bottomleft",legend = c("Non-OECD", "OECD"), 
       pch = 21, pt.bg = brewer.pal(3, "Paired"), cex = 0.6)

dev.off()

#####----- Section 4: Centrality Measures -------

betweenness=igraph::betweenness 

# 4.1 Network Wide Metrics: 

edge_density(net) #print 

diameter(net, directed = T) #print 

diam <- get.diameter(net, directed = T)

# 4.2 Degree: 

#   4.2.1 - Calculating degrees 

in_degree <- degree(net, mode = "in") 

out_degree <- degree(net, mode = "out") 

all_degree <- degree(net, mode = "all") #

#   4.2.2 Plot with size proportional to Out-Degree 

set.seed(1)

png(filename="2005 Outdegree.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

# Change color to region again
colorscheme <- brewer.pal(7, "Spectral") 

V(net)$color = colorscheme[as.numeric(V(net)$Region)] # One color per region

plot(net,
     vertex.frame.color = NA,
     vertex.size = out_degree,
     vertex.label.cex = .7,
     edge.arrow.size = .2,
     edge.width = 2, 
     layout = fruch_layout,
     margin = -0.1)

dev.off()

#   4.2.2 Plot with size proportional to In-Degree 

set.seed(1)

png(filename="2005 Indegree.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

plot(net, 
     vertex.frame.color = NA, 
     vertex.size = in_degree,
     vertex.label.cex = .7, 
     edge.arrow.size = 0.2,
     edge.width = 2, 
     layout = fruch_layout,
     margin = -0.1)

dev.off()

# 4.3 Betweeness Centrality:

bet_cent <- betweenness(net, directed = T) 

#   4.3.2 - Plot with size proportional to betweeness centrality 

set.seed(1)

png(filename="2005 Betweeness.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

plot(net,
     vertex.frame.color = NA,
     vertex.size = bet_cent/6, #repeat on other years
     vertex.label.cex = .7,
     edge.arrow.size = 0.2, 
     edge.width = 2, 
     layout = fruch_layout,
     margin = -0.1)

dev.off()


#   ALT - Undirected betweeness Centrality

# Create Undirected Network for Community Detection 

undirected_net <- graph_from_adjacency_matrix(matrix,
                                              mode = "undirected", diag = F) # Undirected Network


undirected_net <- delete.vertices(undirected_net,
                                  which(degree(undirected_net)==0)) #delete isolated nodes

undirected_bet_cent <- betweenness(undirected_net, directed = F)

undirected_fruch_layout <- layout_with_fr(undirected_net)

# Color

colorscheme <- brewer.pal(7, "Spectral") 

V(net)$color=colorscheme[as.numeric(V(net)$Region)] # One color per region

# Plot network 

set.seed(1)

png(filename="2005 Undirected Betweeness.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

plot(undirected_net, 
     vertex.frame.color = NA,
     vertex.size = undirected_bet_cent/20,
     vertex.label.cex = .7, 
     edge.arrow.size = 0.2, 
     edge.width = 2,
     layout = undirected_fruch_layout,
     margin = -0.1)

dev.off()

# 4.4 Eigenvector Centrality:

#   4.4.1 Directed Eigenvector Centrality (Katz/Alpha Centrality)


katz <- alpha.centrality(net, nodes= V(net), alpha = .3)

plot(net, 
     vertex.frame.color = NA, 
     vertex.size = katz*5,
     vertex.label.cex = .7, 
     edge.arrow.size = 0.2,
     edge.width = 2, 
     layout = fruch_layout,
     main = "2005 World Trade Network sized by Katz Centrality")

dev.off()

#   4.4.2 Undirected Eigenvector Centrality 

undirected_eigen <- eigen_centrality(undirected_net, scale = T) 
#eigenvector. It is a list. Must extract vector for plotting. eigen$vector 

plot(undirected_net,
     vertex.frame.color = NA,
     vertex.size = undirected_eigen$vector*20,
     vertex.label.cex = .7,
     edge.arrow.size = 0.2,
     edge.width = 2,
     layout = fruch_layout,
     margin = -0.1)



#####----- Section 5: Community Detection and Assortativity -------

# 5.1 - Community Detection using betweeness algorithm

#community detection. Cannot get it to work for directed graph

set.seed(1)

png(filename="2005 Communities.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

bet_community <- edge.betweenness.community(undirected_net) 

plot(bet_community, undirected_net, 
     layout = fruch_layout,
     margin = -0.1)
#main = "2005 World Trade Network by Communities", 
#sub = "Communities Detection Based on Edge Betweeness - Newman & Girvan (2004)") 

dev.off()

# 5.2 - Assortativity 

#   5.2.1 - Assortativty by region

assortativity_nominal(net, V(net)$Region) 

#   5.2.2 - Assortativity by GDP 

assortativity(net, V(net)$GDP)

assortativity_nominal(net, V(net)$Income)

#   5.2.3 - Assortativity by OECD Membership

assortativity_nominal(net, V(net)$OECD)


#####----- Section 6: Hubs and Authorities ----------

# 6.1 Hubs  

set.seed(1)

png(filename="2005 Hubs.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

hubs <- hub_score(net, weights = NA)$vector

plot(net, 
     vertex.frame.color = NA, 
     vertex.size = hubs*20,
     vertex.label.cex = .7, 
     edge.arrow.size = 0.2, 
     edge.width = 2,
     layout = fruch_layout,
     margin = -0.1)

dev.off()

# 6.2 Authorities

set.seed(1)

png(filename="2005 authorities.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=8, 
    pointsize=24, 
    res=96)

authorities <- authority_score(net, weights = NA)$vector 

plot(net, 
     vertex.frame.color = NA, 
     vertex.size = authorities*20,
     vertex.label.cex = .7, 
     edge.arrow.size = 0.2, 
     edge.width = 2, 
     layout = fruch_layout,
     margin = -0.1)

dev.off()


#####----- Section 7: Centrality vs. GDP Plots----------

# 7.1 Assembling a dataset of node-level measures 

Country <- V(net)$name 
Country_GDP <- V(net)$True_GDP 
Country_Scaled_GDP <- V(net)$GDP
Country_Region <-attributes[match(Country, attributes$Country), 
                            "Region"]
com_membership <- factor(membership(bet_community))

net_data <- tibble(year = 2005,
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

max_GDP <- max(net_data$Country.GDP/10^12)
net_data$Country.Scaled.GDP <- rescale(net_data$Country.Scaled.GDP, c(0,max_GDP)) 
#RESCALE GDP FOR EACH YEAR. IMPORTANT. This rescales to trillions.

write.csv(net_data, "Network Data 2005.csv")




#####------ Section 8 - Linear Models  --------######

attach(net_data)

# 8.1 Outdegree ~ GDP 

png(filename="outdegree diag.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=10, 
    pointsize=24, 
    res=96)


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

png(filename="indegree diag.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=10, 
    pointsize=24, 
    res=96)

par(mfrow = c(2,2)) #splits the diagnostic plot functions for lm 
plot(indegree_lm) 

dev.off()

#check

par(mfrow = c(1,1))
plot(in.degree, log(Country.GDP)) 


# 8.4 Katz Centrality ~ GDP

katz_lm = lm(log(Country.GDP) ~ katz.cent)

summary(katz_lm)

png(filename="katz diag.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=10, 
    pointsize=24, 
    res=96)

par(mfrow = c(2,2))
plot(katz_lm)

dev.off()

par(mfrow = c(1,1))
plot(katz.cent, log(Country.GDP)) 

# 8.5 Betwenness Centrality ~ GDP

bet_lm = lm(log(Country.GDP) ~ betweeness)

summary(bet_lm)

png(filename="betweeness diag.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=10, 
    pointsize=24, 
    res=96)

par(mfrow = c(2,2))
plot(bet_lm)

dev.off()

par(mfrow = c(1,1))
plot(betweeness, log(Country.GDP)) 


# 8.6 Hub ~ GDP

hub_lm = lm(log(Country.GDP) ~ hub.level)

summary(hub_lm)

png(filename="hub diag.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=10, 
    pointsize=24, 
    res=96)

par(mfrow = c(2,2))
plot(hub_lm)

dev.off()

par(mfrow = c(1,1))
plot(hub.level, log(Country.GDP)) 


# 8.7 Authority ~ GDP

aut_lm = lm(log(Country.GDP) ~ authority.level)

summary(aut_lm)

png(filename="authority diag.png", 
    type="cairo",
    units="in", 
    width= 10, 
    height=10, 
    pointsize=24, 
    res=96)

par(mfrow = c(2,2))
plot(aut_lm)

dev.off()

par(mfrow = c(1,1))
plot(authority.level, Country.GDP)  

# 8.8 Exporting Regression Output into an HTML File 

stargazer(outdegree_lm, indegree_lm, bet_lm, katz_lm, hub_lm, aut_lm, 
          type="html",
          dep.var.labels=c("Log(Gross Domestic Product)"),
          covariate.labels=c("Outdegree","Indegree","Betweeness Centrality",
                             "Katz Centrality","Hub Centrality", "Authority Centrality"),
          out="models.htm")


#####------ Section 9 - GGPlots --------######

#9.1 Out Degree and GDP

options(scipen = 999) ### Turn off scientific notation 

OutDegree_v_GDP <- ggplot(net_data, aes(x = out.degree, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme  
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="Out Degree", y="Gross Domestic Product ($ Trillions)") +
  theme(legend.position = "none")# Labels

OutDegree_v_GDP_F <- OutDegree_v_GDP + 
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5)) 

OutDegree_v_GDP_F # Print plot

ggsave("Outdegree GDP Plot 2005.png", OutDegree_v_GDP_F) #save


# 9.2 In Degree and GDP

InDegree_v_GDP <- ggplot(net_data, aes(x = in.degree, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="In Degree", y="Gross Domestic Product ($ Trillions)") + 
  theme(legend.position = "none")# Labels

InDegree_v_GDP_F <- InDegree_v_GDP + 
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5)) 

InDegree_v_GDP_F  # print plot


ggsave("Indegree GDP Plot 2005.png", InDegree_v_GDP_F) #save

# 9.3 Katz Centrality and GDP 

Katz_v_GDP <- ggplot(net_data, aes(x = katz, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="Katz Centrality", y="Gross Domestic Product ($ Trillions)") + 
  theme(legend.position = "none")# Labels


Katz_v_GDP_F <- Katz_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Katz_v_GDP_F  # Print Plot


ggsave("Katz GDP Plot 2005.png", Katz_v_GDP_F) #save

# 9.4 Eigenvector Centrality and GDP

Eigen_v_GDP <- ggplot(net_data, aes(x = eigenvector.cent, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="Eigenvector Centrality", y="Gross Domestic Product ($ Trillions)") +
  theme(legend.position = "none")# Labels

Eigen_v_GDP_F <- Eigen_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Eigen_v_GDP_F #Print 


# 9.5 Betwenness Centrality and GDP 

Bet_v_GDP <- ggplot(net_data, aes(x = betweeness, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="Betweeness Centrality", y="Gross Domestic Product ($ Trillions)") +
  theme(legend.position = "none")# Labels

Bet_v_GDP_F <- Bet_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Bet_v_GDP_F # Print 

ggsave("Betweenesss GDP Plot 2005.png", Bet_v_GDP_F) #save


# 9.6 Hub Centrality and GDP 

Hub_v_GDP <- ggplot(net_data, aes(x = hub.level, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="Hub Index", y="Gross Domestic Product ($ Trillions)") +
  theme(legend.position = "none")

Hub_v_GDP_F <- Hub_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Hub_v_GDP_F #Print 

ggsave("Hub GDP Plot 2005.png", Hub_v_GDP_F) #save

# 9.7 Authority Centrality and GDP 

Aut_v_GDP <- ggplot(net_data, aes(x = authority.level, y = Country.Scaled.GDP)) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  theme_light() + # Theme
  scale_color_brewer(palette = "Spectral") + # Color Theme  
  labs(x="Authority Index", y="Gross Domestic Product ($ Trillions)") +
  theme(legend.position = "none")

Aut_v_GDP_F <- Aut_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Aut_v_GDP_F # Print 

ggsave("Authority GDP Plot 2005.png", Aut_v_GDP_F)

#####---- Section 10 - Other Regression GGPlots-----####

#10.1 Out Degree and GDP

options(scipen = 999) ### Turn off scientific notation 

OutDegree_v_GDP <- ggplot(net_data, aes(x = out.degree, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=out.degree, y=log(Country.GDP))) + # Regression Line
  theme_light() + # Theme  
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="Out Degree", y="Log(Gross Domestic Product)") +
  theme(legend.position = "none")

OutDegree_v_GDP_F <- OutDegree_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

OutDegree_v_GDP_F # Print plot

ggsave("Outdegree GDP Reg 2005.png", OutDegree_v_GDP_F)

# 10.2 In Degree and GDP

InDegree_v_GDP <- ggplot(net_data, aes(x = in.degree, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=in.degree, y=log(Country.GDP))) + #Regression Line
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="In Degree", y="Log(Gross Domestic Product)") +
  theme(legend.position = "none")

InDegree_v_GDP_F <- InDegree_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5)) 

InDegree_v_GDP_F  # print plot

ggsave("Indegree GDP Reg 2005.png", InDegree_v_GDP_F)


# 10.3 Katz Centrality and GDP 

Katz_v_GDP <- ggplot(net_data, aes(x = katz, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=katz, y=log(Country.GDP))) + # Regression Line
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="Katz Centrality", y="Log(Gross Domestic Product)") +
  theme(legend.position = "none")

Katz_v_GDP_F <- Katz_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Katz_v_GDP_F  # Print Plot

ggsave("Katz GDP Reg 2005.png", Katz_v_GDP_F)

# 10.4 Betwenness Centrality and GDP 

Bet_v_GDP <- ggplot(net_data, aes(x = betweeness, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=betweeness, y=log(Country.GDP))) + # Regression Line
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="Betweeness Centrality", y="log(Gross Domestic Product)") +
  theme(legend.position = "none")

Bet_v_GDP_F <- Bet_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Bet_v_GDP_F # Print 

ggsave("Betweeness GDP Reg 2005.png", Bet_v_GDP_F)

# 10.5 Hub Centrality and GDP 

Hub_v_GDP <- ggplot(net_data, aes(x = hub.level, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=hub.level, y=log(Country.GDP))) + # Regression Line
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="Hub Index", y="log(Gross Domestic Product)") +
  theme(legend.position = "none")

Hub_v_GDP_F <- Hub_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Hub_v_GDP_F #Print 

ggsave("Hub GDP Reg 2005.png", Hub_v_GDP_F)


# 10.6 Authority Centrality and GDP 

Aut_v_GDP <- ggplot(net_data, aes(x = authority.level, y = log(Country.GDP))) + # Data 
  geom_point(size = 3, aes(color = Region)) +  # Scatterplot specification
  stat_smooth(method = lm, aes(x=authority.level, y=log(Country.GDP))) + # Regression Line
  theme_light() + # Theme 
  scale_color_brewer(palette = "Spectral") + # Color Theme 
  labs(x="Authority Index", y="log(Gross Domestic Product)") +
  theme(legend.position = "none")

Aut_v_GDP_F <- Aut_v_GDP +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Aut_v_GDP_F # Print 

ggsave("Athority GDP Reg 2005.png", Aut_v_GDP_F)


######------Section 11 - GGPlot Histograms--------#####

# 11.1 - In-degree Histogram

In_Degree_Hist <- ggplot(data=net_data, aes(in.degree)) + 
  geom_histogram(breaks=seq(0, 10, by = 1), col="black", aes(fill=..count..)) + 
  theme(legend.position = "none") +
  labs(x = "In Degree", y = "Count") + 
  xlim(c(0,10)) + 
  ylim(c(0,20))

In_Degree_Hist <- In_Degree_Hist +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

In_Degree_Hist #print 

ggsave("Indegree Hist 2005.png", In_Degree_Hist)

# 11.2 - Out-degree Histogram 

Out_Degree_Hist <- ggplot(data=net_data, aes(out.degree)) + 
  geom_histogram(breaks=seq(0, 30, by = 1), col="black", aes(fill=..count..)) + 
  theme(legend.position = "none") + 
  labs(x = "Out Degree", y = "Count") + 
  xlim(c(0,30)) + 
  ylim(c(0,10))

Out_Degree_Hist <- Out_Degree_Hist +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Out_Degree_Hist #print 

ggsave("Outdegree Hist 2005.png", Out_Degree_Hist)


# 11.3 - Katz Centrality Histogram

Katz_Degree_Hist <- ggplot(data=net_data, aes(katz.cent)) + 
  geom_histogram(breaks=seq(0, 30, by = 1), col="black", aes(fill=..count..)) + 
  theme(legend.position = "none") 
labs(x = "Katz Centrality", y = "Count") + 
  xlim(c(0,30)) + 
  ylim(c(0,30))

Katz_Degree_Hist <- Katz_Degree_Hist +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Katz_Degree_Hist #print

ggsave("Katz Hist 2005.png", Katz_Degree_Hist)


# 11.4 - Authority Histogram 

Aut_Degree_Hist <- ggplot(data=net_data, aes(authority.level)) + 
  geom_histogram(breaks=seq(0, 1, by = .1), col="black", aes(fill=..count..)) + 
  theme(legend.position = "none")
labs(x = "Authority Index", y = "Count") + 
  xlim(c(0,1)) + 
  ylim(c(0,15))

Aut_Degree_Hist <- Aut_Degree_Hist +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Aut_Degree_Hist #print

# 11.5 - Hub Histogram 

Hub_Degree_Hist <- ggplot(data=net_data, aes(hub.level)) + 
  geom_histogram(breaks=seq(0, 1, by = .1), col="black", aes(fill=..count..)) + 
  theme(legend.position = "none")
labs(x = "Hub Index", y = "Count") + 
  xlim(c(0,1)) + 
  ylim(c(0,5))

Hub_Degree_Hist <- Hub_Degree_Hist +
  theme(plot.title=element_text(size=10, face="bold"), 
        axis.text.x=element_text(size=5), 
        axis.text.y=element_text(size=5))

Hub_Degree_Hist #print

ggsave("Hub Hist 2005.png", Hub_Degree_Hist) #save
