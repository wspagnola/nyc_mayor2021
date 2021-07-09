
# Create Sankey Diagram for Rounds 6-8 of 2021 NYC Mayoral Democratic Primary 

## Use rcv_links.csv, rcv_nodes.csv create by data_extract.R script

library(networkD3)


#### Output Sankey Network Datasets ####
rcv_links <- read.csv("data/rcv_links.csv")
rcv_nodes <- read.csv("data/rcv_nodes.csv")


#### Create Sankey Diagram ####

# Define colors 
color <- "d3.scaleOrdinal() .range(['#1f77b4',  '#d62728', '#ff7f0e', '#2ca02c','#808080'])"

# Create Sankey Diagram
sankeyNetwork(Links= rcv_links,
              Nodes = rcv_nodes,
              Source = "source",
              Target = "target",
              Value = "vote",
              NodeID = "nodeID", 
              NodeGroup = "nodeGroup",
              LinkGroup = 'linkGroup0',
              sinksRight = FALSE,
              fontSize = 14,
              nodeWidth = 30, 
              colourScale = color)
