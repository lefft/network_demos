lefftpack::lazy_setup()

### CREATE FAKE NETWORK DATA --------------------------------------------------

# there will be `n_nodes`-many nodes in total 
n_nodes <- 25

# each node has a unique identifier 
node_ids <- paste0("node_", sprintf("%03d", 0:n_nodes))

# half of them will have children 
parent_nodes <- sample(node_ids, size=as.integer(n_nodes/2), replace=FALSE)


# the distribution of number of children is given by this table: 
freqs <- setNames(c(3, 2.5, 2, 1.5, 1, .4), as.character(1:6))
# which we normalize to get the number of nodes w `freq`-many children 
freqs <- freqs/sum(freqs) # round((freqs / sum(freqs)) * n_nodes)


# number of children that each parent will have 
child_distr <- sapply(parent_nodes, function(node){
  sample(as.numeric(names(freqs)), size=1, prob=freqs)
})

# function to generate list of children given a parent id 
generate_children <- function(parent, n_children, node_ids){
  children <- sample(node_ids[node_ids!=parent], size=n_children)
  dplyr::data_frame(parent = parent, child = children)
}

# create the edge list by calling `generate_children()` for each parent 
edge_list <- do.call("rbind", lapply(seq_along(child_distr), function(idx){
  generate_children(parent=names(child_distr)[idx], child_distr[idx], node_ids)
}))



write.csv(edge_list, "data/fake_network_edgelist.csv", row.names=FALSE)



### PLOT FAKE NETWORK DATA ----------------------------------------------------

# load network plotting dependencies + others after bc pipe namespace issue 
lefftpack::lazy_setup()
library("igraph"); library("networkD3")
source("functions.r")
edge_list <- read.csv("data/fake_network_edgelist.csv", stringsAsFactors=FALSE)
edge_list %>% 
  mutate(child=ifelse(child %in% c("node_005","node_009"), "seed", child)) %>%
  rename(subjid=child) %>% make_forceplot(random_sample=FALSE)



