#+include=FALSE
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
library("magrittr"); library("dplyr"); library("igraph"); library("networkD3")
#'

# read in data and produce a summary: 
# two cols; no na's; $suid4 is "subjid"; $recruiterid is "parent"; etc...
dat <- read.csv("data/<REDACTED>") %T>% (function(x){
  print(data.frame(
    coltypes  = sapply(x, class), 
    uniques   = sapply(x, function(y) length(unique(y))),
    missings  = colSums(is.na(x)), 
    selfloops = c(NA, sum(x[[1]]==x[[2]]))
  ))
})

# clean up dataset
dat %<>% 
  rename(subjid = suid4, parent = recruiterid) %>% 
  mutate_all(as.character) %>% 
  mutate(subjid = paste0("subj", subjid)) %>% 
  mutate(parent = ifelse(parent=="seed", parent, paste0("subj", parent))) %>% 
  select(parent, subjid) %>% 
  data.frame()

# get a record of who the seeds are, e.g. "subj1004"; "subj1019"
seeds <- dat$subjid[dat$parent=="seed"]

# get number of children per seed, display freq table
dat_seedchildren <- dat %>% filter(subjid %in% seeds) %>% group_by(subjid) %>% 
  summarize(num_children = n()) %>% data.frame() %T>% 
  (function(x)(print(table(x$num_children))))

# get number of children per parent, display freq table or barplot
dat_numchildren <- dat %>% group_by(parent) %>% summarize(
  num_children = length(subjid[subjid!=parent]),
  is_seed      = unique(subjid %in% seeds)
) %>% data.frame() %T>% (function(x)(print(table(x$num_children))))



#'## demos of d3 network plotting


#'#### example 1

# Create fake data
src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData)


#'#### example 2

URL <- paste0(
  "https://cdn.rawgit.com/christophergandrud/networkD3/",
  "master/JSONdata//flare.json")

## Convert to list format
Flare <- jsonlite::fromJSON(URL, simplifyDataFrame = FALSE)

# Use subset of data for more readable diagram
Flare$children = Flare$children[1:3]

radialNetwork(List = Flare, fontSize = 10, opacity = 0.9)

diagonalNetwork(List = Flare, fontSize = 10, opacity = 0.9)


#'#### example 3

hc <- hclust(dist(USArrests), "ave")

dendroNetwork(hc, height = 600)


#'#### misc...
# TIM ADD HERE TO SIMPLIFY
EL <- matrix(nc=3, byrow=TRUE, c(
  0,1,0, 0,2,2, 0,3,1, 1,2,0, 2,1,1, 2,3,1, 3,2,0, 0,4,1
)); EL[,1:2] <- EL[,1:2]+1 # necessary for js 0-indexing(?)

LINKS = data.frame(EL[,1:2])-1; names(LINKS) = c("source","target"); LINKS$value = 1

NODES = data.frame(unique(c(LINKS$target, LINKS$source))); names(NODES) = "name"
NODES$group = c(1,2,2,1,3); NODES$name <- c("one","two","three","four","zero")

print(NODES); print(LINKS)

forceNetwork(Links = LINKS, Nodes = NODES, Source = "source", Target = "target",
             Value = "value",   # links$value -- just 1's (not sure wha do)
             NodeID = "name",   # nodes$name -- gives label
             Group = "group",   # nodes$group -- gives color
             opacity = 0.8, fontSize = 75)



#'## first attempt at a plot

## SEEMS LIKE IT MAY NOT BE NECESSARY TO CONVERT TO CHARACTER -- REVISIT...

# vector of nodes, w/o "subj" (numeric)
nodes <- union(dat$parent, dat$subjid) %>% setdiff("seed") %>% 
  unique() %>% (function(x)gsub("subj", "", x)) %>% as.numeric() %>% sort()

# add ranks to use as lkup table
names(nodes) <- rank(nodes)

# vector of seeds, as numeric also
seeds <- sort(as.numeric(gsub("subj", "", dat$subjid[dat$parent=="seed"])))

# two cols: label==subjid and group=is_seed
nodes_df <- data.frame(
  label=nodes, 
  group=ifelse(nodes %in% seeds, 1, 0)
)

# three cols: parent, child, value
edges_df <- dat %>% 
  filter(parent!="seed") %>% 
  mutate(value = 1) %>% 
  mutate(parent = as.numeric(gsub("subj", "", parent))) %>% 
  mutate(subjid = as.numeric(gsub("subj", "", subjid))) %>% 
  data.frame()

edges_df$subj_index <- as.numeric(names(nodes[match(edges_df$subjid, nodes)]))-1
edges_df$par_index  <- as.numeric(names(nodes[match(edges_df$parent, nodes)]))-1

# EDGES <- edges_df %>% filter(parent %in% seeds)
# NODES <- nodes_df %>% filter(label %in% c(EDGES$parent, EDGES$subjid))

(blaowwie <- 
forceNetwork(Links = edges_df, 
             Nodes = nodes_df, 
             Source = "par_index", # 
             Target = "subj_index", # 
             Value = "value",   # links$value -- just 1's (not sure wha do)
             NodeID = "label",   # nodes$name -- gives label
             Group = "group",   # nodes$group -- gives color
             opacity = 0.8, fontSize = 10))


# save it as standalong html...
if (FALSE) {saveNetwork(network=blaowwie, file = 'blaowwie.html')}





