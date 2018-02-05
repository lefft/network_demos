# load network plotting dependencies + others after bc pipe namespace issue 
library("igraph"); library("networkD3"); library("magrittr"); library("dplyr") 

# load general plotting stuff
library("ggplot2"); library("ggthemes"); library("scales"); 
theme_set(theme_minimal())


# read data + clean it up a bit
dat <- read.csv("data/<REDACTED>") %>% 
  rename(subjid=suid4, parent=recruiterid) %>% mutate_all(as.character) %>% 
  mutate(subjid=paste0("subj", subjid)) %>% 
  mutate(parent=ifelse(parent=="seed", parent, paste0("subj", parent))) %>% 
  select(parent, subjid) %>% data.frame()

# record what all the nodes are 
nodes <- union(dat$parent, dat$subjid) %>% setdiff("seed") %>% unique()

# record which nodes are seeds
seeds <- dat$subjid[dat$parent=="seed"]

# load functions: 
source("functions.r")
# **first three have globals as default arg vals -- need to fix eventually**
# 
#   get_children            (get children of a single node)
#   get_nextgen             (get all children of a set of nodes)
#   child_trans_closure     (get all descendants of a single node)
#   get_components          (return the list of a network's components)
#   ancestor_of             (TRUE if x is ancestor of y; FALSE o.w.)
#   
#   children_per_parent     (uses globals `seeds`, `dat$subjid`, `dat$parent`)
#   make_forceplot          (uses globals `seeds`, `dat$subjid`, `dat$parent`)



# get the size of each "component" of the network (defined by a seed)
comp_sizes <- 
  sapply(get_components(net=dat, seeds=dat$subjid[dat$parent=="seed"]), length)

# get the sizes as a df for ggplotting 
compsize_df <- data.frame(
  seed=names(sort(comp_sizes)), size=sort(comp_sizes), stringsAsFactors=FALSE
)

# make a nice plot (can save in out/ if desired)
compsize_plot(comp_sizes=comp_sizes, compsize_df=compsize_df, save=FALSE)


# find out what wave a node is from
get_wave_num <- function(node){
  # NOT COMPLETE -- STILL NEED TO WRITE
}


# seems reasonable...
child_trans_closure(node="<REDACTED>")
child_trans_closure(node="<REDACTED>")
child_trans_closure(node="<REDACTED>")



# this shows that we get everything we want
length(unique(child_trans_closure(node="seed")))


# interesting looking random-walk on a network (from `igraph::`)
# walktrap.community()


### SOMETHING WRONG WITH THE NODE LABELING IN OUTPUT OF make_forceplot()
### THE DF USED TO MAKE THE FIG IS ALL ACCURATE, BUT THE PLOT ENDS UP W 
### SOME WEIRD OR NONSENSICAL EDGES...
make_forceplot(dat=dat, random_sample=TRUE, size=80)
make_forceplot(dat=dat, random_sample=FALSE)

# <REDACTED>; <REDACTED> - weird...
make_forceplot(dat=dat, size=10, include2=c("<REDACTED>","<REDACTED>"))
# <REDACTED>; <REDACTED> - weird...
# <REDACTED> goes to <REDACTED> but they're diff <REDACTED> not seed?!
# <REDACTED> -> <REDACTED>


# why it look like <REDACTED> goes to <REDACTED> but they're both seeds?!
get_children(node="<REDACTED>", net=dat)

