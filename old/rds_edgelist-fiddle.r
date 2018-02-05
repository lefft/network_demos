library("magrittr"); library("dplyr")

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
dat %>% filter(subjid %in% seeds) %>% group_by(subjid) %>% 
  summarize(num_children = n()) %>% data.frame() 

# get number of children per parent, display barplot from freq table
dat_numch <- dat %>% group_by(parent) %>% summarize(
  num_children = length(subjid[subjid!=parent]),
  is_seed      = unique(subjid %in% seeds)
) %>% data.frame() %T>% (function(x)(barplot(table(x$num_children))))

# get number of parents per child, display freq table (everyone has exactly 1)
dat_numpt <- dat %>% group_by(subjid) %>% summarize(
  num_parents = length(parent[parent!=subjid])
) %>% data.frame() %T>% (function(x)print(table(x$num_parents)))


child_list <- list()
for (x in seq_along(unique(dat$parent))){
  child_list[[x]] <- dat$subjid[dat$parent==unique(dat$parent)[x]]
  names(child_list)[x] <- unique(dat$parent)[x]
}
# make a vector with name as parent + values as child
# [NICE + COMPACT BUT ADDS INDICES TO AVOID NON-UNIQUE VALS IN NAMES!]
# parch_pairs <- data.frame(
#   parent=names(unlist(child_list)), child=as.character(unlist(child_list))
# )


cp_pairs <- data.frame(parent="", child="", stringsAsFactors=FALSE)
for (x in seq_along(child_list)){
  for (y in seq_along(child_list[[x]])){
    cp_pairs <- rbind(cp_pairs, c(parent=names(child_list)[x], child=child_list[[x]][y]))
  }
}; cp_pairs %<>% filter(!(child=="" & parent==""))

# NOTE: WE JUST LITERALLY REINVENTED THE ORIGINAL DATA LOL!
# all(table(dat$subjid)==table(cp_pairs$child))
# all(table(dat$parent)==table(cp_pairs$parent))

