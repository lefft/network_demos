
# function to determine if x is an ancestor of y
ancestor_of <- function(x, y, net=dat){
  
  # for now require that edgelist just has edges
  stopifnot(ncol(net)==2)
  
  # get the node list
  nodes <- union(net[[1]], net[[2]])
  
  # throw an error if x or y isn't in the nodelist
  stopifnot(x %in% nodes, y %in% nodes)
  
  # get the descendants of x
  descs_x <- child_trans_closure(x)
  
  # if y is a descendant of x, return TRUE
  if (y %in% descs_x){
    return(TRUE)
  }
  
  # if y isn't a descendant of x, return FALSE
  if (!y %in% descs_x){
    return(FALSE)
  }
}


# find the components of a network
get_components <- function(net, seeds){
  # initialize container
  component_list <- vector(mode="list", length=length(seeds))
  
  # for seed x, add descendant list as x-th element of `components`
  for (x in seq_along(seeds)){
    component_list[[x]] <- child_trans_closure(node=seeds[x])
    names(component_list)[x] <- as.character(seeds[x])
  }
  
  # return the component list
  return(component_list)
}


# for a node, get its transitive closure under child relation
child_trans_closure <- function(node, net=dat, pnt="parent", cld="subjid"){
  # throw an error if user supplied >1 node
  stopifnot(length(node)==1)
  # throw an error if the `node` argument is not in the network
  stopifnot(node %in% union(dat[[pnt]], dat[[cld]]))
  # initialize container to collect the descendants we find
  (descendants <- c())
  # list all of node's children
  (children <- get_children(node))
  # add them to descendant list
  (descendants <- c(descendants, children))
  # for each child, 
  #   - get its children + its children's children + its children's ...
  #   - ...until we hit a dead end
  for (x in seq_along(children)){
    # get the children of x
    (new_descs  <- get_children(children[x]))
    # add them to the descendant list
    (descendants <- c(new_descs, descendants))
    # set none_new==TRUE iff children[x] has no children 
    (none_new <- ifelse(length(new_descs)==0, TRUE, FALSE))
    # jump to the next element of `children` if there's nothing to explore
    if (none_new){next()}
    # as long as there's new children to explore, keep traversing the tree 
    while (!none_new){
      # get next generation
      (new_descs <- get_nextgen(new_descs))
      # add them to inventory of descendents
      (descendants <- c(new_descs, descendants))
      # set none_new==TRUE iff there's no new descendants to explore
      (none_new  <- ifelse(length(new_descs)==0, TRUE, FALSE))
    }
  }
  # return as a vec or some kind of custom object(?)
  # boils down to: do we want to keep track of wave??
  # ...seems maybe unnecessary bc you could iterate over it, rite?!
  return(descendants)
}

get_nextgen <- function(nodes, net=dat, pnt="parent", cld="subjid"){
  # apply get_children() to each node + gather results
  next_generation <- unname(unlist(sapply(nodes, function(x){
    get_children(net=net, pnt=pnt, cld=cld, node=x)
  })))
  # return the result
  return(next_generation)
}

get_children <- function(node, net=dat, pnt="parent", cld="subjid"){
  # undesirable behavior currently when node has >1 element
  stopifnot(length(node)==1)
  # list all of node's children
  node_children <- net[[cld]][net[[pnt]]==node]
  # return it
  return(node_children)
}

# get number of children per parent, display freq table, return df or table
children_per_parent <- function(showtab=TRUE, tabout=FALSE){
  dat_numchildren <- dat %>% group_by(parent) %>% summarize(
    num_children = length(subjid[subjid!=parent]),
    is_seed      = unique(subjid %in% seeds)
  ) %>% data.frame() 
  if (showtab){print(table(dat_numchildren$num_children))}
  if (tabout){
    return(table(dat_numchildren$num_children))
  } else {
    return(dat_numchildren)
  }
}


make_forceplot <- function(dat, random_sample=TRUE, size=NULL, include2=NULL){
  if (random_sample){
    
    # make sure specified nodes `include2` are in the global nodeset
    global_nodes <- union(dat$parent, dat$subjid)
    stopifnot(include2[1] %in% global_nodes, include2[2] %in% global_nodes)
    rm(global_nodes)
    
    # cut out a small subset of the data for plotting
    pdat <- dat[sample(seq_len(nrow(dat)), size=size, replace=FALSE), ] %>% 
      filter(parent!="seed")
    
    # the local nodeset
    nodes <- union(pdat$parent, pdat$subjid) %>% unique() 
    
    # insert rows involving `include2` into `pdat`; update local nodes
    if (!is.null(include2)){
      if (!all(include2 %in% nodes)){
        
        # get the relevant rows
        inc1rows <- dat[dat$parent==include2[1] | dat$subjid==include2[1], ]
        inc2rows <- dat[dat$parent==include2[2] | dat$subjid==include2[2], ]
        
        # add the rows to pdat
        pdat <- rbind(inc1rows, inc2rows, pdat) %>% filter(parent!="seed")
        
        # update the local nodeset
        nodes <- union(pdat$parent, pdat$subjid) %>% unique() 
      }
    }
    
  } else {
    # grab the whole dataset
    pdat <- dat %>% filter(parent!="seed")
    
    # the local nodeset 
    nodes <- union(pdat$parent, pdat$subjid) %>% unique() 
  }
  
  # vector of global seeds 
  seeds <- dat$subjid[dat$parent=="seed"]
  
  # cut to local seeds
  seeds <- seeds[seeds %in% nodes]
  
  # two cols: label==subjid and group=is_seed
  nodes_df <- data.frame(
    label=nodes, 
    group=ifelse(nodes %in% seeds, "seed", "non-seed"),
    stringsAsFactors=FALSE
  )
  
  # set the node names to their ranks, to match w parent/subjid
  names(nodes) <- rank(nodes)
  
  # take out seeds and match by node to derive rank labels for plotting 
  # (it's a js port, so always 'member zero-indexing!)
  pdat %<>% filter(parent!="seed") %>% mutate(value=1) %>% 
    mutate(parent_index = as.numeric(names(nodes[match(parent, nodes)]))-1) %>% 
    mutate(subjid_index = as.numeric(names(nodes[match(subjid, nodes)]))-1)
  
  
  print(forceNetwork(Links=pdat, Nodes=nodes_df, 
                     Source="parent_index", Target="subjid_index", 
                     Value="value", NodeID="label", arrows=TRUE,
                     Group="group", opacity=0.8, fontSize=10))
  
  return(list(pdat=pdat, nodes_df=nodes_df))
}


compsize_plot <- function(comp_sizes, compsize_df, 
                          save=FALSE, outname="compsizeplot", base=FALSE){
  if (base){
    # quick barplot to make sure the data looks reasonable
    bp <- barplot(sort(comp_sizes, decreasing=TRUE), 
                  xlab="seed of component", ylab="component size")
    return(bp)
  }
  sizeplot <- ggplot(compsize_df, aes(x=factor(seed, levels=seed), y=size)) + 
    geom_point(color="gray", size=4.25) +
    geom_text(aes(label=gsub("subj", "", seed)), color="black", size=1.5) +
    scale_y_continuous(limits=c(0, 150), breaks=seq(0, 150, 25)) +
    theme(axis.text.x=element_blank(), axis.title.x=element_text(face="italic"),
          panel.grid.minor=element_blank(), panel.grid.major.x=element_blank()) +
    labs(x=paste0("each component is represented by a point; ",
                  "the label indicates the seed that generated it"),
         y="number of nodes in the component",
         title="network component sizes, in ascending order")
  if (save){ggsave(plot=sizeplot, paste0("out/", outname, ".pdf"), 
                   width=11, height=5, units="in")}
  return(sizeplot)
  # could have an arg for "interactive" too
  # plotly::ggplotly(sizeplot)
}



