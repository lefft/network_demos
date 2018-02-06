
# coding: utf-8

# ### scratchpad for `networkx` graph demos  
# ###### timothy leffel, feb06/2018 <hr style='height:2px; background-color:gray'>

# In[1]:

import networkx as nx
import matplotlib.pyplot as plt
from numpy import random 

get_ipython().magic('matplotlib inline')

random.seed(6933)


# In[2]:

# simple network path graph 
graph = nx.path_graph(6)

# add edges from a list of 2-tupes, as args to `add_edge`, or from file (TODO)
graph.add_edges_from([(1,2),(1,3)])
graph.add_edge(0, 4)

# draw the graph with `nx.draw_networkx()` 
nx.draw_networkx(graph)


# In[3]:

# function to quickly plot various kinds of graphs 
def make_net(graph_method, edge_list, **kwargs): 
    return nx.draw_networkx(graph_method(edge_list), **kwargs) 

# make some edges, in a list 
edges = [('edges','can'), ('can','be stuff'), ('be stuff', 'like'), 
         ('like','bigrams'), ('edgy','edges'), ('orphan','component </3'),
         ('me','like'), ('like','edges')]

# pass in whatever style components you want to via `**kwargs` param 
kwargs = {'font_size': 14, 'node_color': 'lightgray'}

# bind the arguments that won't vary in the subplots (edges and style)
plot_edges_with = lambda nx_method: make_net(nx_method, edges, **kwargs)


# In[4]:

# subplot syntax: `xyz` `x`=nrow, `y`=ncol, `z`=position
plt.subplot(221) 
plot_edges_with(nx.path_graph)

plt.subplot(222) 
plot_edges_with(nx.Graph)

plt.subplot(223) 
plot_edges_with(nx.DiGraph)

plt.subplot(224) 
plot_edges_with(nx.MultiGraph)

# format is: ([l, t, r, b], with l < r, t < b)
# default is: `rect=[0, 0, 1, 1]` 
plt.tight_layout(rect=[.5, .5, 3, 2]) 

# try this but with setting seed inside of `make_net()`! 
plt.show()

