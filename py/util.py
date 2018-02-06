### utility funcs for networkx demos ##########################################

# TODO: 
#   - read edgelist from text file 
#   - measure component sizes etc. 
#   - ... 


import networkx as nx 

# function to quickly plot various kinds of graphs 
def make_net(graph_method, edge_list, **kwargs): 
    return nx.draw_networkx(graph_method(edge_list), **kwargs) 


