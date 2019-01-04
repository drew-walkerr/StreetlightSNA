# This was adopted from the Social Network Workshop class sample from-- : Raffaele Vacca <r.vacca@ufl.edu>
#
#This is adapted to utilize these methods to look at SNA data of the Streetlight Program by Drew Walker



############################################################################## #
###                 IMPORTING AND DISPLAYING NETWORK DATA                   ====
############################################################################## #
## ---- import

# Read the edgelist data into R
elist <- read.csv("./Data/edgelist_full.csv", header = FALSE)

# Look at the imported data
head(elist)

# Let's give meaningful names to the columns.
names(elist) <- c("from", "to", "tie_weight")

# Look at the data again.
head(elist)

# Load igraph.
library(igraph)

# Using a function from igraph, convert the data frame above into a network.
graph <- graph_from_data_frame(elist)

# In igraph, networks are objects of class "igraph"
class(graph)

# Summary information about this network
graph
# The graph is Directed, Named, NOT Weighted, NOT Bipartite.
# The graph has 12 nodes and 144 edges. 
# It has a vertex attribute called "name" and an edge attribute called
# "tie_weight".

# Plot the network
# Set the seed for reproducibility (more on this below)
set.seed(221)
# plot
plot(graph)

# The current network doesn't look good: (1) It has self loops and (2) It has no
# edge threshold, i.e. all edges have been imported, including those with
# tie_weight=0 in the original edge list.

# There are simpler ways to remove self-loops and 0-edges directly from the
# igraph object, but for now, let's manipulate the original edgelist.

# Remove edges with tie_weight=0 from edge list: only keep rows whose weight is 
# different from 0.
library(dplyr)
SLcamp <- dplyr::filter(SLcamp, tie_weight !=0)

# Remove edgelist rows corresponding to self-loops: only keep rows whose "from"
# value is different from the "to" value.
SLcamp <- dplyr::filter(SLcamp, from != to)

# Re-convert edgelist to graph
graph <- graph_from_data_frame(SLcamp)

# Plot again
# Set the seed for reproducibility (more on this below)
set.seed(221)
# Plot
plot(SL)

# Note that the plot() function is extremely flexible. We can set vertex 
# parameters (size, color etc.), edge parameters (width, color, line type etc.), 
# label parameters (font, color, size), vertex layout, and more. See 
# http://igraph.org/r/doc/plot.common.html for details.

# For example, let's plot with smaller arrows and labels for more clarity.
# Set the seed for reproducibility (more on this below).
set.seed(221)
# Plot
plot(SLcamp, edge.arrow.size=0.5, vertex.label.cex=.6)

# Let's look at the summary characteristics of the "corrected" graph.
graph
#SL
SLcamp

# Let's now also import vertex attributes
vert.attr <- read.csv("./Data/attributes.csv")
head(vert.attr)
#SL
SLcamp.attr <- read.csv("./Data/SL campattr.csv")
head(SLcamp.attr)

# The same function we used above can import vertex attributes together with edge data.
SL

# The igraph object now includes vertex attributes
graph

# Create a graph out of a fictitious edge list entered as a matrix.
# Each pair of names is a directed edge (i.e. a row in the edge list.)
small.elist <- matrix(c("mark","paul", "mark","anna", "theo","kelsie", 
                        "mario","anna", "kelsie","mario", "kelsie",
                        "anna"), nrow= 6, byrow=TRUE)

colnames(small.elist) <- c("FROM", "TO")

small.elist

# Use that edge list to create a network as an igraph object.
gr <- graph_from_edgelist(small.elist)

gr <- graph_from_adjacency_matrix(SLcamp)
plot(SL)

# Show the graph.
# Set the seed for reproducibility (more on this below)
set.seed(607)
# Plot
plot(gr)
# Remove labels from plot
set.seed(607)
plot(gr, vertex.label= NA)

# Print the graph (summary info).
gr
# The graph is Directed, Named. It has 6 vertices and 6 edges. It has a vertex
# attribute called "name".

# Note that, unless we use set.seed(), the network layout changes each time we
# plot a graph. This can be avoided in two ways.

# 1) Set the same seed before each plot
set.seed(215)
plot(graph, edge.arrow.size=0.5)
#SL
plot(SLcamp, edge.arrow.size=0.5)
# Plot again
set.seed(215)
plot(graph, edge.arrow.size=0.5)

# 2) Calculate network layout matrix separately, and always use that matrix for plot
set.seed(215)
layout.mat.fr <- layout_(graph=graph, layout=with_fr())

# Plot using that layout matrix
plot(a, layout=layout.mat.fr, edge.arrow.size=0.5)

# Plot again
plot(SLcamp, layout=layout.mat.fr, edge.arrow.size=0.5)

# Plot using different layout algorithms
# As star
set.seed(215)
layout.mat.st <- layout_(graph=graph, layout=as_star())
plot(SLcamp, layout=layout.mat.st, edge.arrow.size=0.5)

# Kamada-kawai
set.seed(215)
layout.mat.kk <- layout_(graph=graph, layout=with_kk())
plot(SLcamp, layout=layout.mat.kk, edge.arrow.size=0.5)

# You can use any matrix (with 2 columns and N rows, N being the number of
# vertices) for the graph layout, e.g. a matrix with spatial coordinates.

# More information on layout functions: http://igraph.org/r/doc/layout_.html

# Note that a layout matrix can be set as the "default" layout matrix for a 
# graph, by setting it as a graph attribute called "layout". If we do that, 
# plot() will always (silently) use that matrix as graph's layout.
graph$layout <- layout.mat.kk 

# Now we don't need to set the "layout" argument any more.
plot(graph, edge.arrow.size=0.5)

# To export the plot to an external file, use png() or pdf()
png(filename = "graph_kk.png", width = 600, height = 600)
plot(graph, edge.arrow.size=0.5)
dev.off()


## ---- end-import
############################################################################## #
###                       ATTRIBUTES AND INDEXING                           ====
############################################################################## #
## ---- attr-indexing

# Vertex sequence of the graph
V(graph)

# Edge sequence of the graph
E(graph)

# Indexing based on vertex and edge attributes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Extract vertex attribute
V(graph)$age

# A vertex attribute is just a vector that we can re-use for any operation. For
# example: What's the average age in the network?
mean(V(graph)$age)
# What's its standard deviation?
sd(V(graph)$age)

# What's the distribution of sex in the network?
# Absolute frequencies
table(V(graph)$sex)
#SLCamp

table(V(SLcamp)$Gender)
# Relative frequencies
prop.table(table(V(graph)$sex))

# If code becomes awkward to read, remember the pipe operator (which is
# automatically imported by igraph):
V(graph)$sex %>% table %>% prop.table

# Vertex names are a vertex attribute created by default by graph_from_data_frame()
V(graph)$name

# We can also set new vertex attributes
V(graph)$new.attribute <- 1:vcount(graph)

# Now a new attribute is listed for the graph.
graph
V(graph)$new.attribute

# Extract edge attribute
E(graph)$tie_weight

# Average tie weight (i.e., strength) in the network
E(graph)$tie_weight %>% mean

# Extract female actors
V(graph)[sex=="F"]

# Extract strong ties
E(graph)[tie_weight > 2]

# Extract age of female actors
V(graph)[sex=="F"]$age

# Mean age of female actors in the network
mean(V(graph)[sex=="F"]$age)

# Display actor sex in graph visualization
# First plot with uniform blue color
plot(SLcamp, vertex.color= "blue", edge.arrow.size=0.5, 
     vertex.label.cex=0.7, vertex.label.color= "white")
# The color can also be set as a vertex attribute in the graph itself, and the
# plot function will recognize it.
V(SLcamp)$color <- "blue"

# Now the plot function recognizes the vertex attribute "color".
plot(SLcamp, edge.arrow.size=0.5, vertex.label.cex=0.7, vertex.label.color= "white")

# Using indexing, set a different color for female actors
V(SLcamp)[Role=="1"]$color
V(SLcamp)[Role=="1"]$color <- "red"



# Plot again
plot(SLcamp, edge.arrow.size=0.5, vertex.label.cex=0.7, vertex.label.color= "white")

# Indexing based on network structure
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# View all actors who know Mark
V(graph)[nei("Mark")]

# View the age of all actors who know Mark.
V(graph)[nei("Mark")]$age

# Average age in Mark's first-order neighborhood.
V(graph)[nei("Mark")]$age %>% mean

# View all edges that are incident on David
E(graph)[inc("David")]
# All edges "to" David
E(graph)[to("David")]
# View the strength of these edges
E(graph)[to("David")]$tie_weight

# Average strength of all incoming edges to David
E(graph)[to("David")]$tie_weight %>% mean

# Conditions in R indexing can always be combined: view all edges to David whose
# tie_weight is ==2
E(graph)[to("David") & tie_weight==2]

# View all edges between women in the network.
# First get the vertex sequence of all women
women <- V(graph)[sex=="F"]
women
# Then get the edges among them.
E(graph)[women %--% women]

# Are there more edges among women or among men?
# Number of edges among women
E(graph)[women %--% women] %>% length
# Number of edges among men
men <- V(graph)[sex=="M"]
E(graph)[men %--% men] %>% length

# What is the strength of ties among women?
E(graph)[women %--% women]$tie_weight

# Compare the distribution of tie strength among women vs among men
E(graph)[women %--% women]$tie_weight %>% table
E(graph)[men %--% men]$tie_weight %>% table

# With summarytools
library(summarytools)
E(graph)[women %--% women]$tie_weight %>% freq

# Example of fancier plot to show the flexibility of igraph's plot.igraph().

# Highlight first-order neighborhood of Mark by setting its vertex shape to square
V(graph)$shape <- "circle"
V(graph)[nei("Mark")]$shape <- "square"

# Set vertex size to index degree centrality
V(graph)$size <- degree(graph)

# Set edge width to tie strenght
E(graph)$width <- E(graph)$tie_weight

# Set color of Mark's edges to red
E(graph)$color <- "grey"
E(graph)[inc("Mark")]$color <- "green"

# Plot result
plot(graph, edge.arrow.size=0, # No arrows
     vertex.label.cex=1, # Label size
     vertex.label.color= "black", # Label color
     vertex.label.dist= 3 # Label distance from vertices
)

# Remove the modified "special" graphical attributes so next plots will go back
# to default values.
graph <- delete_vertex_attr(graph, "color")
graph <- delete_vertex_attr(graph, "shape")
graph <- delete_vertex_attr(graph, "size")
graph <- delete_edge_attr(graph, "width")
graph <- delete_edge_attr(graph, "color")


# ***** EXERCISE 1: 
# Get the average grade (vertex attribute $grade) of women in the network.
# *****

# ***** EXERCISE 2: 
# Get the average grade (vertex attribute $grade) of people older than 23 in the network.
# *****

# ***** EXERCISE 3: 
# Get edges from men to women in the network. What is their average tie weight?
# *****

# ***** EXERCISE 4: 
# Get edges among students in the network whose grade is <90. What is the maximum strength?
# *****


## ---- end-attr-indexing
############################################################################## #
###                       MANIPULATING NETWORKS                             ====
############################################################################## #
## ---- manipulating

# For this section we'll use both "graph" and the "campnet" network.

# Read in the campnet network adjacency matrix
campnet.adj <- read.csv(file="./Data/campnet_adj.csv", row.names = 1)
head(campnet.adj)

#Streetlight version
SLcampnet.adj <- read.csv(file="./Data/SL campnet_adj.csv", row.names = 1)
head(SLcampnet.adj)

# Convert to matrix (it's imported as a data frame)
campnet.adj <- as.matrix(campnet.adj)

#Streetlight version
SLcampnet.adj<- as.matrix(SLcampnet.adj)


# Import into directed graph 
camp <- graph_from_adjacency_matrix(campnet.adj, mode="directed")
#Streetlight version
SLcamp <- graph_from_adjacency_matrix(SLcampnet.adj, mode="directed")

# Read in the vertex attributes
campnet.attr <- read.csv(file="./Data/campattr.csv", row.names = 1)
head(campnet.attr)

#Streetlight Version
SLcampnet.attr <- read.csv(file="./Data/SL campattr.csv", row.names = 1)
head(SLcamp.attr)

# Note that actors in the graph are in the same order as actors in the attribute
# data frame.
V(camp)
rownames(campnet.attr)

#Streetlight Version
V(SLcamp)
rownames(SLcamp.attr)

# So we can simply set the columns from the attribute data frame as vertex 
# attributes (because the order is the same, no joining/merging is needed).
V(camp)$Gender <- campnet.attr$Gender
V(camp)$Role <- campnet.attr$Role

#Streetlight Version
V(SLcamp)$Role <- SLcamp.attr$Role
V(SLcamp)$Gender<- SLcamp.attr$Gender

# Let's print the graph
camp

#Streetlight
SLcamp

# Plot it.
# Calculate layout.
set.seed(219)
#Streetlight below-- if you want camp take out SL
layout.kk <- layout_(graph=SLcamp, layout=with_kk())
# Set as default layout.
camp$layout <- layout.kk
#Streetlight Version
SLcamp$layout <- layout.kk
# Plot the graph.
plot(SLcamp, edge.arrow.size=0.1, vertex.label.cex=0.5)
#Streetlight Version
plot(SLcamp, edge.arrow.size=0.1, vertex.label.cex=0.5)
# To export to png
png(filename = "Streetlight SNA.png", width = 800, height = 800)
plot(camp, edge.arrow.size=1, vertex.label.cex=1)
dev.off()


# Convert from directed to undirected network
# - - - - - - - - - - - - - - - - - - - - - - - -

# Campnet is a directed network
is_directed(SLcamp)

# We can make it undirected in 2 ways:
# * "Collapse": Set an undirected tie in dyad when at least one directed tie
# exists in dyad
# * "Mutual": Set an undirected tie in dyad when two reciprocated directed ties
# exist in dyad. This is equivalent to only keeping mutual ties.

# First option
SLcamp.undir.coll <- as.undirected(SLcamp, mode= "collapse")

# Plot the result
plot(SLcamp.undir.coll, edge.arrow.size=0.5, vertex.label.cex=0.5)

# Note the letter "U" when printing
SLcamp.undir.coll

# Second option
camp.undir.mut <- as.undirected(camp, mode= "mutual")

# Note that non-reciprocated ties are dropped, so this undirected version has
# fewer ties than the "collapse" version
ecount(SLcamp.undir.coll)
ecount(SLcamp.undir.mut)

# Plot the result
plot(SLcamp.undir.mut, edge.arrow.size=0.5, vertex.label.cex=0.5)

# Note that the network layout doesn't seem right. In fact, many ties have been
# dropped (all the non-reciprocated ones), but we're still implicitly using the
# original layout that takes into account all original ties.
# Let's recalculate the layout.
set.seed(219)
camp.undir.mut$layout <- layout_(graph=camp.undir.mut, layout=with_kk())

# Plot again
plot(camp.undir.mut, edge.arrow.size=0.5, vertex.label.cex=0.5)

# Set a lower edge weight threshold in weighted networks
# - - - - - - - - - - - - - - - - - - - - - - - -

# The goal here is to set a threshold on an edge attribute, so that only edges
# whose attribute is equal or higher than the threshold will be kept in the
# network. In igraph we do this by removing the edges whose attribute is lower 
# than the threshold.

# Again, remember that each time even a single tie is removed, the network
# structure changes and the layout should be recalculated.

# "graph" has a the $tie_weight edge attribute that we can use to set thresholds
E(graph)$tie_weight

# Original network with no threshold
plot(graph, edge.arrow.size=0.5, vertex.label.cex=0.5)

# Set threshold=2: remove edges with tie_weight < 2
# All edges with tie_weight < 2
E(graph)[tie_weight < 2]
# Remove them
graph.2 <- delete_edges(graph, edges= E(graph)[tie_weight < 2])
# Compare
ecount(graph)
ecount(graph.2)

# More intuitive syntax to do the same thing.
graph.2 <- graph -  E(graph)[tie_weight < 2]

# Recalculate layout and plot
set.seed(219)
graph.2$layout <- layout_(graph=graph.2, layout=with_kk())
plot(graph.2, edge.arrow.size=0.5, vertex.label.cex=0.5)

# Let's set thresholds from 3 to 5 and plot all results
graph.3 <- graph -  E(graph)[tie_weight < 3]
graph.4 <- graph -  E(graph)[tie_weight < 4]
graph.5 <- graph -  E(graph)[tie_weight < 5]

# Calculate all layouts
set.seed(219)
graph.3$layout <- layout_(graph=graph.3, layout=with_kk())
graph.4$layout <- layout_(graph=graph.4, layout=with_kk())
graph.5$layout <- layout_(graph=graph.5, layout=with_kk())

# Plot all in same window and export to external png file.
# Open device
png("graph_thresholds.png", width= 1000, height= 1500)
# Set graphical window of 3 rows by 2 columns to fit 5 plots
par(mfrow=c(3,2))
# Plot one network after the other
plot(graph, edge.arrow.size=0.5)
# Draw box around plot
box()
plot(graph.2, edge.arrow.size=0.5)
box()
plot(graph.3, edge.arrow.size=0.5)
box()
plot(graph.4, edge.arrow.size=0.5)
box()
plot(graph.5, edge.arrow.size=0.5)
box()
dev.off()

## ---- end-manipulating

