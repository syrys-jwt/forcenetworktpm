# forcenetworktpm
## Setup

First install NetworkD3 and its dependancies. Test if you can get networkD3 examples to work: http://christophergandrud.github.io/networkD3/

````
library(networkD3)
data(MisLinks)
data(MisNodes)
````

## Set Working Directory
you need to set the working directory to the root of this git repository. So, clone the git repository, and use the following command to link to the root. Remember to use forward slashes instead of back slashes.
````
setwd("D:/MyGitProjjects/forcenetworktpm")
````

## Install and Test
````
devtools::install()
library(forcenetworktpm)
forcenetworktpm(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)
````

## Install and Save/Publish
````
devtools::install()
library(forcenetworktpm)
#following will set the graph details to the variable myGraph
myGraph <- forcenetworktpm(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)

setwd("builds")
#edit the my_widge_name below to a name you would like, this will create the widget in the builds folder.
saveWidget(myGraph, file = "my_widget_name.html", selfcontained = FALSE)
setwd('..');

````
