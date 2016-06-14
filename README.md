# forcenetworktpm
##Dependancies
Install the following packages:
````
devtools
````

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

