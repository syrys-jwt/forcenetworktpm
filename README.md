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

## Exporting and Publishing to github
After the above section, you should have the graphs visible on your RStudio window. Click Export-> Save as a Web Page button on the RStudio's view window that is displaying the graph (it may ask you to install some dependancy packages the first time, please accept and install those). After clicking the Save as Web Page button, you sill be prompted to select a location to save the file, please select your project directory, either override the index.html or create a folder for your outputs and save as an html file.

![alt text](http://syrys-jwt.github.io/forcenetworktpm/screenshots/save_as_web.jpg "Save as Web Page")


Once done, please commit and push your changes into github.

If you saved as the root index.html file, you will be able to view the file at the url: `http://syrys-jwt.github.io/forcenetworktpm/`, similarly, if you add it in a different folder, type the folder name at the end of the url followed by the file name.

## Publishing to website
If you want to include/embed the published html file into a wordpress site/page, please login to wordpress, create/find the page you want to embed this to, then click the editor into "Text" mode (sometime this is called the "code" mode, usually visible somehwree at the top right of the wysiwyg editor). Once in the code/text mode, insert the following bit of code:
````
<iframe style="width: 100%; display: block;" src="http://syrys-jwt.github.io/forcenetworktpm/" height="600px"></iframe>
````
Please Note: please change the link above to the appropriate link to your html file, and set a height of the embed element in pixels.
![alt text](http://syrys-jwt.github.io/forcenetworktpm/screenshots/wordpress_embed.jpg "Embed on Wordpress")

Then you can save/publish the page, and view it on your browser. 

All done :D
