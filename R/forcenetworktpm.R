#' Create a D3 JavaScript force directed network graph.
#'
#' @param Links a data frame object with the links between the nodes. It should
#' include the \code{Source} and \code{Target} for each link. These should be
#' numbered starting from 0. An optional \code{Value} variable can be included
#' to specify how close the nodes are to one another.
#' @param Nodes a data frame containing the node id and properties of the nodes.
#' If no ID is specified then the nodes must be in the same order as the Source
#' variable column in the \code{Links} data frame. Currently only a grouping
#' variable is allowed.
#' @param Source character string naming the network source variable in the
#' \code{Links} data frame.
#' @param Target character string naming the network target variable in the
#' \code{Links} data frame.
#' @param Value character string naming the variable in the \code{Links} data
#' frame for how wide the links are.
#' @param NodeID character string specifying the node IDs in the \code{Nodes}
#' data frame.
#' @param Nodesize character string specifying the a column in the \code{Nodes}
#' data frame with some value to vary the node radius's with. See also
#' \code{radiusCalculation}.
#' @param Group character string specifying the group of each node in the
#' \code{Nodes} data frame.
#' @param height numeric height for the network graph's frame area in pixels.
#' @param width numeric width for the network graph's frame area in pixels.
#' @param colourScale character string specifying the categorical colour
#' scale for the nodes. See
#' \url{https://github.com/mbostock/d3/wiki/Ordinal-Scales}.
#' @param fontSize numeric font size in pixels for the node text labels.
#' @param fontFamily font family for the node text labels.
#' @param linkDistance numeric or character string. Either numberic fixed
#' distance between the links in pixels (actually arbitrary relative to the
#' diagram's size). Or a JavaScript function, possibly to weight by
#' \code{Value}. For example:
#' \code{linkDistance = JS("function(d){return d.value * 10}")}.
#' @param linkWidth numeric or character string. Can be a numeric fixed width in
#' pixels (arbitrary relative to the diagram's size). Or a JavaScript function,
#' possibly to weight by \code{Value}. The default is
#' \code{linkWidth = JS("function(d) { return Math.sqrt(d.value); }")}.
#' @param radiusCalculation character string. A javascript mathematical
#' expression, to weight the radius by \code{Nodesize}. The default value is
#' \code{radiusCalculation = JS("Math.sqrt(d.nodesize)+6")}.
#' @param charge numeric value indicating either the strength of the node
#' repulsion (negative value) or attraction (positive value).
#' @param linkColour character vector specifying the colour(s) you want the link
#' lines to be. Multiple formats supported (e.g. hexadecimal).
#' @param opacity numeric value of the proportion opaque you would like the
#' graph elements to be.
#' @param zoom logical value to enable (\code{TRUE}) or disable (\code{FALSE})
#' zooming.
#' @param legend logical value to enable node colour legends.
#' @param bounded logical value to enable (\code{TRUE}) or disable
#' (\code{FALSE}) the bounding box limiting the graph's extent. See
#' \url{http://bl.ocks.org/mbostock/1129492}.
#' @param opacityNoHover numeric value of the opacity proportion for node labels
#' text when the mouse is not hovering over them.
#' @param clickAction character string with a JavaScript expression to evaluate
#' when a node is clicked.
#'
#' @examples
#' # Load data
#' data(MisLinks)
#' data(MisNodes)
#' # Create graph
#' forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.4, zoom = TRUE)
#'
#' # Create graph with legend and varying node radius
#' forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Nodesize = "size",
#'              radiusCalculation = "Math.sqrt(d.nodesize)+6",
#'              Group = "group", opacity = 0.4, legend = TRUE)
#'
#' \dontrun{
#' #### JSON Data Example
#' # Load data JSON formated data into two R data frames
#' # Create URL. paste0 used purely to keep within line width.
#' URL <- paste0("https://cdn.rawgit.com/christophergandrud/networkD3/",
#'               "master/JSONdata/miserables.json")
#'
#' MisJson <- jsonlite::fromJSON(URL)
#'
#' # Create graph
#' forceNetwork(Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.4)
#'
#' # Create graph with zooming
#' forceNetwork(Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.4, zoom = TRUE)
#'
#'
#' # Create a bounded graph
#' forceNetwork(Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.4, bounded = TRUE)
#'
#' # Create graph with node text faintly visible when no hovering
#' forceNetwork(Links = MisJson$links, Nodes = MisJson$nodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.4, bounded = TRUE,
#'              opacityNoHover = TRUE)
#'
#' ## Specify colours for specific edges
#' # Find links to Valjean (11)
#' which(MisNodes == "Valjean", arr = TRUE)[1] - 1
#' ValjeanInds = which(MisLinks == 11, arr = TRUE)[, 1]
#'
#' # Create a colour vector
#' ValjeanCols = ifelse(1:nrow(MisLinks) %in% ValjeanInds, "#bf3eff", "#666")
#'
#' forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 0.8, linkColour = ValjeanCols)
#'
#'
#' ## Create graph with alert pop-up when a node is clicked.  You're
#' # unlikely to want to do exactly this, but you might use
#' # Shiny.onInputChange() to allocate d.XXX to an element of input
#' # for use in a Shiny app.
#'
#' MyClickScript <- 'alert("You clicked " + d.name + " which is in row " +
#'        (d.index + 1) +  " of your original R data frame");'
#'
#' forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
#'              Target = "target", Value = "value", NodeID = "name",
#'              Group = "group", opacity = 1, zoom = FALSE,
#'              bounded = TRUE, clickAction = MyClickScript)
#' }
#'

#' @source
#' D3.js was created by Michael Bostock. See \url{http://d3js.org/} and, more
#' specifically for force directed networks
#' \url{https://github.com/mbostock/d3/wiki/Force-Layout}.
#' @seealso \code{\link{JS}}.
#'
#' @export
forcenetworktpm <- function(Links,
                         Nodes,
                         Source,
                         Target,
                         Value,
                         NodeID,
                         Nodesize,
                         Group,
                         height = NULL,
                         width = NULL,
                         colourScale = JS("d3.scale.category20()"),
                         fontSize = 7,
                         fontFamily = "serif",
                         linkDistance = 50,
                         linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
                         radiusCalculation = JS(" Math.sqrt(d.nodesize)+6"),
                         charge = -120,
                         linkColour = "#666",
                         opacity = 0.6,
                         zoom = FALSE,
                         legend = FALSE,
                         bounded = FALSE,
                         opacityNoHover = 0,
                         clickAction = NULL)
{
        # Hack for UI consistency. Think of improving.
        colourScale <- as.character(colourScale)
        linkWidth <- as.character(linkWidth)
        radiusCalculation <- as.character(radiusCalculation)

        # Subset data frames for network graph
        if (!is.data.frame(Links)) {
                stop("Links must be a data frame class object.")
        }
        if (!is.data.frame(Nodes)) {
                stop("Nodes must be a data frame class object.")
        }
        if (missing(Value)) {
                LinksDF <- data.frame(Links[, Source], Links[, Target])
                names(LinksDF) <- c("source", "target")
        }
        else if (!missing(Value)) {
                LinksDF <- data.frame(Links[, Source], Links[, Target], Links[, Value])
                names(LinksDF) <- c("source", "target", "value")
        }
        if (!missing(Nodesize)){
                NodesDF <- data.frame(Nodes[, NodeID], Nodes[, Group], Nodes[, Nodesize])
                names(NodesDF) <- c("name", "group", "nodesize")
                nodesize = TRUE

        }else{

                NodesDF <- data.frame(Nodes[, NodeID], Nodes[, Group])
                names(NodesDF) <- c("name", "group")
                nodesize = FALSE


        }
        LinksDF <- data.frame(LinksDF, colour=linkColour)
        LinksDF$colour = as.character(LinksDF$colour)
                                                View(NodesDF)

        # create options
        options = list(
                NodeID = NodeID,
                Group = Group,
                colourScale = colourScale,
                fontSize = fontSize,
                fontFamily = fontFamily,
                clickTextSize = fontSize * 2.5,
                linkDistance = linkDistance,
                linkWidth = linkWidth,
                charge = charge,
                # linkColour = linkColour,
                opacity = opacity,
                zoom = zoom,
                legend = legend,
                nodesize = nodesize,
                radiusCalculation = radiusCalculation,
                bounded = bounded,
                opacityNoHover = opacityNoHover,
                clickAction = clickAction
        )

        # create widget
        htmlwidgets::createWidget(
                name = "forcenetworktpm",
                x = list(links = LinksDF, nodes = Nodes, options = options),
                width = width,
                height = height,
                htmlwidgets::sizingPolicy(padding = 10, browser.fill = TRUE),
                package = "forcenetworktpm"
        )
}

#' @rdname networkD3-shiny
#' @export
forcenetworktpmOutput <- function(outputId, width = "100%", height = "500px") {
        shinyWidgetOutput(outputId, "forcenetworktpm", width, height,
                          package = "forcenetworktpm")
}

#' @rdname networkD3-shiny
#' @export
renderForcenetworktpm <- function(expr, env = parent.frame(), quoted = FALSE) {
        if (!quoted) { expr <- substitute(expr) } # force quoted
        shinyRenderWidget(expr, forcenetworktpmOutput, env, quoted = TRUE)
}






































#' Create character strings that will be evaluated as JavaScript
#'
#' @param ... character string to evaluate
#'
#' @source A direct import of \code{JS} from Ramnath Vaidyanathan, Yihui Xie,
#' JJ Allaire, Joe Cheng and Kenton Russell (2015). \link{htmlwidgets}: HTML
#' Widgets for R. R package version 0.4.
#'
#' @export

JS <- function (...)
{
    x <- c(...)
    if (is.null(x))
        return()
    if (!is.character(x))
        stop("The arguments for JS() must be a chraracter vector")
    x <- paste(x, collapse = "\n")
    structure(x, class = unique(c("JS_EVAL", oldClass(x))))
}



#' Internal function from Wei Luo to convert a data frame to a JSON array
#'
#' @param dtf a data frame object.
#' @source Function from:
#' \url{http://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis/}
#' @keywords internal
#' @noRd
toJSONarray <- function(dtf){
  clnms <- colnames(dtf)

  name.value <- function(i){
    quote <- '';
    if(class(dtf[, i])!='numeric' && class(dtf[, i])!='integer'){
      quote <- '"';
    }
    paste('"', i, '" : ', quote, dtf[,i], quote, sep='')
  }
  objs <- apply(sapply(clnms, name.value), 1, function(x){paste(x,
                                                          collapse=', ')})
  objs <- paste('{', objs, '}')

  res <- paste('[', paste(objs, collapse=', '), ']')

  return(res)
}


#' Read a text file into a single string
#'
#' @source Code taken directly from Ramnath Vaidyanathan's Slidify
#' \url{https://github.com/ramnathv/slidify}.
#' @param doc path to text document
#' @return string with document contents
#' @keywords internal
#' @noRd
read_file <- function(doc, ...){
  paste(readLines(doc, ...), collapse = '\n')
}


#' Utility function to handle margins
#' @param margin an \code{integer}, a named \code{vector} of integers,
#'    or a named \code{list} of integers specifying the margins
#'    (top, right, bottom, and left)
#'    in \code{px}/\code{pixels} for our htmlwidget.  If only a single
#'    \code{integer} is provided, then the value will be assumed to be
#'    the \code{right} margin.
#' @return named \code{list} with top, right, bottom, left margins
#' @noRd
margin_handler <- function(margin){
  # margin can be either a single value or a list with any of
  #    top, right, bottom, left
  # if margin is a single value, then we will stick
  #    with the original behavior of networkD3 and use it for the right margin
  if(!is.null(margin) && length(margin) == 1 && is.null(names(margin))){
    margin <- list(
      top = NULL,
      right = margin,
      bottom = NULL,
      left = NULL
    )
  } else if(!is.null(margin)){
    # if margin is a named vector then convert to list
    if(!is.list(margin) && !is.null(names(margin))){
      margin <- as.list(margin)
    }
    # if we are here then margin should be a list and
    #   we will use the values supplied with NULL as default
    margin <- modifyList(
      list(top = NULL, right = NULL, bottom = NULL, left = NULL),
      margin
    )
  } else {
    # if margin is null, then make it a list of nulls for each position
    margin <- list(top = NULL, right = NULL, bottom = NULL, left = NULL)
  }
}


#' Function to convert igraph graph to a list suitable for networkD3
#'
#' @param g an \code{igraph} class graph object
#' @param group an object that contains node group values, for example, those
#' created with igraph's \code{\link{membership}} function.
#' @param what a character string specifying what to return. If
#' \code{what = 'links'} or \code{what = 'nodes'} only the links or nodes are
#' returned as data frames, respectively. If \code{what = 'both'} then both
#' data frames will be return in a list.
#'
#' @return A list of link and node data frames or only the link or node data
#' frames.
#'
#' @examples
#' # Load igraph
#' library(igraph)
#'
#' # Use igraph to make the graph and find membership
#' karate <- make_graph("Zachary")
#' wc <- cluster_walktrap(karate)
#' members <- membership(wc)
#'
#' # Convert to object suitable for networkD3
#' karate_d3 <- igraph_to_networkD3(karate, group = members)
#'
#' # Create force directed network plot
#' forceNetwork(Links = karate_d3$links, Nodes = karate_d3$nodes,
#'              Source = 'source', Target = 'target', NodeID = 'name',
#'              Group = 'group')
#'
#' \dontrun{
#' # Example with data from data frame
#' # Load data
#' ## Original data from http://results.ref.ac.uk/DownloadSubmissions/ByUoa/21
#' data('SchoolsJournals')
#'
#' # Convert to igraph
#' SchoolsJournals <- graph.data.frame(SchoolsJournals, directed = FALSE)
#'
#' # Remove duplicate edges
#' SchoolsJournals <- simplify(SchoolsJournals)
#'
#' # Find group membership
#' wt <- cluster_walktrap(SchoolsJournals, steps = 6)
#' members <- membership(wt)
#'
#' # Convert igraph to list for networkD3
#' sj_list <- igraph_to_networkD3(SchoolsJournals, group = members)
#'
#' # Plot as a forceDirected Network
#' forceNetwork(Links = sj_list$links, Nodes = sj_list$nodes, Source = 'source',
#'              Target = 'target', NodeID = 'name', Group = 'group',
#'              zoom = TRUE, linkDistance = 200)
#' }
#'
#' @importFrom igraph V as_data_frame graph.data.frame simplify cluster_walktrap membership
#' @importFrom magrittr %>%
#' @export

igraph_to_networkD3 <- function(g, group, what = 'both') {
    # Sanity check
    if (!('igraph' %in% class(g))) stop('g must be an igraph class object.',
                                      call. = FALSE)
    if (!(what %in% c('both', 'links', 'nodes'))) stop('what must be either "nodes", "links", or "both".',
                                                     call. = FALSE)

    # Extract vertices (nodes)
    temp_nodes <- V(g) %>% as.matrix %>% data.frame
    temp_nodes$name <- row.names(temp_nodes)
    names(temp_nodes) <- c('id', 'name')

    # Convert to base 0 (for JavaScript)
    temp_nodes$id <- temp_nodes$id - 1

    # Nodes for output
    nodes <- temp_nodes$name %>% data.frame %>% setNames('name')
    # Include grouping variable if applicable
    if (!missing(group)) {
      group <- as.matrix(group)
      if (nrow(nodes) != nrow(group)) stop('group must have the same number of rows as the number of nodes in g.',
                                          call. = FALSE)
      nodes <- cbind(nodes, group)
    }
    row.names(nodes) <- NULL

    # Convert links from names to numbers
    links <- as_data_frame(g, what = 'edges')
    links <- merge(links, temp_nodes, by.x = 'from', by.y = 'name')
    links <- merge(links, temp_nodes, by.x = 'to', by.y = 'name')
    links <- links[, c('id.x', 'id.y')] %>% setNames(c('source', 'target'))

    # Output requested object
    if (what == 'both') {
      return(list(links = links, nodes = nodes))
    }
    else if (what == 'links') {
      return(links)
    }
    else if (what == 'nodes') {
      return(nodes)
    }
}


#' Save a network graph to an HTML file
#'
#' Save a networkD3 graph to an HTML file for sharing with others. The HTML can
#' include it's dependencies in an adjacent directory or can bundle all
#' dependencies into the HTML file (via base64 encoding).
#'
#' @param network Network to save (e.g. result of calling the function
#'   \code{simpleNetwork}).
#'
#' @inheritParams htmlwidgets::saveWidget
#'
#' @export
saveNetwork <- function(network, file, selfcontained = TRUE) {
    htmlwidgets::saveWidget(network, file, selfcontained)
}
