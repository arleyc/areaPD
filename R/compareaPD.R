#' compareaPD
#'
#' @param x dataframe containing the assignment of samples (rows) to areas (columns)
#' @param phy phylogenetic tree(s) of class phylo or multiPhylo
#' @param area name of selected area to measure complementary phylogenetic diversity
#' of the other areas in the set
#' @param plot logical value indicating whether to plot the input tree. Defaults to
#' plot = TRUE unless more than one input tree is provided.
#'
#' @details
#' compareaPD calculates the complementary phylogenetic diversity of an area given a
#' selected area from the set. When there is a single input tree, the function plots
#' the branches used for calculating the complementary PD of a given area (in red)
#' and the branches representing the PD of the selected area (in green). Currently,
#' the function is implemented to deal with five areas only.
#'
#' @return
#' An object of class “vector” or "matrix" containing the complementary phylogenetic
#' diversity values for each area and for all combinations of areas given all input
#' trees.
#' A plot showing the branches used for calculating the complementary PD of
#' a given area (in red) and the branches representing the PD of the selected
#' area (in green). The complementary PD value is given in the title of each
#' plot.
#'
#' @export
#'
#' @examples
#' #complementary PD calculation for a set of areas given a phylogeny and
#' #a selected area
#' data("homodata")
#' data("homotree")
#' homoPD<-compareaPD(homodata[1:5,],homotree,LCL)

compareaPD <- function(x, phy, area, plot=T) {

  ## check dependencies

  requireNamespace("graphics", quietly = TRUE)
  requireNamespace("ape", quietly = TRUE)

  # from phylo to multyphylo
  if (class(phy)=="phylo") {
    phy<-list(phy)
    class(phy)<-"multiPhylo"
  }

  if (length(phy)>1) {
    plot=F
  }

  # all tips
  tips<-colnames(x)

  # calculate PD paths
  compPDval<-matrix(NA,ncol=4,nrow=length(phy))

  for (j in 1:length(phy)) {

   PDpath<-vector("list", dim(x)[1])

   for (i in 1:dim(x)[1]) {

     areatips<-tips[x[i,]>0]

     # find label number for area tips
     mytips<-match(areatips,phy[[j]]$tip.label)

     # find edges corresponding to tips
     myedges<-match(mytips,phy[[j]]$edge[,2])

     # when there is a single tip
     if (length(myedges)==1) {
       newedge<-c(1:dim(phy[[j]]$edge)[1])[phy[[j]]$edge[myedges,][1]==phy[[j]]$edge[,2]]
       while (length(newedge)==1) {
         myedges<-c(myedges,newedge)
         newedge<-c(1:dim(phy[[j]]$edge)[1])[phy[[j]]$edge[newedge,][1]==phy[[j]]$edge[,2]]
       }
     }

     # when there are 2 or more tips
     if (length(myedges)>1){

       # find ancestral edges to paint path to root
       newedges<-match(phy[[j]]$edge[myedges,][,1],phy[[j]]$edge[,2])
       newedges <- newedges[!is.na(newedges)]

       # repeat finding more ancestral edges and re-paint
       while (length(newedges)>1) {
         myedges<-c(myedges,newedges)
         newedges<-match(phy[[j]]$edge[newedges,][,1],phy[[j]]$edge[,2])
         newedges <- newedges[!is.na(newedges)]
       }
     }

     PDpath[[i]] <- myedges
     names(PDpath) <- rownames(x)
   }

  # calculate complementary PD

   myarea<-which(names(PDpath)==deparse(substitute(area)))
   otherareas<-setdiff(c(1:dim(x)[1]),myarea)
   compPDpath<-vector("list", dim(x)[1]-1)
   names(compPDpath)<-rownames(x)[rownames(x)!=deparse(substitute(area))]

   for (k in 1:4) {
     compPDpath[[k]]<-setdiff(PDpath[[otherareas[k]]],PDpath[[myarea]])
   }

   for (k in 1:4) {
    compPDval[j,k]<-sum(phy[[j]]$edge.length[compPDpath[[k]]])
   }
}

  colnames(compPDval)<-names(compPDpath)

  # plots PDpaths for each complementary area
  if (plot==T) {
   graphics::par(mfrow=c(2,2), mar=c(0, 0, 1, 0))

   for (i in 1:4) {

     # create vector of edge colors
     edgecols<-rep("black",dim(phy[[1]]$edge)[1])

     # change colors of myedges
     edgecols[compPDpath[[i]]]<-c("red")
     edgecols[PDpath[[deparse(substitute(area))]]]<-c("green")

     # plot tree with painted terminal branches
     ape::plot.phylo(phy[[1]],edge.color=edgecols,edge.width=3, use.edge.length=F)
     graphics::title(main=paste("Comp. PD Area",names(compPDpath[i]),"=",sum(phy[[1]]$edge.length[compPDpath[[i]]])))
   }
 }
  return(compPDval)
}
