#' endemismPD
#'
#' @param x dataframe containing the assignment of samples (rows) to areas (columns)
#' @param phy phylogenetic tree(s) of class phylo or multiPhylo
#'
#' @details
#' endemismPD calculates the endemism phylogenetic diversity of an area given the
#' other areas in the set. When there is a single input tree, the function plots
#' the branches used for calculating the endemism PD of a given area (in red).
#' Currently, the function is implemented to deal with five areas only.
#'
#' @return
#' An object of class “vector” or "matrix" containing the endemism phylogenetic
#' diversity values for each area given all input trees.
#' A plot showing the branches used for calculating the endemism PD of
#' a given area (in red). The endemism PD value is given in the title of each
#' plot.
#'
#' @export
#'
#' @examples
#' #endemism PD calculation for a set of areas given a phylogeny
#' data("homodata")
#' data("homotree")
#' homoPD<-endemismPD(homodata[1:5,],homotree)

endemismPD <- function(x, phy, plot=T) {

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
  endPDval<-matrix(NA,ncol=5,nrow=length(phy))

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

  # calculate endemism PD

   endPDpath<-vector("list", dim(x)[1])
   names(endPDpath)<-rownames(x)

    for (k in 1:5) {
      otherareas<-setdiff(c(1,2,3,4,5),c(k))
      myPDpath<-PDpath[[k]]
      for (i in 1:length(otherareas)) {
        myPDpath<-setdiff(myPDpath,intersect(myPDpath,PDpath[[otherareas[i]]]))
      }
      endPDpath[[k]]<-myPDpath
    }

   for (k in 1:5) {
    endPDval[j,k]<-sum(phy[[j]]$edge.length[endPDpath[[k]]])
   }
}

  colnames(endPDval)<-names(endPDpath)

  # plots PDpaths for each complementary area
  if (plot==T) {
   graphics::par(mfrow=c(3,2), mar=c(0, 0, 1, 0))

   for (i in 1:5) {

     # create vector of edge colors
     edgecols<-rep("black",dim(phy[[1]]$edge)[1])

     # change colors of myedges
     edgecols[endPDpath[[i]]]<-c("red")

     # plot tree with painted terminal branches
     ape::plot.phylo(phy[[1]],edge.color=edgecols,edge.width=3, use.edge.length=F)
     graphics::title(main=paste("End. PD Area",names(endPDpath[i]),"=",sum(phy[[1]]$edge.length[endPDpath[[i]]])))
   }
 }
  return(endPDval)
}
