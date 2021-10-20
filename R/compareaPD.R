#' compareaPD
#'
#' @param x dataframe containing the assignment of samples (rows) to areas (columns)
#' @param phy phylogenetic tree of class phylo
#' @param area name of selected area to measure complementary phylogenetic diversity
#' of the other areas in the set
#'
#' @return
#' @export
#'
#' @examples
compareaPD <- function(x, phy, area) {

  # all tips
  x <- x[1:5,]
  tips<-colnames(x)
  PDpath<-vector("list", dim(x)[1])

  # calculate PD paths

  for (i in 1:dim(x)[1]) {
    areatips<-tips[x[i,]>0]

    # find label number for area tips
    mytips<-match(areatips,phy$tip.label)

    # find edges corresponding to tips
    myedges<-match(mytips,phy$edge[,2])

    # when there is a single tip
    if (length(myedges)==1) {
      newedge<-c(1:dim(phy$edge)[1])[phy$edge[myedges,][1]==phy$edge[,2]]
      while (length(newedge)==1) {
        myedges<-c(myedges,newedge)
        newedge<-c(1:dim(phy$edge)[1])[phy$edge[newedge,][1]==phy$edge[,2]]
      }
    }

    # when there are 2 or more tips
    if (length(myedges)>1){

      # find ancestral edges to paint path to root
      newedges<-match(phy$edge[myedges,][,1],phy$edge[,2])
      newedges <- newedges[!is.na(newedges)]

      # repeat finding more ancestral edges and re-paint
      while (length(newedges)>1) {
        myedges<-c(myedges,newedges)
        newedges<-match(phy$edge[newedges,][,1],phy$edge[,2])
        newedges <- newedges[!is.na(newedges)]
      }
    }
    PDpath[[i]]<-myedges
    names(PDpath) <- rownames(x)
  }

  # calculate complementary PD

  myarea<-which(names(PDpath)==deparse(substitute(area)))
  otherareas<-setdiff(c(1:dim(x)[1]),myarea)
  compPDpath<-vector("list", dim(x)[1]-1)
  names(compPDpath)<-rownames(x)[rownames(x)!=deparse(substitute(area))]

  for (i in 1:4) {
    compPDpath[[i]]<-setdiff(PDpath[[otherareas[i]]],PDpath[[myarea]])
  }

  # plots PDpaths for each complementary area

  graphics::par(mfrow=c(2,2), mar=c(0, 0, 1, 0))

  for (i in 1:4) {

    # create vector of edge colors
    edgecols<-rep("black",dim(phy$edge)[1])

    # change colors of myedges
    edgecols[compPDpath[[i]]]<-c("red")
    edgecols[PDpath[[deparse(substitute(area))]]]<-c("green")

    # plot tree with painted terminal branches
    plot(phy,edge.color=edgecols,edge.width=3, use.edge.length=F)
    graphics::title(main=paste("Comp. PD Area",names(compPDpath[i]),"=",sum(phy$edge.length[compPDpath[[i]]])))
  }


}
