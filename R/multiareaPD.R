multiareaPD <- function(x, phy) {

  # check dependencies

  requireNamespace("picante", quietly = TRUE)
  requireNamespace("VennDiagram", quietly = TRUE)
  requireNamespace("imager", quietly = TRUE)

  # calculate area PD
  x <- x[1:5,]
  PDvals <- picante::pd(x,phy)
  PD12 <- picante::pd(x[1,]+x[2,],phy)
  PD13 <- picante::pd(x[1,]+x[3,],phy)
  PD14 <- picante::pd(x[1,]+x[4,],phy)
  PD15 <- picante::pd(x[1,]+x[5,],phy)
  PD23 <- picante::pd(x[2,]+x[3,],phy)
  PD24 <- picante::pd(x[2,]+x[4,],phy)
  PD25 <- picante::pd(x[2,]+x[5,],phy)
  PD34 <- picante::pd(x[3,]+x[4,],phy)
  PD35 <- picante::pd(x[3,]+x[5,],phy)
  PD45 <- picante::pd(x[4,]+x[5,],phy)
  PD123 <- picante::pd(x[1,]+x[2,]+x[3,],phy)
  PD124 <- picante::pd(x[1,]+x[2,]+x[4,],phy)
  PD125 <- picante::pd(x[1,]+x[2,]+x[5,],phy)
  PD134 <- picante::pd(x[1,]+x[3,]+x[4,],phy)
  PD135 <- picante::pd(x[1,]+x[3,]+x[5,],phy)
  PD145 <- picante::pd(x[1,]+x[4,]+x[5,],phy)
  PD234 <- picante::pd(x[2,]+x[3,]+x[4,],phy)
  PD235 <- picante::pd(x[2,]+x[3,]+x[5,],phy)
  PD245 <- picante::pd(x[2,]+x[4,]+x[5,],phy)
  PD345 <- picante::pd(x[3,]+x[4,]+x[5,],phy)
  PD1234 <- picante::pd(x[1,]+x[2,]+x[3,]+x[4,],phy)
  PD1235 <- picante::pd(x[1,]+x[2,]+x[3,]+x[5,],phy)
  PD1245 <- picante::pd(x[1,]+x[2,]+x[4,]+x[5,],phy)
  PD1345 <- picante::pd(x[1,]+x[3,]+x[4,]+x[5,],phy)
  PD2345 <- picante::pd(x[2,]+x[3,]+x[4,]+x[5,],phy)
  PD12345 <- picante::pd(x[1,]+x[2,]+x[3,]+x[4,]+x[5,],phy)

  # find the tips for the area
  tips<-colnames(x)

  PDpath<-vector("list", dim(x)[1])

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

      # add ancestral edges to the list of edges to paint
      # myedges<-c(myedges,newedges)

      # repeat finding more ancestral edges and re-paint
      while (length(newedges)>1) {
        myedges<-c(myedges,newedges)
        newedges<-match(phy$edge[newedges,][,1],phy$edge[,2])
        newedges <- newedges[!is.na(newedges)]
      }
    }
    PDpath[[i]]<-myedges
  }

  # plots PDpaths for each area

  graphics::par(mfrow=c(3,2), mar=c(0, 0, 1, 0))

  for (i in 1:dim(x)[1]) {

    # create vector of edge colors
    edgecols<-rep("black",dim(phy$edge)[1])

    # change colors of myedges
    edgecols[PDpath[[i]]]<-c("red")

    # plot tree with painted terminal branches
    plot(phy,edge.color=edgecols,edge.width =3, use.edge.length=T)
    title(main=paste("PD Area",rownames(x[i,]),"=",PDvals[i,1]))
  }

  # create, save, and plot Venn's diagram
  # calculation of area intersections
  area1<-PDvals[1,1]
  area2<-PDvals[2,1]
  area3<-PDvals[3,1]
  area4<-PDvals[4,1]
  area5<-PDvals[5,1]
  n12<-area1+area2 - PD12[,1]
  n13<-area1+area3 - PD13[,1]
  n14<-area1+area4 - PD14[,1]
  n15<-area1+area5 - PD15[,1]
  n23<-area2+area3 - PD23[,1]
  n24<-area2+area4 - PD24[,1]
  n25<-area2+area5 - PD25[,1]
  n34<-area3+area4 - PD34[,1]
  n35<-area3+area5 - PD35[,1]
  n45<-area4+area5 - PD45[,1]
  n123<-PD123[,1] - area1 - area2 - area3 + n12 + n13 + n23
  n124<-PD124[,1] - area1 - area2 - area4 + n12 + n14 + n24
  n125<-PD125[,1] - area1 - area2 - area5 + n12 + n15 + n25
  n134<-PD134[,1] - area1 - area3 - area4 + n13 + n14 + n34
  n135<-PD135[,1] - area1 - area3 - area5 + n13 + n15 + n35
  n145<-PD145[,1] - area1 - area4 - area5 + n14 + n15 + n45
  n234<-PD234[,1] - area2 - area3 - area4 + n23 + n24 + n34
  n235<-PD235[,1] - area2 - area3 - area5 + n23 + n25 + n35
  n245<-PD245[,1] - area2 - area4 - area5 + n24 + n25 + n45
  n345<-PD345[,1] - area3 - area4 - area5 + n34 + n35 + n45
  n1234<-area1 + area2 + area3 + area4 - n12 - n13 - n14 - n23 - n24 -n34 + n123 + n124 +
    n134 + n234 - PD1234[,1]
  n1235<-area1 + area2 + area3 + area5 - n12 - n13 - n15 - n23 - n25 -n35 + n123 + n125 +
    n135 + n235 - PD1235[,1]
  n1245<-area1 + area2 + area4 + area5 - n12 - n14 - n15 - n24 - n25 -n45 + n124 + n125 +
    n145 + n245 - PD1245[,1]
  n1345<-area1 + area3 + area4 + area5 - n13 - n14 - n15 - n34 - n35 -n45 + n134 + n135 +
    n145 + n345 - PD1345[,1]
  n2345<-area2 + area3 + area4 + area5 - n23 - n24 - n25 - n34 - n35 -n45 + n234 + n235 +
    n245 + n345 - PD2345[,1]
  n12345<-PD12345[,1] - area1 - area2 - area3 - area4 - area5 + n12 + n13 + n14 + n15 +
    n23 + n24 + n25 + n34 + n35 + n45 - n123 - n124 - n125 - n134 - n135 - n145 - n234 -
    n235 - n245 - n345 + n1234 + n1235 + n1245 + n1345 + n2345

  # make Venn object without plotting
  vennfig<-draw.quintuple.venn(area1,area2,area3,area4,area5,n12,n13,n14,n15,n23,n24,n25,n34
                               ,n35,n45,n123,n124,n125,n134,n135,n145,n234,n235,n245,n345,n1234,n1235,n1245,n1345,
                               n2345,n12345,
                               category=c("TGC","LCL","TCS","TCO","TQS"),
                               fill = c("dodgerblue", "goldenrod1", "red", "seagreen3", "orchid3"),
                               cex = c(1.5, 1.5, 1.5, 1.5, 1.5, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.55, 1, 0.55,
                                       1, 0.55, 1, 0.55, 1, 0.55, 1, 1, 1, 1, 1, 1.5),
                               cat.col = c("dodgerblue", "goldenrod1", "red", "seagreen3", "orchid3"),
                               cat.cex = 1.5, cat.fontface = "bold", margin = 0.05, ind=F)

  # save Venn plot as jpeg file
  jpeg(filename = "Venn_diagram.jpeg")
  grid.draw(vennfig)
  dev.off()

  im<-imager::load.image("Venn_diagram.jpeg")
  graphics::par(mar=c(1, 1, 1, 1))
  plot(im,axes=FALSE)

  return (c(area1,area2,area3,area4,area5,n12,n13,n14,n15,n23,n24,n25,n34,n35,n45,n123,n124,n125,n134,n135,n145,n234,n235,n245,n345,n1234,n1235,n1245,n1345,n2345,n12345))

}
