makeVennPD <- function(x) {

  ## check dependencies

  requireNamespace("VennDiagram", quietly = TRUE)
  requireNamespace("imager", quietly = TRUE)

  # sum PD values for all species across areas
  PDvals <- x[[1]] + x[[2]] + x[[3]] + x[[4]]

  # create, save, and plot Venn's diagram
  # make Venn object without plotting
  vennfig<-draw.quintuple.venn(PDvals[1],PDvals[2],PDvals[3],PDvals[4],PDvals[5],PDvals[6],
                               PDvals[7],PDvals[8],PDvals[9],PDvals[10],PDvals[11],PDvals[12],PDvals[13],PDvals[14],
                               PDvals[15],PDvals[16],PDvals[17],PDvals[18],PDvals[19],PDvals[20],PDvals[21],PDvals[22],
                               PDvals[23],PDvals[24],PDvals[25],PDvals[26],PDvals[27],PDvals[18],PDvals[29],PDvals[30],
                               PDvals[31],
                               category=c("TGC","LCL","TCS","TCO","TQS"),
                               fill = c("dodgerblue", "goldenrod1", "red", "seagreen3", "orchid3"),
                               cex = c(1.5, 1.5, 1.5, 1.5, 1.5, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.55, 1, 0.55,
                                       1, 0.55, 1, 0.55, 1, 0.55, 1, 1, 1, 1, 1, 1.5),
                               cat.col = c("dodgerblue", "goldenrod1", "red", "seagreen3", "orchid3"),
                               cat.cex = 1.5, cat.fontface = "bold", margin = 0.05, ind=F)

  # save Venn plot as jpeg file
  jpeg(filename = "multi_Venn_diagram.jpeg")
  grid.draw(vennfig)
  dev.off()

  library(imager)
  im<-load.image("multi_Venn_diagram.jpeg")
  graphics::par(mfrow=c(1,1),mar=c(1, 1, 1, 1))
  plot(im,axes=FALSE)

  return (PDvals)
}
