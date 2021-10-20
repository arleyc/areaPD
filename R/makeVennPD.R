#' makeVennPD
#'
#' @param x list of vectors containing phylogenetic diversity values within
#' and between areas produced by multiareaPD
#'
#' @details
#' makeVennPD uses the function VennDiagram::draw.quintuple.venn to draw
#' a Venn's diagram showing total PD values for each area in a set, the
#' PD exclusive of an area (endemism PD), and PD values for all intersections
#' between different combinations of areas, based on the PD values obtained
#' from separate multiareaPD analyses each representing a different phylogeny
#' of codistributed taxa (e.g., alleles, species, etc.) across the same
#' set of areas.

#' @return
#' An object of class “vector” containing the phylogenetic diversity
#' values for each area and for all combinations of areas, and a plot
#' of the Venn's diagram representing the combination of the input
#' trees and set of areas.
#'
#' @export
#'
#' @examples
#'#drawing a Venn's diagram for a set of five areas given two
#'#different phylogenetic trees
#'data("homodata")
#'data("homotree")
#'homoPD<-multiareaPD(homodata,homotree)
#'data("limnodata")
#'data("limnotree")
#'limnoPD<-multiareaPD(limnodata,limnotree)
#'data("contodata")
#'data("contotree")
#'contoPD<-multiareaPD(contodata,contotree)
#'data("latidata")
#'data("latitree")
#'latiPD<-multiareaPD(latidata,latitree)
#'vennout<-makeVennPD(list(homoPD,limnoPD,contoPD,latiPD))

makeVennPD <- function(x) {

  ## check dependencies

  requireNamespace("VennDiagram", quietly = TRUE)
  requireNamespace("imager", quietly = TRUE)
  requireNamespace("grid", quietly = TRUE)
  requireNamespace("grDevices", quietly = TRUE)

  # sum PD values for all species across areas
  PDvals <- x[[1]] + x[[2]] + x[[3]] + x[[4]]

  # create, save, and plot Venn's diagram
  # make Venn object without plotting
  vennfig<-VennDiagram::draw.quintuple.venn(PDvals[1],PDvals[2],PDvals[3],PDvals[4],PDvals[5],PDvals[6],
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
  grDevices::jpeg(filename = "multi_Venn_diagram.jpeg")
  grid::grid.draw(vennfig)
  grDevices::dev.off()
  im<-imager::load.image("multi_Venn_diagram.jpeg")
  graphics::par(mfrow=c(1,1),mar=c(1, 1, 1, 1))
  plot(im,axes=FALSE)

  return (PDvals)
}
