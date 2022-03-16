
<!-- README.md is generated from README.Rmd. Please edit that file -->

# areaPD

<!-- badges: start -->
<!-- badges: end -->

The functions in areaPD can calculate phylogenetic diversity (PD) and
endemism PD for a set of geographic areas, the complementarity PD given
a selected area, and constructing a Venn’s diagram of PD within and
between areas, given one or more phylogenetic trees and an assignment of
samples (e.g., alleles, species, etc.) to areas.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("arleyc/areaPD")
```

## Example

``` r
library(areaPD)
#PD calculation for a set of areas and a given phylogeny
data("homodata")
data("homotree")
homoPD<-multiareaPD(homodata[1:5,],homotree)
```

<img src="man/figures/README-example1-1.png" width="100%" />

``` r
limnoPD<-multiareaPD(limnodata[1:5,],limnotree)
```

<img src="man/figures/README-example1-2.png" width="100%" />

``` r
contoPD<-multiareaPD(contodata[1:5,],contotree)
```

<img src="man/figures/README-example1-3.png" width="100%" />

``` r
latiPD<-multiareaPD(latidata[1:5,],latitree)
```

<img src="man/figures/README-example1-4.png" width="100%" />

``` r
#complementarity PD calculation for a set of areas given a phylogeny
#(in red) and a selected area (LCL, in green)
homoPDcomp<-compareaPD(homodata[1:5,],homotree,LCL)
```

<img src="man/figures/README-example2-1.png" width="100%" />

``` r
#endemism PD calculation for a set of areas given a phylogeny
homoPDend<-endemismPD(homodata[1:5,],homotree)
```

<img src="man/figures/README-example3-1.png" width="100%" />

``` r
#drawing a Venn's diagram for a set of five areas given four
#different phylogenetic trees
vennout<-makeVennPD(list(homoPD,limnoPD,contoPD,latiPD))
```

<img src="man/figures/README-example4-1.png" width="100%" />

## References

Faith, P. D. (1992) Conservation evaluation and phylogenetic diversity.
Biological Conservation, 61, 1-10.

Faith, P. D. (2016) The PD phylogenetic diversity framework: linking
evolutionary history to feature diversity for biodiversity conservation.
In: Biodiversity Conservation and Phylogenetic Systematics, Topics in
Biodiversity and Conservation, 14, DOI 10.1007/978-3-319-22461-9_3.

Faith, P. D., Reid, C. A. M., and Hunter, J. (2004) Integrating
phylogenetic diversity, complementarity, and endemism for conservation
assessment. Conservation Biology, 18, 255-261.

Moritz, C. and Faith, D. P. (1998) Comparative phylogeography and the
identification of genetically divergent areas for conservation.
Molecular Ecology, 7, 419-429.
