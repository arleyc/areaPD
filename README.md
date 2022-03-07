
<!-- README.md is generated from README.Rmd. Please edit that file -->

# areaPD

<!-- badges: start -->
<!-- badges: end -->

The goal of areaPD is to calculate phylogenetic diversity (PD) for a set
of geographic areas, calculating complementarity PD given a selected
area, and constructing a Venn’s diagram of PD within and between areas,
given one or more phylogenetic trees and an assignment of samples (e.g.,
alleles, species, etc.) to areas.

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
data("limnodata")
data("limnotree")
limnoPD<-multiareaPD(limnodata[1:5,],limnotree)
```

<img src="man/figures/README-example1-2.png" width="100%" />

``` r
data("contodata")
data("contotree")
contoPD<-multiareaPD(contodata[1:5,],contotree)
```

<img src="man/figures/README-example1-3.png" width="100%" />

``` r
data("latidata")
data("latitree")
latiPD<-multiareaPD(latidata[1:5,],latitree)
```

<img src="man/figures/README-example1-4.png" width="100%" />

``` r
#complementary PD calculation for a set of areas given a phylogeny
#(in red) and a selected area (LCL, in green)
homoPDcomp<-compareaPD(homodata[1:5,],homotree,LCL)
```

<img src="man/figures/README-example2-1.png" width="100%" />

``` r
limnoPDcomp<-compareaPD(limnodata[1:5,],limnotree,LCL)
```

<img src="man/figures/README-example2-2.png" width="100%" />

``` r
contoPDcomp<-compareaPD(contodata[1:5,],contotree,LCL)
```

<img src="man/figures/README-example2-3.png" width="100%" />

``` r
latiPDcomp<-compareaPD(latidata[1:5,],latitree,LCL)
```

<img src="man/figures/README-example2-4.png" width="100%" />

``` r
#drawing a Venn's diagram for a set of five areas given four
#different phylogenetic trees
vennout<-makeVennPD(list(homoPD,limnoPD,contoPD,latiPD))
```

<img src="man/figures/README-example3-1.png" width="100%" />
