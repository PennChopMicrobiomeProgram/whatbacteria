
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whatbacteria

<!-- badges: start -->

[![R-CMD-check](https://github.com/PennChopMicrobiomeProgram/whatbacteria/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PennChopMicrobiomeProgram/whatbacteria/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Taxon phenotype and susceptibility databases for the mirix package.

## Installation

You can install the development version of whatbacteria from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("PennChopMicrobiomeProgram/whatbacteria")
```

## Usage

``` r
library(whatbacteria)
```

To access the databases directly:

``` r
head(taxon_phenotypes)
#>            taxon   rank    aerobic_status    gram_stain                     doi
#> 1 Actinomycetota Phylum              <NA> Gram-positive   10.1128/MMBR.00019-15
#> 2   Bacteroidota Phylum              <NA> Gram-negative                    <NA>
#> 3      Bacillota Phylum              <NA> Gram-positive 10.1099/00207713-28-1-1
#> 4 Pseudomonadota Phylum              <NA> Gram-negative                    <NA>
#> 5  Negativicutes  Class              <NA> Gram-negative    10.4056/sigs.2981345
#> 6     Clostridia  Class obligate anaerobe Gram-positive                    <NA>
head(taxon_susceptibility)
#>              taxon  rank antibiotic       value                      doi
#> 1    Lactobacillus Genus vancomycin   resistant     10.1128/AEM.01738-18
#> 2      Leuconostoc Genus vancomycin   resistant   10.1053/jhin.1998.0605
#> 3      Pediococcus Genus vancomycin   resistant   10.1053/jhin.1998.0605
#> 4   Erysipelothrix Genus vancomycin   resistant   10.1053/jhin.1998.0605
#> 5       Mycoplasma Genus vancomycin   resistant      10.1038/nrmicro1464
#> 6 Faecalibacterium Genus vancomycin susceptible 10.3389/fmicb.2017.01226
```

To get a particular lineage’s susceptibilities:

``` r
what_antibiotic(
   c("Enterococcus faecalis", "Lactobacillus", "Lactobacillus delbrueckii"),
   "vancomycin")
#> [1] NA            "resistant"   "susceptible"
```

To get a particular lineage’s phenotypes:

``` r
what_phenotype(
   c("Bacteroidetes", "Firmicutes", "Firmicutes; Negativicutes"),
   "gram_stain")
#> [1] "Gram-negative" "Gram-positive" "Gram-negative"
```
