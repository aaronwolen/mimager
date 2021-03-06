# mimager: The Microarray Imager

[![Travis-CI Build Status](https://travis-ci.org/aaronwolen/mimager.svg?branch=master)](https://travis-ci.org/aaronwolen/mimager)
[![codecov](https://codecov.io/gh/aaronwolen/mimager/branch/master/graph/badge.svg)](https://codecov.io/gh/aaronwolen/mimager)
[![Bioc](http://www.bioconductor.org/shields/years-in-bioc/mimager.svg)](https://www.bioconductor.org/packages/devel/bioc/html/mimager.html#since)

*mimager* simplifies the process of imaging microarrays and inspecting them for spatial artifacts by providing a consistent visualization interface that supports many of Bioconductor's microarray object classes.

## Installation

You can install the latest release from Bioconductor:

```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("mimager")
```

or the current development version using `devtools`:

```r
# library(devtools)
install_github("aaronwolen/mimager", build_vignettes = TRUE)
```

## Example

```r
library(mimager)
library(affydata)
data("Dilution")

mimage(Dilution, transform = arle, nrows = 1, legend.label = "RLE")
```

![mimager example](http://i.imgur.com/2Wf4y8v.jpg)
