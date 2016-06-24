# `fwdata` : accessing data

small, lightweight package for accessing BWG data for the CESAB FUNCTIONALWEBS projects

## Installation

```r
library(devtools)
install_github("richfitz/datastorr")
install_github("SrivastavaLab/fwdata")
```

How to use it

```r

## first, sign in
fw_auth()

## then, load a specific version of the dataset
fw_data("0.0.1")

```
