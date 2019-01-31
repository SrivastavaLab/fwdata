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
library(fwdata)
## first, sign in -- you will need a secret password from Andrew.
## DO NOT WRITE IT DOWN
fw_auth()

## then, load a specific version of the dataset
fw_data("0.0.1")

## What datasets do you have on YOUR machine
fw_versions()

## What datasets exist so far?
fw_versions(local = FALSE)

```

To access the BWG biomass estimate data, use the switch `biomass = TRUE`:

```r
## then, load a specific version of the dataset
fw_data("0.0.9000", biomass = TRUE)

## What datasets do you have on YOUR machine
fw_versions(biomass = TRUE)

## What datasets exist so far?
fw_versions(local = FALSE, biomass = TRUE)
```
