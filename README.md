# fwdata
small, lightweight package for accessing BWG data for the CESAB FUNCTIONALWEBS projects

How to use it

```r
fw_auth()

ourdata <- datastorr::github_release_info("SrivastavaLab/cesabfunctionalwebsdata",
                               read=readRDS)

datastorr::github_release_version_current(ourdata, local = FALSE)

d <- datastorr::github_release_get(ourdata, "0.0.1")

```
