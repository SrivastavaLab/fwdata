## data accessing functions



## Core data:
fw_info <- function(path) {
  datastorr::github_release_info("SrivastavaLab/cesabfunctionalwebsdata",
                                 filename=NULL,
                                 read=readRDS,
                                 private = FALSE,
                                 path=NULL)
}

##' @title Get available versions of data
##'
##' @param local Logical indicating if local or github versions should
##'   be polled.  With any luck, \code{local=FALSE} is a superset of
##'   \code{local=TRUE}.  For \code{fw_version_current}, if
##'   \code{TRUE}, but there are no local versions, then we do check
##'   for the most recent github version.
##' @export
fw_versions <- function(local=TRUE, path=NULL) {
  datastorr::github_release_versions(fw_info(path), local)
}

##' @title Find current version
##'
##' @param local Logical indicating if local or github versions should
##'   be polled.
##'
##' @export
fw_version_current <- function(local=TRUE, path=NULL) {
  datastorr::github_release_version_current(fw_info(path), local)
}



##' @title Download CESAB Functionalwebs Data
##'
##' @param version Version number.  The default will load the most
##'   recent version on your computer or the most recent version known
##'   to the package if you have never downloaded the data before.
##'   With \code{fw_del}, specifying \code{version=NULL} will
##'   delete \emph{all} data sets.
##'
##' @param path Path to store the data at.  If not given,
##'   \code{datastorr} will use \code{rappdirs} to find the best place
##'   to put persistent application data on your system.  You can
##'   delete the persistent data at any time by running
##'   \code{fw_del(NULL)} (or \code{fw_del(NULL, path)} if you
##'   use a different path).
##'
##' @export
fw_data <-  function(version=NULL, path=NULL) {
  datastorr::github_release_get(fw_info(path), version)
}

### authentication --------------


#' Asks for your password
#'
#' This password is only for members of the CESAB functional webs group. Please do not add it to your code!
#'
#'
#' @export
fw_auth <- function(){
  id <- system.file("identification.rds", package = "fwdata")

  id_raw <- readRDS(id)

  password <- getPass::getPass()

  ## decrypt
  k <- charToRaw(paste0(password, "1234"))
  aes <- digest::AES(k, mode = "ECB")
  p_w_ending <- aes$decrypt(id_raw)

  answer <- substr(p_w_ending, 1, 40)

  Sys.setenv(GITHUB_TOKEN=answer)
}
