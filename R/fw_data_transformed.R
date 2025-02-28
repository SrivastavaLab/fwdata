#' @importFrom dplyr mutate filter %>% select rename group_by summarise left_join
#' @importFrom tidyr unite separate
#' @importFrom utils read.csv

## data accessing functions

##' @title Retrieve Functional Web Data Information
##'
##' @description This function fetches the information of a dataset from GitHub, specifically
##' the dataset related to either biomass or functional webs, depending on the provided argument.
##'
##' @param path Optional path to save the data.
##' @param biomass Logical indicating if the biomass dataset should be accessed. Defaults to FALSE.
##'
##' @return Returns the release information for the relevant dataset from GitHub.
##'
##' @export
fw_info <- function(path, biomass = FALSE) {
  if(biomass){
    repo <- "SrivastavaLab/bwgbiomass"
  } else {
    repo <- "SrivastavaLab/cesabfunctionalwebsdata"
  }

  datastorr::github_release_info(repo,
                                 filename="all_data.rds",
                                 read=readRDS,
                                 private = FALSE,
                                 path=NULL)
}

##' @title Get Available Versions of Data
##'
##' @description This function retrieves the available versions of data from either local storage
##' or GitHub, based on the specified parameters. If `local = FALSE`, the function will check for
##' the most recent version on GitHub.
##'
##' @param local Logical indicating if local or GitHub versions should be polled. Defaults to TRUE.
##' @param biomass Logical indicating if the biomass dataset should be accessed. Defaults to FALSE.
##' @param path Optional path to save the data.
##'
##' @return A list of available versions of the data.
##'
##' @export
fw_versions <- function(local=TRUE, path=NULL, biomass = FALSE) {
  datastorr::github_release_versions(fw_info(path, biomass), local)
}

##' @title Find Current Version of Data
##'
##' @description This function retrieves the current version of data, either from local storage or GitHub.
##'
##' @param local Logical indicating if local or GitHub versions should be polled. Defaults to TRUE.
##' @param biomass Logical indicating if the biomass dataset should be accessed. Defaults to FALSE.
##' @param path Optional path to save the data.
##'
##' @return The current version of the data.
##'
##' @export
fw_version_current <- function(local=TRUE, path=NULL, biomass = FALSE) {
  datastorr::github_release_version_current(fw_info(path, biomass), local)
}

##' @title Download CESAB Functionalwebs Data
##'
##' @description This function downloads a specific version of the CESAB Functionalwebs dataset.
##' If no version is provided, it will load the most recent version from either local storage or GitHub.
##' The `fw_del` function can be used to delete downloaded datasets.
##'
##' @param version Version number. Default is `NULL` to load the most recent version.
##' @param path Path to store the data at. Defaults to `NULL`, in which case `datastorr` will use `rappdirs` for best storage location.
##' @param biomass Logical indicating if the biomass dataset should be accessed. Defaults to FALSE.
##'
##' @return The specified version of the dataset.
##'
##' @export
fw_data <-  function(version=NULL, path=NULL, biomass = FALSE) {
  datastorr::github_release_get(fw_info(path, biomass), version)
}

### authentication --------------

##' @title Authenticate User and Set GITHUB_TOKEN
##'
##' @description This function checks if a valid GITHUB_TOKEN is already set. If not, it prompts the user
##' to enter a password, which is used to generate and store the GITHUB_TOKEN for future access.
##' The GITHUB_TOKEN is required for accessing private repositories.
##'
##' @return Invisibly returns `TRUE` after successful authentication and token setup.
##'
##' @export
fw_auth <- function() {
  # Check if a GITHUB_TOKEN is already set
  existing_token <- Sys.getenv("GITHUB_TOKEN")

  if (nzchar(existing_token)) {
    message("Using the existing GITHUB_TOKEN.")
    return(invisible(TRUE))
  }

  # If no token is set, proceed with generating a new one
  id <- system.file("identification.rds", package = "fwdata")

  if (id == "") {
    stop("Identification file not found in the fwdata package.")
  }

  id_raw <- readRDS(id)

  # Prompt the user for a password
  password <- getPass::getPass("Please enter your password: ")

  ## Decrypt the stored key
  k <- charToRaw(paste0(password, "1234"))
  aes <- digest::AES(k, mode = "ECB")
  p_w_ending <- aes$decrypt(id_raw)

  # Extract and set the token
  answer <- substr(p_w_ending, 1, 40)
  Sys.setenv(GITHUB_TOKEN = answer)

  message("A new GITHUB_TOKEN has been set.")
  return(invisible(TRUE))
}


# input files -------------------------------------------------------------

##' @title Load and Transform Data
##'
##' @description This function loads the CESAB Functionalwebs data and applies various transformations.
##' Additional files such as "Extra_traits.csv" and "taxon_level_traits.csv" are read and merged into the dataset.
##' It also applies specific data transformations based on traits, correcting certain values as specified by the user.
##'
##' @param version Version of the dataset to load. Default is "0.7.7".
##' @param path Path to store the data at. Defaults to `NULL`, in which case the system's default path is used.
##' @param biomass Logical indicating if the biomass dataset should be accessed. Defaults to FALSE.
##' @param private Logical indicating if the dataset should be accessed privately. Defaults to FALSE.
##'
##' @return The transformed dataset, including additional trait corrections and transformations.
##'
##' @export
fw_data_transformed <- function(version = "0.7.7", path = NULL, biomass = FALSE, private = FALSE) {

  extra_traits <- read.csv(file.path("inst", "extdata", "Extra_traits.csv"), stringsAsFactors = FALSE)
  taxon_level_traits <- read.csv(file.path("inst", "extdata", "taxon_level_traits.csv"), stringsAsFactors = FALSE)
  latest <- fw_data(version, path, biomass=biomass)

  corr.visits <- read.csv(file.path("inst", "extdata", "visits_correct_LatLong_corrected.csv"), stringsAsFactors = FALSE) %>%
    select(visit_id, latitude, cor_long) %>%
    mutate(visit_id = as.character(visit_id))

  #these are some trait values that were missing before, for now I've just done certain traits needed later in code

  # negate function ---------------------------------------------------------------------------------------------------------------------

  "%nin%" <- Negate(f = "%in%")

  #**fw start 4**
  #making Diptera.434 equal to Diptera.276 as per rodrigo's directions
  dip434<-latest$traits  %>%
    filter(bwg_name=="Diptera.276") %>%
    mutate(bwg_name = "Diptera.434") #ideally need to change this in abundance dataset too!

  #make Diptera.44 into a Forcipomyiinae ceratopogonid (diane's id based on rodrigo's photos)
  dip44<-latest$traits  %>%
    filter(bwg_name=="Diptera.44") %>%
    select(species_id:bwg_name) %>%
    cbind(latest$traits  %>%
            filter(bwg_name=="Diptera.156") %>%
            select(domain:BF4))

  #make Diptera.4 into a Dasyhelea ceratopogonid (#identification confirmed by Paula Omena)
  dip4<-latest$traits  %>%
    filter(bwg_name=="Diptera.4") %>%
    select(species_id:bwg_name) %>%
    cbind(latest$traits  %>%
            filter(bwg_name=="Diptera.670") %>%
            select(domain:BF4))

  #make Diptera.112 into a Culex (#identification confirmed by Diane based on photos from Rodrigo)
  dip112<-latest$traits  %>%
    filter(bwg_name=="Diptera.112") %>%
    select(species_id:bwg_name) %>%
    cbind(latest$traits  %>%
            filter(bwg_name=="Diptera.100") %>%
            select(domain:BF4))

  #make Diptera.61 into a Tanypodinae (#identification confirmed by Diane based on photos from Rodrigo)
  dip61<-latest$traits  %>%
    filter(bwg_name=="Diptera.61") %>%
    select(species_id:bwg_name) %>%
    cbind(latest$traits  %>%
            filter(bwg_name=="Diptera.69") %>%
            select(domain:BF4))

  #make Diptera.62 into a Chironominae (#identification confirmed by Diane based on photos from Rodrigo)
  dip62<-latest$traits  %>%
    filter(bwg_name=="Diptera.62") %>%
    select(species_id:bwg_name) %>%
    cbind(latest$traits  %>%
            filter(bwg_name=="Diptera.79") %>%
            select(domain:BF4))

  #make Diptera.42 into a Ceratopogoninae (#identification confirmed by Fabiola)
  dip42<-latest$traits  %>%
    filter(bwg_name=="Diptera.42") %>%
    select(species_id:bwg_name) %>%
    cbind(latest$traits  %>%
            filter(bwg_name=="Diptera.52") %>%
            select(domain:BF4))

  #make Diptera.38 into a Ceratopogoninae (#identification confirmed by Fabiola)
  dip38<-latest$traits  %>%
    filter(bwg_name=="Diptera.38") %>%
    select(species_id:bwg_name) %>%
    cbind(latest$traits  %>%
            filter(bwg_name=="Diptera.52") %>%
            select(domain:BF4))
  #**fw end 4**

  #**fw start 5**
  # housekeeping

  ## Note - still need to clean Limoniidae vs. Tipulidae (because not all parts of bwg are consistent with 2012 elevation of Limoniinae as family)
  ## Note - taxon_name=="Ocyptamus" this is a syrphid that accidentally was listed as a predator - check traits
  ## after reviewing taxonomy I did not alter subfamily Limoniinae as appears used consistently in database

  taxa_to_update<-taxon_level_traits$taxon_name %>% as.list()
  oddities<-extra_traits$taxon_name %>% as.list()
  # Combine all rows to be added into a single data frame
  rows_to_add <- bind_rows(dip44, dip4, dip112, dip61, dip62, dip42, dip38)

  latest$traits <- latest$traits %>%
    mutate(realm = replace(realm, bwg_name%in%c("Coleoptera.20"), "aquatic"), #pers. comm from Barbara Richardson
           realm = replace(realm, bwg_name%in%c("Coleoptera.60","Coleoptera.47"), "terrestrial"), #pers. comm from Vinicius Farjalla
           family = replace(family,family=="Vellidae", "Veliidae"),
           family = replace(family, family=="Axymiidae", "Syrphidae"),
           subfamily = replace(subfamily, bwg_name=="Branchiopoda.3","Daphniinae"),
           subord = replace(subord, subord=="Zigoptera", "Zygoptera"),
           ord = replace(ord, ord=="Opisthopora<U+FFFD>", "Opisthopora"),
           taxon_name = replace(taxon_name, bwg_name == "Odonata.9", "Oreiallagma oreas"),
           genus = replace(genus, bwg_name == "Odonata.9", "Oreiallagma"),
           species = replace(species, bwg_name == "Odonata.9", "oreas"),
           taxon_level = replace(taxon_level,bwg_name == "Odonata.9", "species"),
           taxon_name = replace(taxon_name, bwg_name == "Diptera.7", "Brachycera"),
           family = replace(family, bwg_name == "Diptera.7", NA),
           taxon_level = replace(taxon_level,bwg_name == "Diptera.7", "subord"),
           subfamily = replace(subfamily, genus == "Culicoides", "Ceratopogoninae_omnivore"),
           taxon_name = replace(taxon_name, bwg_name=="Branchiopoda.3", "Daphniinae"),
           taxon_name = replace(taxon_name, taxon_name=="Axymiidae", "Syrphidae"),
           taxon_name = replace(taxon_name, taxon_name=="Zigoptera", "Zygoptera"),
           taxon_name = replace(taxon_name, bwg_name == "Oligochaeta.13", "Oligochaeta_Naididae_and_Enchytraeidae"),
           MD6 = replace(MD6, ord =="Odonata", 3),
           MD1 = replace(MD1, ord =="Odonata", 0),
           FD1 = replace(FD1, phylum=="Platyhelminthes", 3),
           FD6 = replace(FD6, phylum =="Platyhelminthes", 3),
           FG4 = replace(FG4, phylum =="Platyhelminthes", 2),
           CP1 = replace(CP1, phylum =="Platyhelminthes",3),
           CP2 = replace(CP2, phylum =="Platyhelminthes",0),
           CP3 = replace(CP3, phylum =="Platyhelminthes",0),
           FD8 = replace(FD8, family =="Elateridae", 3),
           FG6 = replace(FG6, family =="Elateridae", 3),
           MD6 = replace(MD6, family =="Elateridae", 3),
           LO4 = replace(LO4, family =="Elateridae", 3),
           FD1=replace(FD1, bwg_name=="Diptera.332",0),#corrected a syphid larvae known to be a predator and only a predator
           FD2=replace(FD2, bwg_name=="Diptera.332",0),
           FD3=replace(FD3, bwg_name=="Diptera.332",0),
           FD4=replace(FD4, bwg_name=="Diptera.332",0),
           FD5=replace(FD5, bwg_name=="Diptera.332",0),
           FD6=replace(FD6, bwg_name=="Diptera.332",0),
           FD7=replace(FD7, bwg_name=="Diptera.332",0),
           FD4=replace(FD4, bwg_name=="Diptera.49",1),
           FD8=replace(FD8, bwg_name=="Diptera.49",0),
           FD3 = replace(FD3, genus =="Rhabdomastrix", 3),
           FD4 = replace(FD4, genus =="Limonia", 3),
           FD3 = replace(FD3, bwg_name=="Diptera.430", 3),
           FD1 = replace(FD1, bwg_name=="Diptera.430", 3),
           FD8 = replace(FD8, bwg_name=="Diptera.430", 1),
           family = replace(family, bwg_name=="Diptera.430", "Sarcophagidae"),
           taxon_name = replace(taxon_name, bwg_name=="Diptera.430", "Sarcophagidae"),
           subphylum = replace(subphylum, class=="Insecta","Hexapoda"),
           subfamily = replace(subfamily, genus=="Corethrella","Corethrellinae"),
           species = replace(species, taxon_name=="Culex_albipes","Culex_albipes"),
           species = replace(species, taxon_name=="Culex_aphylactus","Culex_aphylactus"),
           domain = replace(domain, kingdom=="Animalia","Eukaryota"),
           subclass = replace(subclass, ord=="Diptera","Neoptera"),
           FD4 = replace(FD4, taxon_name=="Sphaeromias",0),
           FD8 = replace(FD8, taxon_name=="Sphaeromias",3),
           family = replace(family, family=="Enchytraeoidae","Enchytraeidae"),
           taxon_name = replace(taxon_name, family=="Enchytraeidae","Enchytraeidae"),
           BS3 = replace(BS3, family=="Enchytraeidae",0),
           BS4 = replace(BS4, family=="Enchytraeidae",3),
           BS5 = replace(BS5, family=="Enchytraeidae",0),
           LO7 = replace(LO7, family=="Enchytraeidae",0),
           species = if_else(!is.na(species), paste0(genus, "_", species), species)) %>%
    filter(!bwg_name %in% c("Diptera.276", "Diptera.44", "Diptera.4", "Diptera.112", "Diptera.61", "Diptera.62", "Diptera.42", "Diptera.38")) %>%
    bind_rows(rows_to_add)

  traits_species_names <- latest$traits %>%
    # Separate the taxa for updating and not updating, process and rebind them
    filter(taxon_name %nin% taxa_to_update) %>%
    rbind(
      latest$traits %>%
        filter(taxon_name %in% taxa_to_update) %>%
        select(species_id:bwg_name, functional_group:barcode, taxon_name) %>%
        left_join(taxon_level_traits) %>%
        relocate(functional_group:barcode, .after = subspecies) %>%
        relocate(taxon_name, .after = taxon_level)
    ) %>%
    # Now handle oddities and rebind them
    filter(taxon_name %nin% oddities) %>%
    rbind(
      latest$traits %>%
        filter(taxon_name %in% oddities) %>%
        select(-(AS1:BF4)) %>%
        left_join(extra_traits, by = c("taxon_name" = "taxon_name"))
    )

  species_long_list <- traits_species_names$bwg_name %>%
    unique() %>%
    as.list()

  #starting with traits --------------------------------------------------------

  traits<-traits_species_names %>%
    filter(bwg_name %in% species_long_list)

  traits_pub<-traits_species_names %>%
    filter(bwg_name %in% species_long_list)

  dataset_list<-datasets$dataset_id %>% as.list()

  #Sarah - can you rewrite the next two lines of code so it inserts the current date
  datasets_pub <- datasets %>%
    filter(public_release < Sys.Date())

  public_datasets <-datasets_pub$dataset_id %>% as.list()

  #now just the visits to archive-------------------------

  visits <-latest$visits%>%
    filter(dataset_id %in% dataset_list) %>%
    select(-latitude, -longitude) %>%
    left_join(corr.visits) %>%
    rename(longitude = cor_long)

  visits_pub <-visits %>%
    filter(dataset_id %in% public_datasets)

  #now just the bromeliads in the datasets to archive-----------------------------------------

  bromeliads <-latest$bromeliads%>%
    filter(dataset_id %in% dataset_list) %>%
    filter(bromeliad_id %nin% c("5846", "7956", "9856"))

  bromeliads_pub <- bromeliads %>%
    filter(dataset_id %in% public_datasets)

  public_bromeliads <-bromeliads_pub$bromeliad_id %>% as.list()
  bromeliads_list <-bromeliads$bromeliad_id %>% as.list()

  #and the matching abundance info, filtered for the truly aquatic macro species

  abundance <-latest$abundance %>%
    filter(bromeliad_id %in% bromeliads_list) %>%
    filter(bwg_name %in% species_long_list) %>%
    mutate(species_id=replace(species_id, bwg_name=="Diptera.276","4321")) %>%
    mutate(bwg_name=replace(bwg_name, bwg_name=="Diptera.276","Diptera.434")) %>%
    unite(col = "all", dataset_id:bromeliad_id, sep = "/") %>%
    group_by(all) %>%
    summarise(abundance = sum(abundance)) %>%
    separate(all, into = c("dataset_id","species_id", "bwg_name", "bromeliad_id"), sep = "/")

  abundance_pub <- abundance %>%
    filter(bromeliad_id %in% public_bromeliads) %>%
    filter(bwg_name %in% species_long_list)

  # saving output as a list of dfs -----------------------------------------------------------------------------------------------------------------
  # Public data
  public_data <- list(
    datasets = datasets_pub ,
    visits = visits_pub,
    traits = traits_pub,
    bromeliads = bromeliads_pub,
    abundance = abundance_pub
  )

  # Private data
  private_data <- list(
    datasets = datasets,
    visits = visits,
    traits = traits,
    bromeliads = bromeliads,
    abundance = abundance
  )

  #Sarah - here I have these saved to csv, but actually we want fwdata to export these as a list of dataframes
  #I would like the ones with public_ at the start to be accessed by fwdata without a password
  #and the ones without public_ at the start at the start to be accessed only with a password

  # Step 5: Use fw_auth() for private data access
  if (private) {
    # Call fw_auth to check authentication
    fw_auth()
    message("Accessing private data.")
    return(private_data)
  }

  # Step 4: Return only public data if no private access requested
  message("Accessing public data only.")
  return(public_data)
}
