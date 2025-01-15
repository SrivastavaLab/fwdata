

#This scripts generates two functions used in later scripts:
#       (1)  identify_higher_levels
#       (2)  bottom.up



identify_higher_levels <- function(levels_to_search,
                                   levels_to_return, 
                                   target_name,
                                   target_level, # NAME of resource level
                                   trait_table){
  
  # where in level list is the target's level
  index.tax.resource = which(target_level == levels_to_search)
  
  # where is the row for this animal? find it's name on the list
  row.resource = which(trait_table[,target_level] == target_name)
  tree.resource = c()
  for (lev in levels_to_search[index.tax.resource:length(index.tax.resource)]){
    if (lev %in% levels_to_return){
      tree.resource = c(tree.resource, trait_table[row.resource,lev])
    }
  }
  
  # only unique names please
  tree.resource = unique(tree.resource)
  tree.resource = tree.resource[!is.na(tree.resource)]
  return(tree.resource)
}

bottom.up = function(consumer, resource, trait_table){
  
  
  # logic to either stop or coerce if trait_table is a tbl
  if ("tbl_df" %in% class(trait_table)) stop("no, pass in trait_table as a data.frame please")
  if (grepl(' ', consumer)) stop("no, no, no, there is a space in '", consumer, "', I won't go further...")
  if (grepl(' ', resource)) stop("no, no, no, there is a space in '", resource, "', I won't go further...")
  
  ## define names of taxonomic groups to search for and to return
  tax.levels = c('phylum', 'class', 'subclass', 'ord', 'subord', 'family',
                 'subfamily', 'tribe', 'genus', 'species', 'bwg_name', 'taxon_name')
  tax.to.return = c('phylum','class', 'subclass','ord','subord', 'family',
                    'subfamily', 'genus', 'species', 'bwg_name', 'taxon_name')
  
  # take columns with names matching tax.levels
  phylogeny = trait_table[,tax.levels]
  #DS added subclass, subord, subfamily
  # first get the taxonomic level of the two taxa
  # booleans to know if the resource and consumer were found in the database
  res.found = FALSE
  cons.found = FALSE
  # loop on the different taxonimic level (do I find the name at the class level, at the order level,...)
  for (level in tax.levels){
    if (resource %in% phylogeny[,level]){
      #If I did not already found the name in a higher taxonomic level I go
      # This is to solve the issue of having the same name at genus level and taxon name
      # in such case I want to return the genus name, not the taxon name
       if (!res.found){
         resource.level = level
         res.found = TRUE
         # cat('resource.level = ', resource.level, '\n')
       }
    }
    if (consumer %in% phylogeny[,level]){
      if (!cons.found){
        consumer.level = level
        cons.found = TRUE
        # cat('consumer.level = ', consumer.level)
      }
    }
  }
  
  ## some resources are found in the phylogeny table above -- but not all! there
  ## are also "generic resources"
  
  generic_foods <- c("FPOM","CPOM", "bromeliad.tissue", "algae.diatom", "algae.other",     
               "cyanobacteria", "fungi","bacteria", "ciliates", "amoeba", 
               "rotifers", "other.protozoans", "Copepoda", "Acari")
  
  if (resource %in% generic_foods){
    # indicate resource found if generic
    if (!res.found){
      resource.level = level
      res.found = TRUE
      # cat('resource.level = ', resource.level, '\n')
    }
  }
  
  if (consumer %in% generic_foods){
    # indicate resource found if generic
    if (!cons.found){
      consumer.level = level
      cons.found = TRUE
      # cat('resource.level = ', resource.level, '\n')
    }
  }
  
  # generate an error if one name is not found in the database
  if (!cons.found){
    stop('consumer species ', consumer, ' not found in the database')
  }
  if(!res.found){
    stop('resource species ', resource, ' not found in the database')
  }
  
  # identify the higher taxa of resource and consumer
  tree.resource <- identify_higher_levels(
    levels_to_search = tax.levels,
    levels_to_return = tax.to.return,
    target_name = resource, 
    target_level = resource.level,
    trait_table = trait_table)
  
  # if there are no resources and if its a generic food, use just its name
  if(length(tree.resource) == 0 & resource %in% generic_foods){
    tree.resource <- resource
  }
  
  tree.consumer <- identify_higher_levels(
    levels_to_search = tax.levels,
    levels_to_return = tax.to.return,
    target_name = consumer, 
    target_level = consumer.level,
    trait_table = trait_table)
  
  # if there are no CONSUMERS and if its a generic food, use just its name
  if(length(tree.consumer) == 0 & consumer %in% generic_foods){
    tree.consumer <- consumer
  }
  
  # expand these out
  result = expand.grid(tree.consumer, tree.resource, stringsAsFactors = FALSE)
  names(result) = c('consumer', 'resource')
  # result = unique(result)
  return(result)
}





















# edit this function to use only a data.frame

top.down = function(consumer, resource, database){
  tax.levels = c('class', 'subclass', 'ord', 'subord', 'family', 'subfamily', 'tribe', 'genus', 'species', 'bwg_name')
  tax.to.return = c('class', 'ord', 'family', 'genus', 'species', 'bwg_name')
  phylogeny = database$traits[,tax.levels]
  # first get the taxonomic level of the two taxa
  for (level in tax.levels){
    if (resource %in% phylogeny[,level]){
      resource.level = level
    }
    if (consumer %in% phylogeny[,level]){
      consumer.level = level
    }
  }
  # then get all the level above
  index.tax.resource = which(resource.level == tax.levels)
  row.resource = which(database$traits[,resource.level] == resource)
  tree.resource = c()
  for (lev in tax.levels[index.tax.resource:length(index.tax.resource)]){
    if (lev %in% tax.to.return){
      tree.resource = c(tree.resource, database$traits[row.resource,lev])
    }
  }
  tree.resource = unique(tree.resource)
  cat('tree resource = ', tree.resource, '\n')
  index.tax.consumer = which(consumer.level == tax.levels)
  row.consumer = which(database$traits[,consumer.level] == consumer)
  tree.consumer = c()
  for (lev in tax.levels[index.tax.consumer:length(index.tax.consumer)]){
    if (lev %in% tax.to.return){
      tree.consumer = c(tree.consumer, database$traits[row.consumer,lev])
    }
  }
  tree.consumer = unique(tree.consumer)
  cat('tree consumer = ', tree.consumer, '\n')
  result = expand.grid(tree.consumer, tree.resource)
  names(result) = c('consumer', 'resource')
  # result = unique(result)
  return(result)
}





# This function return all the bgw name in a given genera/order/.... until class
# what.is.it('Culex', database) wil inform you that Culex is a genus and will give you all the known species in this genus
what.is.it = function(one.name, database){
  # @name: a identification at a given specific taxonomic level 
  # @ database: the fw_database used for copmarisons
  # will return a list of two elements:
  # 1- the taxnomic level identified for 'one.name' 
  # 2- all the bwg_names that belongs to the taxnomic level identified for 'one.name'
  tax.levels = c('class', 'subclass', 'ord', 'subord', 'family', 'subfamily', 'tribe', 'genus', 'species', 'bwg_name')
  phylogeny = database$traits[,tax.levels]
  for (level in tax.levels){
    if (one.name %in% phylogeny[,level]){   ## May need to change to phylogeny[,level][[1]]
      species.list = phylogeny$bwg_name[which(phylogeny[,level] == one.name)]
      right.level = level
    }
  }
  
  return(list(right.level, species.list))
}

# This function is the opposite of what.is.it. (going towar higher taxonomic resolution)
# what.is.it('Culex', database) wil inform you that Culex is a genus and will give you all the known species in this genus
what.is.it.back = function(one.name, database){
  # @name: a identification at a given specific taxonomic level 
  # @ database: the fw_database used for copmarisons
  # will return a list of two elements:
  # 1- the taxnomic level identified for 'one.name' 
  # 2- all the bwg_names that belongs to the taxnomic level identified for 'one.name'
  tax.levels = c('class', 'subclass', 'ord', 'subord', 'family', 'subfamily', 'tribe', 'genus', 'species', 'bwg_name')
  phylogeny = database$traits[,tax.levels]
  for (level in tax.levels){
    if (one.name %in% phylogeny[,level]){   ## May need to change to phylogeny[,level][[1]]
      species.list = phylogeny$bwg_name[which(phylogeny[,level] == one.name)]
      right.level = level
    }
  }
  
  return(list(right.level, species.list))
}
