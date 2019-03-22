# Data uit apr bestand ---------------------------------------

library('XML')
library(data.table)
a3resultsadaptive <- function(filename) {
  
  #important to say stringsAsFactors = FALSE!!!
  data <- xmlParse(filename)
    #hier worden een aantal variabelen gedefinieerd die je gaat opnemen in raw
    #in RT.Rmd worden nog een aantal variabelen gedefinieerd
  parameter=as.numeric(sapply(data["//procedure/parameter"], xmlValue))
  result=sapply(data["//result"], xmlValue)
    #virtual trial wordt toegevoegd zodat result even lang zou zijn als parameter
    #er wordt in apex een virtual trial gegenereerd - deze trial zou de volgende zijn die zou worden aangeboden als het niet zou afgebroken zijn 
  virtualtrial = (length(result)+1)
  result[virtualtrial] <- "NaN"

  raw <- data.frame(
  parameter,
  result,
  stringsAsFactors=FALSE
  )
  
  meantrials <- mean(tail(parameter,4))
  
  #hier creëer je een vecor die alle informatie bevat
  r <- c()
  r$raw <- raw
  r$mean <- data.frame(meantrials)
  
  entries <- data["//interactive/entry"];
  for (i in 1:length(entries)) {
    a <- xmlAttrs(entries[[i]] );
    if ("description" %in% labels(a)) {
      r$interactive[[ a[["description"]] ]] <- a[["new_value"]]
    }
    if ("expression" %in% labels(a)) {
      r$interactive[[ a[["expression"]] ]] <- a[["new_value"]]
    }
  }
  if (! ("interactive" %in% names(r)) ) {
    entries <- xmlToDataFrame(nodes = data["//general/interactive/entry"], stringsAsFactors = FALSE);
    for (i in 1:nrow(entries)) {
      r$interactive[[ entries[i, "expression"] ]]  <- entries[i, "new_value"] 
      r$interactive[[ entries[i, "description"] ]]  <- entries[i, "new_value"] 
    }
  }

  
  return(r)
}
