############################################
####### Public PHD Functions Library #######
####### Marvin Law PHD   ###################
############################################

# Check if numeric responses fit threshold

numthresholdcheck <- function(data, column, num1, num2) {
  dat <- data %>% 
    select(column) %>% 
    mutate_all(as.numeric) %>% 
    filter(. <num1 | .>num2)
  
  if(dim(dat)[1] != 0) {
    return(dat)
  } else {"All OK"}
}

# Check if character responses fit threshold

charthresholdcheck <- function(data, column, char) {
  dat <- data %>% 
    select(column) %>%
    filter_all(any_vars(!. %in% char))
  
  if(dim(dat)[1] != 0) {
    return(dat)
  } else {"All OK"}
}

# Creates a histogram of a variable in a data file

Histo <- function(data, variable) {
  data %>% 
    ggplot(aes_string(x = variable, color = variable)) +
    geom_histogram() +
    scale_color_brewer(palette = "Set1") +
    xlab(renamevarnames(variable))
}

# Correlation with significance asterisks and APA formatting
# Function adapted from Bertolt: "http://myowelt.blogspot.com/2008/04/beautiful-correlation-tables-in-r.html"

corstarsl <- function(x){ 
  require(Hmisc) 
  require(tidyverse)
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "**", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  Rnew <- Rnew %>% 
    rownames_to_column() %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_all(list(~str_replace(., "0.", ".")))
  
  rownames(Rnew) <- Rnew$rowname
  
  Rnew <- Rnew %>% 
    select(-rowname)
  
  rownames(Rnew) <- c(paste0(1:nrow(Rnew), ". ", rownames(Rnew)))
  colnames(Rnew) <- 1:ncol(Rnew)
  
  return(Rnew) 
}

corstarssigcheck <- function(x){ 
  require(Hmisc) 
  require(tidyverse)
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "**", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## truncate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their appropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  Rnew <- Rnew %>% 
    rownames_to_column() %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_all(list(~str_replace(., "0.", ".")))
  
  rownames(Rnew) <- Rnew$rowname
  
  Rnew <- Rnew %>% 
    select(-rowname)

  return(Rnew) 
}
