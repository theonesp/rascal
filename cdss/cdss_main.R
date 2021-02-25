# SQL environment set up

#install.packages("RPostgreSQL")
library(RPostgreSQL)

# Load configuration settings
dbdriver <- 'PostgreSQL'
host  <- '127.0.0.1'

port <-'5432'
user <- 'postgres'
password <- 'postgres'

dbname <- 'simul_rep'
schema <- 'simul_rep_db'

# Connect to the database using the configuration settings
con <- dbConnect(dbDriver(dbdriver), dbname = dbname, host = host, port = port, 
                 user = user, password = password)
rm(password) # removes the password


# Set the default schema
dbExecute(con, paste("SET search_path TO ", schema, sep=" "))

# function avoiding add the connection data in each iteration
run_query<-function(query){
  query_output<-dbGetQuery(con,query)
  return(query_output)
}

# function avoiding add the connection data in each iteration
run_query<-function(query){
  query_output<-dbGetQuery(con,query)
  return(query_output)
}

# function for reading sql files from a folder
getSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""
  
  while (TRUE){
    line <- readLines(con, n = 1)
    
    if ( length(line) == 0 ){
      break
    }
    
    line <- gsub("\\t", " ", line)
    
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}


parameters <-run_query(getSQL('cdss/sql/parameters_extraction.sql'))    
