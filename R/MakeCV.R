#' Extract your publications and get your cites.
#'
#'
#' @import scholar
#' @import ggplot2
#' @import stringr
#' @import dplyr
#' @param user Google Scholar user ID.
#' @export
#' @return CV cites.
#' @examples \dontrun{
#' makeCV(user="y3nT7tkAAAAJ&hl")
#'}



makeCV <- function(df, orcid=orcid){ 

  # First: 
    df<-get_publications(id)
  colnames(df)<-c("title","authors","journal","number","total.citations","publication.date","cid","pubid")
  df<-df[!is.na(df$journal),] 
  df<-df[!is.na(df$publication.date),] 
  df$year<-as.numeric(str_split_fixed(df$publication.date,"/",2)[,1])
  df <- df %>% dplyr::filter(journal != "")

  # Second 
  data <- oa_fetch(
    entity = "works", author.orcid = c("0000-0002-7288-6336"),  verbose = TRUE)
  data$title <- gsub("<i>|</i>", "", data$title)
  
  j <- data.frame()
  for (s in 1:nrow(data)) {
    data1 <- data$author[[s]]
    formatted_authors_row <- extract_authors(data1)
    j[s, "citation_names"] <- formatted_authors_row
  }
  
  j$title <- data$title
  
  df1 <- df %>% left_join(j, by ="title")
  
  # Some names does not match 
  df1$number <- trimws(df1$number)
  
  df1$cv <- paste0(df1$citation_names, " (",df1$year, "). ", df1$title, ". ", df1$journal,", ",df1$number) 
  df1$cv <- gsub("\\.\\s*,", ".,", df1$cv)  
  df1$cv <- gsub("\\s+", " ", df1$cv)
  df1$cv <- trimws(df1$cv)

  return(df1)
}


format_name <- function(name) {
  name_parts <- strsplit(name, " ")[[1]]  # Split the name by spaces
  last_name <- name_parts[length(name_parts)]  # Extract the last name
  first_initial <- substr(name_parts[1], 1, 1)  # Extract the first initial
  
  if (length(name_parts) > 2) {  # If there is a middle name
    middle_initial <- substr(name_parts[2], 1, 1)  # Extract middle initial
    return(paste0(last_name, ", ", first_initial, ".", middle_initial, "."))
  } else {  # No middle name
    return(paste0(last_name, ", ", first_initial, "."))
  }
}

# Function to extract and format authors from a nested structure (i.e., from df$author)
extract_authors <- function(nested_df) {
  first_author <- c()
  middle_author <- c()
  senior_author <- c()
  
  # Loop through each author in the nested structure
  for (i in 1:nrow(nested_df)) {
    name <- nested_df$au_display_name[i]
    position <- nested_df$author_position[i]
    
    # Append the formatted name to the correct author group
    if (position == "first") {
      first_author <- c(first_author, format_name(name))  # Append to vector
    } else if (position == "middle") {
      middle_author <- c(middle_author, format_name(name))  # Append to vector
    } else if (position == "last" || position == "senior") {  # Check for last or senior
      senior_author <- c(senior_author, format_name(name))  # Append to vector
    }
  }
  
  # Combine the authors for each group
  first_author_combined <- paste(first_author, collapse = ", ")
  middle_author_combined <- paste(middle_author, collapse = ", ")
  senior_author_combined <- paste(senior_author, collapse = ", ")
  
  # Return the full citation string, combining authors and adding '&' where appropriate
  if (length(senior_author_combined) > 0 && length(middle_author_combined) > 0) {
    return(paste(first_author_combined, ", ", middle_author_combined, ", &", senior_author_combined))
  } else if (length(senior_author_combined) > 0) {
    return(paste(first_author_combined, " &", senior_author_combined))
  } else {
    return(first_author_combined)
  }
}
