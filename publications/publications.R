render_publications <- function(year = NULL, author = "Bürkner, P. C.", bold_author = author,
                                status = NULL, project = NULL) {
  # Read BibTeX file
  pubs <- suppressMessages(suppressWarnings(
    bib2df::bib2df("../publications/publications.bib")
  ))

  if (!is.null(status)) {
    pubs <- pubs[pubs$STATUS %in% status, ]
  }

  if (!is.null(year)) {
    pubs <- pubs[pubs$YEAR %in% year, ]
    # status overwrites year
    pubs <- pubs[is.na(pubs$STATUS) | pubs$STATUS %in% "published", ]
  }

  if (!is.null(author)) {
    sel <- sapply(pubs$AUTHOR, function(authors) any(authors %in% author))
    pubs <- pubs[sel, ]
  }

  if (!is.null(project)) {
    projects <- strsplit(pubs$PROJECTS, ",")
    projects <- lapply(projects, trimws)
    sel <- sapply(projects, function(ps) any(ps %in% project))
    pubs <- pubs[sel, ]
  }

  for (i in seq_len(nrow(pubs))) {
    pub <- pubs[i, ]
    
    # Format citation
    authors <- paste(pub$AUTHOR[[1]], collapse = ", ")

    # Bold name
    if (!is.null(bold_author)) {
      authors <- gsub(bold_author, paste0("**", bold_author, "**"), authors)
    }

    # display "in review" and "accepted" status
    year <- if (!anyNA(pub$STATUS)) pub$STATUS else pub$YEAR

    citation <- sprintf("- %s (%s). %s. *%s*.", authors, year, pub$TITLE, pub$JOURNAL)

    if (!anyNA(pub$NOTE)) {
      citation <- paste0(citation, " ", pub$NOTE, ".")
    }
    
    # Add DOI only if present
    if (!anyNA(pub$DOI)) {
      citation <- paste0(citation, " doi:", pub$DOI)
    }
    
    cat(citation, "\n")
    
    # Add buttons
    # handle PDF separately from other buttons to add the correct local paths
    if (!anyNA(pub$PDFNAME)) {
      cat(sprintf("[PDF](%s){.btn .btn-outline-primary .btn role=\"button\" .btn-page-header .btn-xs}\n", 
      paste0("../publications/pdf/", pub$PDFNAME)))
    }
    # all other buttons use standard urls
    links <- c(
      "JOURNAL", "CONFERENCE", "PREPRINT", "WEBSITE", 
      "SOFTWARE", "CODEDATA", "CODE", "DATA", "TALK"
    )
    links <- paste0(links, "LINK")
    names <- c(
      "Journal", "Conference", "Preprint", "Website", 
      "Software", "Code & Data", "Code", "Data", "Talk"
    )
    stopifnot(length(links) == length(names))
    for (i in seq_along(links)) {
      if (!anyNA(pub[[links[i]]])) {
        cat(sprintf(paste0("[", names[i], "](%s){.btn .btn-outline-primary .btn role=\"button\" .btn-page-header .btn-xs}\n"), 
        pub[[links[i]]]))
      }
    }

    # Generate clean BibTeX entry
    bibtex_entry <- sprintf("@%s{%s,\n\tauthor = {%s},\n\ttitle = {%s},\n\tjournal = {%s},\n\tyear = {%s}",
                            tolower(pub$CATEGORY),
                            pub$BIBTEXKEY,
                            paste(pub$AUTHOR[[1]], collapse = " and "),
                            pub$TITLE,
                            pub$JOURNAL,
                            pub$YEAR)
    
    # Add DOI only if present
    if (!anyNA(pub$DOI)) {
      bibtex_entry <- paste0(bibtex_entry, sprintf(",\n\tdoi = {%s}", pub$DOI))
    }

    bibtex_entry <- paste0(bibtex_entry, "\n}")

    # Escape the BibTeX for HTML
    bibtex_entry <- gsub("\n", "&#10;", bibtex_entry, fixed = TRUE)
    bibtex_entry <- htmltools::htmlEscape(bibtex_entry)  
   
    cat(sprintf(
      '[BibTeX]{.btn .btn-outline-primary .btn-xs data-bibtex="%s" role="button"}\n',
      bibtex_entry
    ))
    
    cat("\n\n")
  }

  return(invisible(NULL))
}