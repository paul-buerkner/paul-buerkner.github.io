render_publications <- function(year = NULL, project = NULL, bold_author = "Bürkner, P. C.") {
  # Read BibTeX file
  pubs <- suppressMessages(suppressWarnings(bib2df::bib2df("publications.bib")))

  if (!is.null(year)) {
    pubs <- pubs[pubs$YEAR %in% year, ]
  }

  for (i in 1:nrow(pubs)) {
    pub <- pubs[i, ]
    
    # Format citation (customize as needed)
    authors <- paste(pub$AUTHOR[[1]], collapse = ", ")
    # Bold your name
    authors <- gsub(bold_author, paste0("**", bold_author, "**"), authors)
    
    citation <- sprintf("- %s (%s). %s. *%s*.",
                      authors, pub$YEAR, pub$TITLE, pub$JOURNAL)
    
    # Add DOI only if present
    if (!is.na(pub$DOI)) {
      citation <- paste0(citation, " doi:", pub$DOI)
    }
    
    cat(citation, "\n")
    
    # Add buttons
    if (!anyNA(pub$PDFLINK)) {
      cat(sprintf("[PDF](%s){.btn .btn-outline-primary .btn role=\"button\" .btn-page-header .btn-xs}\n", pub$PDFLINK))
    }
    if (!anyNA(pub$JOURNALLINK)) {
      cat(sprintf("[Journal](%s){.btn .btn-outline-primary .btn role=\"button\" .btn-page-header .btn-xs}\n", pub$JOURNALLINK))
    }
    if (!anyNA(pub$PREPRINTLINK)) {
      cat(sprintf("[Preprint](%s){.btn .btn-outline-primary .btn role=\"button\" .btn-page-header .btn-xs}\n", pub$PREPRINTLINK))
    }
    if (!anyNA(pub$SOFTWARELINK)) {
      cat(sprintf("[Software](%s){.btn .btn-outline-primary .btn role=\"button\" .btn-page-header .btn-xs}\n", pub$SOFTWARELINK))
    }
    if (!anyNA(pub$CODEDATALINK)) {
      cat(sprintf("[Code & Data](%s){.btn .btn-outline-primary .btn role=\"button\" .btn-page-header .btn-xs}\n", pub$CODEDATALINK))
    }
    if (!anyNA(pub$CODELINK)) {
      cat(sprintf("[Code](%s){.btn .btn-outline-primary .btn role=\"button\" .btn-page-header .btn-xs}\n", pub$CODELINK))
    }
    if (!anyNA(pub$DATALINK)) {
      cat(sprintf("[Data](%s){.btn .btn-outline-primary .btn role=\"button\" .btn-page-header .btn-xs}\n", pub$DATALINK))
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
    if (!is.na(pub$DOI)) {
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
}