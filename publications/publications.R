render_publications <- function(year = NULL, project = NULL, bold_author = "Bürkner, P. C.") {
  # Read BibTeX file
  pubs <- suppressMessages(suppressWarnings(bib2df::bib2df("publications.bib")))

  if (!is.null(year)) {
    pubs <- pubs[pubs$YEAR %in% year, ]
  }

  # Sort by year (descending)
  # pubs <- pubs[order(-pubs$YEAR), ]

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
    bibtex_escaped <- gsub("<", "&lt;", bibtex_entry)
    bibtex_escaped <- gsub(">", "&gt;", bibtex_escaped)
    
    # Add collapsible BibTeX button - content breaks to new line when open
    cat(sprintf('<details style="display: inline-block;"><summary class="btn btn-outline-primary btn-page-header btn-xs" style="cursor: pointer;">BibTeX</summary><pre style="margin-top: 10px; margin-bottom: 20px;"><code>%s</code></pre></details>', bibtex_escaped))
    cat("\n\n")
  }
}