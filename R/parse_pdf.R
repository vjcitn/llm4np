# from lawremi/wizrd

`:=` <- function(x, y) {
    sym <- substitute(x)
    stopifnot(is.name(sym))
    call <- substitute(y)
    stopifnot(is.call(call))
        
    nm <- deparse(sym)
    call$name <- nm 
    
    assign(nm, eval(call, parent.frame()), parent.frame())
}

#' manage parsed neuropsych report
#' @name Parsed_NP
#' @import S7
#' @export
Parsed_NP := new_class(
  properties = list(
    text = class_character,
    tables = class_list),
  validator = function(self) {
    if (FALSE) 1
    }
  )

sqspsrc = "https://static1.squarespace.com/static/50a3e393e4b07025e1a4f0d0/t/510b1429e4b0f6b4fb681381/1359680553898/de-identified+report+1.pdf"
pecssrc = "https://www.registeredpsychologist.com.au/wp-content/uploads/2020/09/PECS-Example-ADHD-Report.pdf"

#' parse text and extract tables from a pdf
#' @import pdftools
#' @import tabulapdf
#' @param pdfpath character(1) path to a PDF, may be a URL, expected to be a neuropsychology evaluation report
#' @note There can be tabulapdf errors thrown that do not register in R.
#' They do not seem to be trappable.  Be sure you are comfortable with
#' the event noted.
#' @note The original URLs for the PDFs included with the package can be found using
#' `llm4np:::sqspsrc` and `llm4np:::pecssrc`.
#' @examples
#' deidpath = system.file("pdfs", "de-identified+report+1.pdf", package="llm4np")
#' deid = parse_nppdf(deidpath)
#' deid
#' @export
parse_nppdf = function(pdfpath) {
  txt = pdftools::pdf_text(pdfpath)
  suppressMessages({
  tabs = tabulapdf::extract_tables(pdfpath)
  })
  Parsed_NP(text=txt, tables=tabs)
}

method(print, Parsed_NP) <- function(x, ...) {
  cat(sprintf("Parsed NP pdf with %d pages and %d tables.\n",
      length(x@text), length(x@tables)))
}
       
