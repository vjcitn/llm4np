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

#' parse text and extract tables from a pdf
#' @import pdftools
#' @import tabulapdf
#' @note There can be tabulapdf errors thrown that do not register in R.
#' They do not seem to be trappable, so be sure you are comfortable with
#' the event noted.
#' @examples
#' nelsonasd = system.file("pdfs", "nelsonAutDeid.pdf", package="llm4np")
#' nels = parse_nppdf(nelsonasd)
#' nels
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
       
