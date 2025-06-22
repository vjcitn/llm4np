stopifnot(nchar(Sys.getenv("OPENAI_API_KEY"))>0)
library(shiny)
library(ollamar)
library(wizrd)
sqspsrc = "https://static1.squarespace.com/static/50a3e393e4b07025e1a4f0d0/t/510b1429e4b0f6b4fb681381/1359680553898/de-identified+report+1.pdf"
pecssrc = "https://www.registeredpsychologist.com.au/wp-content/uploads/2020/09/PECS-Example-ADHD-Report.pdf"

# First version, assumes apache serving a pdf with NP report
# writes a markdown file

library(llm4np)

baseprompt_old = "Please summarize the text {txt} in five short paragraphs
of at most 45 words.  Use the concepts in the 'val' field of {df} as much as possible,
choosing the most specific term that is relevant, and when you do use these terms, please
append the associated value of the 'tag' field in parentheses, prefixing the tag with
'PTO:'.  After producing the paragraphs, please produce three lists of tags that were
used, corresponding to 'strengths', 'weaknesses' and 'needs'.  Do not use headings with
the lists, just provide the lists as sentences preceded by the category."

listprompt = "Please analyze the text {txt} using the concepts in the 'val' field of {df} 
as much as possible, choosing the most specific term that is relevant.  Produce three 
lists of tags that were used, corresponding to 'strengths', 'weaknesses' and 'needs'.  
Use markdown headings with the lists.  Begin each list item with the 'val' 
element chosen, and then your explanation for why that element was chosen, in parentheses."

baseprompt = "Please summarize the text {txt} in five short paragraphs of at most 45 words.  
  Use the concepts in the 'val' field of {df} as much as possible, choosing the most specific 
term that is relevant, and when you do use these terms, please append the associated value 
of the 'tag' field in parentheses, prefixing the tag with 'PTO:'.  After producing the 
paragraphs, please produce three lists of up to five tags that were used, corresponding to 'strengths', 
'weaknesses' and 'needs'.  Use headings with the lists and provide the lists as sentences 
preceded by the category, and add the 'PTO' and tag in parentheses for each item."

baseprompt = gsub("\\n", " ", baseprompt)

pprompt = "<p>Enter your prompt text below.  <p>Use the template variable '{txt}' to refer<br>
to the parsed text of the NP report, and '{df}' to refer to the ontology table."

mods = ollamar::list_models()

ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText("CRAIN-DP NP Feature Extractor"),
   helpText("Parse and analyze NP evaluations with R and
LLMs"),
   radioButtons("llmchoice", "LLM", c("gpt-4o-mini", mods$name), selected="gpt-4o-mini"),
   helpText("Example reports found on web"),
   radioButtons("reptchoice", "Report", c("SQSP", "PECS")),
   width=2
   ),
  mainPanel(
   tabsetPanel(
    tabPanel("report", 
      htmlOutput("rept")),
    tabPanel("prompt",  width="510px",
      tags$p("Enter your prompt below, or use the text in place.", tags$br(), 
               "Use the template variable '{txt}' to refer to the parsed text of the NP report",
               tags$br(), "and '{df}' to refer to the ontology table."),
      textAreaInput("thepr", label=NULL, value=baseprompt, height="200px", width="500px"),
      actionButton("exec", "Run prompt"),
      htmlOutput("finished")
     ),
    tabPanel("ontotab",
      tags$p("This table includes all terms at ", tags$a("ASDPTO in NCBO", href=
                  "https://bioportal.bioontology.org/ontologies/ASDPTO", target="_blank")),
      DT::dataTableOutput("otab")),
    tabPanel("About",
      tags$p("License: ", tags$a("CC BY-NC 4.0", href="https://creativecommons.org/licenses/by-nc/4.0/")),
      tags$p("This app is being developed in support of CRAIN-DP: Community oRiented AI-augmented
       Neuropsychological Deep Phenotyping"),
      tags$p("Two NP evaluations in PDF format were obtained from the web, denoted SQSP ",
        tags$a("(pdf)", href="https://static1.squarespace.com/static/50a3e393e4b07025e1a4f0d0/t/510b1429e4b0f6b4fb681381/1359680553898/de-identified+report+1.pdf", target="_blank"), "and PECS ", 
        tags$a("(pdf)", href="https://www.registeredpsychologist.com.au/wp-content/uploads/2020/09/PECS-Example-ADHD-Report.pdf", target="_blank")),
      helpText("The software stack in use:"),
      verbatimTextOutput("sessinf")
      )
    )
   )
  )
 )

server = function(input, output) {

 pretty_rmd <- function(x) {
    x |> strwrap() |> paste(">", text = _, collapse = "\n") |> cat()
 }

 data("nmdf", package="llm4np")
 output$otab = DT::renderDataTable(nmdf)

 output$prhelp = renderUI({
  pprompt
  })
 output$rept = renderUI({
   if (input$reptchoice == "SQSP") thepdf = sqspsrc
    else thepdf = pecssrc
    tags$iframe(style="height:600px; width:100%", src=thepdf)
   })
   
 analyze <- eventReactive(input$exec, {
   if (input$reptchoice == "SQSP") thepdf = sqspsrc
    else thepdf = pecssrc
   parsedrep <- parse_nppdf(thepdf) 
   repdata = gsub("\\n", " ", paste(parsedrep@text, collapse = " "))
   print(substr(repdata,1000,1050))
   ag = openai_agent(input$llmchoice) |>
      instruct("You are a helpful assistant with training in neuropsychology.")
   prompted_agent <- ag |> prompt_as(input$thepr)
   print(prompted_agent)
   prompted_agent |> predict(list(txt=repdata, df=nmdf)) |> writeLines("abcd.md")
   })

  makemd = reactive({
   input$exec
   analyze()
   })

  output$finished = renderUI({
   makemd() # writes abcd.md
   includeMarkdown({
    "abcd.md"
    })
   })

   output$sessinf = renderPrint({
    sessionInfo()
    })
}

runApp(list(ui=ui, server=server))
  
