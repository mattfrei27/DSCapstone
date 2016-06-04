
shinyUI(fluidPage(
  
  headerPanel('Text Prediction Tool'),
  textInput("text", label = h3("Text input:"), value = ""),
  
  hr(),
  h3('Predicted word:'),
  fluidRow(column(12, verbatimTextOutput("word"))),
  h3('Other likely words:'),
  fluidRow(column(12, verbatimTextOutput("otherWords")))
)) 