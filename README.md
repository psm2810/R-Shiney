# R-Shiney
#Topic Modelling using R Shiney


options(shiny.reactlog=TRUE)
packages <- c("Matrix","shiny","quanteda","shinydashboard","RColorBrewer","DT","visNetwork","tm",
              "igraph","wordcloud","scatterD3","reshape","grid","tidyverse","shinyjs","shinyBS","stm","treemap",
              "slickR","slam","tidytext","tidyverse","devtools","dendextend","textclean","data.table",
              "textstem","tokenizers","ggExtra","sentimentr","plotly","networkD3","network","networkDynamic",
              "ndtv","sna","quanteda","readtext","syuzhet","lsa","tm")

# packages <- c(packages ,"ReporteRs")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]

if (length(new.packages)){
  for (i in 1:length(new.packages)) {
    install.packages(new.packages[i], dependencies = c("Suggests","Depends","Imports"))
  }
}

lapply(packages,library,character.only=TRUE)

# for (i in 1:length(packages)) {
#   suppressPackageStartupMessages(library(packages[i], character.only=T))
# }



source('directoryInput.R')
source('functions.R')

#source("./inst/app/functions.R")

# put stop words to start with here
exp.stop <- c()
exp.filter <- c()

###################################################
##############       UI       #####################
###################################################

############### Header content ####################

header <- dashboardHeader(title = "TextTrove")

############### Sidebar content ###################

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
    menuItem("User Inputs", tabName = "model", icon = icon("user")),
    menuItem("Exploratory Analysis", tabName = "expl", icon = icon("pie-chart")),
    menuItem("Model Resuts", tabName = "topics", icon = icon("bar-chart")),
    menuItem("Analytics Version", tabName = "new", icon = icon("dashboard"))
  )
)

############### Body content ######################

body <- dashboardBody( 
  useShinyjs(),
  tabItems(
    # Topic Modeling Tab
    tabItem(tabName = "model",
            fluidRow(
              box(status="primary",
                  title = "Step 1: Load Dataset",
                  column(9,
                         fileInput("dataFileToUpload", "Choose Data File To Upload")
                  ),
                  br(),  
                  actionButton("submitDataForUpload", "Submit", icon = icon("envelope")),
                  column(12, br()), hr(), 
                  
                  div(id = "advUploadOptions",
                      checkboxInput("headerPresent", "Header Row Present", TRUE),
                      br(), br(),
                      
                      radioButtons("columnSeparator",
                                   "Separator",
                                   c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                                   inline = TRUE,
                                   ","),
                      br(),
                      radioButtons("quoteAroundData", "Quotes Around Text",
                                   c(None = "",
                                     "Double Quote" = "\"",
                                     "Single Quote" = "'"),
                                   inline = TRUE,
                                   "\"")
                  )
                  # hr(),
                  # 
                  # directoryInput('load.directory', label = 'Or load a previous model (then move to Results)', value = '~'),
                  # bsTooltip("load.directory", "Select the directory to load a model.",
                  #           "left", options = list(container = "body")),
                  # actionButton("load.model","Load Model", icon = icon("upload"))
                  
              ),
              
              box(status="primary",
                  title = "Step 2: Pre-processing",
                  column(6, selectInput("tpDocs",
                                        "Select Text Column",
                                        c()),
                         bsTooltip("tpDocs", "Select which column contains the column of text.",
                                   "left", options = list(container = "body"))),
                  column(6, selectInput("tpLabels", "Select Category Column", c()),
                         bsTooltip("tpLabels", "Select which column contains the Category",
                                   "left", options = list(container = "body"))),
                  hr(),
                  
                  column(6, selectInput("labelc1", "Select Categories Group-1", c(), multiple = T),
                         
                         bsTooltip("labelc1", "Select Categories for First Group (Multiple)",
                                   "left", options = list(container = "body"))),
                  
                  column(6, selectInput("labelc2", "Select Categories Group-2", c(), multiple = T),
                         
                         bsTooltip("labelc2", "Select Categories for Second Group (Multiple)",
                                   "left", options = list(container = "body"))),
                  
                  #column(12, hr()),
                  
                  column(6, selectInput("tpstp",
                                        "Select Stop Words",
                                        c("also", "really", "that", "this", "hsbc", "my"),
                                        multiple = T),
                         bsTooltip("tpstp", "Select Stop words to remove (Multiple)",
                                   "left", options = list(container = "body")),
                         textInput("stopwords", label = "Custom Stop Words",
                                   value = paste(exp.stop, collapse = ", "),
                                   placeholder = "also, such, really..."),
                         bsTooltip("stopwords", "Include additional stop words to remove:",
                                   "left", options = list(container = "body"))),

                  column(6, selectInput("tpflt",
                                        "Select Search Categories (DISABLED)",
                                        c("Card", "Wealth", "Mortgage", "Loan", "Staff"),
                                        multiple = T),
                         bsTooltip("tpflt", "Select Search words to include (Multiple)",
                                   "left", options = list(container = "body")),
                         textInput("filterwords", label = "Custom Search Words (DISABLED)",
                                   value = paste(exp.filter, collapse = ", "),
                                   placeholder = "staff, time, card..."),
                         bsTooltip("filterwords", "Include additional search words:",
                                   "left", options = list(container = "body"))),

                  column(12, br()),

                  hr(), hr(),
                  column(12, actionButton("dfm.update", "Pre-Process", icon = icon("flask")), 
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         actionLink("expl.res", "Show Results", icon = icon("cubes")))
              )
            ),
            
            fluidRow(
              box(status="primary",
                  title = "Step 3: Topic Modelling",
                  box(sliderInput("minDoc",
                                  "Minimum Share of Documents (for Terms):",
                                  min = 0,  max = 30,  post = "%", value = 5, step = 1),
                      bsTooltip("minDoc", "Remove sparse terms:",
                                "left", options = list(container = "body")),
                      column(6,checkboxInput("stemming", label = "Stemming", value = FALSE)),
                      column(6,checkboxInput("lemmatise", label = "Lemmatisation", value = FALSE)), width = 12),
                  #box(radioButtons("ngrams", label = NULL, choices = list("Unigrams" = 1, "Bigrams" = 2), selected = 1)),
                  
                  br(), br(),
                  
                  box(
                    column(6, selectInput("model1",
                                          "Model-1 Categories",
                                          c(), multiple = T),
                           bsTooltip("model1", "Select Categories for First Model (Multiple)",
                                     "left", options = list(container = "body"))),
                    
                    column(6, selectInput("model2",
                                          "Model-2 Categories",
                                          c(), multiple = T),
                           bsTooltip("model2", "Select Categories for Second Model (Multiple)",
                                     "left", options = list(container = "body"))),
                    hr(),
                    
                    sliderInput("num.topics",
                                "Number of Topics:",
                                min = 1,  max = 20,  value = 5, step = 1),
                    bsTooltip("num.topics", "Set to zero to auto-detect topics.",
                              "left", options = list(container = "body")),
                    
                    sliderInput("iter",
                                "Maximum Number of Iterations:",
                                min = 50,  max = 1000,  value = 200, step = 50),
                    bsTooltip("iter", "Adjust higher if the algorithm is not converging.",
                              "left", options = list(container = "body")),
                    hr(),
                    column(6, radioButtons("modtyp", label = "Model Type",
                                           choices = list("STM" = 1, "LDA" = 2), selected = 1)),
                    
                    column(6, actionButton("topic.update", "Run Model", icon = icon("cogs"))), width = 12),
                  #                 ,box(a(tags$button(tags$b("Explore Model")),target="_blank",href="corrviz.html"))
                  
                  br()
                  
              ),
              
              box(
                status="primary",
                title = "Topic Visuals Settings",
                width = 6,
                sliderInput("parm",
                            "Minimum Correlation for Visuals",
                            min = 0,  max = 0.5,  value = 0.1, step = 0.01),
                bsTooltip("parm", "Higher threshold means less edges, Lower means more edges.",
                          "left", options = list(container = "body")),
                hr(),
                
                column(12,
                       actionButton("network.update", "Create Visuals", icon = icon("photo")), 
                       HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                       actionLink("top.res", "Show Results", icon = icon("cubes")))
              ),
              
              box(status="primary",
                  title = "Save results",
                  
                  directoryInput('directory', label = 'Selected Directory', value = '~'),
                  bsTooltip("directory", "Select the directory  to save the results.",
                            "left", options = list(container = "body")),
                  
                  actionButton("save.results","Save Model", icon = icon("save"))
              )
            )),
            
    tabItem(tabName = "expl",
            fluidRow(
              box(status = NULL, width = 12,
              column(12, actionLink("bac1", "Back to User Inputs", icon = icon("reply")),
                     
                     HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                     HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                     HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                     HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                     HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                     HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                     HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                     HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),

                     actionLink("fro1", "Go To Model Results", icon = icon("share")))),
              
              box(status="primary", h3(htmlOutput("out_labels")), width = 12),
              
              box(status="primary",
                  title = "Exploratory Analysis",
                  width = 12,
                  
                  box(id = "hide1",
                      title = "Original Classes",
                      plotlyOutput("ori_pie"), width = 6
                      ),

                  box(id = "hide2",
                      title = "Re-Grouped Classes",
                      plotlyOutput("grp_pie"), width = 6
                      ),
                  
                  box(id = "hide4",
                      title = "Keyness Distribution",
                      plotlyOutput("key_plot"), 
                      width = 6),
                  
                  box(title = "Sentiment Dispersion across Documents",
                      plotlyOutput("scat_plot"), 
                      width = 6),

                    box(status="primary",
                        title = "Representative Documents",
                        dataTableOutput("doc.table2"), width = 12
                    ),
                  

                  hr(),

                  column(12, br()),
                  
                  column(12, column(3, selectInput("labelgrp",  "Select Category Groups", c(), multiple = F)),
                         
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),

                         column(3, selectInput("compview",  "Switch On Comparison View", c(), multiple = F))),
                  
                   box(title = "Summary Statistics",
                       uiOutput("info_dyn"),
                       width = 6),
                  
                  box(title = "Basic Features (Avg. Estimates)",
                      plotlyOutput("basic_feat"),
                      width = 6),
                         
                   box(title = "Word Cloud",
                       plotOutput("word_plot"), 
                       width = 6),
                         
                  box(title = "Sentiments & Emotions",
                      plotlyOutput("sent_plot"),
                      width = 6),
                         
                   hr(),
                   column(12, br()),
                  
                  box(width = 12,
                      column(4, radioButtons("freq_rare", label = "Popular & Rare Phrases",
                                             choices = list("Frequent Phrases" = 1, "Rare Phrases" = 2), selected = 1)),

                      column(4, radioButtons("uni_bi", label = "Phrase Structure",
                                             choices = list("1-Word Phrase" = 1, "2-Word Phrase" = 2), selected = 1)),

                      column(4, selectInput("nei_wrd", "Choose Key Words", c(), multiple = F))),

                  hr(),
                  column(12, br()),

                  box(id = "hide3",
                      width = 12,
                      sliderInput("rare_slide",
                                  "Relative Frequency Threshold to remove Rare Phrases:",
                                  min = 0,  max = 1,  post= "%", value = 0.2, step = 0.01),
                      bsTooltip("rare_slide", "Remove Rare Phrases",
                                "left", options = list(container = "body"))),

                  hr(),
                  column(12, br()),

                  box(title = "Key Phrases across Documents",
                      plotlyOutput("phs_plot"), width = 6
                  ),

                  box(id = "hide5",
                      title = "Lexical Dispersion",
                      plotOutput("lex_plot"), width = 6
                  )

              )
            )

    ),
    
    # Topics Tab
    tabItem(tabName = "topics",
            fluidRow(
              box(status = NULL, width = 12,
                  column(12, actionLink("bac2", "Back to User Inputs", icon = icon("reply")),
                         
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'), HTML('&emsp;'),
                         
                         actionLink("fro2", "Go To Exploratory Results", icon = icon("share")))),
              
              box(status="primary",
                  radioButtons("out_mod", label = "Compare Model",
                               choices = list("Model-1" = 1, "Model-2" = 2), selected = 1), width = 2),

              box(status="primary", h2(textOutput("out_model")), width = 8),

              box(status = "primary",
                  downloadButton("downloadData", label = "Results"),
                  downloadButton("downloadTopic", label = "Legends"),
                  width = 2)
            ),

            fluidRow(
              box(status="primary",
                  title = "Topic Model: Expected Topic Proportions",
                  plotOutput("topic_summ", height = "500px"), width = 6
              ),

              box(status="primary",
                  title = "Topic Model: Model Generation",
                  #htmlOutput("topic_clus_ani"),
                  #ndtv:::ndtvAnimationWidgetOutput("topic_clus_ani"),
                  dendroNetworkOutput("topic_clus", height = "500px"), 
                  width = 6
              )

            ),

            # fluidRow(
            #   box(title = "Topic Model: MAP Estimate distribution",
            #       plotOutput("topic_hist"), width = 12
            #   )
            # ),

            fluidRow(
              box(status="primary", h3(textOutput("sel_model")), width = 12),
              
              box(status="primary",
                  title = "Topic Network",
                  visNetworkOutput("topic.network", height = "450px"), 
                  width = 6, collapsible = F),
              
              box(status="primary",
                  title = "Neighbour Network",
                  forceNetworkOutput("topic.network2", height = "500px"), 
                  width = 6, collapsible = F)
              
              #actionButton("nei.net", "Neighbour Network", icon = icon("flask"))
            ),

            fluidRow(
              box(status="primary",
                  title = "Topic Word Cloud: Size Proportional to Word Probability",
                  plotOutput("topic.wordcloud"), width = 6
              ),

              box(status="primary",
                  title = "Topic Model: Sentiment & Emotion",
                  plotlyOutput("topic.sentiment"), width = 6
              )
            ),

            fluidRow(
              box(status="primary",
                  title = "Representative Documents",
                  dataTableOutput("doc.table"), width = 12
              )
            )
    ),


    # ,
    #
    # # Understanding Models tab
    # tabItem(tabName = "model_create",
    #         fluidPage(
    #           titlePanel("Included Content"),
    #           mainPanel(
    #             includeHTML("corrviz.html")
    #           )
    #         )
    # )
  
    ######### Analytics Version #############
    tabItem(tabName = "new",
            fluidRow(
              
              tabsetPanel(
                
                tabPanel("Overview",
                         box(width = 12, solidHeader = T,
                         br(),
                         # h3(p("Version: Beta"), align = "right"),
                         h3(p("Welcome to the NLP app, using this app the following NLP tasks can be achieved:")),
                         
                         br(),
                         
                         h4(p("1. Data Pre Processing")),
                         h4(p("2. Tokenization")),
                         h4(p("3. Construction of Doucument Term Matrix")),
                         h4(p("4. N-grams analysis")),
                         h4(p("5. Word Context Window Analysis")),
                         h4(p("5. Text Classification using Naive Bayes Classifier")),
                         h4(p("6. Sentiment Anslysis")),
                         h4(p("7. Latent Semantic Analysis")),
                         h4(p("8. Feature Engineering on Text - Create your feature")),
                         
                         br(),
                         br(),
                         
                         p("To use this app your souce data should be a csv file with two columns", align = "justify"),
                         br(),
                         p("Column 1: Containing the TEXT variable", align = "Justify"),
                         p("Column 2: Target / Label variable, if your data is not labeled populate this as numeric 1 throughout", align = "Justify"),
                         br(),
                         
                         h5("You are Good to Go - ", strong("HAPPY NLP !!!")),
                         br(),
                         h4(p("Want to contribute as a developer Or Have a suggestion:")),
                         p("Drop a note to: arindam1chatterjee@hsbc.co.in"))
                         
                         
                ),
                
                tabPanel("Upload Data", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             fileInput("File", h3("Upload Data")),
                             selectInput('Text',"Select Text Variable",'Text'),
                             #textInput("Text", h3("Name of the Text Variable"), 
                             #value = ""),
                             
                             radioButtons("HasLabel", h3("Data has a Text Variable"),
                                          c("Yes" = 1, "No" = 2)),
                             
                             selectInput('Label',"Select Target Variable",'Label')
                             
                             #textInput("Label", h3("Name of the Target Variable"), 
                             #value = "")
                             
                           ),
                           
                           
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         
                                         tabPanel("Data Snapshot", h3("Sample Rows from the uploaded data"), tableOutput("filedf1")),
                                         tabPanel("Data Summary", h3("Variable Details in the uploaded data"), tableOutput("filedf2"))
                                         #tabPanel("Text Length",plotOutput("LengthPlot"))
                                         
                             )
                           )
                         )
                         
                ),
                
                # ************************** Tab 2 : Organize data into DTM *****************************************
                
                tabPanel("Organize Data", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             h3(p("The siders are applicable for Word Frequency and Document Frequency tab")),
                             sliderInput("MinFreq", h3("Minimum Word Frequency"),
                                         min = 0, max = 20, value = 2),
                             sliderInput("MinDocFreq", h3("Minimum Document Frequency"),
                                         min = 0, max = 20, value = 2),
                             sliderInput("topnwords", h3("Max Words to Plot"),
                                         min = 5, max = 100, value = 25)
                             
                             
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Tokenization", 
                                                  h3("Details of Preprocessing and Tokenization"),
                                                  p("Following preprocessing steps has been performed on the data -"),
                                                  br(),
                                                  p("1. Stopwords have been removed"),
                                                  p("2. Numbers have been removed"),
                                                  p("3. Punctuations have been removed"),
                                                  p("4. Similar words have been stemmed"),
                                                  br(),
                                                  p("Rows of the DTM denote the documents and Columns denote tokens"),
                                                  textOutput("tokenizesummary"),
                                                  br(),
                                                  textOutput("tokenizesummary1")),
                                         #h3("Distribution of Text Length"),
                                         #br(),
                                         tabPanel("Text Length",plotOutput("LengthPlot")),
                                         tabPanel("Tokens Per Document", 
                                                  br(),
                                                  p("Distribution of Number Of Words in a document"),
                                                  br(),
                                                  plotOutput("tokperdocPlot"),
                                                  br(),
                                                  br(),
                                                  p("Tokens per Document distribution would help infer whether 
                                                    shorter or longer sentences are more prevalent in the data")
                                                  ),
                                         
                                         tabPanel("Word Frequency",
                                                  plotOutput("tokfreq")),
                                         tabPanel("Document Frequency",plotOutput("docfreq"))
                                         
                                         )
                           )
                )
                
                
                
                
                         ),
                
                # ************************** Tab 3 : Bag of Words Analysis *****************************************
                
                tabPanel("Bag Of Words Analysis", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             sliderInput("MinFreq2", h3("Minimum Word Frequency"),
                                         min = 0, max = 20, value = 2),
                             sliderInput("MinDocFreq2", h3("Minimum Document Frequency"),
                                         min = 0, max = 20, value = 2),
                             sliderInput("topnwords2", h3("Max Words to Plot"),
                                         min = 5, max = 100, value = 25)
                             
                             
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Word Frequency",plotOutput("tokfreq2")),
                                         tabPanel("WordCloud", plotOutput("wordcloud")),
                                         tabPanel("Comparative WordCloud", plotOutput("compwordcloud")),
                                         tabPanel("Word Significance",plotOutput("keyness"))
                             )
                           )
                         )
                         
                         
                         
                         
                ),
                
                # ************************** Tab 4 : Ngrams analysis *****************************************
                
                tabPanel("N-Grams Analysis", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             sliderInput("MinFreq3", h3("Minimum Word Frequency"),
                                         min = 0, max = 20, value = 2),
                             
                             sliderInput("topnwords3", h3("Max Words to Plot"),
                                         min = 5, max = 100, value = 25),
                             radioButtons("whichgram", 
                                          h3("N-grams Selection"), 
                                          c("Bigram" = 2, "Trigram" = 3))
                             
                             
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Word Frequency",plotOutput("ngramsfreq")),
                                         tabPanel("WordCloud", plotOutput("wordcloudngrams")),
                                         tabPanel("Word Significance",plotOutput("keynessngrams"))
                             )
                           )
                         )
                         
                         
                         
                         
                ),
                
                
                # ************************** Tab 5 : Context Word Analysis ********************************
                
                tabPanel("Context Word Analysis", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             textInput("Cword", h3("Enter Context Word"), 
                                       value = ""),
                             
                             sliderInput("window", h3("Length of Context window"), 
                                         min = 1, max = 20, value = 3, step = 1),
                             
                             sliderInput("MinFreq4", h3("Minimum Word Frequency"),
                                         min = 0, max = 20, value = 2),
                             
                             sliderInput("topnwords4", h3("Max Words to Plot"),
                                         min = 5, max = 100, value = 25)
                             
                             
                             
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Word Frequency",plotOutput("tokfreqcword")),
                                         tabPanel("WordCloud", plotOutput("wrdcldcwords")),
                                         tabPanel("Custom Bigrams", plotOutput("bigramscword"))
                                         
                             )
                           )
                         )
                         
                         
                         
                         
                ),
                
                # ************************** Tab 6 : NB Classifier *****************************************
                
                tabPanel("Naive Bayes Classifier", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             selectInput("nb.ind", h3("Run Naive Bayes"), 
                                         choices = list("Yes" = 1, "No" = 2), selected = 2),
                             
                             
                             sliderInput("trfrac", h3("Training Fraction"),
                                         min = 0, max = 1, value = 0.7, step = 0.05),
                             
                             
                             selectInput("prior", h3("Choice of Prior Probabilities"), 
                                         choices = list("termfreq" = 1, "docfreq" = 2,
                                                        "uniform" = 3), selected = 1)
                             
                             
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Naive Bayes Results",tableOutput("nbsummary")),
                                         tabPanel("Posterior Probabilities", tableOutput("postprob"))
                                         # tabPanel("Confusion Matrix", "")
                                         
                             )
                           )
                         )
                         
                         
                         
                         
                ),
                
                # ************************** Tab 7 : Sentiment Analysis *****************************************
                
                tabPanel("Sentiment Analysis", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             selectInput("sent.ind", h3("Run Sentiment Analysis"), 
                                         choices = list("Yes" = 1, "No" = 2), selected = 2),
                             
                             # radioButtons("senttype", h3("Sentiment Analysis options"),
                             #              c("Setiment Score" = 1, "Qualitative Sentiment" = 2)),
                             
                             selectInput("sentbigram", h3("Use bigrams"), 
                                         choices = list("Yes" = 1, "No" = 2), selected = 2)
                             
                             
                             
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Sentiment Score",plotOutput("sent")),
                                         #tabPanel("Qualitative Sentiment", plotOutput("sentbigrams"))
                                         tabPanel("Qualitative Sentiment", tableOutput("nrc"), plotOutput("nrcplot"))
                                         
                                         
                             )
                           )
                         )
                         
                         
                         
                         
                ),
                
                #************************** Latent Semantic Analysis *********************************  
                
                tabPanel("Latent Semantic Analysis", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             selectInput("lsa.ind", h3("Run Latent Semantic Analysis"), 
                                         choices = list("Yes" = 1, "No" = 2), selected = 2),
                             
                             sliderInput("svd", h3("Number of Singular values to retain"),
                                         min = 0, max = 100, value = 25, step = 5),
                             
                             selectInput("result.type", 
                                         h3("Choose results to retain"), 
                                         choices = list("Term Vectors" = 'lsatv', "Document Vectors" = 'lsadv' ),
                                         selected = 'lsadv'),
                             
                             br(),
                             helpText("Click Download to save Term/Document vectors"),
                             downloadButton('Dnld.LSA', 'Download')
                             
                             
                             
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("SVD details",plotOutput("lsa.tk")),
                                         tabPanel("Sample features", tableOutput("sample.lsadnld"))
                                         
                                         
                             )
                           )
                         )
                         
                         
                         
                         
                ),
                
                #******************************Generate Features***************************
                
                tabPanel("Generate Features", fluid = TRUE,
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             selectInput("feat.ind", h3("Generate Features"), 
                                         choices = list("Yes" = 1, "No" = 2), selected = 2),
                             
                             sliderInput("ftfrac", h3("Sampling fraction for feature generation"),
                                         min = 0, max = 100, value = 100, step = 5),
                             
                             selectInput("feat.type", 
                                         h3("Choose Features"), 
                                         choices = list("Term Frequency" = "tf", "TFIDF" = "tfidf", 
                                                        "WordVectors" = "wv", "Sentiment Score" = "sentsc",
                                                        "Qualitative Sentiment" = "qsent" , "Custom Bigram" = "bg"),
                                         
                                         selected = "tfidf"),
                             
                             br(),
                             helpText("Click Download to Save Features"),
                             downloadButton('Dnld.feat', 'Download')
                             
                             
                             
                             
                             
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Term Frequency",tableOutput("feat.termfreq")),
                                         tabPanel("TFIDF", tableOutput("feat.tfidf")),
                                         tabPanel("Sentiment Based", tableOutput("feat.sentscore")),
                                         tabPanel("Word Vectors", "")
                                         
                                         # tabPanel("Custom Bigrams", ""),
                                         # tabPanel("Token Density", "")
                                         # 
                                         
                                         
                                         
                             )
                           )
                         )
                         
                         
                         
                         
                )
                
                
                
                
                
              )
              
              
              
            )
        )
    )
)

############### Dashboard page ####################

ui <- dashboardPage(header, sidebar, body)

##############    SERVER       #####################

server <- function(input, output, session) {
  
  # reactive object that stores intermediate results
  storedData <- reactiveValues()
  
  storedData$data <- NULL
  storedData$trim_data <- NULL
  
  # load previous model
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$load.directory
    },
    handlerExpr = {
      if (input$load.directory > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'load.directory'))
        updateDirectoryInput(session, 'load.directory', value = path)
      }
    }
  )
  
  output$directory = renderText({
    readDirectoryInput(session, 'load.directory')
  })
  
  # save model
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'directory'))
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )
  
  output$directory = renderText({
    readDirectoryInput(session, 'directory')
  })
  
  #shinyjs below was from stmGUI: https://github.com/dzangri/stmGUI
  
  shinyjs::onclick("toggleAdvDataUpload",
                   shinyjs::toggle(id = "advUploadOptions",
                                   anim = TRUE))
  observe({
    shinyjs::toggleState("submitDataForUpload",
                         !is.null(input$dataFileToUpload))
  })
  observe({
    shinyjs::toggleState("dataInputTitle-nextStep",
                         !is.null(storedData$data))
  })
  
  observeEvent(input$submitDataForUpload, ({
    shinyjs::html("dataInputTextResult", "")
    
    userData <- input$dataFileToUpload
    
    withProgress(message = "Loading data, please wait...", {
      setProgress(0.5)
      
      readDataArgs <- list(userData$datapath, header = input$headerPresent, sep = input$columnSeparator,
                           quote = input$quoteAroundData)
      
      shinyjs::toggleState("moveFromStep1To2")
      
      tryCatch({
        storedData$data <- do.call(read.csv, readDataArgs)
        storedData$data$rowNum <- 1:nrow(storedData$data)
      }, error = function(e) {
        funName <- deparse(substitute(read.csv))
        shinyjs::html("dataInputTextResult",
                      paste("ERROR: Error while running '",
                            funName, "':\n",
                            e,
                            sep = ""))
        storedData$data <- NULL
        return(NULL)
      }, warning = function(w) {
        shinyjs::html("dataInputTextResult",
                      paste("WARNING: Warning while reading data:\n",
                            w,
                            sep = "\n"))
        storedData$data <- NULL
        return(NULL)
      }, finally = {
      })
      
      setProgress(1)
      
    })
    
    showModal(modalDialog(paste0("Data Uploaded with ", nrow(storedData$data), " rows."),
                          easyClose = T, footer = modalButton("OK")))
    
  }))
  
  
  observe({
    userData <- storedData$data
    
    shinyjs::disable("tpflt")
    shinyjs::disable("filterwords")
    
    if (!is.null(userData)) {
      shinyjs::enable("tpDocs")
      dataColumnNames <- setdiff(colnames(userData),c("rowNum"))
      shinyjs::enable("tpLabels")
      updateSelectInput(session, "tpDocs", choices = dataColumnNames)
      updateSelectInput(session, "tpLabels", choices = c("(None)",dataColumnNames))
    } else {
      shinyjs::disable("tpDocs")
      shinyjs::disable("tpLabels")
    }
  })
  
  
  observeEvent(input$tpLabels, {
    
    userData <- storedData$data
    if (input$tpLabels!= "(None)") {
      shinyjs::enable("labelc1")
      shinyjs::enable("labelc2")
      dataColumnNames <- sort(unique(as.character(userData[,input$tpLabels])))
      if (length(dataColumnNames)==2) {
      updateSelectInput(session, "labelc1", choices = dataColumnNames, selected = dataColumnNames[1])
      updateSelectInput(session, "labelc2", choices = dataColumnNames, selected = dataColumnNames[2])
      } else {
        updateSelectInput(session, "labelc1", choices = dataColumnNames)
        updateSelectInput(session, "labelc2", choices = dataColumnNames)
      }
    } else {
      shinyjs::enable("labelc1")
      shinyjs::enable("labelc2")
      updateSelectInput(session, "labelc1", choices = c(""))
      updateSelectInput(session, "labelc2", choices = c(""))
      shinyjs::disable("labelc1")
      shinyjs::disable("labelc2")
    }
    
  })
  
  
  # observeEvent(paste0(input$labelc1, input$labelc2), {
  # 
  #   userData <- storedData$data
  #   dataColumnNames <- sort(unique(as.character(userData[,input$tpLabels])))
  #   
  #   if(length(input$labelc1)>0){ 
  #     lab1 <- input$labelc1
  #     lab2 <- input$labelc2
  #   }
  #   
  #   if(length(input$labelc2)>0){
  #     lab1 <- input$labelc1
  #     lab2 <- input$labelc2
  #   }
  #   
  #   rem <- unique(c(input$labelc1, input$labelc2))
  #   dataColumnNames2 <- setdiff(dataColumnNames, rem)
  #   if (length(dataColumnNames)>2){
  #   updateSelectInput(session, "labelc1", choices = dataColumnNames2, selected = lab1)
  #   updateSelectInput(session, "labelc2", choices = dataColumnNames2, selected = lab2)
  #   
  #   showModal(modalDialog(paste0(lab1, " ", lab2), easyClose = T, footer = modalButton("OK")))
  #   }
  # 
  # })
  

# 
#   observeEvent(input$labelc1, {
# 
#     userData <- storedData$data
#     if (input$tpLabels!= "(None)") {
# 
#       shinyjs::enable("labelc2")
#       dataColumnNames2 <- sort(unique(as.character(userData[,input$tpLabels])))
#       dataColumnNames3 <- setdiff(dataColumnNames2,input$labelc1)
#       updateSelectInput(session, "labelc2", choices = dataColumnNames3)
#     }
#     # else {
#     #   shinyjs::disable("labelc2")
#     # }
# 
#   })

  
  
  
  
  # Topic
  
  dist_val <- reactiveValues()
 
  
  observeEvent(input$dfm.update, {
    
    withProgress(message = "Pre-Processing Data .... ",  {
      
      setProgress(0.15)
      
      # pre defined jargon list
      jargon_list <- list(
        
        list('Loan',c('personal loan','personal loans','pr loan','car loan','car loans','loan')),
        
        list('Card',c('credit card','credit cards','cr card','cr cards','debit card','debit cards','dr card','dr cards')),
        
        list('Wealth', c('wealth','inv','investment','insurance','security','bond','mutual','fund'))
      )
      
      #storedData$data <- storedData$data[complete.cases(storedData$data[ ,input$tpDocs]),]
      #storedData$data$rowNum <- 1:nrow(storedData$data)
      
      # MyCorpus <- corpus(tolower(as.character(storedData$data[,input$tpDocs])))
      # 
      # # sets input data row number as primary key -- ensures matchback for datasets without a primary key
      # docvars(MyCorpus, "rowNum") <- storedData$data$rowNum
      # stp <- unlist(strsplit(input$stopwords,","))
      # stp <- trimws(stp)
      # ngram <- ifelse(input$ngrams==1,1L, 1L:2L)
      # 
      # Dfm <- dfm(MyCorpus, remove = c(stopwords("english"), stp), remove_numbers = TRUE, remove_punct = TRUE,
      #            stem = input$stemming, ngrams = ngram)
      # 
      # tdfm <- dfm_trim(Dfm, min_docfreq = input$minDoc)
      # 
      # # we now export to a format that we can run the topic model with
      # z$Corpus <- MyCorpus
      # z$dtm <- convert(tdfm, to= "topicmodels")
      # z$dfm <- convert(tdfm, to = "stm", docvars = docvars(MyCorpus))
      # z$raw_documents <- as.character(storedData$data[,input$tpDocs])
      # z$tdfm <- tdfm
      
      # Selecting search vector from the pre defined jargon list as per user input
      
      selected_vec <- c("")
      
      if(!is.null(input$tpflt)) {
        for (j in 1:length(input$tpflt)) {
          for (i in 1:length(jargon_list)) {
            if (input$tpflt[j] == jargon_list[[i]][[1]] ) {
              temp_vec = unlist(jargon_list[[i]][[2]])
              selected_vec <- c(selected_vec, temp_vec)
              break 
            }
          }
        }
      } else {selected_vec <- c("")}
      
      flt <- unlist(strsplit(input$filterwords,","))
      
      final_flt <- c(selected_vec,flt)
      final_flt <- unique(trimws(final_flt))
      
      userData <- storedData$data
      if (input$tpLabels!= "(None)") {
        shinyjs::enable("model1")
        shinyjs::enable("model2")
        dataColumnNames <- sort(unique(as.character(userData[,input$tpLabels])))
        updateSelectInput(session, "model1", choices = dataColumnNames, selected = input$labelc1)
        updateSelectInput(session, "model2", choices = dataColumnNames, selected = input$labelc2)
        txt <- paste0(" with ",toupper(input$tpLabels)," comparison.")
      } else {
        shinyjs::enable("model1")
        shinyjs::enable("model2")
        updateSelectInput(session, "model1", choices = c(""))
        updateSelectInput(session, "model2", choices = c(""))
        shinyjs::disable("model1")
        shinyjs::disable("model2")
        txt <- " without comparison view."
      }
      
      temp_data <- storedData$data[complete.cases(tolower(storedData$data[ ,input$tpDocs])),]
      
      #temp_data[ ,input$tpDocs] <- replace_emoji(temp_data[ ,input$tpDocs])
      #temp_data[ ,input$tpDocs] <- replace_emoticon(temp_data[ ,input$tpDocs])
      setProgress(0.35)
      
      temp_data[ ,input$tpDocs] <- replace_contraction(temp_data[ ,input$tpDocs])
      temp_data[ ,input$tpDocs] <- replace_internet_slang(temp_data[ ,input$tpDocs])
      setProgress(0.50)
      
      temp_data[ ,input$tpDocs] <- mgsub(temp_data[ ,input$tpDocs], "[^[:alpha:]]", " ", fixed = F)
      temp_data[ ,input$tpDocs] <- mgsub(temp_data[ ,input$tpDocs], "[[:punct:]]", " ", fixed = F)
      temp_data[ ,input$tpDocs] <- strip(temp_data[ ,input$tpDocs])
      temp_data[ ,input$tpDocs] <- replace_white(temp_data[ ,input$tpDocs])
      temp_data[ ,input$tpDocs] <- replace_kern(temp_data[ ,input$tpDocs])
      
      nest_temp <- data.frame(txt = temp_data[ ,input$tpDocs])
      word_cnt <- unnest_tokens(nest_temp, words, txt)
      
      setProgress(0.80)
      
      if(length(final_flt)>0) {
        temp_data <- temp_data[grep(paste(final_flt, collapse='|'), temp_data[,input$tpDocs], ignore.case=TRUE),]
      }
      
      temp_data <- temp_data[complete.cases(tolower(temp_data[ ,input$tpDocs])),]
      
      storedData$trim_data <- temp_data
      storedData$trim_data$rowNum <- 1:nrow(storedData$trim_data)
      
      #########################################
      
      feat_data <- storedData$data
      
      stp <- unlist(strsplit(input$stopwords,","))
      stp <- trimws(stp)
      
      if(((length(input$labelc1)>0) & (length(input$labelc2)>0))) {
        
        shinyjs::show(id = "hide1")
        shinyjs::show(id = "hide2")
        shinyjs::show(id = "hide4")
        
        feat_data1 <- feat_data[feat_data[[input$tpLabels]] %in% input$labelc1, ]
        feat_data2 <- feat_data[feat_data[[input$tpLabels]] %in% input$labelc2, ]
        
        labelc3 <- c(input$labelc1, input$labelc2)
        feat_data3 <- feat_data[!feat_data[[input$tpLabels]] %in% labelc3, ]
        
        feat_data1$category = "Group-1"
        feat_data2$category = "Group-2"
        if(nrow(feat_data3)>0) {feat_data3$category = "Others"} else {feat_data3 <- NULL}
        feat_data_comb <- rbind(feat_data1,feat_data2,feat_data3)
        
        
        temp_data1 <- temp_data[temp_data[[input$tpLabels]] %in% input$labelc1, ]
        temp_data2 <- temp_data[temp_data[[input$tpLabels]] %in% input$labelc2, ]
        
        labelc3 <- c(input$labelc1, input$labelc2)
        temp_data3 <- temp_data[!temp_data[[input$tpLabels]] %in% labelc3, ]
        
        temp_data1$category = "Group-1"
        temp_data2$category = "Group-2"
        if(nrow(temp_data3)>0) {temp_data3$category = "Others"} else {temp_data3 <- NULL}
        temp_data_comb <- rbind(temp_data1,temp_data2,temp_data3)
        
        
        myCorpus <- corpus(as.character(temp_data_comb[which(temp_data_comb$category != "Others"),input$tpDocs]))
        docvars(myCorpus, "cat") <- temp_data_comb$category[!temp_data_comb$category %in% c("Others")]
        dist_val$dfm <- dfm(myCorpus, remove = c(stopwords("en"), stopwords("SMART"), stp), remove_numbers = TRUE, remove_punct = TRUE,
                            stem = F, ngrams = 1, groups = "cat")
        
        key <- textstat_keyness(dist_val$dfm, target = "Group-1")
        key$mpg_type <- ifelse(key$chi2 < 0, "Reference: Group-2", "Target: Group-1")
        key <- key[order(key$chi2), ]
        
        data1<- data.frame(head(key,10))
        data2<- data.frame(tail(key,10))
        data3<- rbind(data1,data2)
        
        data3 <- data3[order(data3$chi2), ]
        data3$feature <- factor(data3$feature, levels = data3$feature)
        dist_val$chi <- data3
        
        
        labelp1 <- temp_data[,input$tpLabels]
        d1 <- as.data.frame(table(labelp1))
        names(d1) <- c("Class","Freq")
        # d1 <- d1[order(d1$Freq),]
        # d1["pos"] <- cumsum(d1$Freq)- d1$Freq/2
        dist_val$dist_in <- as.data.frame(d1[order(d1$Class),])
        
        labelp2 <- temp_data_comb$category
        d2 <- as.data.frame(table(labelp2))
        names(d2) <- c("Class","Freq")
        # d2 <- d2[order(d2$Freq),]
        # d2["pos"] <- cumsum(d2$Freq)- d2$Freq/2
        dist_val$dist_mr <- as.data.frame(d2[order(d2$Class),])
        dist_val$grp_data <- as.data.frame(temp_data_comb)
        dist_val$feat_data <- as.data.frame(feat_data_comb)
        dataColumnNames2 <- sort(unique(as.character(temp_data_comb$category)))
        
        
        nnn <- sentiment_by(get_sentences(temp_data_comb[[input$tpDocs]]))
        scat <- data.frame(Sent_Id = nnn$element_id, Sentiment_Score = round((nnn$ave_sentiment), 2), 
                           Class = temp_data_comb$category)
        
        dist_val$scat <- scat[order(scat$Class),]
        
        updateSelectInput(session, "compview", choices = c("No", "Yes"))
        
      } else {
        shinyjs::hide(id = "hide1")
        shinyjs::hide(id = "hide2")
        shinyjs::hide(id = "hide4")
        
        dataColumnNames2 <- c("")
        
        myCorpus_nc <- corpus(as.character(temp_data[,input$tpDocs]))
        docvars(myCorpus_nc, "rowNum") <- temp_data$rowNum
        dist_val$dfm <- dfm(myCorpus_nc, remove = c(stopwords("en"), stopwords("SMART"), stp), remove_numbers = TRUE, remove_punct = TRUE,
                            stem = F, ngrams = 1)
        
        nnn <- sentiment_by(get_sentences(temp_data[[input$tpDocs]]))
        dist_val$scat <- data.frame(Sent_Id = nnn$element_id, Sentiment_Score = round((nnn$ave_sentiment), 2))
        
        updateSelectInput(session, "compview", choices = c("No"))
        
      }
      
      updateSelectInput(session, "labelgrp", choices = c("Overall", dataColumnNames2))
      
      setProgress(1)
    })
    
    showModal(modalDialog(HTML(paste0("Pre-Processing done",txt,"<br>","Data has ",nrow(storedData$trim_data)," rows."))
                          , easyClose =  T, footer = modalButton("OK")))
  })
  
  
  
  output$ori_pie <- renderPlotly({
    
    plot_ly(dist_val$dist_in, labels = ~Class, values = ~Freq, marker = list(colors = c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 12, name = "Set3"))), type = 'pie')  %>%
    layout(title = "",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  output$grp_pie <- renderPlotly({
    
    plot_ly(dist_val$dist_mr, labels = ~Class, values = ~Freq, marker = list(colors = c("royalblue", "indianred", "darkgray")), type = 'pie')  %>%
      layout(title = "",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  output$key_plot <- renderPlotly({
    
    plot_ly(dist_val$chi, y = ~feature, x = ~chi2, color = ~mpg_type, 
            colors = c("indianred","royalblue"), type= 'bar', orientation = 'h') %>%
      layout(title = "",
             xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "", showgrid = FALSE))
  })  


  output$scat_plot <- renderPlotly({
    
    if(length(input$labelc1)>0 && length(input$labelc2)>0){
      plot_ly(dist_val$scat, y = ~Sentiment_Score, x = ~Sent_Id, color = ~Class, 
              colors = c("royalblue", "indianred"), type= 'scatter', mode = 'markers') %>%
        layout(title = "",
               xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
               yaxis = list(title = "", showgrid = FALSE))
    } else {
      plot_ly(dist_val$scat, y = ~Sentiment_Score, x = ~Sent_Id, type= 'scatter', mode = 'markers',
              colors = "royalblue") %>%
        layout(title = "",
               xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
               yaxis = list(title = "", showgrid = FALSE))
    }
  })   
  
  
  # expert table 2
  Docs.ex <- reactive({
    temp_data2 <- storedData$trim_data
    temp_data <- storedData$data[complete.cases(tolower(storedData$data[ ,input$tpDocs])),]
    
    nnn <- sentiment_by(get_sentences(temp_data2[[input$tpDocs]]))
    doc_senti <- data.frame(rowNum = nnn$element_id, Sentiment_Score = round((nnn$ave_sentiment), 2))
    ldaProbs.x <- merge(temp_data, doc_senti, by = "rowNum")
    if(input$tpLabels!="(None)") {
      ldaProbs.x[order(ldaProbs.x$Sentiment_Score, decreasing = T), c("rowNum", input$tpLabels, "Sentiment_Score", input$tpDocs)]
    } else {
      ldaProbs.x[order(ldaProbs.x$Sentiment_Score, decreasing = T), c("rowNum", "Sentiment_Score", input$tpDocs)]
    }
  })
  
  
  
  #Representative Document
  
  output$doc.table2 <- DT::renderDT({
    temp <- Docs.ex()
    if(input$tpLabels!="(None)") {
      colnames(temp) <- c("Row ID","Category","Sentiment Score", "Text")
      temp$Text <- as.character(temp$Text)
      temp$Category <- as.character(temp$Category)
    } else {
      colnames(temp) <- c("Row ID","Sentiment Score","Text")
      temp$Text <- as.character(temp$Text)  
    }
    datatable(temp, rownames= FALSE)
  }, 
  
  options = list(
    autoWidth = FALSE,
    #columnDefs = list(list(width = '50%', targets = list(3))),
    pageLength = 5,
    dom = 'tip'
  ),
  
  rownames= FALSE) 
  
  
  
  
  output$info_dyn <- renderUI({
    
    if(input$compview=="Yes") {
    column(12, 
           column(6, h4(paste0("Group-1"))),  column(6, h4(paste0("Group-2"))),
           
           infoBoxOutput("doc_cnt", width = 6), infoBoxOutput("doc_cnt2", width = 6),
           infoBoxOutput("sen_cnt", width = 6), infoBoxOutput("sen_cnt2", width = 6),
           infoBoxOutput("wrd_cnt", width = 6), infoBoxOutput("wrd_cnt2", width = 6))
    } else {
      column(12, infoBoxOutput("doc_cnt", width = 12), infoBoxOutput("sen_cnt", width = 12), 
             infoBoxOutput("wrd_cnt", width = 12))
    }
    
  })
  

    
  
  
  # f1 <- reactiveValues(comment_text = NULL, df = NULL, emo_df = NULL, freq_final = NULL, freq_wrd = NULL, doc_t = NULL, 
  #                      sen_t = NULL, wrd_t = NULL, neibour = NULL, ldp = NULL, clean_text = NULL, temp_data = NULL)
  

  observeEvent(paste0(input$freq_rare, input$labelgrp, input$rare_slide, input$uni_bi, input$compview), {
    
    stp <- unlist(strsplit(input$stopwords,","))
    stp <- trimws(stp)
    

    if(input$labelgrp!="Overall"){
      updateSelectInput(session, "compview", selected = "No")
      shinyjs::hide(id = "compview")
    } else {
      shinyjs::show(id = "compview")
    }
    
    
    withProgress(message = "Updating all visuals .... ",  {
      setProgress(0.20)
    
    if(input$compview=="No"){
      
    if(input$labelgrp=="Overall") {
      temp_data <- storedData$data
      trim_data <- storedData$trim_data
    } else {
      temp2 <- dist_val$feat_data
      temp_data <- temp2[which(temp2$category == input$labelgrp), ]
      trim <- dist_val$grp_data
      trim_data <- trim[which(trim$category == input$labelgrp), ]
    }
      
      comments <- as.character(temp_data[,input$tpDocs])
      
      if(length(comments)>0){
        
      myCorpus_nc2 <- corpus(as.character(trim_data[,input$tpDocs]))
      docvars(myCorpus_nc2, "rowNum") <- trim_data$rowNum
      dist_val$dfm_s <- dfm(myCorpus_nc2, remove = c(stopwords("en"), stopwords("SMART"), stp), remove_numbers = TRUE, remove_punct = TRUE,
                            stem = F, ngrams = 1)
      
      dist_val$doc_t <- nrow(temp_data)
      dist_val$sen_t <- sum(unlist(sapply(tokenize_sentences(comments),length)))
      dist_val$wrd_t <- sum(unlist(sapply(tokenize_words(comments),length)))

      comment_text <- temp_data[,input$tpDocs]
      length <- str_length(comment_text)
      ncap <- str_count(comment_text, "[A-Z]")
      nexcl <- str_count(comment_text, fixed("!"))
      nquest <- str_count(comment_text, fixed("?"))
      npunct <- str_count(comment_text, "[[:punct:]]")
      ndigits <- str_count(comment_text, "[[:digits]]")
      nword <- unlist(sapply(tokenize_words(comment_text), length))
      nsentence <- unlist(sapply(tokenize_sentences(comment_text), length))

      setProgress(0.40)
      avg_sen_per_doc <- round(sum(nsentence)/length(comment_text),2)
      avg_word_per_sen <- round(sum(nword)/sum(nsentence),2)
      avg_cap_per_sen <- round(sum(ncap)/sum(nsentence),2)
      avg_excl_per_sen <- round(sum(nexcl)/sum(nsentence),2)
      avg_ques_per_sen <- round(sum(nquest)/sum(nsentence),2)

      df <- data.frame(feature=c("Exclm per Sent", "Ques per Sent", "Caps per Sent", "Sent per Doc", "Word per Sent"),
                         value=c(avg_excl_per_sen, avg_ques_per_sen, avg_cap_per_sen, avg_sen_per_doc, avg_word_per_sen))
      
     names(df) <- c("feature","value")
     dist_val$df <- df
     
     comments2 <- trim_data[,input$tpDocs]
     d <- as.data.frame(comments2)

     if(nrow(d)>0) {
       sentil <- unnest_tokens(d, word, comments2) %>%
       left_join(get_sentiments("nrc"), by = "word") %>%
       filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA")) %>%
       group_by(sentiment) %>%
       summarize(freq = n()) %>%
       mutate(value=round(freq/sum(freq)*100),1) %>%
       ungroup()
        
       sentil <- sentil[order(sentil$value),]
       dist_val$emo_df <- sentil
       setProgress(0.60)
       
       if(input$freq_rare==1){
          shinyjs::hide(id = "hide3")
         
          if(input$uni_bi==1){
            shinyjs::show(id = "hide5")
            
            myDfm <- quanteda::tokens(as.character(comments2)) %>%
              tokens_remove("\\p{P}", valuetype = "regex", padding = FALSE) %>%
              tokens_remove(c(stopwords("en"), stopwords("SMART"), stp), padding  = FALSE) %>%
              tokens_ngrams(n = 1L) %>%
              dfm()
            
            xx <- data.frame(cnt = quanteda::topfeatures(myDfm, n=length(quanteda::featnames(myDfm))))
            yy <- data.frame(word = rownames(xx), freq = xx$cnt)
            yy$word <- mgsub(yy$word, "[[:punct:]]", " ", fixed = F)
            
            } else {
             shinyjs::hide(id = "hide5")
              
              myDfm <- quanteda::tokens(as.character(comments2)) %>%
                tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
                tokens_remove(c(stopwords("en"), stopwords("SMART"), stp), padding  = FALSE) %>%
                tokens_ngrams(n = 2L) %>%
                dfm()
              
              xx <- data.frame(cnt = topfeatures(myDfm, n=length(featnames(myDfm))))
              yy <- data.frame(word = rownames(xx), freq = xx$cnt)
              yy$word <- mgsub(yy$word, "[[:punct:]]", " ", fixed = F)
              
            }

          word_cnt <- yy
          #word_cnt <- word_cnt[!(word_cnt$word %in% c(stopwords("en"), stopwords("SMART"), stp)),]
          rows <- nrow(word_cnt)
          setProgress(0.80)
          
          word_cnt <- word_cnt[order(word_cnt$freq),]
          dist_val$freq_final <- tail(word_cnt, min(10,rows))
          datacolumns <- dist_val$freq_final$word
          updateSelectInput(session, "nei_wrd", choices = rev(datacolumns))
          setProgress(1)
          
       } else {
         
         shinyjs::show(id = "hide3")
         cut_off <- as.numeric(strsplit(as.character(input$rare_slide),"%")[[1]])/100
         
         if(input$uni_bi==1){
           shinyjs::show(id = "hide5")
           
           myDfm <- quanteda::tokens(as.character(comments2)) %>%
             tokens_remove("\\p{P}", valuetype = "regex", padding = FALSE) %>%
             tokens_remove(c(stopwords("en"), stopwords("SMART"), stp), padding  = FALSE) %>%
             tokens_ngrams(n = 1L) %>%
             dfm()
           
           xx <- data.frame(cnt = topfeatures(myDfm, n=length(featnames(myDfm))))
           yy <- data.frame(word = rownames(xx), freq = xx$cnt)
           yy$word <- mgsub(yy$word, "[[:punct:]]", " ", fixed = F)
           
         } else {
           shinyjs::hide(id = "hide5")
           
           myDfm <- quanteda::tokens(as.character(comments2)) %>%
             tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
             tokens_remove(c(stopwords("en"), stopwords("SMART"), stp), padding  = FALSE) %>%
             tokens_ngrams(n = 2L) %>%
             dfm()
           
           xx <- data.frame(cnt = quanteda::topfeatures(myDfm, n=length(quanteda::featnames(myDfm))))
           yy <- data.frame(word = rownames(xx), freq = xx$cnt)
           yy$word <- mgsub(yy$word, "[[:punct:]]", " ", fixed = F)
           
         }

         word_cnt <- yy %>% filter(freq > sum(freq)*cut_off)
         #word_cnt <- word_cnt[!(word_cnt$word %in% c(stopwords("en"), stopwords("SMART"), stp)),]
         rows <- nrow(word_cnt)
         setProgress(0.80)
        
         word_cnt <- word_cnt[order(word_cnt$freq),]
         dist_val$freq_final <- head(word_cnt, min(10,rows))
         datacolumns <- dist_val$freq_final$word
         updateSelectInput(session, "nei_wrd", choices = rev(datacolumns))
         setProgress(1)
         
       }
      }
      }
    } else {
      
      temp2 <- dist_val$feat_data
      temp_data <- temp2[which(temp2$category == "Group-1" | temp2$category == "Group-2"), ]
      trim <- dist_val$grp_data
      trim_data <- trim[which(trim$category == "Group-1" | trim$category == "Group-2"), ]

      comments_chk <- as.character(temp_data[,input$tpDocs])

      if(length(comments_chk)>0) {

        myCorpus_nc2 <- corpus(as.character(trim_data[,input$tpDocs]))
        docvars(myCorpus_nc2, "cat") <- trim_data$category

        dist_val$dfm_c <- dfm(myCorpus_nc2, remove = c(stopwords("en"), stopwords("SMART"), stp), remove_numbers = TRUE, remove_punct = TRUE,
                              stem = F, ngrams = 1, groups = "cat")


        comments <- as.character(temp_data[which(temp2$category == "Group-1"),input$tpDocs])
        dist_val$doc_t <- nrow(temp_data[which(temp2$category == "Group-1"),])
        dist_val$sen_t <- sum(unlist(sapply(tokenize_sentences(comments),length)))
        dist_val$wrd_t <- sum(unlist(sapply(tokenize_words(comments),length)))
        
        
        comments <- as.character(temp_data[which(temp2$category == "Group-2"),input$tpDocs])
        dist_val$doc_t2 <- nrow(temp_data[which(temp2$category == "Group-2"),])
        dist_val$sen_t2 <- sum(unlist(sapply(tokenize_sentences(comments),length)))
        dist_val$wrd_t2 <- sum(unlist(sapply(tokenize_words(comments),length)))

 
        comment_text <- temp_data[which(temp2$category == "Group-1"),input$tpDocs]
        length <- str_length(comment_text)
        ncap <- str_count(comment_text, "[A-Z]")
        nexcl <- str_count(comment_text, fixed("!"))
        nquest <- str_count(comment_text, fixed("?"))
        npunct <- str_count(comment_text, "[[:punct:]]")
        ndigits <- str_count(comment_text, "[[:digits]]")
        nword <- unlist(sapply(tokenize_words(comment_text), length))
        nsentence <- unlist(sapply(tokenize_sentences(comment_text), length))

        avg_sen_per_doc1 <- round(sum(nsentence)/length(comment_text),2)
        avg_word_per_sen1 <- round(sum(nword)/sum(nsentence),2)
        avg_cap_per_sen1 <- round(sum(ncap)/sum(nsentence),2)
        avg_excl_per_sen1 <- round(sum(nexcl)/sum(nsentence),2)
        avg_ques_per_sen1 <- round(sum(nquest)/sum(nsentence),2)


        comment_text <- temp_data[which(temp2$category == "Group-2"),input$tpDocs]
        length <- str_length(comment_text)
        ncap <- str_count(comment_text, "[A-Z]")
        nexcl <- str_count(comment_text, fixed("!"))
        nquest <- str_count(comment_text, fixed("?"))
        npunct <- str_count(comment_text, "[[:punct:]]")
        ndigits <- str_count(comment_text, "[[:digits]]")
        nword <- unlist(sapply(tokenize_words(comment_text), length))
        nsentence <- unlist(sapply(tokenize_sentences(comment_text), length))

        avg_sen_per_doc2 <- round(sum(nsentence)/length(comment_text),2)
        avg_word_per_sen2 <- round(sum(nword)/sum(nsentence),2)
        avg_cap_per_sen2 <- round(sum(ncap)/sum(nsentence),2)
        avg_excl_per_sen2 <- round(sum(nexcl)/sum(nsentence),2)
        avg_ques_per_sen2 <- round(sum(nquest)/sum(nsentence),2)

        setProgress(0.40)

        df <- data.frame(feature=c("Exclm per Sent", "Ques per Sent", "Caps per Sent", "Sent per Doc", "Word per Sent"),
                         value_1=c(avg_excl_per_sen1, avg_ques_per_sen1, avg_cap_per_sen1, avg_sen_per_doc1, avg_word_per_sen1),
                         value_2=c(avg_excl_per_sen2, avg_ques_per_sen2, avg_cap_per_sen2, avg_sen_per_doc2, avg_word_per_sen2))

        names(df) <- c("feature","value1","value2")
        dist_val$df <- df

        chk_temp <- trim_data[,input$tpDocs]
        chk <- as.data.frame(chk_temp)

        if(nrow(chk)>0) {

          comments2 <- trim_data[which(temp2$category == "Group-1"),input$tpDocs]
          d <- as.data.frame(comments2)
          sentil1 <- unnest_tokens(d, word, comments2) %>%
            left_join(get_sentiments("nrc"), by = "word") %>%
            filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA")) %>%
            group_by(sentiment) %>%
            summarize(freq = n()) %>%
            mutate(value_1=round(freq/sum(freq)*100),1) %>%
            ungroup()

          comments2 <- trim_data[which(temp2$category == "Group-2"),input$tpDocs]
          d <- as.data.frame(comments2)
          sentil2 <- unnest_tokens(d, word, comments2) %>%
            left_join(get_sentiments("nrc"), by = "word") %>%
            filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA")) %>%
            group_by(sentiment) %>%
            summarize(freq = n()) %>%
            mutate(value_2=round(freq/sum(freq)*100),1) %>%
            ungroup()

          sentil <- as.data.frame(merge(sentil1, sentil2, by = "sentiment", all = TRUE))
          sentil$val_sum <- sentil$value_1 + sentil$value_2
          sentil <- sentil[order(sentil$val_sum),]
          dist_val$emo_df <- sentil

          setProgress(0.60)

          comments2 <- trim_data[,input$tpDocs]
          
          if(input$freq_rare==1){
            shinyjs::hide(id = "hide3")
            
            if(input$uni_bi==1){
              shinyjs::show(id = "hide5")
              
              myDfm <- quanteda::tokens(as.character(comments2)) %>%
                tokens_remove("\\p{P}", valuetype = "regex", padding = FALSE) %>%
                tokens_remove(c(stopwords("en"), stopwords("SMART"), stp), padding  = FALSE) %>%
                tokens_ngrams(n = 1L) %>%
                dfm()
              
              xx <- data.frame(cnt = quanteda::topfeatures(myDfm, n=length(quanteda::featnames(myDfm))))
              yy <- data.frame(word = rownames(xx), freq = xx$cnt)
              yy$word <- mgsub(yy$word, "[[:punct:]]", " ", fixed = F)
              
            } else {
              shinyjs::hide(id = "hide5")
              
              myDfm <- quanteda::tokens(as.character(comments2)) %>%
                tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
                tokens_remove(c(stopwords("en"), stopwords("SMART"), stp), padding  = FALSE) %>%
                tokens_ngrams(n = 2L) %>%
                dfm()
              
              xx <- data.frame(cnt = topfeatures(myDfm, n=length(featnames(myDfm))))
              yy <- data.frame(word = rownames(xx), freq = xx$cnt)
              yy$word <- mgsub(yy$word, "[[:punct:]]", " ", fixed = F)
              
            }
            
            word_cnt <- yy
            #word_cnt <- word_cnt[!(word_cnt$word %in% c(stopwords("en"), stopwords("SMART"), stp)),]
            rows <- nrow(word_cnt)
            setProgress(0.80)
            
            word_cnt <- word_cnt[order(word_cnt$freq),]
            dist_val$freq_final <- tail(word_cnt, min(10,rows))
            datacolumns <- dist_val$freq_final$word
            updateSelectInput(session, "nei_wrd", choices = rev(datacolumns))
            setProgress(1)

          } else {
            
            shinyjs::show(id = "hide3")
            cut_off <- as.numeric(strsplit(as.character(input$rare_slide),"%")[[1]])/100
            
            if(input$uni_bi==1){
              shinyjs::show(id = "hide5")
              
              myDfm <- quanteda::tokens(as.character(comments2)) %>%
                tokens_remove("\\p{P}", valuetype = "regex", padding = FALSE) %>%
                tokens_remove(c(stopwords("en"), stopwords("SMART"), stp), padding  = FALSE) %>%
                tokens_ngrams(n = 1L) %>%
                dfm()
              
              xx <- data.frame(cnt = topfeatures(myDfm, n=length(featnames(myDfm))))
              yy <- data.frame(word = rownames(xx), freq = xx$cnt)
              yy$word <- mgsub(yy$word, "[[:punct:]]", " ", fixed = F)
              
            } else {
              shinyjs::hide(id = "hide5")
              
              myDfm <- quanteda::tokens(as.character(comments2)) %>%
                tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
                tokens_remove(c(stopwords("en"), stopwords("SMART"), stp), padding  = FALSE) %>%
                tokens_ngrams(n = 2L) %>%
                dfm()
              
              xx <- data.frame(cnt = quanteda::topfeatures(myDfm, n=length(quanteda::featnames(myDfm))))
              yy <- data.frame(word = rownames(xx), freq = xx$cnt)
              yy$word <- mgsub(yy$word, "[[:punct:]]", " ", fixed = F)
              
            }
            
            word_cnt <- yy %>% filter(freq > sum(freq)*cut_off)
            #word_cnt <- word_cnt[!(word_cnt$word %in% c(stopwords("en"), stopwords("SMART"), stp)),]
            rows <- nrow(word_cnt)
            setProgress(0.80)
            
            word_cnt <- word_cnt[order(word_cnt$freq),]
            dist_val$freq_final <- head(word_cnt, min(10,rows))
            datacolumns <- dist_val$freq_final$word
            updateSelectInput(session, "nei_wrd", choices = rev(datacolumns))
            setProgress(1)
            
          }
        }
      }
    }
    })
      
  })
  
  
  
  
  observeEvent(paste0(input$freq_rare, input$labelgrp, input$rare_slide, input$uni_bi, input$nei_wrd), {
    
    withProgress(message = "Updating Lexicon .... ",  {
      setProgress(0.20)
    
    comments2a <- storedData$trim_data[,input$tpDocs]
    d <- as.data.frame(comments2a)
   
    if((nrow(d)>0 & length(input$nei_wrd)>0)) {
    fulltext <- corpus(d,text_field = "comments2a")
    context_words <- input$nei_wrd

    bt <- kwic(fulltext, pattern = context_words, case_insensitive = TRUE) 
    kwic2 <- as.data.frame(bt)
    
    nword <- unlist(sapply(tokenize_words((d$comments2a),simplify = T),length))
    nword_max<-max(nword)
    nword_min<- min(nword)
    
    kwic2$index <- ((kwic2$to - nword_min)/(nword_max - nword_min))*100
    kwic3 <- kwic2[sample(1:nrow(kwic2),min(10,nrow(kwic2))),]
    kwic3$id <- substr(kwic3$docname,5,length(kwic3$docname))
    d$id <- rownames(d)
   
    kwic4 <- merge(x=kwic3, y=d, by="id", all.x=T)
    setProgress(0.60)
    
    if(nrow(kwic4)>0){
    kwic4text<-corpus(kwic4, text_field = "comments2a")
    kwic5 <- kwic(kwic4text, pattern = context_words, case_insensitive = TRUE) 
    dist_val$ldp <- kwic5
    setProgress(1)
    }
    }
    
    })
    
  })
  

  observe({
    if (input$tabs=="expl") {
       if((input$tpLabels!= "(None)" & input$uni_bi==1)) {
         showModal(modalDialog(HTML(paste0("Explore the Processed data in this tab.","<br>",
                                           "All plots are available for current selection.")), 
                               easyClose =  T, footer = modalButton("OK")))
         
       } else if((input$tpLabels!= "(None)" & input$uni_bi==2)) {
         showModal(modalDialog(HTML(paste0("Explore the Processed data in this tab.","<br>",
                                           "Lexicon Dispersion is unavailable for 2-Word Phrases.")),
                               easyClose = T, footer = modalButton("OK")))
         
       } else if((input$tpLabels== "(None)" & input$uni_bi==1)) {
         showModal(modalDialog(HTML(paste0("Explore the Processed data in this tab.","<br>",
                                           "Original, Regrouped & Keyness plots are unavailable for this selection")),
                               easyClose = T, footer = modalButton("OK")))
         
       } else if((input$tpLabels== "(None)" & input$uni_bi==2)) {
         showModal(modalDialog(HTML(paste0("Explore the Processed data in this tab.","<br>",
                                           "Original, Regrouped, Keyness & Lexican plots are unavailable for this selection")),
                               easyClose = T, footer = modalButton("OK")))
         
       }
    }
  })

  
  output$doc_cnt <- renderInfoBox({
    infoBox(
      paste("Documents"), value = dist_val$doc_t,
      icon = icon("edit"), color = "blue"
    )
  })
  
  
  output$sen_cnt <- renderInfoBox({
    infoBox(
      paste("Sentences"), value = dist_val$sen_t, 
      icon = icon("list"), color = "blue"
    )
  }) 
  
  output$wrd_cnt <- renderInfoBox({
    infoBox(
      paste("Words"), value = dist_val$wrd_t, 
      icon = icon("thumbs-up"), color = "blue"
    )
  }) 
  
  
  
  output$doc_cnt2 <- renderInfoBox({
    infoBox(
      paste("Documents"), value = dist_val$doc_t2,
      icon = icon("edit"), color = "red"
    )
  })
  
  
  output$sen_cnt2 <- renderInfoBox({
    infoBox(
      paste("Sentences"), value = dist_val$sen_t2, 
      icon = icon("list"), color = "red"
    )
  }) 
  
  output$wrd_cnt2 <- renderInfoBox({
    infoBox(
      paste("Words"), value = dist_val$wrd_t2, 
      icon = icon("thumbs-up"), color = "red"
    )
  }) 
  
  
  output$basic_feat <- renderPlotly({
    
    if(input$compview=="No"){
      
    plot_ly(as.data.frame(dist_val$df), y = ~feature, x = ~value, type= 'bar', orientation = 'h', 
            marker = list(color = "royalblue")) %>%
      layout(title = "",
             xaxis = list(title = "Value", showgrid = FALSE),
             yaxis = list(title = "", showgrid = FALSE, categoryarray = ~feature, categoryorder = "array"))
    } else {
      
      plot_ly(as.data.frame(dist_val$df), y = ~feature, x = ~value2, type= 'bar', orientation = 'h', 
              name = "Group-2", marker = list(color = "indianred")) %>%
        add_trace(x = ~value1, name = "Group-1", marker = list(color = "royalblue")) %>%
        layout(title = "",
               barmode = "group",
               xaxis = list(title = "Value", showgrid = FALSE),
               yaxis = list(title = "", showgrid = FALSE, categoryarray = ~feature, categoryorder = "array"))
    }
  }) 
  
  
  
  output$word_plot <- renderPlot({
    
    if(input$compview=="No"){
       textplot_wordcloud(dist_val$dfm_s, comparison = F, color = c("royalblue"), max_words = 75, min_size = 1)
    } else {
      textplot_wordcloud(dist_val$dfm_c, comparison = T, color = c("royalblue","indianred"), max_words = 75, min_size = 1, labelsize = 2)
    }

  })
  
  
  
  output$sent_plot <- renderPlotly({
    
    if(input$compview=="No"){
      
    plot_ly(as.data.frame(dist_val$emo_df), y = ~sentiment, x = ~value, type= 'bar', orientation = 'h',
            marker = list(color = "royalblue")) %>%
      layout(title = "",
             xaxis = list(title = "Percentage", showgrid = FALSE),
             yaxis = list(title = "", showgrid = FALSE, categoryarray = ~sentiment, categoryorder = "array"))
    } else {
      
      plot_ly(as.data.frame(dist_val$emo_df), y = ~sentiment, x = ~value_2, type= 'bar', orientation = 'h', 
              name = "Group-2", marker = list(color = "indianred")) %>%
        add_trace(x = ~value_1, name = "Group-1", marker = list(color = "royalblue")) %>%
        layout(title = "",
               barmode = "group",
               xaxis = list(title = "Percentage", showgrid = FALSE),
               yaxis = list(title = "", showgrid = FALSE, categoryarray = ~sentiment, categoryorder = "array"))
    }
  }) 
  
  
  output$phs_plot <- renderPlotly({
    
    plot_ly(as.data.frame(dist_val$freq_final), y = ~word, x = ~freq, type= 'bar', orientation = 'h',
            marker = list(color = "royalblue")) %>%
      layout(title = "",
             xaxis = list(title = "Counts", showgrid = FALSE),
             yaxis = list(title = "", showgrid = FALSE, categoryarray = ~word, categoryorder = "array"))
  }) 
  
  
  output$lex_plot <- renderPlot({
    
    textplot_xray(dist_val$ldp)+ aes(color = keyword) + scale_color_manual(values = c("red")) + 
      scale_x_continuous(labels = waiver())
    
  })
  
  
  
  observeEvent(input$expl.res, {
    updateTabItems(session, "tabs", selected = "expl")
  })
  
  
  observeEvent(input$top.res, {
    updateTabItems(session, "tabs", selected = "topics")
  })
  
  
  observeEvent(input$bac1, {
    updateTabItems(session, "tabs", selected = "model")
  })
  
  
  observeEvent(input$fro1, {
    updateTabItems(session, "tabs", selected = "topics")
  })
  
  
  observeEvent(input$bac2, {
    updateTabItems(session, "tabs", selected = "model")
  })
  
  
  observeEvent(input$fro2, {
    updateTabItems(session, "tabs", selected = "expl")
  })
  
  
  
  
  
  
  
  
  v1 <- reactiveValues(probdocs = NULL, probterms = NULL, topicnames = NULL, 
                       stmFit = NULL, out = NULL, sentiment = NULL, topicnamesa = NULL,
                       topicnames_doc = NULL, probterms_doc = NULL, sentiment_doc = NULL,
                       clusters = NULL)
  
  v2 <- reactiveValues(probdocs = NULL, probterms = NULL, topicnames = NULL, 
                       stmFit = NULL, out = NULL, sentiment = NULL, topicnamesa = NULL,
                       topicnames_doc = NULL, probterms_doc = NULL, sentiment_doc = NULL,
                       clusters = NULL)
  
  z1 <- reactiveValues(Corpus = NULL, dtm = NULL, dfm = NULL, raw_documents = NULL, tdfm = NULL)
  z2 <- reactiveValues(Corpus = NULL, dtm = NULL, dfm = NULL, raw_documents = NULL, tdfm = NULL)
  z3 <- reactiveValues(Corpus = NULL, dtm = NULL, dfm = NULL, raw_documents = NULL, tdfm = NULL)
  
  # topic models
  
  observeEvent(input$topic.update, {
    
    withProgress(message = "Running Model .... ",  {
      setProgress(0.20)
      
      temp_data <- storedData$trim_data
      
      if(length(input$model1)==0){
        temp_data1 <- temp_data
      } else {
        temp_data1 <- temp_data[temp_data[[input$tpLabels]] %in% input$model1, ]
      }
      
      if(length(input$model2)==0){
        temp_data2 <- temp_data
      } else {
        temp_data2 <- temp_data[temp_data[[input$tpLabels]] %in% input$model2, ]
      }
      
      temp_data1$category = "Group: Model-1"
      temp_data2$category = "Group: Model-2"
      temp_data_comb <- rbind(temp_data1,temp_data2)
      
      # creating corpus with Stemming / Lemmatization
      if(input$lemmatise==TRUE) {
        
        if(input$stemming==TRUE) {
          MyCorpus1 <- corpus(stem_strings(lemmatize_strings(str_to_lower(as.character(temp_data1[,input$tpDocs])))))
          MyCorpus2 <- corpus(stem_strings(lemmatize_strings(str_to_lower(as.character(temp_data2[,input$tpDocs])))))
          MyCorpus3 <- corpus(stem_strings(lemmatize_strings(str_to_lower(as.character(temp_data_comb[,input$tpDocs])))))
        } else {
          MyCorpus1 <- corpus(lemmatize_strings(str_to_lower(as.character(temp_data1[,input$tpDocs]))))
          MyCorpus2 <- corpus(lemmatize_strings(str_to_lower(as.character(temp_data2[,input$tpDocs]))))
          MyCorpus3 <- corpus(lemmatize_strings(str_to_lower(as.character(temp_data_comb[,input$tpDocs]))))
        }
        
      } else {
        
        if(input$stemming==TRUE) {
          MyCorpus1 <- corpus(stem_strings(str_to_lower(as.character(temp_data1[,input$tpDocs]))))
          MyCorpus2 <- corpus(stem_strings(str_to_lower(as.character(temp_data2[,input$tpDocs]))))
          MyCorpus3 <- corpus(stem_strings(str_to_lower(as.character(temp_data_comb[,input$tpDocs]))))
        } else {
          MyCorpus1 <- corpus(str_to_lower(as.character(temp_data1[,input$tpDocs])))
          MyCorpus2 <- corpus(str_to_lower(as.character(temp_data2[,input$tpDocs])))
          MyCorpus3 <- corpus(str_to_lower(as.character(temp_data_comb[,input$tpDocs])))
        }
        
      }
      
      
      # sets input data row number as primary key -- ensures matchback for datasets without a primary key
      docvars(MyCorpus1, "rowNum") <- temp_data1$rowNum
      docvars(MyCorpus2, "rowNum") <- temp_data2$rowNum
      docvars(MyCorpus3, "cat") <- temp_data_comb$category
      
      stp <- unlist(strsplit(input$stopwords,","))
      stp <- trimws(stp)
      #ngram <- ifelse(input$ngrams==1,1L, 1L:2L)
      ngram <- 1L
      
      Dfm1 <- dfm(MyCorpus1, remove = c(stopwords("en"), stopwords("SMART"), stp), remove_numbers = TRUE, remove_punct = TRUE,
                  stem = F, ngrams = ngram)
      
      Dfm2 <- dfm(MyCorpus2, remove = c(stopwords("en"), stopwords("SMART"), stp), remove_numbers = TRUE, remove_punct = TRUE,
                  stem = F, ngrams = ngram)
      
      Dfm3 <- dfm(MyCorpus3, remove = c(stopwords("en"), stopwords("SMART"), stp), remove_numbers = TRUE, remove_punct = TRUE,
                  stem = F, ngrams = ngram, groups = "cat")
      
      minDoc2 <- as.numeric(strsplit(as.character(input$minDoc),"%")[[1]])/100 
      tdfm1 <- dfm_trim(Dfm1, min_docfreq = minDoc2)
      tdfm2 <- dfm_trim(Dfm2, min_docfreq = minDoc2)
      tdfm3 <- dfm_trim(Dfm3, min_docfreq = minDoc2)
      
      
      # we now export to a format that we can run the topic model with
      z1$Corpus <- MyCorpus1
      z1$dtm <- convert(tdfm1, to= "topicmodels")
      z1$dfm <- convert(tdfm1, to = "stm", docvars = docvars(MyCorpus1))
      z1$raw_documents <- as.character(temp_data1[,input$tpDocs])
      z1$tdfm <- tdfm1
      
      z2$Corpus <- MyCorpus2
      z2$dtm <- convert(tdfm2, to= "topicmodels")
      z2$dfm <- convert(tdfm2, to = "stm", docvars = docvars(MyCorpus2))
      z2$raw_documents <- as.character(temp_data2[,input$tpDocs])
      z2$tdfm <- tdfm2
      
      z3$Corpus <- MyCorpus3
      #z3$dtm <- convert(tdfm3, to= "topicmodels")
      #z3$dfm <- convert(tdfm3, to = "stm", docvars = docvars(MyCorpus3))
      z3$raw_documents <- data.frame(docs = temp_data_comb[,input$tpDocs], cat = temp_data_comb$category)
      z3$tdfm <- tdfm3
      
      
      k <- input$num.topics
      dfm1 <- z1$dfm
      raw_documents1 <- z1$raw_documents
      
      dfm2 <- z2$dfm
      raw_documents2 <- z2$raw_documents
      
      # use quanteda converter to convert our Dfm
      
      setProgress(0.20)
      
      out1 <- prepDocuments(dfm1$documents, dfm1$vocab, dfm1$meta, lower.thresh = 1, subsample = NULL)
      out2 <- prepDocuments(dfm2$documents, dfm2$vocab, dfm2$meta, lower.thresh = 1, subsample = NULL)
      
      sink("myfile1.txt", append=F, split=TRUE)
      modtyp <- ifelse(input$modtyp==1,"Spectral", "LDA")
      stmFit1 <- stm(out1$documents, out1$vocab, K = k, #prevalence =~ Party + s(Time),
                     max.em.its = input$iter, data = out1$meta, init.type = paste0(modtyp), seed = 42)
      sink()
      
      setProgress(0.40)
      
      sink("myfile2.txt", append=F, split=TRUE)
      stmFit2 <- stm(out2$documents, out2$vocab, K = k, #prevalence =~ Party + s(Time),
                     max.em.its = input$iter, data = out2$meta, init.type = paste0(modtyp), seed = 42)
      sink()
      
      setProgress(0.60)
      
      probterms1 <- data.frame(t(data.frame(probs = stmFit1$beta[[1]])))  # words (rows) x topics (columns)
      probterms2 <- data.frame(t(data.frame(probs = stmFit2$beta[[1]])))  # words (rows) x topics (columns)
      
      row.names(probterms1) <- stmFit1$vocab
      probdocs1 <- data.frame(stmFit1$theta)
      
      row.names(probterms2) <- stmFit2$vocab
      probdocs2 <- data.frame(stmFit2$theta)
      
      topic1.names <- character(length = ncol(stmFit1$theta))
      topic1.names2 <- character(length = ncol(stmFit1$theta))
      topic1a.names <- character(length = ncol(stmFit1$theta))
      
      for (i in 1:ncol(stmFit1$theta)){
        temp <- order(-probterms1[,i])
        temp2 <- rownames(probterms1[temp,])
        topic1.names[i] <- paste(temp2[1:5], collapse = " \n ")
        topic1a.names[i] <- paste(temp2[1:10], collapse = " ")
        topic1.names2[i] <- paste(temp2[1:5], collapse = "; ")
        setProgress(0.60 + (i/ncol(stmFit1$theta))*0.10)
      }
      
      topic2.names <- character(length = ncol(stmFit2$theta))
      topic2.names2 <- character(length = ncol(stmFit2$theta))
      topic2a.names <- character(length = ncol(stmFit2$theta))
      
      for (i in 1:ncol(stmFit2$theta)){
        temp <- order(-probterms2[,i])
        temp2 <- rownames(probterms2[temp,])
        topic2.names[i] <- paste(temp2[1:5], collapse = " \n ")
        topic2a.names[i] <- paste(temp2[1:10], collapse = " ")
        topic2.names2[i] <- paste(temp2[1:5], collapse = "; ")
        setProgress(0.70 + (i/ncol(stmFit2$theta))*0.10)
      }
      
      setProgress(0.80)
      
      wrd <- unlist(strsplit(topic1.names2,"; "))
      my_dict <- dictionary(list(words = wrd))
      new_dfm <- dfm_select(z1$tdfm, my_dict)
      dis <- textstat_simil(new_dfm, selection = NULL, margin = c("features"), method = "jaccard", upper=T)
      clus1 <- hclust(dist(dis),method="ward.D2")
      
      wrd <- unlist(strsplit(topic2.names2,"; "))
      my_dict <- dictionary(list(words = wrd))
      new_dfm <- dfm_select(z2$tdfm, my_dict)
      dis <- textstat_simil(new_dfm, selection = NULL, margin = c("features"), method = "jaccard", upper=T)
      clus2 <- hclust(dist(dis),method="ward.D2")
      
      v1$out <- out1
      v1$stmFit <- stmFit1
      v1$probdocs <- probdocs1
      v1$probterms <- probterms1
      v1$topicnames <- topic1.names
      v1$topicnamesa <- topic1a.names
      v1$sentiment <- data.frame(word=rownames(probterms1)) %>% left_join(get_sentiments("nrc"), by = "word")
      v1$sentiment_doc <- na.omit(v1$sentiment)
      v1$topicnames_doc <- data.frame(id=paste0("X",seq(1,length(topic1.names2))), topic=topic1.names2)
      v1$probterms_doc <- v1$probterms
      v1$probterms_doc[] <- lapply(v1$probterms_doc, function(x) paste0(round(exp(x)*100.0,1),"%"))
      v1$clusters <- clus1
      
      
      v2$out <- out2
      v2$stmFit <- stmFit2
      v2$probdocs <- probdocs2
      v2$probterms <- probterms2
      v2$topicnames <- topic2.names
      v2$topicnamesa <- topic2a.names
      v2$sentiment <- data.frame(word=rownames(probterms2)) %>% left_join(get_sentiments("nrc"), by = "word")
      v2$sentiment_doc <- na.omit(v2$sentiment)
      v2$topicnames_doc <- data.frame(id=paste0("X",seq(1,length(topic2.names2))), topic=topic2.names2)
      v2$probterms_doc <- v2$probterms
      v2$probterms_doc[] <- lapply(v2$probterms_doc, function(x) paste0(round(exp(x)*100.0,1),"%"))
      v2$clusters <- clus2
      
      setProgress(0.90)
      
      # stmCorrViz(stmFit, "www/corrviz.html", documents_raw=raw_documents,
      #            documents_matrix=out$documents, labels_number=7,
      #            title="Topic Model: Using STM", display= F)
      
      #toLDAvis(mod=stmFit, docs=out$documents)
      
      txt1 <- readLines(con <- file("myfile1.txt"))
      close(con)
      txt2 <- readLines(con <- file("myfile2.txt"))
      close(con)
      com1 <- ifelse(trimws(txt1[length(txt1)]) == "Model Converged","",". Please increase Iterations")
      com2 <- ifelse(trimws(txt2[length(txt2)]) == "Model Converged","",". Please increase Iterations")
      setProgress(1)
    })
    
    showModal(modalDialog(HTML(paste0("Model-1: ",txt1[length(txt1)],com1,"<br> Model-2: ",txt2[length(txt2)],com2)), 
                          easyClose = T, footer = modalButton("OK")))
    
    file.remove("myfile1.txt")
    file.remove("myfile2.txt")
    
  })
  
  
  
  # Network and Topic Video
  x1 <- reactiveValues(nodes = NULL, edges = NULL, vert = NULL, link = NULL, net = NULL)
  x2 <- reactiveValues(nodes = NULL, edges = NULL, vert = NULL, link = NULL, net = NULL)
  
  observeEvent(input$network.update, {
    
    withProgress(message = "Creating Visuals .... ",  {
      setProgress(0.10)
      
      #####################################
      
      results1 <- new.topic.network(v1$stmFit, input$parm, v1$topicnames)
      x1$nodes <- results1[[1]]
      x1$edges <- results1[[2]]
      
      results2 <- new.topic.network(v2$stmFit, input$parm, v2$topicnames)
      x2$nodes <- results2[[1]]
      x2$edges <- results2[[2]]
      
      ###############################################
      
      setProgress(0.20)
      
      num_topic <- input$num.topics
      coloa <- brewer.pal(n = 8, name = "Dark2")
      colob <- brewer.pal(n = 12, name = "Set3")
      col_pal <- c(coloa, colob)
      col_pal <- col_pal[1:num_topic]
      
      weight1 <- data.frame(topic_no = seq(1:num_topic), weights = 20*(colMeans(v1$probdocs)/max(colMeans(v1$probdocs))))
      weight2 <- data.frame(topic_no = seq(1:num_topic), weights = 20*(colMeans(v2$probdocs)/max(colMeans(v2$probdocs))))
      
      nodet1 <- data.frame()
      linkt1 <- data.frame()
      nodet2 <- data.frame()
      linkt2 <- data.frame()
      
      for (i in 1:num_topic){
        
        context_words1 <- unlist(strsplit(as.character(v1$topicnamesa[i])," "))
        context_words1 <- trimws(context_words1)
        context_words1 <- context_words1[1:5]
        
        link1 <- as.data.frame(t(combn(context_words1,2)))
        names(link1) <- c("from_t", "to_t")
        link1$topic_no <- i
        
        node1 <- as.data.frame(context_words1)
        names(node1) <- c("key_word")
        node1$word_no <- seq(1:5)
        node1$topic_no <- i
        
        if (i < 10) {
          node1$type <- paste0("Topic-0", node1$topic_no)
          node1$id <- paste0("T0", node1$topic_no, "W", node1$word_no)
        } else {
          node1$type <- paste0("Topic-", node1$topic_no)
          node1$id <- paste0("T", node1$topic_no, "W", node1$word_no)
        }
        
        linkt1 <- rbind(linkt1, link1)
        nodet1 <- rbind(nodet1, node1)
        
        #######################################
        
        context_words2 <- unlist(strsplit(as.character(v2$topicnamesa[i])," "))
        context_words2 <- trimws(context_words2)
        context_words2 <- context_words2[1:5]
        
        link2 <- as.data.frame(t(combn(context_words2,2)))
        names(link2) <- c("from_t", "to_t")
        link2$topic_no <- i
        
        node2 <- as.data.frame(context_words2)
        names(node2) <- c("key_word")
        node2$word_no <- seq(1:5)
        node2$topic_no <- i
        
        if (i < 10) {
          node2$type <- paste0("Topic-0", node2$topic_no)
          node2$id <- paste0("T0", node2$topic_no, "W", node2$word_no)
        } else {
          node2$type <- paste0("Topic-", node2$topic_no)
          node2$id <- paste0("T", node2$topic_no, "W", node2$word_no)
        }
        
        linkt2 <- rbind(linkt2, link2)
        nodet2 <- rbind(nodet2, node2)
    
      }
      
      setProgress(0.30)
      
      node_m1 <- merge(nodet1, weight1, by = "topic_no", all.x = T)
      node_m1 <- node_m1[order(node_m1$weights, decreasing = TRUE),]
      node_m1 <- node_m1[!duplicated(node_m1$key_word), ]
      node_m1 <- node_m1[order(node_m1$id, decreasing = FALSE),]
      node_m1_t <- node_m1[,c("key_word", "id")]
      
      node_m2 <- merge(nodet2, weight2, by = "topic_no", all.x = T)
      node_m2 <- node_m2[order(node_m2$weights, decreasing = TRUE),]
      node_m2 <- node_m2[!duplicated(node_m2$key_word), ]
      node_m2 <- node_m2[order(node_m2$id, decreasing = FALSE),]
      node_m2_t <- node_m2[,c("key_word", "id")]
      
      setProgress(0.50)
      
      link_mt1 <- merge(linkt1, node_m1_t, by.x = "from_t", by.y = "key_word", all.x = T)
      link_mt1 <- link_mt1[,c("id", "to_t", "topic_no")]
      names(link_mt1) <- c("from", "to_t", "topic_no")
      link_mt1a <- merge(link_mt1, node_m1_t, by.x = "to_t", by.y = "key_word", all.x = T)
      link_mt1a <- link_mt1a[,c("from", "id", "topic_no")]
      names(link_mt1a) <- c("from", "to", "topic_no")
      link_m1 <- merge(link_mt1a, weight1, by = "topic_no", all.x = T)
      link_m1 <- link_m1[order(link_m1$from, link_m1$to),]
      
      
      link_mt2 <- merge(linkt2, node_m2_t, by.x = "from_t", by.y = "key_word", all.x = T)
      link_mt2 <- link_mt2[,c("id", "to_t", "topic_no")]
      names(link_mt2) <- c("from", "to_t", "topic_no")
      link_mt2a <- merge(link_mt2, node_m2_t, by.x = "to_t", by.y = "key_word", all.x = T)
      link_mt2a <- link_mt2a[,c("from", "id", "topic_no")]
      names(link_mt2a) <- c("from", "to", "topic_no")
      link_m2 <- merge(link_mt2a, weight2, by = "topic_no", all.x = T)
      link_m2 <- link_m2[order(link_m2$from, link_m2$to),]
      
      setProgress(0.70)
      
      #showModal(modalDialog(HTML(paste0("Link-1:", nrow(link_m1),"<br>",
      #                                  "Link-2:", nrow(link_m2))), easyClose = T, footer = modalButton("OK")))
      
      net_m1 <- network(link_m1, vertex.attr=node_m1, matrix.type="edgelist", 
                      loops=F, multiple=F, ignore.eval = F)
      
      net_m2 <- network(link_m2, vertex.attr=node_m2, matrix.type="edgelist", 
                        loops=F, multiple=F, ignore.eval = F)
      
      net_m1 %n% "net.name" <- "Topic Creation" #  network attribute
      net_m1 %v% "key_word"    # Node attribute
      net_m1 %e% "topic_no"     # Edge attribute
      net_m1 %v% "col" <- col_pal[net_m1 %v% "topic_no"]
      
      net_m2 %n% "net.name" <- "Topic Creation" #  network attribute
      net_m2 %v% "key_word"    # Node attribute
      net_m2 %e% "topic_no"     # Edge attribute
      net_m2 %v% "col" <- col_pal[net_m2 %v% "topic_no"]
      
      
      setProgress(0.80)
      
      vs_1 <- data.frame(onset=0, terminus=nrow(link_m1)+1, vertex.id=1:nrow(node_m1))
      es_1 <- data.frame(onset=1:nrow(link_m1), terminus=nrow(link_m1)+1, 
                       head=as.matrix(net_m1, matrix.type="edgelist")[,1],
                       tail=as.matrix(net_m1, matrix.type="edgelist")[,2])
      net1.dyn <- networkDynamic(base.net=net_m1, edge.spells=es_1, vertex.spells=vs_1)
      
      setProgress(0.90)
      
      vs_2 <- data.frame(onset=0, terminus=nrow(link_m2)+1, vertex.id=1:nrow(node_m2))
      es_2 <- data.frame(onset=1:nrow(link_m2), terminus=nrow(link_m2)+1, 
                         head=as.matrix(net_m2, matrix.type="edgelist")[,1],
                         tail=as.matrix(net_m2, matrix.type="edgelist")[,2])
      net2.dyn <- networkDynamic(base.net=net_m2, edge.spells=es_2, vertex.spells=vs_2)
      
      x1$vert <- vs_1
      x1$link <- es_1
      x1$net <- net1.dyn
      
      x2$vert <- vs_2
      x2$link <- es_2
      x2$net <- net2.dyn
      
      setProgress(1)
    })
    
    showModal(modalDialog(HTML(paste0("Visuals created.")), easyClose = T, footer = modalButton("OK")))
    
  })
  
  
  # save and load
  
  observeEvent(input$save.results, {
    
    mindoc2 <- as.numeric(strsplit(as.character(input$minDoc),"%")[[1]])/100
    
    dir <- readDirectoryInput(session, 'directory')
    
    dir1.terms <- paste0(dir,"/prob-terms1.csv")
    dir1.docs <- paste0(dir,"/prob-docs1.csv")
    dir1.topics <- paste0(dir,"/topic-names1.csv")
    dir1.topic.big <- paste0(dir,"/topic-big-names1.csv")
    dir1.sentiment <- paste0(dir,"/sentiment1.csv")
    dir1.parms <- paste0(dir,"/sparameters1.csv")
    
    dir2.terms <- paste0(dir,"/prob-terms2.csv")
    dir2.docs <- paste0(dir,"/prob-docs2.csv")
    dir2.topics <- paste0(dir,"/topic-names2.csv")
    dir2.topic.big <- paste0(dir,"/topic-big-names2.csv")
    dir2.sentiment <- paste0(dir,"/sentiment2.csv")
    dir2.parms <- paste0(dir,"/sparameters2.csv")
    
    write.csv(v1$probterms, dir1.terms, row.names = T)
    write.csv(v1$probdocs, dir1.docs, row.names = T)
    write.csv(v1$sentiment, dir1.sentiment, row.names = F)
    write.csv(v1$topicnames, dir1.topics, row.names = F)
    write.csv(v1$topicnamesa, dir1.topic.big, row.names = F)
    parameters1 <- data.frame(Stopwords = input$stopwords,
                              minDoc = mindoc2,
                              stem = input$stemming,
                              lemma = input$lemmatise,
                              NumTopics = input$num.topics,
                              Iterations = input$iter)
    
    write.csv(parameters1, dir1.parms, row.names = F)
    
    stmFit1 <- v1$stmFit
    out1 <- v1$out
    cluster1 <- v1$clusters
    save(stmFit1, file = paste0(dir,"/stmFit1.RData"))
    save(out1, file = paste0(dir,"/out1.RData"))
    save(cluster1, file = paste0(dir,"/cluster1.RData"))
    
    
    write.csv(v2$probterms, dir2.terms, row.names = T)
    write.csv(v2$probdocs, dir2.docs, row.names = T)
    write.csv(v2$sentiment, dir2.sentiment, row.names = F)
    write.csv(v2$topicnames, dir2.topics, row.names = F)
    write.csv(v2$topicnamesa, dir2.topic.big, row.names = F)
    parameters2 <- data.frame(Stopwords = input$stopwords,
                              minDoc = mindoc2,
                              stem = input$stemming,
                              lemma = input$lemmatise,
                              NumTopics = input$num.topics,
                              Iterations = input$iter)
    
    write.csv(parameters2, dir2.parms, row.names = F)
    
    stmFit2 <- v2$stmFit
    out2 <- v2$out
    cluster2 <- v2$clusters
    save(stmFit2, file = paste0(dir,"/stmFit2.RData"))
    save(out2, file = paste0(dir,"/out2.RData"))
    save(cluster2, file = paste0(dir,"/cluster2.RData"))
    
    #     
    # # Create a docx object
    # doc = docx()
    # 
    # # add a document title
    # doc = addParagraph( doc, "Topic Model: Results & Documentation", stylename = "TitleDoc" )
    # 
    # # some text
    # doc = addParagraph( doc, 
    #                     "Structural Topic Modelling Technique has been used to define this model.
    #                     Like other topic models, the STM is a generative model of word counts. That means we define a data generating process
    #                     for each document and then use the data to find the most likely values for the parameters within the model.
    #                     Within this framework (which is the same as other topic models like LDA), a topic is defined as a mixture over words 
    #                     where each word has a probability of belonging to a topic. And a document is a mixture over topics, meaning that a single
    #                     document can be composed of multiple topics. As such, the sum of the topic proportions across all topics for a document is one, 
    #                     and the sum word probabilities for a given topic is one.", 
    #                     stylename = "Normal")
    # doc = addParagraph( doc, "", stylename = "Normal" )
    # 
    # # add a section title
    # doc = addTitle( doc, "Model Parameters and Topics", level = 1 )
    # doc = addParagraph( doc, "", stylename = "Normal" )
    # doc = addParagraph( doc, "Here are the parameters defined to generate this Topic Model.", stylename = "Normal" )
    # 
    # # add a table with Model Parameters
    # parameters.table = FlexTable( data = parameters, 
    #                               header.cell.props = cellProperties( background.color = "#003366" ),
    #                               header.text.props = textBold( color = "white" ), add.rownames = F )
    # doc = addFlexTable(doc, parameters.table)
    # doc = addParagraph( doc, "", stylename = "Normal" )
    # 
    # 
    # doc = addParagraph( doc, "Here are the Topics generated in this Topic Model.", stylename = "Normal" )
    # 
    # # add a table with Model Topics
    # topicnames.table = FlexTable( data = v$topicnames_doc, 
    #                              header.cell.props = cellProperties( background.color = "#003366" ),
    #                              header.text.props = textBold( color = "white" ), add.rownames = F )
    # doc = addFlexTable(doc, topicnames.table)
    # doc = addParagraph( doc, "", stylename = "Normal" )
    # 
    # 
    # # add a section title
    # doc = addTitle( doc, "Topic Probabilities and Sentiments", level = 1 )
    # doc = addParagraph( doc, "", stylename = "Normal" )
    # doc = addParagraph( doc, "Here are the Probabilities for each Term to be in a Topic ", stylename = "Normal" )
    # 
    # # add a table with Model Parameters
    # probterms.table = FlexTable( data = v$probterms_doc, 
    #                              header.cell.props = cellProperties( background.color = "#003366" ),
    #                              header.text.props = textBold( color = "white" ), add.rownames = T )
    # doc = addFlexTable(doc, probterms.table)
    # doc = addParagraph( doc, "", stylename = "Normal" )
    # 
    # 
    # doc = addParagraph( doc, "Here are the sentiments of key Terms mentioned above.", stylename = "Normal" )
    # 
    # # add a table with Model Topics
    # sentiment.table = FlexTable( data = v$sentiment_doc, 
    #                               header.cell.props = cellProperties( background.color = "#003366" ),
    #                               header.text.props = textBold( color = "white" ), add.rownames = F )
    # doc = addFlexTable(doc, sentiment.table)
    # doc = addParagraph( doc, "", stylename = "Normal" )
    # 
    # 
    # # write the doc
    # writeDoc( doc, file = paste0(dir,"/Model_Results.docx" ))
    # 
    
    showModal(modalDialog("Model Saved", easyClose =  T, footer = modalButton("OK")))
  })
  
  
  
  observeEvent(input$load.model, {
    dir <- readDirectoryInput(session, 'load.directory')
    
    v1$probterms <- read.csv(file = paste0(dir,"/prob-terms1.csv"), stringsAsFactors = F, row.names = 1)
    v1$probdocs <- read.csv(file = paste0(dir,"/prob-docs1.csv"), stringsAsFactors = F, row.names = 1)
    v1$sentiment <- read.csv(file = paste0(dir,"/sentiment1.csv"), stringsAsFactors = F, row.names = NULL)
    
    v2$probterms <- read.csv(file = paste0(dir,"/prob-terms2.csv"), stringsAsFactors = F, row.names = 1)
    v2$probdocs <- read.csv(file = paste0(dir,"/prob-docs2.csv"), stringsAsFactors = F, row.names = 1)
    v2$sentiment <- read.csv(file = paste0(dir,"/sentiment2.csv"), stringsAsFactors = F, row.names = NULL)
    
    load(paste0(dir,"/stmFit1.RData"))
    v1$stmFit <- stmFit1
    load(paste0(dir,"/stmFit2.RData"))
    v2$stmFit <- stmFit2
    
    load(paste0(dir,"/out1.RData"))
    v1$out <- out1
    load(paste0(dir,"/out2.RData"))
    v2$out <- out2
    
    load(paste0(dir,"/cluster1.RData"))
    v1$clusters <- cluster1
    load(paste0(dir,"/cluster2.RData"))
    v2$clusters <- cluster2
    
    
    topic1.names <- character(length = ncol(v1$probterms))
    topic1a.names <- character(length = ncol(v1$probterms))
    
    for (i in 1:ncol(v1$probterms)){
      temp <- order(-v1$probterms[,i])
      temp2 <- rownames(v1$probterms[temp,])
      topic1.names[i] <- paste(temp2[1:5], collapse = " \n ")
      topic1a.names[i] <- paste(temp2[1:10], collapse = " ")
    }
    v1$topicnames <- topic1.names
    v1$topicnamesa <- topic1a.names
    
    
    topic2.names <- character(length = ncol(v2$probterms))
    topic2a.names <- character(length = ncol(v2$probterms))
    
    for (i in 1:ncol(v2$probterms)){
      temp <- order(-v2$probterms[,i])
      temp2 <- rownames(v2$probterms[temp,])
      topic2.names[i] <- paste(temp2[1:5], collapse = " \n ")
      topic2a.names[i] <- paste(temp2[1:10], collapse = " ")
    }
    v2$topicnames <- topic2.names
    v2$topicnamesa <- topic2a.names
    

    showModal(modalDialog("Model Uploaded", easyClose = T, footer = modalButton("OK")))
    
  })
  
  
  
  ### Network
  

  output$word_cloud_comb <- renderPlot({
    
    if(length(input$model1)==0 & length(input$model2)==0){
      textplot_wordcloud(z3$tdfm, comparison = F, color = c("royalblue"), max_words = 75, min_size = 1)
    } else {
      textplot_wordcloud(z3$tdfm, comparison = T, color = c("royalblue", "indianred"), max_words = 75, min_size = 1)
    }
    
  })
  
  
  
  output$out_model <- reactive({
    
    if(input$out_mod==1) {
      paste0("Model-1: ", toupper(input$tpLabels)," as ", 
             ifelse(length(input$model1)>0,paste(input$model1, collapse=", ")," ALL SELECTIONS"),".")
    } else {
      paste0("Model-2: ", toupper(input$tpLabels)," as ", 
             ifelse(length(input$model2)>0,paste(input$model2, collapse=", ")," ALL SELECTIONS"),".")
    }
    
  })
  
  
  
  output$out_labels <- reactive({
    
    if(input$tpLabels != "(None)"){
      HTML(paste0("Group-1:   Category ", toupper(input$tpLabels)," as ", paste(input$labelc1, collapse=", "),". <br> <br>",
           "Group-2:  Category ", toupper(input$tpLabels)," as ", paste(input$labelc2, collapse=", "),". "))
    } else {
      HTML(paste0("Data is not Categorized."))
    }
    
  })
  

  
  output$topic_corr <- renderPlot({
    
    if(input$out_mod==1){
      plot(topicCorr(v1$stmFit))
    } else {
      plot(topicCorr(v2$stmFit))
    }
    
  })
  
  
  output$topic_summ <- renderPlot({
    
    if(input$out_mod==1){
      plot(v1$stmFit, type = "summary")
    } else {
      plot(v2$stmFit, type = "summary")
    }
    
  })
  
  
  output$topic_hist <- renderPlot({
    
    if(input$out_mod==1){
      plot(v1$stmFit, type = "hist")
    } else {
      plot(v2$stmFit, type = "hist")
    }
  })
  
  
  #ndtv:::renderNdtvAnimationWidget
  
  # output$topic_clus_ani <- ndtv:::renderNdtvAnimationWidget({
  #   
  #   if(input$out_mod==1){
  #   
  #    compute.animation(x1$net, animation.mode = "kamadakawai",
  #                     slice.par=list(start=0, end=nrow(x1$link)+1, interval=round((nrow(x1$link)/30), 0), 
  #                                    aggregate.dur=1, rule='any'))
  #     
  #    obj <- render.d3movie(x1$net, usearrows = F, 
  #                    displaylabels = F, label=x1$net %v% "key_word",
  #                    bg="#ffffff", vertex.border="#333333",
  #                    vertex.cex = sna::degree(x1$net)/2,  
  #                    vertex.col = x1$net %v% "col",
  #                    edge.lwd = (x1$net %e% "weights")/3, 
  #                    edge.col = '#55555599',
  #                    vertex.tooltip = paste("<b>", (x1$net %v% "key_word") , "</b><br>",
  #                                           "<b>", (x1$net %v% "type"), "</b>" ),
  #                    edge.tooltip = paste("<b>Topic:", (x1$net %e% "topic_no"), "</b><br>", 
  #                                         "<b>Weight:", (x1$net %e% "weights" ),"</b>"),
  #                    output.mode='htmlWidget',
  #                    #launchBrowser=T, filename="Media-Network-Dynamic.html",
  #                    render.par=list(tween.frames = 30, show.time = F),
  #                    plot.par=list(mar=c(0,0,0,0)))
  #    
  #    obj
  #    
  #   } else {
  #     
  #     compute.animation(x2$net, animation.mode = "kamadakawai",
  #                       slice.par=list(start=0, end=nrow(x2$link)+1, interval=round((nrow(x2$link)/30), 0), 
  #                                      aggregate.dur=1, rule='any'))
  #     
  #     obj <- render.d3movie(x2$net, usearrows = F, 
  #                    displaylabels = F, label=x2$net %v% "key_word",
  #                    bg="#ffffff", vertex.border="#333333",
  #                    vertex.cex = sna::degree(x2$net)/2,  
  #                    vertex.col = x2$net %v% "col",
  #                    edge.lwd = (x2$net %e% "weights")/3, 
  #                    edge.col = '#55555599',
  #                    vertex.tooltip = paste("<b>", (x2$net %v% "key_word") , "</b><br>",
  #                                           "<b>", (x2$net %v% "type"), "</b>" ),
  #                    edge.tooltip = paste("<b>Topic:", (x2$net %e% "topic_no"), "</b><br>", 
  #                                         "<b>Weight:", (x2$net %e% "weights" ),"</b>"),
  #                    output.mode='htmlWidget',
  #                    #launchBrowser=T, filename="Media-Network-Dynamic.html",
  #                    render.par=list(tween.frames = 30, show.time = F),
  #                    plot.par=list(mar=c(0,0,0,0)))
  #     
  #     obj
  #     
  #   }
  #   
  # }) 
  
  
  output$topic_clus <- renderDendroNetwork({
    
    num_topic <- input$num.topics
    coloa <- brewer.pal(n = 8, name = "Dark2")
    colob <- brewer.pal(n = 12, name = "Set3")
    col_pal <- c(coloa, colob)
    col_pal <- col_pal[1:num_topic]
    
    if(input$out_mod==1){
      
      #plot(v1$clusters, cex=0.8, hang = -3)
      #plot(color_branches(as.dendrogram(v1$clusters,hang = -3),k=input$num.topics), cex=0.7)
      #rect.hclust(v1$clusters, k=input$num.topics)
      dendroNetwork(v1$clusters, textColour = col_pal[cutree(v1$clusters, num_topic)], treeOrientation = "vertical",
                    zoom = T, nodeStroke = "blue", fontSize = 15)
    } else {
      #plot(v2$clusters, cex=0.8, hang = -3)
      #plot(color_branches(as.dendrogram(v2$clusters,hang = -3),k=input$num.topics), cex=0.7)
      #rect.hclust(v2$clusters, k=input$num.topics)
      dendroNetwork(v2$clusters, textColour = col_pal[cutree(v2$clusters, num_topic)], treeOrientation = "vertical",
                    zoom = T, nodeStroke = "blue", fontSize = 15)
    }
    
  })
  
  
  
  output$topic.network <- renderVisNetwork({
    
    if(input$out_mod==1){
      visNetwork(x1$nodes, x1$edges) %>%
        visExport() %>%
        visNodes(labelHighlightBold = T) %>%
        visOptions(highlightNearest = T, selectedBy = "community", nodesIdSelection = T) %>%
        visInteraction(navigationButtons = T)
    } else {
      visNetwork(x2$nodes, x2$edges) %>%
        visExport() %>%
        visNodes(labelHighlightBold = T) %>%
        visOptions(highlightNearest = T, selectedBy = "community", nodesIdSelection = T) %>%
        visInteraction(navigationButtons = T)
    }
    
  })
  
  
  
  terms <- reactive({
    
    if(input$out_mod==1){
      freq <- as.data.frame(v1$probterms)
      temp <- as.integer(input$topic.network_selected)
      data.frame(word = rownames(v1$probterms), freq = freq[,temp])
    } else {
      freq <- as.data.frame(v2$probterms)
      temp <- as.integer(input$topic.network_selected)
      data.frame(word = rownames(v2$probterms), freq = freq[,temp])
    }
    
  })
  
  docs <- reactive({
    
    if(input$out_mod==1){
      freq <- as.data.frame(v1$probdocs)
      temp <- as.integer(input$topic.network_selected)
      data.frame(docname = rownames(v1$probdocs), freq = freq[,temp], rowNum = v1$out$meta$rowNum)
    } else {
      freq <- as.data.frame(v2$probdocs)
      temp <- as.integer(input$topic.network_selected)
      data.frame(docname = rownames(v2$probdocs), freq = freq[,temp], rowNum = v2$out$meta$rowNum)
    }
    
  })
  
  
  
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$topic.wordcloud <- renderPlot({
    w <- terms()
    gray <- head(rev(brewer.pal(n = 8, name = "Greys")), 4)
    blue <- rev(head(rev(brewer.pal(n = 9, name = "Blues")), 4))
    pal <- c(gray, blue)
    
    try <- try(wordcloud_rep(w$word,
                             exp(as.double(w$freq)),
                             scale=c(3,1),
                             max.words=75,
                             random.order = F,
                             random.color = F,
                             rot.per=0.1,
                             colors=pal))
    
    if("try-error" %in% class(try)){
      showModal(modalDialog("Choose a Topic from the Network", easyClose = T, footer = modalButton("OK")))
    } 
    else {wordcloud_rep(w$word,
                        exp(as.double(w$freq)),
                        scale=c(3,1),
                        max.words=75,
                        random.order = F,
                        random.color = F,
                        rot.per=0.1,
                        colors=pal)
      } 
  })
  
  
  
  # Sentiment
  
  emotions <- reactive({
    df <- terms()
    if(input$out_mod==1){
      df2 <- v1$sentiment %>% left_join(df, by = "word")
      df2 <-  na.omit(df2)
    } else {
      df2 <- v2$sentiment %>% left_join(df, by = "word")
      df2 <-  na.omit(df2)
    }
    df2$cnt <- exp(as.double(df2$freq))
    
    temp2 <- df2 %>%
      filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA")) %>% 
      group_by(sentiment) %>%
      summarize( freq2 = sum(cnt)) %>%
      mutate(percent=round(freq2/sum(freq2)*100,1)) %>% 
      ungroup()
    
    na.omit(temp2) 
  })
  

  output$topic.sentiment <- renderPlotly({
    em <- emotions()
    em <- em[order(em$percent),]
    try <- try(plot_ly(em, y = ~sentiment, x = ~percent, type= 'bar', orientation = 'h', marker = list(color = "royalblue")) %>%
                 layout(title = "",
                        xaxis = list(title = "Percentage", showgrid = FALSE),
                        yaxis = list(title = "", showgrid = FALSE, categoryarray = ~sentiment, categoryorder = "array"))
               )
    
    if("try-error" %in% class(try)){
      #showModal(modalDialog("Choose a topic from the Network", easyClose = T, footer = modalButton("OK")))
    }
    else {plot_ly(em, y = ~sentiment, x = ~percent, type= 'bar', orientation = 'h', marker = list(color = "royalblue")) %>%
          layout(title = "",
               xaxis = list(title = "Percentage", showgrid = FALSE),
               yaxis = list(title = "", showgrid = FALSE, categoryarray = ~sentiment, categoryorder = "array"))
    }
    
  })
  
  
  
  # expert table
  Docs <- reactive({
    temp_data <- storedData$trim_data
    temp_data2 <- storedData$data
    
    if(input$out_mod==1){
      
      if(length(input$model1)==0){
        temp_data <- temp_data
        temp_data2 <- temp_data2
      } else {
        temp_data <- temp_data[temp_data[[input$tpLabels]] %in% input$model1, ]
        temp_data2 <- temp_data2[temp_data2[[input$tpLabels]] %in% input$model1, ]
      }
      
    } else {
      
      if(length(input$model2)==0){
        temp_data <- temp_data
        temp_data2 <- temp_data2
      } else {
        temp_data <- temp_data[temp_data[[input$tpLabels]] %in% input$model2, ]
        temp_data2 <- temp_data2[temp_data2[[input$tpLabels]] %in% input$model2, ]
      }
      
    }
    
    d <- docs()
    ldaProbs <- data.frame(rowNum = d$rowNum, Prob = as.double(d$freq), stringsAsFactors = F)
    nnn <- sentiment_by(get_sentences(temp_data [[input$tpDocs]]))
    doc_senti <- data.frame(rowNum = nnn$element_id, Sentiment_Score = round((nnn$ave_sentiment), 2))
    ldaProbs <- merge(ldaProbs, doc_senti, by = "rowNum")
    ldaProbs <- merge(ldaProbs, temp_data2, by = "rowNum")
    if(input$tpLabels!="(None)") {
      ldaProbs[order(ldaProbs$Prob, decreasing = T), c("rowNum", input$tpLabels, "Prob", "Sentiment_Score", input$tpDocs)]
    } else {
      ldaProbs[order(ldaProbs$Prob, decreasing = T), c("rowNum","Prob", "Sentiment_Score", input$tpDocs)]
    }
  })
  
  
  
  #Representative Document
  
  output$doc.table <- DT::renderDT({
    temp <- Docs()
    if(input$tpLabels!="(None)") {
      colnames(temp) <- c("Row ID","Category",paste0("Topic-",as.integer(input$topic.network_selected)," Proportion"),"Sentiment Score", "Text")
      temp$Text <- as.character(temp$Text)
      temp$Category <- as.character(temp$Category)
    } else {
      colnames(temp) <- c("Row ID",paste0("Topic-",as.integer(input$topic.network_selected)," Proportion"),"Sentiment Score","Text")
      temp$Text <- as.character(temp$Text)  
    }
    datatable(temp, rownames= FALSE)
  } %>% formatPercentage(paste0("Topic-",as.integer(input$topic.network_selected)," Proportion"), 1), 
  
  options = list(
    autoWidth = TRUE,
    #columnDefs = list(list(width = '50%', targets = list(3))),
    #pageLength = 10,
    dom = 'tip'
  ),
  
  rownames= FALSE) 
  
  
  
  
  # Download table
  Raw_Docs <- reactive({
    temp_data <- storedData$trim_data
    
    if(input$out_mod==1){
      
      if(length(input$model1)==0){
        temp_data <- temp_data
      } else {
        temp_data <- temp_data[temp_data[[input$tpLabels]] %in% input$model1, ]
      }
      d <- as.data.frame(v1$probdocs)
      topic_list <- as.data.frame(v1$topicnames_doc)
    } else {
      
      if(length(input$model2)==0){
        temp_data <- temp_data
      } else {
        temp_data <- temp_data[temp_data[[input$tpLabels]] %in% input$model2, ]
      }
      d <- as.data.frame(v2$probdocs)
      topic_list <- as.data.frame(v2$topicnames_doc)
    }
    
    DT <- as.data.table(d)
    ldaProbs_raw <- DT[, id := colnames(.SD)[max.col(.SD, ties.method="first")]]
    ldaProbs_raw$rowNum <- as.integer(row.names(ldaProbs_raw))
    ldaProbs_raw <- as.data.frame(ldaProbs_raw)
    ldaProbs_raw <- ldaProbs_raw %>% left_join(topic_list, by = "id")
    nnn <- sentiment_by(get_sentences(temp_data [[input$tpDocs]]))
    doc_senti <- data.frame(rowNum = nnn$element_id, Sentiment_Score = round((nnn$ave_sentiment), 2))
    ldaProbs_raw <- merge(ldaProbs_raw, doc_senti, by = "rowNum")
    ldaProbs_raw <- merge(ldaProbs_raw, temp_data, by = "rowNum")
    target <- sort(colnames(ldaProbs_raw[1,!(colnames(ldaProbs_raw) %in% c("rowNum", input$tpLabels, input$tpDocs, "topic","id","Sentiment_Score"))]))
    
    if(input$tpLabels!="(None)") {
      ldaProbs_raw[, c("rowNum", input$tpLabels, "topic", target, "Sentiment_Score", input$tpDocs)]
    } else {
      ldaProbs_raw[, c("rowNum", "topic", target, "Sentiment_Score", input$tpDocs)]
    }
  })
  
  
  
  Raw_Topics <- reactive({
    
    if(input$out_mod==1){
      as.data.frame(v1$topicnames_doc)
    } else {
      as.data.frame(v2$topicnames_doc)
    } 
  })
  
  
  
  output$sel_model <- reactive({
    
    if(input$out_mod==1) {
      paste0("Topic-",as.integer(input$topic.network_selected),": ",
             v1$topicnamesa[as.integer(input$topic.network_selected)],".")
      
    } else {
      paste0("Topic-",as.integer(input$topic.network_selected),": ",
             v2$topicnamesa[as.integer(input$topic.network_selected)],".")

    }
    
  })
  
  
  
  neighbour <- reactive({
    
    data_topic <- storedData$trim_data
    comment <- input$tpDocs
    
    stp <- unlist(strsplit(input$stopwords,","))
    stp <- trimws(stp)
    
    if(input$out_mod==1) {
      
      # d <- as.data.frame(v1$probdocs)
      # DT <- as.data.table(d)
      # data_topic_t <- DT[, max_id := colnames(.SD)[max.col(.SD, ties.method="first")]]
      # data_topic_t$rowNum <- as.integer(row.names(data_topic_t))
      # data_topic_t <- as.data.frame(data_topic_t)
      # data_topic <- merge(data_topic_t, data_t, by = "rowNum")
      # data_topic <- data_topic[which(data_topic$max_id == paste0("X",as.integer(input$topic.network_selected))),]
      
      context_sent <- v1$topicnamesa[as.integer(input$topic.network_selected)]
      neigh.network(data_topic, comment, context_sent, stp)
      
    } else {
      
      # d <- as.data.frame(v2$probdocs)
      # DT <- as.data.table(d)
      # data_topic_t <- DT[, max_id := colnames(.SD)[max.col(.SD, ties.method="first")]]
      # data_topic_t$rowNum <- as.integer(row.names(data_topic_t))
      # data_topic_t <- as.data.frame(data_topic_t)
      # data_topic <- merge(data_topic_t, data_t, by = "rowNum")
      # data_topic <- data_topic[which(data_topic$max_id == paste0("X",as.integer(input$topic.network_selected))),]
      
      context_sent <- v2$topicnamesa[as.integer(input$topic.network_selected)]
      neigh.network(data_topic, comment, context_sent, stp)
      
    }
    
  })
  
  
  kk <- reactiveValues()
  
  output$topic.network2 <- renderForceNetwork({

    kk <- neighbour()
    forceNetwork(Links = kk[[1]], Nodes = kk[[2]], Source="from", Target="to",
                 Value = "weight", NodeID = "topic_word2", Group = "type",
                 linkColour = "#afafaf", fontSize=15, zoom=T, legend=T,
                 opacity = 1, charge=-100, colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                 bounded = T, opacityNoHover = 0.5, Nodesize = kk[[3]])
  })
  

  
  
  
  
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      txt <- ifelse(input$out_mod==1, "Model-1 Results-", "Model-2 Results-")
      paste(txt, Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(Raw_Docs(), file, row.names = F)
    }
  )
  
  
  output$downloadTopic <- downloadHandler(
    filename = function() {
      txt <- ifelse(input$out_mod==1, "Model-1 Topics-", "Model-2 Topics-")
      paste(txt, Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(Raw_Topics(), file, row.names = F)
    }
  )
  



############## Analytics version #########################

# ************************** Tab 1 : Data Upload *****************************************

data_analytics <- reactive({
  file11<- input$File
  if(is.null(file11)){return()}
  
  readtext(file11$datapath)
  
})

data.df <- reactive({
  as.data.frame(data_analytics())
  
})

# 
observe({
  namedata<-data.df()
  updateSelectInput(session, 'Text', choices = names(namedata))
  updateSelectInput(session, 'Label', choices = names(namedata))
  #   # updateSelectInput(session, 'Feature', choices = names(data))
  #   # updateSelectInput(session, inputId = "target", choices = names(data),selected = names(data))
  #   # updateSelectInput(session, 'Key', choices = names(data))
  #   # updateSelectInput(session, 'variable', choices = names(data))
  #   # updateSelectInput(session, 'variable1', choices = names(data))
  #   #updateSelectInput(session, "combobox", choices = colnames(data))
  #   
}) # end observe


#file snapshot

#******************Display first few records of file***********************************

output$filedf1 <- renderTable({
  if(is.null(data.df())){return()}
  head(data.df(),25)
})

#******************Display summary of file ***********************************

output$filedf2 <- renderTable({
  if(is.null(data_analytics())){return()}
  summary(data.df())
})

#**************Plot Text length**************************************

output$LengthPlot <- renderPlot({
  
  Inp_Data_df<- data.df()
  Sentiment <- input$Label
  L<- as.data.frame(nchar(Inp_Data_df$text))
  L$Sentiment <- as.factor((Inp_Data_df[,input$Label]))
  names(L)[names(L)=="nchar(Inp_Data_df$text)"] <- "len"
  
  library(ggplot2)
  
  ggplot(L, aes(x = len, fill = Sentiment)) +
    
    theme_bw() +
    
    geom_histogram(binwidth = 5) +
    
    labs(y = "Text Count", x = "Length of Text",
         
         title = "Distribution of Review Lengths by Sentiment")
  
})


#Create Reactive Token object

Tok_data <- reactive({
  
  Inp_Data_df<- data.df()
  fulltext<-quanteda::corpus(Inp_Data_df,text_field = "text")
  Tok_data <- quanteda::tokens(fulltext,
                     remove_numbers = TRUE,
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_separators = TRUE,
                     remove_twitter = TRUE,
                     include_docvars = TRUE
  )
  
  Tok_data <- tokens_remove(Tok_data,stopwords("english"))
  tokens_wordstem(Tok_data)
  
  
})

#Create Reactive DTM object


dtm <- reactive({
  
  dfm(Tok_data(), tolower = TRUE ,stem = TRUE, 
      remove_punct=TRUE, remove = stopwords("english"))
  
})

dtm_subset <- reactive({
  
  
  dtm_temp <- dfm(Tok_data(), tolower = TRUE ,stem = TRUE, 
                  remove_punct=TRUE, remove = stopwords("english"))
  
  min.freq<- as.numeric(input$MinFreq)
  min.docfreq<- as.numeric(input$MinDocFreq)
  
  dfm_trim(dtm_temp,min_termfreq = min.freq ,min_docfreq = min.docfreq)
})

dtm_subset_2 <- reactive({
  
  
  dtm_temp2 <- dfm(Tok_data(), tolower = TRUE ,stem = TRUE, 
                   remove_punct=TRUE, remove = stopwords("english"))
  
  min.freq2<- as.numeric(input$MinFreq2)
  min.docfreq2<- as.numeric(input$MinDocFreq2)
  
  dfm_trim(dtm_temp2,min_termfreq = min.freq2 ,min_docfreq = min.docfreq2) 
})

dtm_subset_3 <- reactive({
  
  
  dtm_temp3 <- dfm(Tok_data(), tolower = TRUE ,stem = TRUE, 
                   remove_punct=TRUE, remove = stopwords("english"))
  
  min.freq3<- as.numeric(input$MinFreq3)
  min.docfreq3<- as.numeric(input$MinDocFreq3)
  
  dfm_trim(dtm_temp3,min_termfreq = min.freq3,min_docfreq = min.docfreq3) 
})



# ************************** Tab 2 : Organize data into DTM ******************************
#Organize Data
#Output - Word Frequency for top 50 words
#Output - Document - Token distribution plot
#Output - Tokenization snapshot

dtm.r <- reactive({nrow(dtm_subset())})
dtm.c <- reactive({ncol(dtm_subset())})
dtm.r1<- reactive({nrow(dtm())})
dtm.c1<- reactive({ncol(dtm())})
output$tokenizesummary <- renderText({
  
  paste("Text Data is Tokenized, Document Term matrix created with ", dtm.r1(), "rows" ,dtm.c1(), "col")
})

output$tokenizesummary1 <- renderText({
  
  paste("Text Data is Tokenized, Subsetted Document Term matrix created with ", dtm.r(), "rows" ,dtm.c(), "col")
})


output$tokenize<- renderText({
  
  
  Tok_data()[1:5]
  
})

#*****************Plot Tokens per Document*************************

output$tokperdocPlot <- renderPlot({
  
  tokens_in_doc <- as.data.frame(rowSums(dtm()))
  names(tokens_in_doc)[names(tokens_in_doc)=="rowSums(dtm())"] <- "f"
  
  library(ggplot2)
  
  ggplot(tokens_in_doc, aes(x = f)) +
    
    theme_bw() +
    
    geom_histogram(binwidth = 1) +
    
    labs(y = "# Doc", x = "Token Count per Doc",
         
         title = "Distribution of Tokens per Document")
  
  
})

#********************Plot word frequency************************************

output$tokfreq <- renderPlot({
  
  
  tokfreq <- textstat_frequency(dtm_subset(), n = input$topnwords)
  
  par(mar=c(7,8,7,7)) 
  
  barplot(tokfreq$frequency,names.arg=tokfreq$feature, main = "Most Frequent Words",
          xlab="Words", ylab="Frequency",col="blue",las=2)
  
})

#*******************Plot document frequency***********************************************

output$docfreq <- renderPlot({
  
  tokfreq <- textstat_frequency(dtm_subset(), n = input$topnwords)
  
  par(mar=c(7,8,7,7)) 
  
  barplot(tokfreq$docfreq,names.arg=tokfreq$feature, main = "Words Occurences in Documents",
          xlab="Words", ylab="Number Of Documents",col="blue",las=2)
  
})

# ************************** Tab 3 : Bag of Words Analysis *******************************

#****************************Plot Word Frequeny****************************


output$tokfreq2 <- renderPlot({
  
  
  tokfreq_2 <- textstat_frequency(dtm_subset_2(), n = input$topnwords2)
  
  par(mar=c(7,8,7,7)) 
  
  barplot(tokfreq_2$frequency,names.arg=tokfreq_2$feature, main = "Most Frequent Words",
          xlab="Tokens", ylab="frequency",col="blue",las=2)
  
})


#****************************Plot WordCloud****************************

output$wordcloud <- renderPlot({
  
  
  textplot_wordcloud(dtm_subset_2(),random_order = FALSE,rotation=0.20, random_color = FALSE,
                     min_count = input$MinFreq2 , max_words = input$topnwords2)
  
})


#****************************Plot Comparative WordCloud****************************
has.yes <- reactive({ as.numeric(input$HasLabel)})

output$compwordcloud <- renderPlot({
  if(has.yes() == 1){
    
    Inp_Data_df<- data.df()
    fulltext<-quanteda::corpus(Inp_Data_df,text_field = "text")
    Tok_data <- quanteda::tokens(fulltext,
                       remove_numbers = TRUE,
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_separators = TRUE,
                       remove_twitter = TRUE,
                       include_docvars = TRUE
                       
    )
    
    Tok_data <- quanteda::tokens_remove(Tok_data,stopwords("english"))
    Tok_data <- tokens_wordstem(Tok_data)
    
    dtm_comp <- dfm(fulltext, tolower = TRUE ,stem = TRUE, 
                    remove_punct=TRUE, remove = stopwords("english"), groups = input$Label)
    
    dtm_comp_subset <- reactive({
      min.docfreq4<- as.numeric(input$MinDocFreq2)
      
      dfm_trim(dtm_comp,min_docfreq = min.docfreq4)
    })
    
    textplot_wordcloud(dtm_comp_subset(), comparison = TRUE, 
                       random_order = FALSE,rotation=0.20, random_color = FALSE,
                       min_count = input$MinFreq2, max_words = input$topnwords2)
  }
  else
  {
    dtm_subset_3 <- reactive({
      min.docfreq5<- as.numeric(input$MinDocFreq2)
      
      dfm_trim(dtm(),min_docfreq = min.docfreq5)
    })
    
    textplot_wordcloud(dtm_subset_3(),random_order = FALSE,rotation=0.20, random_color = FALSE,
                       min_count = input$MinFreq2 , max_words = input$topnwords2)
  }
  
})


#****************************Key Words Plot****************************
has.yes <- reactive({ as.numeric(input$HasLabel)})

output$keyness <- renderPlot({
  if(has.yes() == 1){
    
    key <- textstat_keyness(dtm(),docvars(dtm(),input$Label)==1)
    
    attr(key, input$Text) <- c('1', '0')
    
    textplot_keyness(key,n=input$topnwords2,min_count = input$MinFreq2)
  }
  else
  {
    NULL
  }
})  

# ************************** Tab 4 : Ngrams Analysis *******************************


#****************************Plot ngram freq****************************


output$ngramsfreq <- renderPlot({
  
  whichn <- as.numeric(input$whichgram)
  
  ngram1<-tokens_ngrams(Tok_data(),n = whichn)
  
  dtm_ngram <- dfm(ngram1)
  
  dtm_subset_4 <- reactive({
    min.freq6<- as.numeric(input$MinFreq3)
    
    dfm_trim(dtm_ngram,min_termfreq = min.freq6)
  })    
  
  tokfreq_ngram <- textstat_frequency(dtm_subset_4(), n = input$topnwords3)
  
  par(mar=c(12,8,7,7)) 
  
  barplot(tokfreq_ngram$frequency,names.arg=tokfreq_ngram$feature, main = "Most Frequent N-grams",
          xlab="Tokens", ylab="frequency",col="blue",las=2)
  
  
})

#****************************Plot WordCloud ngrams ****************************

output$wordcloudngrams <- renderPlot({
  
  whichn <- as.numeric(input$whichgram)
  
  ngram1<-tokens_ngrams(Tok_data(),n = whichn)
  
  dtm_ngram <- dfm(ngram1)
  
  
  textplot_wordcloud(dtm_ngram,random_order = FALSE,rotation=0.20, random_color = FALSE,
                     min_count = input$MinFreq3 , max_words = input$topnwords3)
  
})

#****************************Key Words Plot****************************

has.yes <- reactive({ as.numeric(input$HasLabel)})




output$keynessngrams <- renderPlot({
  if(has.yes() == 1){
    
    whichn <- as.numeric(input$whichgram)
    
    ngram1<-tokens_ngrams(Tok_data(),n = whichn)
    
    dtm_ngram <- dfm(ngram1)
    
    
    key_ngram <- textstat_keyness(dtm_ngram,docvars(dtm_ngram,input$Label)==1)
    
    attr(key_ngram, input$Text) <- c('1', '0')
    
    textplot_keyness(key_ngram,n=input$topnwords3,min_count = input$MinFreq3)
  }
  else
  {
    NULL
  }
})

# ************************** Tab 5 : Context Word Analysis ********************************

#**************Padded Tokenisation **************

Tok_data_pad <- reactive({
  
  Inp_Data_df<- data.df()
  fulltext<-quanteda::corpus(Inp_Data_df,text_field = "text")
  Tok_data_p <- quanteda::tokens(fulltext,
                       remove_numbers = TRUE,
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_separators = TRUE,
                       remove_twitter = TRUE,
                       include_docvars = TRUE,
                       ngrams= 1,
                       padding=TRUE
  )
  
  Tok_data_p <- tokens_remove(Tok_data_p,stopwords("english"))
  tokens_wordstem(Tok_data_p)
  
})

#***********Contextual Tokenisation from padded tokens*********************

Tok_data_context <- reactive({
  
  context_words <- input$Cword
  w<- as.numeric(input$window)
  
  tokens_select(Tok_data_pad(),context_words, window = w, selection = "keep")
  
  
})

#****************************Plot Word Frequeny****************************


output$tokfreqcword <- renderPlot({
  
  dtm_cword <- dfm(Tok_data_context())
  
  dtm_subset_5 <- reactive({
    min.freq7<- as.numeric(input$MinFreq4)
    
    dfm_trim(dtm_cword,min_termfreq = min.freq7)
  })    
  
  tokfreq_cword <- textstat_frequency(dtm_subset_5(), n = input$topnwords4)
  barplot(tokfreq_cword$frequency,names.arg=tokfreq_cword$feature, main = "Most Frequent Words",
          xlab="Tokens", ylab="frequency",col="blue",las=2)
  
})



#****************************Plot word cloud ****************************


output$wrdcldcwords <- renderPlot({
  
  
  dtm_cword <- dfm(Tok_data_context())
  
  textplot_wordcloud(dtm_cword,random_order = FALSE,rotation=0.20, random_color = FALSE,
                     min_count = input$MinFreq4 , max_words = input$topnwords4)
  
  
})

#****************************Plot Custom Bigrams****************************


output$bigramscword <- renderPlot({
  
  ngram2<-tokens_ngrams(Tok_data_context(),n = 2)
  
  dtm_Cust_bigram <- dfm(ngram2)
  
  dtm_subset_6 <- reactive({
    min.freq8<- as.numeric(input$MinFreq4)
    
    dfm_trim(dtm_Cust_bigram,min_termfreq = min.freq8)
  }) 
  
  tokfreq_cword_bigram <- textstat_frequency(dtm_subset_6(), n = input$topnwords4)
  
  par(mar=c(7,8,7,7)) 
  
  barplot(tokfreq_cword_bigram$frequency,names.arg=tokfreq_cword_bigram$feature, main = "Most Frequent bigrams",
          xlab="Tokens", ylab="frequency",col="blue",las=2)
  
  
})

#*******************Naive Bayes Classifier***********************************

nb.yes <- reactive({ as.numeric(input$nb.ind)})


ntrain <- reactive({
  if(nb.yes() == 1){
    
    input$trfrac*ndoc(dtm()) } 
})

id_train <- reactive({
  
  #docvars(dtm(), "id_numeric") <- 1:ndoc(dtm())
  
  if(nb.yes() == 1){
    set.seed(999)
    
    sample(1:ndoc(dtm()), ntrain(), replace = FALSE) }
  
})

dtm_nb <- reactive({
  
  if(nb.yes() == 1){
    
    dtm1<- dtm()
    
    docvars(dtm1, "id_numeric") <- 1:ndoc(dtm1)
    
    dtm1 }
  
})




training_dtm <- reactive({
  
  if(nb.yes() == 1){
    # training set
    
    dfm_subset(dtm_nb(), id_numeric %in% id_train()) }
  
  
})

test_dtm <- reactive({
  
  if(nb.yes() == 1){
    
    # test set
    
    dfm_subset(dtm_nb(), !id_numeric %in% id_train())  }
  
  
})


nvbys<- reactive({
  
  if(nb.yes() == 1){
    
    #Train Naive Bayes
    textmodel_nb(training_dtm(), docvars(training_dtm(), input$Label)) }
  
})



FinalData <- reactive({
  
  if(nb.yes() == 1){
    
    #Predict labels on Train
    
    pred_train<- as.data.frame(predict(nvbys(),training_dtm()))
    names(pred_train)=c('nb.predicted')
    
    #Predict labels on Test
    
    pred_test<- as.data.frame(predict(nvbys(),test_dtm()))
    names(pred_test)=c('nb.predicted')
    # 
    # 
    # #Confusion matrix Train
    # 
    actual_class <- docvars(training_dtm() , input$Label)
    predicted_class <- pred_train$nb.predicted
    class_table <- as.data.frame(table(actual_class, predicted_class))
    Accuracy  <-(class_table[1,3]+class_table[4,3])/sum(class_table$Freq)
    Recall    <-class_table[4,3]/(class_table[4,3]+class_table[2,3])
    Precision <-class_table[4,3]/(class_table[4,3]+class_table[3,3])
    Sensitivity <- class_table[4,3]/(class_table[4,3]+class_table[2,3])
    Specificity <- class_table[1,3]/(class_table[3,3]+class_table[1,3])
    train_CM<- rbind(Accuracy,Recall,Precision,Sensitivity,Specificity)
    
    # 
    
    #Confusion matrix Test
    
    actual_class_test <- docvars(test_dtm() , input$Label)
    predicted_class_test <- pred_test$nb.predicted
    class_table_test <- as.data.frame(table(actual_class_test, predicted_class_test))
    Accuracy  <-(class_table_test[1,3]+class_table_test[4,3])/sum(class_table_test$Freq)
    Recall    <-class_table_test[4,3]/(class_table_test[4,3]+class_table_test[2,3])
    Precision <-class_table_test[4,3]/(class_table_test[4,3]+class_table_test[3,3])
    Sensitivity <- class_table_test[4,3]/(class_table_test[4,3]+class_table_test[2,3])
    Specificity <- class_table_test[1,3]/(class_table_test[3,3]+class_table_test[1,3])
    test_CM<- rbind(Accuracy,Recall,Precision,Sensitivity,Specificity)
    
    
    # 
    # sensitivity<-as.data.frame(cbind(train_Sensitivity,test_Sensitivity))
    # rownames(sensitivity)<-"Sensitivity"
    # colnames(sensitivity)<-c("Train","Test")
    # 
    # Specificity<-as.data.frame(cbind(train_Specificity,test_Specificity))
    # rownames(Specificity)<-"Specificity"
    # colnames(Specificity)<-c("Train","Test")
    # 
    # Precision<-as.data.frame(cbind(train_Precision,test_Precision))
    # rownames(Precision)<-"Precision"
    # colnames(Precision)<-c("Train","Test")
    # 
    # Recall<-as.data.frame(cbind(train_Recall,test_Recall))
    # rownames(Recall)<-"Recall"
    # colnames(Recall)<-c("Train","Test")
    # 
    # Accuracy<-as.data.frame(cbind(train_Accuracy,test_Accuracy))
    # rownames(Accuracy)<-"Accuracy"
    # colnames(Accuracy)<-c("Train","Test")
    # 
    KPI<-c("Accuracy","Sensitivity","Specificity","Precision","Recall")
    # 
    
    Data1<- as.data.frame(cbind(train_CM,test_CM))
    a1<-  t(as.matrix(Data1[-3]))
    names(Data1)= c('Train','Test')
    cbind(KPI,Data1)
    
    
  }
  
})


output$nbsummary <- renderTable({
  
  if(nb.yes() == 1){
    
    head(FinalData())   }
  
})

output$nbplot<- renderPlot({
  
  if(nb.yes() == 1){
    
    barplot(FinalData()$a1,main='Performance of NB classification',ylab='', xlab='KPI',beside = TRUE, 
            col=c("red","blue"))
    
    legend('bottomright',fill=colours,legend=c('Train','Test'))
  }
  
})


post.prob <- reactive({
  pp<-as.data.frame(nvbys()$PcGw)
  row.names(pp) <- c('Class1' , 'Class0')
  pp.t<- as.data.frame(t(pp))
  pp.t[order(-pp.t$Class1),]
})

output$postprob <- renderTable({post.prob()})





#*************************Sentiment Analysis****************************

#******************* Unigrams ***********************************

sent.yes <- reactive({
  as.numeric(input$sent.ind)  
  
})



lsd_toks <- reactive({
  
  if(sent.yes()==1){
    
    tokens_lookup(Tok_data(), data_dictionary_LSD2015[1:2]) }
  
})

lsd_dtm <- reactive({
  
  if(sent.yes()==1){
    
    as.data.frame(dfm(lsd_toks()))   }
  
})

NetSent <- reactive({
  
  if(sent.yes()==1){
    
    lsd_dtm.df <- lsd_dtm()
    lsd_dtm.df$SentScore <- lsd_dtm.df$positive - lsd_dtm.df$negative
    lsd_dtm.df    }
  
})

output$sent <- renderPlot({
  
  if(sent.yes()==1){
    
    hist(NetSent()$SentScore, breaks = 10, col = "blue", border = "white", 
         main = "Sentiment SCore Distribution" , xlab = "Sentiment Score", ylab = "Number of Documents")  }
})

#******************* Bigrams ***************************

lsd_toks_bigrams <- reactive ({
  
  if(sent.yes()==1){
    
    Inp_Data_df<- data.df()
    fulltext_sent <-quanteda::corpus(Inp_Data_df,text_field = "text")
    sent_tok_bigram <- quanteda::tokens(fulltext_sent,remove_punct = TRUE, ngrams=1:2)    
    tokens_lookup(sent_tok_bigram, data_dictionary_LSD2015[1:4]) }
})


lsd_dtm_bigrams <- reactive({
  
  if(sent.yes()==1){
    
    as.data.frame(dfm(lsd_toks_bigrams()))   }
})

NetSentBigrams <- reactive({
  
  if(sent.yes()==1){
    
    lsd_dtm_bigrams.df <- lsd_dtm_bigrams()
    lsd_dtm_bigrams.df$SentScore <- 
      
      lsd_dtm_bigrams.df$positive + lsd_dtm_bigrams.df$neg_negative 
    - lsd_dtm_bigrams.df$negative - lsd_dtm_bigrams.df$neg_positive
    
    lsd_dtm_bigrams.df    }
  
})

output$sentbigrams <- renderPlot({
  
  if(sent.yes()==1){
    
    hist(NetSentBigrams()$SentScore, breaks = 10, col = "blue", border = "white", 
         main = "Sentiment SCore Distribution" , xlab = "Sentiment Score", ylab = "Number of Documents")  }
})


#****************** Qualitative Sentiment **************************#

library(syuzhet)



nrc_sent <- reactive ({
  
  if(sent.yes()==1){
    
    s<-get_nrc_sentiment(data.df()$text)
    s[,1:8]    }
})

nrc_sent_summ <- reactive({
  
  if(sent.yes()==1){
    
    td1<-data.frame(t(nrc_sent()))
    td2<-data.frame(rowSums(td1[2:nrow(data.df())]))
    colnames(td2)<- "Frequency"
    td3<-as.data.frame(rownames(td1))
    colnames(td3)<- "Sentiment"
    cbind(td3,td2)  }
  
})

output$nrc<- renderTable({
  if(sent.yes()==1){
    
    nrc_sent_summ()  }
})

output$nrcplot <- renderPlot({
  
  if(sent.yes()==1){
    
    barplot(nrc_sent_summ()$Frequency,names.arg=nrc_sent_summ()$Sentiment, main = "Sentiment Frequency",
            xlab="Sentiment", ylab="Frequency",col="blue") }
})

#***************************** Latent Semantic Analysis **************************
library(lsa)

lsa.yes<- reactive({ as.numeric(input$lsa.ind)})

lsaspace <- reactive({
  
  if(lsa.yes() == 1){
    
    dtm.matrix<- as.matrix(dtm())
    tdm.matrix <- t(dtm.matrix)
    
    lsa(tdm.matrix, dims=dimcalc_share())   }
  
})

sv<- reactive({
  
  if(lsa.yes() == 1){
    
    lsat_m_sk=as.data.frame(lsaspace()$sk)
    colnames(lsat_m_sk) <- "Svalues"
    lsat_m_sk$CummSvalues <- cumsum(lsat_m_sk$Svalues)
    #lsat_m_sk$PropSvalues <- lsat_m_sk$CummSvalues/lsat_m_sk[nrow(lsat_m_sk),3]
    lsat_m_sk[1:200,]  }
  
})
output$lsa.tk<- renderPlot({
  
  if(lsa.yes() == 1){
    
    barplot(sv()$CummSvalues,names.arg=rownames(sv()), main = "Cummulative Singular Values",
            xlab="Index", ylab="Singular Values",col="blue")  }
})

n.sv <- reactive({as.numeric(input$svd)})

lsa.dk.reduced <- reactive({
  
  if(lsa.yes() == 1){
    
    lsat_m_dk=as.data.frame(lsaspace()$dk)
    lsat_m_dk[ ,1:n.sv()]   }
})

lsa.tk.reduced <- reactive({
  
  if(lsa.yes() == 1){
    
    lsat_m_tk=as.data.frame(lsaspace()$tk)
    lsat_m_tk[,1:n.sv()]   }
})

datafordnld <- reactive({
  if(lsa.yes() == 1){
    
    if(input$result.type == "lsatv")
      lsa.tk.reduced()
    else lsa.dk.reduced()   }
  
})

output$sample.lsadnld <- renderTable({
  if(lsa.yes() == 1){
    head(datafordnld())  }
})

output$Dnld.LSA <- downloadHandler(
  
  
  filename = function(){
    paste(input$result.type, "csv", sep=".")
  },
  
  content = function(file){
    write.table(datafordnld(), file, sep = ",", row.names = FALSE)
  }
  
  
)

#dim(lsat_m_dk)

#lsat_m_dk_reduced <- lsat_m_dk[,1:10]
#lsa_feat <- cbind(lsat_m_dk_reduced,Inp_Data_df$Sentiment)

#********************** Generate Features *******************************

# Document term Frequency

dtm.yes = reactive({as.numeric(input$do.dtm)})

feat.termfreq <- reactive({
  #Index_termfreq <- as.data.frame(dtm()@Dimnames$docs)
  as.data.frame(dtm())
  #cbind(Index_termfreq, Term_freq)
})

output$feat.termfreq<- renderTable({ head(feat.termfreq())})

# TF - IDF

dtm.tfidf <- reactive({dfm_tfidf(dtm())})

dtm.tfidf.df <- reactive({as.data.frame(dtm.tfidf())})

output$feat.tfidf <- renderTable({ head(dtm.tfidf.df())})

#  Sentiment score

feat.sentscore <- reactive({lsd_dtm()})
output$feat.sentscore <- renderTable({ head(feat.sentscore())})

Download.feat <- reactive({
  if(input$feat.type == "tf")
  {feat.termfreq()}
  
  else if(input$feat.type == "tfidf")
  {dtm.tfidf.df()}
  
  #if(input$feat.type == "sentsc")
  else
  {feat.sentscore()}
})

output$Dnld.feat <- downloadHandler(
  
  filename = function(){
    paste(input$feat.type, "csv", sep=".")
  },
  
  content = function(file){
    write.table(Download.feat(), file, sep = ",", row.names = FALSE)
  }
  
  
)



}
# Run the application 
shinyApp(ui = ui, server = server)
