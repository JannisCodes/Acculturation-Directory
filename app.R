
# app stuff
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(metathis)
#library(dashboardthemes)

# data manipulation stuff
library(dplyr)
library(stringr)
library(tidyr)

# display stuff
library(DT)


load("scales-data.RData")

scalesSelected <- dt.Scales.Included %>%
    dplyr::select(id, 
                  Scale, 
                  ShortReference,
                  Affect = AffectFinal,
                  Behavior = BehaviorFinal,
                  Cognition = CognitionFinal,
                  Desire = DesireFinal,
                  NItems,
                  nlifeDomain) %>%
    mutate(Scale = sub("\\(.*", "", Scale),
           details = paste0(as.character(icon("eye-open", lib = "glyphicon")), " #", id)) %>%
    mutate_at(vars(Affect, Behavior, Cognition, Desire), ~replace_na(., 0)) %>%
    mutate_at(vars(Affect, Behavior, Cognition, Desire),
              ~(ifelse(.==1, as.character(icon("ok", lib = "glyphicon")), ""))) %>%
    relocate(details, .after = id)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    title = "Acculturation Scales",
    dashboardHeader(title= span(tags$img(src = "https://raw.githubusercontent.com/JannisCodes/acculturation-review/main/assets/images/FaviconCreation/LogoV1_20x20.png?token=AJEZTKGKIMVPQ6SBJQ7JNDLBJDW2A", height = "18px"), "Acculturation Scales")
    # dashboardHeader(title= span(tags$img(src = "https://raw.githubusercontent.com/JannisCodes/acculturation-review/main/assets/images/FaviconCreation/LogoV2_20x20.png?token=AJEZTKA365MU2ITKIADDGUTBJDW52", height = "18px"), "Acculturation Scales")
    ),

    # Sidebar
    dashboardSidebar(
        sidebarMenu(id = "sidebarMenu",
                    menuItem("The Scales", tabName = "scales", icon = icon("tasks")),
                    menuItem("References", tabName = "references", icon = icon("book-reader")),
                    menuItem("About", tabName = "about", icon = icon("info"))),
        shinyjs::useShinyjs(),
        tags$footer(HTML("<strong>Copyright &copy; 2021</strong> 
                   <br>This work is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc/4.0/\" target=\"_blank\">Creative Commons Attribution-NonCommercial 4.0 International License</a>.
                   <br><a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc/4.0/\" target=\"_blank\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-nc/4.0/88x31.png\" /></a>
                   <br>Last updated:<br>"), 
                   updateDate,
                   id = "sideFooter",
                   align = "left",
                   style = "
                  position:absolute;
                  bottom:0;
                  width:100%;
                  padding: 10px;
                  "
        )
    ),

    dashboardBody(
        
        tags$head(
            tags$link(rel = "shortcut icon", href = "favicon.ico"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(HTML("
                            var openTab = function(tabName){
                              $('a', $('.sidebar')).each(function() {
                                if(this.getAttribute('data-value') == tabName) {
                                  this.click()
                                };
                              });
                            };
                            $('.sidebar-toggle').attr('id','menu');
                            var dimension = [0, 0];
                                $(document).on('shiny:connected', function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange('dimension', dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange('dimension', dimension);
                                });
                          "))
        ),
        meta() %>%
            meta_social(
                title = "Acculturation-Review: Scale Directory",
                description = "An interactive directoy of psychological acculturation measurement tools (including items).",
                url = "https://acculturation-review.shinyapps.io/scale-directory/",
                image = "https://raw.githubusercontent.com/JannisCodes/acculturation-scales/master/www/media.png?token=AJEZTKBM2DG2GPDBRGAY25DBI7KTK",
                image_alt = "Acculturation Scale Directory",
                twitter_creator = "@JannisWrites",
                twitter_card_type = "summary",
                twitter_site = "@JannisWrites"
            ),
        
        shinyjs::useShinyjs(),
        
        # ### changing theme
        # shinyDashboardThemes(
        #     theme = "poor_mans_flatly"
        # ),
        
        tabItems(
            tabItem(tabName = "scales",
                    fluidRow(
                    box(
                        width = 12,
                        status = "primary",
                        DT::dataTableOutput("scalesTable")
                    )),
                fluidRow(
                    box(
                        title = "Filters",
                        width = 6,
                        
                        searchInput(
                            inputId = "scaleSearch", 
                            label = "Search within scale names",
                            placeholder = "Your search term (e.g., adaptation) ...",
                            btnSearch = icon("search"),
                            btnReset = icon("remove")
                        ),
                        
                        hr(),
                        
                        awesomeCheckbox(
                            inputId = "ExperienceCheck",
                            label = tags$b("Filter by Experience Aspects"), 
                            value = FALSE
                        ),
                        prettySwitch(
                            inputId = "AffectSwitch",
                            label = "Affect", 
                            status = "success",
                            value = TRUE,
                            inline = TRUE,
                            fill = TRUE
                        ),
                        prettySwitch(
                            inputId = "BehaviorSwitch",
                            label = "Behavior", 
                            status = "success",
                            value = TRUE,
                            inline = TRUE,
                            fill = TRUE
                        ),
                        prettySwitch(
                            inputId = "CognitionSwitch",
                            label = "Cognition", 
                            status = "success",
                            value = TRUE,
                            inline = TRUE,
                            fill = TRUE
                        ),
                        prettySwitch(
                            inputId = "DesireSwitch",
                            label = "Desire", 
                            status = "success",
                            value = TRUE,
                            inline = TRUE,
                            fill = TRUE
                        ),
                        
                        hr(),
                        
                        sliderInput("sliderNItems", "Number of Items", 
                                    min = min(dt.Scales.Included$NItems, na.rm = TRUE), 
                                    max = max(dt.Scales.Included$NItems, na.rm = TRUE),
                                    value = c( min(dt.Scales.Included$NItems, na.rm = TRUE), max(dt.Scales.Included$NItems, na.rm = TRUE)),
                                    step = 1
                        ),
                        sliderInput("sliderNDomains", "Number of Domains", 
                                    min = min(scalesSelected$nlifeDomain, na.rm = TRUE), 
                                    max = max(scalesSelected$nlifeDomain, na.rm = TRUE),
                                    value = c(min(scalesSelected$nlifeDomain, na.rm = TRUE), max(scalesSelected$nlifeDomain, na.rm = TRUE)),
                                    step = 1
                        )
                    ),
                    box(
                        title = "Information",
                        width = 6,
                        tags$h5(tags$b("Currently Selected:")),
                        valueBoxOutput("nScalesSelected"),
                        valueBoxOutput("aveItemN"), 
                        valueBoxOutput("totItemN"),
                        tags$h5(tags$b("The Project:")),
                        HTML(
                            paste0(
                                "This directory of acculturation scales is part of the publication '",
                                tags$i("The Migration Experience: A Conceptual Framework and Systematic Review of Psychological Acculturation") , 
                                "' (",
                                tags$a(href="https://doi.org/toBePublished", target="_blank", "doi.org/toBePublished"),
                                "). As part of our systematic review of the literature on acculturation, we collected and analyzed 
                                scales that were used to measure 'psychological acculturation'. For all scales we extracted the publicly available
                                scale construction (see 'View' column) and then coded whether the scales conceptualized psychological acculturation in terms of
                                affect (e.g., feeling at home), behavior (e.g., language use), cognition (e.g., ethnic identification), 
                                and desire (e.g., independence wish)."
                                )
                            ),
                    )
                )
            ),
            tabItem(tabName = "references",
                      fluidRow(
                          box(
                              title = "Scale References",
                              width = 12,
                              HTML(refSec)
                          )
                      )
            ),
            tabItem(tabName = "about",
                    fluidRow(
                        box(
                            title = "How to Use the Directory",
                            solidHeader = TRUE,
                            width = 12,
                            tags$b("Interface"),br(),
                            "The", a("scale directory", onclick = "openTab('scales')", href="#"),
                            "consists of three main interface elements:",
                            tags$ol(
                                tags$li("The scale data table allows direct access to the scale directory. The table shows all scales that 
                                        fit the current filters and lists a number of key information about the scale. Next to the name of the scale,
                                        the apa short reference, the number or items, and number of life domains, the overview also indicates 
                                        whether the scale included any of the affect, behavior, cognition, and/or desire aspects (ABCD). 
                                        Lastly, the first column is a click-able area, which gives access to additional information about the scale.
                                        Wherever (publicly) available, we list the exact items, the response options, the life domains considered,
                                        as well as some information on the validation sample."),
                                tags$li("The filter section currently houses three main filters to identify scales that fit your needs:",
                                        tags$ul(
                                            tags$li("The",
                                                    tags$i("Title Search"),
                                                    "lets you search for words in the title of the scale. This might aid users searching for a
                                                    particular scale by name or for a topic that might be included as part of the scale name (e.g., 
                                                    attitudes)."
                                            ),
                                            tags$li("The",
                                                    tags$i("Experience Aspect Filter"),
                                                    "allows to filter the inclusion of the affect, behavior, cognition,
                                                    and/or desire aspects. If this filter is disabled any combination of ABCD will be
                                                    displayed but once enabled only the scales that fit your ABCD selection will be selected. 
                                                    For more information on the ABCD aspects see the 'Model' section below. Note that within
                                                    this section the header of the data table allows for simple sorting options by the content
                                                    of the available columns."
                                                    ),
                                            tags$li("The",
                                                    tags$i("Number of Items Filter"),
                                                   "allows to filter the acculturation scales by the number of items. You can use the slider to 
                                                   select the minimum and maximum number of items the scale is allowed to have."
                                                    ),
                                            tags$li("The",
                                                    tags$i("Number of Domains Filter"),
                                                    "allows to filter the scales by the number of life domains (i.e., situational contexts) assessed
                                                    within the scale. You can use the slider to select the minimum and maximum number of domains you 
                                                    would like to be included. For more information on the life domains see the 'Model' section below."
                                                    )
                                            )
                                        ),
                                tags$li("The information section offers a top-level overview of the current scale selection.
                                        The current version shows the number scales that fit the current filter choices, 
                                        the average number of items of the selected scales, the total number of items of all
                                        selected scales, as well as a short general introduction to the directory.")
                            ),
                            tags$b("Features"),br(),
                            "This scale directory has three main functions.",
                            tags$ol(
                                tags$li("Measurement Selection: The most practical function of this application is to aid researchers and practitioners 
                                        in the selection of acculturation measurements. The study of acculturation has produced an immense number of 
                                        acculturation scales, and making a choice between these different tools can be difficult. Not only is it difficult 
                                        to gain an overview of the number of scales used within the literature, also the diversity in style and content can 
                                        be overwhelming. We hope that the filter options we provide in our application can offer a first theory-based and 
                                        intuitive entry into the plethora for acculturation scales. It should be noted that this directory is not meant to 
                                        replace a full methodological review and does only present a small amount of information on the scales."),
                                tags$li("Scale Access: We do so by showcasing all (publicly) available scale items by clicking the eye icon in the
                                        'View' column. We additionally list the full reference to the
                                        scale in the", a("References", onclick = "openTab('references')", href="#"),"tab (also see the 'Reference' 
                                        column in the", a("scale directory", onclick = "openTab('scales')", href="#"),")."),
                                tags$li("Exploration of Review Results: As part of the framework development and systematic review, we have arrived at a 
                                        number of conclusions about the methodological literature on acculturation. We hope that readers can use this 
                                        directory in conjunction with the main article and explore the results themselves. The data table and the appended 
                                        filter allow readers an interactive access to the data and users might gain an intuitive understanding of the current 
                                        state of the literature"),
                            )
                        ),
                        box(
                            title = "The Review",
                            solidHeader = TRUE,
                            width = 12,
                            "One of the key challenges to researching psychological acculturation is an immense heterogeneity in theories and measures. 
                            These inconsistencies make it difficult to compare past literature on acculturation, hinder straight-forward measurement 
                            selections, and hampers the development of an overarching framework. To structure our understanding of the migration process, 
                            we propose to utilize the four basic elements of human experiences (wanting, feeling, thinking, and doing) as a conceptual 
                            framework. We use this framework to build a theory-driven literature synthesis of past theoretical 
                            (final ", tags$i("N"), " = 92), methodological (final ", tags$i("N"), " = 233) and empirical literature (final ", 
                            tags$i("N"), " = 530). We find that especially empirical works have understudied the more internal aspects of acculturation 
                            (motivations and feelings) and have often fallen short of capturing all four aspects of the migration experience. 
                            We also show differences between publication fields and discuss how the framework can aid transparent and functional theories, 
                            studies, and interventions going forward."
                        ),
                        box(
                            title = "The Model",
                            solidHeader = TRUE,
                            width = 12,
                            column(
                                width = 4,
                                img(src="https://raw.githubusercontent.com/JannisCodes/acculturation-review/main/Figures/ConceptualFrameworkStatic.png?token=AJEZTKCP3PRD3B7NCUQH4YLBIR5NS", width="100%"),
                                HTML(
                                    "<em>Figure 1</em>: Conceptual Framework with Context from the main manuscript."
                                )
                            ),
                            column(
                                width = 8,
                                HTML(
                                    "<br>To build a framework that would comprehensively structure the concept of psychological acculturation across a wide range of contexts, 
                                    we propose to utilize the basic elements of human experiences. Building on discussions with experts in the field and past reviews, 
                                    we propose that the psychological acculturation experience can be understood in terms of affects, behaviors, cognitions, and desires. 
                                    Psychological acculturation in this framework might, for example, be understood or measured in terms of behavioral acculturation, 
                                    such as language use, or voting; cognitive acculturation, such as ethnic identification, or cultural values endorsement; 
                                    affective acculturation, such as feeling at home, or loneliness; motivational acculturation, such as the satisfaction of competence or 
                                    independence needs; or as a combination of any or all of these aspects. Coding the available acculturation scales identified during the 
                                    systematic review inspired the creation of this scale directory and the chosen filter options.<br><br>",
                                    "Three major contextual factors often found within the literature are the cultures of the dominant and non-dominant groups, 
                                    the interacting individuals, and the interaction situation or life domain. All of these contextual elements will likely have a profound 
                                    impact on the experience of affects, behaviors, cognitions, and desires. As part of the directory we present here, we list the situational 
                                    context that is captured in the acculturation scales. One way of structuring this situational context is what we will here refer to as 
                                    the life domains &#8212 the idea that the social experience will take place within different domains in life. Based on sociological 
                                    theories of social institutions (Durkheim, 1982), literature on life domains in acculturation (Arends-Tóth & van de Vijver, 2006, 2007; Zane & Mak, 2004), 
                                    a categorization of psychological influences by the British Psychological Society (Michie et al., 2005), and Bronfenbrenner's 
                                    Ecological systems theory (Bronfenbrenner, 1992), we conceptualized a range of life domains relevant to the migration process 
                                    (also see Figure 1). These life domains are exlicitly or indirectly the target of many acculturation scales. We list all domains 
                                    that were explicitly refered to by the authors of the scales (within the scale pop-up box, via the 'View' button) and offer the option
                                    to filter scales by the number of life domains they address."
                                )
                            )
                            
                        )
                    )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$ExperienceCheck, {
        if(input$ExperienceCheck == TRUE){
            shinyjs::enable("AffectSwitch")
            shinyjs::enable("BehaviorSwitch")
            shinyjs::enable("CognitionSwitch")
            shinyjs::enable("DesireSwitch")
        }else{
            shinyjs::disable("AffectSwitch")
            shinyjs::disable("BehaviorSwitch")
            shinyjs::disable("CognitionSwitch")
            shinyjs::disable("DesireSwitch")
        }
        })
    
    scaleSelectedReact <- reactive({
        if(input$ExperienceCheck == TRUE){
            scalesSelected %>%
                filter(grepl(tolower(input$scaleSearch),tolower(Scale)),
                       Affect == ifelse(input$AffectSwitch==TRUE, as.character(icon("ok", lib = "glyphicon")), ""),
                       Behavior == ifelse(input$BehaviorSwitch==TRUE, as.character(icon("ok", lib = "glyphicon")), ""),
                       Cognition == ifelse(input$CognitionSwitch==TRUE, as.character(icon("ok", lib = "glyphicon")), ""),
                       Desire == ifelse(input$DesireSwitch==TRUE, as.character(icon("ok", lib = "glyphicon")), ""),
                       NItems >= input$sliderNItems[[1]],
                       NItems <= input$sliderNItems[[2]], 
                       nlifeDomain >= input$sliderNDomains[[1]], 
                       nlifeDomain <= input$sliderNDomains[[2]])
        } else {
            scalesSelected %>%
                filter(grepl(tolower(input$scaleSearch),tolower(Scale)),
                       NItems >= input$sliderNItems[[1]],
                       NItems <= input$sliderNItems[[2]], 
                       nlifeDomain >= input$sliderNDomains[[1]], 
                       nlifeDomain <= input$sliderNDomains[[2]])
        }
    })
    
    output$scalesTable = DT::renderDataTable({
        datatable(scaleSelectedReact(), 
                  colnames = c("View", "Scale", "Reference", 
                               "Affect", "Behavior", "Cognition", "Desire", 
                               "Number of Items", "Number of Life Domains"), 
                  rownames = FALSE, 
                  selection = 'none',
                  # class = 'cell-border strip hover',
                  escape = FALSE,
                  extensions = 'Scroller',
                  options = list(
                      scrollX = TRUE,
                      scrollY = 500,
                      scroller = TRUE,
                      searching = FALSE,
                      columnDefs=list(list(targets=5:length(scalesSelected)-1, class="dt-center"),
                                      list(targets=0, visible=FALSE))
                      )
                  ) %>% 
            formatStyle(2, cursor = 'pointer')
                                             }) 
    
    output$nScalesSelected <- renderValueBox({
        valueBox(
            nrow(scaleSelectedReact()),
            paste0("of ", nrow(dt.Scales.Included), " scales"),
            icon = icon("filter"),
            color = "light-blue"
        )
    })
    output$aveItemN <- renderValueBox({
        valueBox(
            round(mean(scaleSelectedReact()$NItems),1),
            "items (average)",
            icon = icon("clipboard-list"),
            color = "light-blue"
        )
    })
    output$totItemN <- renderValueBox({
        valueBox(
            format(sum(scaleSelectedReact()$NItems), big.mark=","),
            "items (total)",
            icon = icon("tasks"),
            color = "light-blue"
        )
    })
    
    observeEvent(input$scalesTable_cell_clicked, {
        info = input$scalesTable_cell_clicked
        id = sub(".*#", "", info$value)
        # do nothing if not clicked yet, or the clicked cell is not in the last column
        if (is.null(info$value) || info$col != 1) return()
        showModal(modalDialog(
            title = paste0("Information: ", dt.Scales.Included$Scale[dt.Scales.Included$id==id]),
            easyClose = TRUE,
            # info$row,
            # info$col,
            # info$value,
            tags$div(
                HTML(paste(tags$h4("Items:"),
                           dt.Scales.Included$Item[dt.Scales.Included$id==id],
                           "<hr>",
                           tags$h4("Response:"),
                           dt.Scales.Included$ResponseRangeAnchors[dt.Scales.Included$id==id],
                           "<hr>",
                           tags$h4("Sample:"),
                           tags$b("Sampling strategy: "),
                           dt.Scales.Included$Sample[dt.Scales.Included$id==id],
                           "<br>",
                           tags$b("Host Country: "),
                           dt.Scales.Included$HostCountry[dt.Scales.Included$id==id],
                           "<br>",
                           tags$b("Origin Country: "),
                           dt.Scales.Included$OriginCountry[dt.Scales.Included$id==id],
                           "<hr>",
                           tags$h4("Life Domains:"),
                           dt.Scales.Included$lifeDomain[dt.Scales.Included$id==id],
                           sep = ""))
            ),
            footer = modalButton("OK")
        ))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)