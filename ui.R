source("data.R")

# UI
shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  # HEADING ##########################################################################################################################################
  navbarPage(id = "MainNav",
             windowTitle = "Regional Economic Dashboard",
             title = div(
               span(a(img(src = "Govscot_logo_white.png", height=20), href = "https://www.gov.scot/"), style = "padding-right:40px;"),
               span("Regional Economic Dashboard")
             ),
             # HOME PAGE #############################################################################################################################
             tabPanel("Home",
               tags$head(tags$link(rel="shortcut icon", src="./www/favicon.ico")),
               setZoom(id = "SetEffects"), setShadow(id = "SetEffects"),
               fluidRow(
                 column(width = 6,
                        actionLink(
                          "GoToLocalAuthoritiesTab",
                          label = div(
                            tags$b("Local Authorities", style = "color: black;"),
                            tags$p("Data and data visualisations for a range of economic indicators, where the geographical split is made in terms of Scotland's 32 council areas. ", style = "color: black;"),
                            img(src = "tab1.png", height = "60%"), 
                            style = "border: solid 1px black; height: 220px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
                            id = "SetEffects" 
                          )
                        )
                 ),
                 column(width = 6,
                        actionLink(
                          "GoToRegionsTab",
                          label = div(
                            tags$b("Regions", style = "color: black;"),
                            tags$p("Data and data visualisations for a range of economic indicators, where the geographical split is made in terms of 8 combined local authorities (combined authority regions).", style = "color: black;"),
                            img(src = "tab2.png", height = "60%"), 
                            style = "border: solid 1px black; height: 220px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
                            id = "SetEffects"
                          )
                        )
                 ),
                 style = "padding: 10px; margin-top: 20px;"
               ),
               fluidRow(
                 column(width = 6,
                        actionLink(
                          "GoToRuralAndUrbanTab",
                          label = div(
                            tags$b("Rural and Urban", style = "color: black;"),
                            tags$p("Data and data visualisations for a range of economic indicators, where the geographical split is made in terms of 4 Rural / Urban Economy Areas (Using Scottish Government Fourfold RESAS Classification 2018). ", style = "color: black;"),
                            img(src = "tab3.png", height = "50%"), 
                            style = "border: solid 1px black; height: 220px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
                            id = "SetEffects" 
                          )
                        )
                 ),
                 column(width = 6,
                        actionLink(
                          "GoToEnterpriseRegionAreasTab",
                          label = div(
                            tags$b("Enterprise Region Areas", style = "color: black;"),
                            tags$p("Data and data visualisations for a range of economic indicators, where the geographical split is made in terms of 2 enterprise regions (Highlands and Islands Enterprise & Scottish Enterprise).", style = "color: black;"),
                            img(src = "tab4.jpg", height = "50%"), 
                            style = "border: solid 1px black; height: 220px; width: 100%; text-align: center; padding: 5px; border-radius: 0px;",
                            id = "SetEffects"
                          )
                        )
                 ),
                 style = "padding: 10px; margin-top: 20px;"
               )
             ), 
             # BACKGROUND #####
             tabPanel("Background",
                      tags$head(tags$link(rel="shortcut icon", src="./www/favicon.ico")),
                      h1("Scotland's Regional Economic Dashboard"),
                      h2("1.	Introduction"),
                      p("This dashboard has been produced to support users access analysis for geographies of interest beyond the areas normally published. The data included in this publication is broken down to the following levels, where possible:"),
                      p("- Countries (UK and Scotland)"),
                      p("- Combined Authority Regions"),
                      p("- Rural/ Urban Economy Areas"),
                      p("- Enterprise Region Areas"),
                      p("- Local Authority Areas"),
                      p("- Travel to work area 2011-based"),
                      p("- Highlands and Islands Enterprise Region Area Offices"),
                      p("- Clyde Gateway"),
                      p("The data and geographical breakdowns included have been developed in consultation with the Sub-Scotland Economic Statistics Group. The purpose of the Group is to identify the key strategic statistical information required by all interested parties, and to develop and implement a strategy for prioritising and meeting these needs while minimising, where possible, the burden on data suppliers and maintaining quality fit for purpose. More information on the group, as well as minutes from previous meetings, can be found here: "),
                      p(a("https://www.gov.scot/groups/sub-scotland-economic-statistics-group/", href = "https://www.gov.scot/groups/sub-scotland-economic-statistics-group/")),
                      h2("2.	Sources"),
                      p("Data in this analysis is sourced from the following publications:"),
                      p("- Gross Value Added (balanced), Office for National Statistics"),
                      p("- Businesses in Scotland, Scottish Government"),
                      p("- Business Demography, Office for National Statistics"),
                      p("- Export Statistics Scotland, Scottish Government"),
                      p("- Business Enterprise Research and Development Scotland, Scottish Government"),
                      p("- Business Register and Employment Survey"),
                      p("- Annual Population Survey"),
                      p("- Model based estimates of unemployment, Office for National Statistics"),
                      p("- Annual Survey of Hours and Earnings, Office for National Statistics"),
                      p("- Midyear estimates of population, National Records of Scotland"),
                      p("Please refer to the specific datasets to see the source for each indicator."),
                      h2("3.	Feedback"),
                      p("We welcome your feedback on these statistics, if you have any enquiries relating to these statistics then please contact us at:"),
                      p("industrystatistics@gov.scot"),
                      p("0131 244 6813"),
                      p("Scottish Government"),
                      p("Business and Innovation Statistics"),
                      p("5th Floor"),
                      p("5 Atlantic Quay"),
                      p("150 Broomielaw"),
                      p("Glasgow"),
                      p("G2 8LU")
             ),
             # LOCAL AUTHORITIES ##########################################################################################################################
             tabPanel(
               value = "LocalAuthoritiesTab",
               title = tags$div(icon("map-marked-alt", lib = "font-awesome"), "Local Authorities"),
               h1("Highlighted indicators"),
                 navlistPanel(widths=c(3,9),
                              # Map for GVA per head ####
                              tabPanel(" GVA per head",
                                       fluidRow(width = 12,
                                                column(width=12,
                                                       tags$b(textOutput("t1_la__map_caption")),
                                                       sliderInput("t1_la_input", label = "", min = start_year_t1_la , max = end_year_t1_la, value = end_year_t1_la, width = "50%", sep = "", step = 1),
                                                       withSpinner(leafletOutput("t1_la_map"), type = 5),
                                                       p("Source: "), 
                                                       a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                       p("*NA = Data not available")
                                                )
                                       )
                              ),
                              # Map for Business registrations per 10,000 adults ####
                              tabPanel("Business registrations per 10,000 adults",
                                       fluidRow(width = 12,
                                                column(width=12,
                                                       tags$b(textOutput("t3b_la__map_caption")),
                                                       sliderInput("t3b_la_input", label = "", min = start_year_t3b_la , max = end_year_t3b_la, value = end_year_t3b_la, width = "50%", sep = "", step = 1),
                                                       withSpinner(leafletOutput("t3b_la_map"), type = 5),
                                                       p("Source: "), 
                                                       a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                       p("*NA = Data not available")
                                                )
                                       )
                              ),
                              # Map for Business de-registrations per 10,000 adults ####
                              tabPanel("Business de-registrations per 10,000 adults",
                                       fluidRow(width = 12,
                                                column(width=12,
                                                       tags$b(textOutput("t4b_la__map_caption")),
                                                       sliderInput("t4b_la_input", label = "", min = start_year_t4b_la , max = end_year_t4b_la, value = end_year_t4b_la, width = "50%", sep = "", step = 1),
                                                       withSpinner(leafletOutput("t4b_la_map"), type = 5),
                                                       p("Source: "), 
                                                       a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                       p("*NA = Data not available")
                                                )
                                       )
                              ),
                              # Map for Business 3-year survival rate ####
                              tabPanel("Business 3-year survival rate",
                                       fluidRow(width = 12,
                                                column(width=12,
                                                       tags$b(textOutput("t5b_la__map_caption")),
                                                       sliderInput("t5b_la_input", label = "", min = start_year_t5b_la , max = end_year_t5b_la, value = end_year_t5b_la, width = "50%", sep = "", step = 1),
                                                       withSpinner(leafletOutput("t5b_la_map"), type = 5),
                                                       p("Source: "), 
                                                       a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                       p("*NA = Data not available")
                                                )
                                       )
                              ),
                              # Map for International exports per head ####
                              tabPanel("International exports per head",
                                       fluidRow(width = 12,
                                                column(width=12,
                                                       tags$b(textOutput("t8a_la__map_caption")),
                                                       sliderInput("t8a_la_input", label = "", min = start_year_t8a_la , max = end_year_t8a_la, value = end_year_t8a_la, width = "50%", sep = "", step = 1),
                                                       withSpinner(leafletOutput("t8a_la_map"), type = 5),
                                                       p("Source: "), 
                                                       a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                       p("*NA = Data not available")
                                                )
                                       )
                              ),
                              # Map for BERD per head ####
                              tabPanel("BERD per head",
                                       fluidRow(width = 12,
                                                column(width=12,
                                                       tags$b(textOutput("t9_la__map_caption")),
                                                       sliderInput("t9_la_input", label = "", min = start_year_t9_la , max = end_year_t9_la, value = end_year_t9_la, width = "50%", sep = "", step = 1),
                                                       withSpinner(leafletOutput("t9_la_map"), type = 5),
                                                       p("Source: "), 
                                                       a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                       p("*NA = Data not available")
                                                )
                                       )
                              ),
                              # Map for Employment rate ####
                              tabPanel("Employment rate",
                                       fluidRow(width = 12,
                                                column(width=12,
                                                       tags$b(textOutput("t14a_la__map_caption")),
                                                       sliderInput("t14a_la_input", label = "", min = start_year_t14a_la , max = end_year_t14a_la, value = end_year_t14a_la, width = "50%", sep = "", step = 1),
                                                       withSpinner(leafletOutput("t14a_la_map"), type = 5),
                                                       p("Source: "), 
                                                       a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                       p("*NA = Data not available")
                                                )
                                       )
                              ),
                              # Map for Self-employment rate ####
                              tabPanel("Self-employment rate",
                                       fluidRow(width = 12,
                                                column(width=12,
                                                       tags$b(textOutput("t15b_la__map_caption")),
                                                       sliderInput("t15b_la_input", label = "", min = start_year_t15b_la , max = end_year_t15b_la, value = end_year_t15b_la, width = "50%", sep = "", step = 1),
                                                       withSpinner(leafletOutput("t15b_la_map"), type = 5),
                                                       p("Source: "), 
                                                       a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                       p("*NA = Data not available")
                                                )
                                       )
                              ),
                              # Map for Unemployment rate ####
                              tabPanel("Unemployment rate",
                                       fluidRow(width = 12,
                                                column(width=12,
                                                       tags$b(textOutput("t17b_la__map_caption")),
                                                       sliderInput("t17b_la_input", label = "", min = start_year_t17b_la , max = end_year_t17b_la, value = end_year_t17b_la, width = "50%", sep = "", step = 1),
                                                       withSpinner(leafletOutput("t17b_la_map"), type = 5),
                                                       p("Source: "), 
                                                       a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                       p("*NA = Data not available")
                                                )
                                       )
                              ),
                              # Map for Median gross weekly pay ####
                              tabPanel("Median gross weekly pay",
                                       fluidRow(width = 12,
                                                column(width=12,
                                                       tags$b(textOutput("t18a_la__map_caption")),
                                                       sliderInput("t18a_la_input", label = "", min = start_year_t18a_la , max = end_year_t18a_la, value = end_year_t18a_la, width = "50%", sep = "", step = 1),
                                                       withSpinner(leafletOutput("t18a_la_map"), type = 5),
                                                       p("Source: "), 
                                                       a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                       p("*NA = Data not available")
                                                )
                                       )
                              )
              ),
              h1("Full data set"),
              tabsetPanel(
                # SUMMARY TOOL #######################################################################################################
                tabPanel("Summary Tool",
                  p("Please choose the local authorities and indicators from the checkbox inputs below (the pair: Scotland and Gross Value Added are pre-selected)."),
                  column(width = 2,
                         checkboxGroupInput(inputId = "checked_areas_la", label = "Local Authority", width = NULL, choices = vector_of_local_authorities, selected = "Scotland")      
                  ),
                  column(width = 3,
                         checkboxGroupInput(inputId = "checked_indicators_la", label = "Indicator", width = NULL, choices = vector_of_indicators, selected = "Gross Value Added")      
                  ),
                  column(width= 7,
                   # p("Table here"),
                   DTOutput("local_authorities_table")
                   # img(src = "aaa.png", height = "60%"),
                  )
                ),
                # TIME SERIES ####
                tabPanel("Time Series",
                         navlistPanel(widths=c(3,9),
                                      tabPanel("Gross Value Added",
                                               fluidRow(width = 12,
                                                        column(width=3,
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t1_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t1_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t1_la_wide)))),
                                                                   selected = positions_t1_la_wide
                                                                 )
                                                               )
                                                        ),
                                                        column(width=9,
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t1_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t1_la_graph"),
                                                                 p("Source: "),
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("VAT/PAYE registered enterprises",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t2a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t2a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t2a_la_wide)))),
                                                                   selected = positions_t2a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t2a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t2a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      ### tutaj
                                      tabPanel("Small businesses",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t2b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t2b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t2b_la_wide)))),
                                                                   selected = positions_t2b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t2b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t2b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("VAT/PAYE (Business Births)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t3a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t3a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t3a_la_wide)))),
                                                                   selected = positions_t3a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t3a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t3a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("VAT/PAYE (Business Births) per 10,000 adults",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t3b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t3b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t3b_la_wide)))),
                                                                   selected = positions_t3b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t3b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t3b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("VAT/PAYE (Business Deaths)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t4a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t4a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t4a_la_wide)))),
                                                                   selected = positions_t4a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t4a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t4a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("VAT/PAYE (Business Deaths) per 10,000 adults",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t4b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t4b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t4b_la_wide)))),
                                                                   selected = positions_t4b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t4b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t4b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("3-year business survival",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t5a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t5a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t5a_la_wide)))),
                                                                   selected = positions_t5a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t5a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t5a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("3-year business survival rate",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t5b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t5b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t5b_la_wide)))),
                                                                   selected = positions_t5b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t5b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t5b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Foreign-owned enterprises",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t6a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t6a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t6a_la_wide)))),
                                                                   selected = positions_t6a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t6a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t6a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Foreign-owned enterprises (EU)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t6b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t6b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t6b_la_wide)))),
                                                                   selected = positions_t6b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t6b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t6b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Foreign-owned business jobs",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t7a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t7a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t7a_la_wide)))),
                                                                   selected = positions_t7a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t7a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t7a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Foreign-owned business jobs (EU)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t7b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t7b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t7b_la_wide)))),
                                                                   selected = positions_t7b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t7b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t7b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("International Exports Value",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t8a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t8a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t8a_la_wide)))),
                                                                   selected = positions_t8a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t8a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t8a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("International Exports Value (EU)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t8b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t8b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t8b_la_wide)))),
                                                                   selected = positions_t8b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t8b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t8b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("BERD",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t9_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t9_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t9_la_wide)))),
                                                                   selected = positions_t9_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t9_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t9_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("BERD Jobs",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t10_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t10_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t10_la_wide)))),
                                                                   selected = positions_t10_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t10_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t10_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("High growth enterprises",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t11_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t11_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t11_la_wide)))),
                                                                   selected = positions_t11_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t11_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t11_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("VAT/PAYE Businesses Jobs",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t12_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t12_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t12_la_wide)))),
                                                                   selected = positions_t12_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t12_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t12_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Employment",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t13_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t13_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t13_la_wide)))),
                                                                   selected = positions_t13_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t13_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t13_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Working age employment rate",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t14a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t14a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t14a_la_wide)))),
                                                                   selected = positions_t14a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t14a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t14a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Working age employment rate (95% CI)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t14b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t14b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t14b_la_wide)))),
                                                                   selected = positions_t14b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t14b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t14b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Self-employed",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t15a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t15a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t15a_la_wide)))),
                                                                   selected = positions_t15a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t15a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t15a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Self-employed rate",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t15b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t15b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t15b_la_wide)))),
                                                                   selected = positions_t15b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t15b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t15b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Self-employed (95% CI)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t15c_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t15c_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t15c_la_wide)))),
                                                                   selected = positions_t15c_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t15c_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t15c_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Self-employed Females",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t16a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t16a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t16a_la_wide)))),
                                                                   selected = positions_t16a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t16a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t16a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Self-employed Females rate",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t16b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t16b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t16b_la_wide)))),
                                                                   selected = positions_t16b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t16b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t16b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Self-employed Females rate (95% CI)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t16c_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t16c_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t16c_la_wide)))),
                                                                   selected = positions_t16c_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t16c_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t16c_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Unemployment",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t17a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t17a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t17a_la_wide)))),
                                                                   selected = positions_t17a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t17a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t17a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Unemployment rate",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t17b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t17b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t17b_la_wide)))),
                                                                   selected = positions_t17b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t17b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t17b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Unemployment rate (95% CI)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t17c_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t17c_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t17c_la_wide)))),
                                                                   selected = positions_t17c_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t17c_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t17c_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Median gross weekly pay (RA)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t18a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t18a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t18a_la_wide)))),
                                                                   selected = positions_t18a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t18a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t18a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Median gross weekly pay (RA) (SE)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t18b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t18b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t18b_la_wide)))),
                                                                   selected = positions_t18b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t18b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t18b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Median gross weekly pay (WE)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t19a_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t19a_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t19a_la_wide)))),
                                                                   selected = positions_t19a_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t19a_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t19a_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Median gross weekly pay (WE) (SE)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t19b_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t19b_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t19b_la_wide)))),
                                                                   selected = positions_t19b_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t19b_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t19b_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Population",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t20_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t20_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t20_la_wide)))),
                                                                   selected = positions_t20_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t20_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t20_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Population (16+)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t21_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t21_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t21_la_wide)))),
                                                                   selected = positions_t21_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t21_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t21_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      ),
                                      tabPanel("Population (16-64)",
                                               fluidRow(width = 12,
                                                        column(width=3, 
                                                               wellPanel(
                                                                 checkboxGroupInput(
                                                                   inputId = "t22_la_input_time_series",
                                                                   label = "",
                                                                   choiceNames = names(t22_la_wide),
                                                                   choiceValues = c(seq(1:length(names(t22_la_wide)))),
                                                                   selected = positions_t22_la_wide
                                                                 )
                                                               )
                                                        ),      
                                                        column(width=9, 
                                                               fluidRow(
                                                                 withSpinner(dygraphOutput("t22_la_graph"), type = 5),
                                                                 align = "center"
                                                               ),
                                                               fluidRow(
                                                                 textOutput("legendDivID_t22_la_graph"),
                                                                 p("Source: "), 
                                                                 a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                 p("*NA = Data not available"),
                                                                 collapsible = FALSE,
                                                                 width = 12,
                                                                 style="margin-bottom: 100px;"
                                                               )
                                                        )
                                               )
                                      )
                         )
                )
              )
             ),
             # REGIONS ##########################################################################################################################
             tabPanel(
               value = "RegionsTab",
               title = tags$div(icon("map-marked-alt", lib = "font-awesome"), "Regions"),
               # Highlighted indicators ####
               h1("Highlighted indicators"),
               navlistPanel(widths=c(3,9),
                            tabPanel(" GVA per head",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t1_rg_input", label = "", min = start_year_t1_rg , max = end_year_t1_rg, value = end_year_t1_rg, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t1_rg_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Business registrations per 10,000 adults",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t3b_rg_input", label = "", min = start_year_t3b_rg , max = end_year_t3b_rg, value = end_year_t3b_rg, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t3b_rg_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Business de-registrations per 10,000 adults",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t4b_rg_input", label = "", min = start_year_t4b_rg , max = end_year_t4b_rg, value = end_year_t4b_rg, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t4b_rg_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Business 3-year survival rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t5b_rg_input", label = "", min = start_year_t5b_rg , max = end_year_t5b_rg, value = end_year_t5b_rg, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t5b_rg_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("International exports per head",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t8a_rg_input", label = "", min = start_year_t8a_rg , max = end_year_t8a_rg, value = end_year_t8a_rg, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t8a_rg_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("BERD per head",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t9_rg_input", label = "", min = start_year_t9_rg , max = end_year_t9_rg, value = 2017, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t9_rg_barchart", height = "500px"), type = 5),
                                                     p("NOTE: Where bar is 0, data is not available for chosen combination of region and year."),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Employment rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t14a_rg_input", label = "", min = start_year_t14a_rg , max = end_year_t14a_rg, value = end_year_t14a_rg, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t14a_rg_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Self-employment rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t15b_rg_input", label = "", min = start_year_t15b_rg , max = end_year_t15b_rg, value = end_year_t15b_rg, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t15b_rg_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Unemployment rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t17b_rg_input", label = "", min = start_year_t17b_rg , max = end_year_t17b_rg, value = end_year_t17b_rg, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t17b_rg_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            )
                            # NO DATA AVALIABLE
                            # tabPanel("Median gross weekly pay", 
                            #          fluidRow(width = 12,
                            #                   column(width=12,
                            #                          sliderInput("t18a_rg_input", label = "", min = start_year_t18a_rg , max = end_year_t18a_rg, value = end_year_t18a_rg, width = "50%", sep = "", step = 1),
                            #                          withSpinner(plotOutput("t18a_rg_barchart", height = "500px"), type = 5),
                            #                          p("Source: "), 
                            #                          a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                            #                          p("*NA = Data not available")
                            #                   )
                            #          )
                            # )
               ),
               h1("Full data set"),
               tabsetPanel(
                 # SUMMARY TOOL #######################################################################################################
                 tabPanel("Summary Tool",
                          p("Please choose the regions and indicators from the checkbox inputs below (the pair: Scotland and Gross Value Added are pre-selected)."),
                          column(width = 2,
                                 checkboxGroupInput(inputId = "checked_areas_r", label = "Region", width = NULL, choices = vector_of_regions, selected = "Scotland")      
                          ),
                          column(width = 3,
                                 checkboxGroupInput(inputId = "checked_indicators_r", label = "Indicator", width = NULL, choices = vector_of_indicators, selected = "Gross Value Added")      
                          ),
                          column(width= 7,
                                 # p("Table here"),
                                 DTOutput("regions_table")
                                 # img(src = "aaa.png", height = "60%"),
                          )
                 ),
                 # TIME SERIES ####
                 tabPanel("Time Series",
                          navlistPanel(widths=c(3,9),
                                       tabPanel("Gross Value Added",
                                                fluidRow(width = 12,
                                                         column(width=3,
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t1_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t1_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t1_rg_wide)))),
                                                                    selected = positions_t1_rg_wide
                                                                  )
                                                                )
                                                         ),
                                                         column(width=9,
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t1_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t1_rg_graph"),
                                                                  p("Source: "),
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE registered enterprises",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t2a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t2a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t2a_rg_wide)))),
                                                                    selected = positions_t2a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t2a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t2a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       ### tutaj
                                       tabPanel("Small businesses",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t2b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t2b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t2b_rg_wide)))),
                                                                    selected = positions_t2b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t2b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t2b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Births)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t3a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t3a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t3a_rg_wide)))),
                                                                    selected = positions_t3a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t3a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t3a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Births) per 10,000 adults",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t3b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t3b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t3b_rg_wide)))),
                                                                    selected = positions_t3b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t3b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t3b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Deaths)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t4a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t4a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t4a_rg_wide)))),
                                                                    selected = positions_t4a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t4a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t4a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Deaths) per 10,000 adults",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t4b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t4b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t4b_rg_wide)))),
                                                                    selected = positions_t4b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t4b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t4b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("3-year business survival",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t5a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t5a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t5a_rg_wide)))),
                                                                    selected = positions_t5a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t5a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t5a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("3-year business survival rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t5b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t5b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t5b_rg_wide)))),
                                                                    selected = positions_t5b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t5b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t5b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned enterprises",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t6a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t6a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t6a_rg_wide)))),
                                                                    selected = positions_t6a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t6a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t6a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned enterprises (EU)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t6b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t6b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t6b_rg_wide)))),
                                                                    selected = positions_t6b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t6b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t6b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned business jobs",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t7a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t7a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t7a_rg_wide)))),
                                                                    selected = positions_t7a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t7a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t7a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned business jobs (EU)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t7b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t7b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t7b_rg_wide)))),
                                                                    selected = positions_t7b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t7b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t7b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("International Exports Value",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t8a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t8a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t8a_rg_wide)))),
                                                                    selected = positions_t8a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t8a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t8a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("International Exports Value (EU)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t8b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t8b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t8b_rg_wide)))),
                                                                    selected = positions_t8b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t8b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t8b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("BERD",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t9_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t9_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t9_rg_wide)))),
                                                                    selected = positions_t9_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t9_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t9_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("BERD Jobs",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t10_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t10_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t10_rg_wide)))),
                                                                    selected = positions_t10_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t10_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t10_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("High growth enterprises",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t11_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t11_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t11_rg_wide)))),
                                                                    selected = positions_t11_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t11_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t11_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE Businesses Jobs",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t12_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t12_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t12_rg_wide)))),
                                                                    selected = positions_t12_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t12_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t12_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Employment",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t13_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t13_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t13_rg_wide)))),
                                                                    selected = positions_t13_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t13_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t13_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Working age employment rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t14a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t14a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t14a_rg_wide)))),
                                                                    selected = positions_t14a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t14a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t14a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Working age employment rate (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t14b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t14b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t14b_rg_wide)))),
                                                                    selected = positions_t14b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t14b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t14b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t15a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t15a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t15a_rg_wide)))),
                                                                    selected = positions_t15a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t15a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t15a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t15b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t15b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t15b_rg_wide)))),
                                                                    selected = positions_t15b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t15b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t15b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t15c_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t15c_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t15c_rg_wide)))),
                                                                    selected = positions_t15c_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t15c_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t15c_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed Females",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t16a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t16a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t16a_rg_wide)))),
                                                                    selected = positions_t16a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t16a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t16a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed Females rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t16b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t16b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t16b_rg_wide)))),
                                                                    selected = positions_t16b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t16b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t16b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed Females rate (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t16c_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t16c_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t16c_rg_wide)))),
                                                                    selected = positions_t16c_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t16c_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t16c_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Unemployment",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t17a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t17a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t17a_rg_wide)))),
                                                                    selected = positions_t17a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t17a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t17a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Unemployment rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t17b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t17b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t17b_rg_wide)))),
                                                                    selected = positions_t17b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t17b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t17b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Unemployment rate (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t17c_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t17c_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t17c_rg_wide)))),
                                                                    selected = positions_t17c_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t17c_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t17c_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (RA)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t18a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t18a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t18a_rg_wide)))),
                                                                    selected = positions_t18a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t18a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t18a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (RA) (SE)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t18b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t18b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t18b_rg_wide)))),
                                                                    selected = positions_t18b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t18b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t18b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (WE)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t19a_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t19a_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t19a_rg_wide)))),
                                                                    selected = positions_t19a_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t19a_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t19a_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (WE) (SE)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t19b_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t19b_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t19b_rg_wide)))),
                                                                    selected = positions_t19b_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t19b_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t19b_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Population",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t20_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t20_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t20_rg_wide)))),
                                                                    selected = positions_t20_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t20_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t20_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Population (16+)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t21_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t21_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t21_rg_wide)))),
                                                                    selected = positions_t21_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t21_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t21_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Population (16-64)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t22_rg_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t22_rg_wide),
                                                                    choiceValues = c(seq(1:length(names(t22_rg_wide)))),
                                                                    selected = positions_t22_rg_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t22_rg_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t22_rg_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       )
                          )
                 )
               )
             ),
             # RURAL AND URBAN ##########################################################################################################################
             tabPanel(
               value = "RuralAndUrbanTab",
               title = tags$div(icon("map-marked-alt", lib = "font-awesome"), "Rural and Urban"),
               h1("Highlighted indicators"),
               navlistPanel(widths=c(3,9),
                            tabPanel(" GVA per head",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t1_ru_input", label = "", min = start_year_t1_ru , max = end_year_t1_ru, value = end_year_t1_ru, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t1_ru_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Business registrations per 10,000 adults",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t3b_ru_input", label = "", min = start_year_t3b_ru , max = end_year_t3b_ru, value = end_year_t3b_ru, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t3b_ru_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Business de-registrations per 10,000 adults",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t4b_ru_input", label = "", min = start_year_t4b_ru , max = end_year_t4b_ru, value = end_year_t4b_ru, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t4b_ru_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Business 3-year survival rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t5b_ru_input", label = "", min = start_year_t5b_ru , max = end_year_t5b_ru, value = end_year_t5b_ru, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t5b_ru_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("International exports per head",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t8a_ru_input", label = "", min = start_year_t8a_ru , max = end_year_t8a_ru, value = end_year_t8a_ru, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t8a_ru_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("BERD per head",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t9_ru_input", label = "", min = start_year_t9_ru , max = end_year_t9_ru, value = end_year_t9_ru, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t9_ru_barchart", height = "500px"), type = 5),
                                                     p("NOTE: Where bar is 0, data is not available for chosen combination of region and year."),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Employment rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t14a_ru_input", label = "", min = start_year_t14a_ru , max = end_year_t14a_ru, value = end_year_t14a_ru, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t14a_ru_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Self-employment rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t15b_ru_input", label = "", min = start_year_t15b_ru , max = end_year_t15b_ru, value = end_year_t15b_ru, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t15b_ru_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Unemployment rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t17b_ru_input", label = "", min = start_year_t17b_ru , max = end_year_t17b_ru, value = end_year_t17b_ru, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t17b_ru_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            )
                            # NO DATA AVALIABLE
                            # tabPanel("Median gross weekly pay", 
                            #          fluidRow(width = 12,
                            #                   column(width=12,
                            #                          sliderInput("t18a_ru_input", label = "", min = start_year_t18a_ru , max = end_year_t18a_ru, value = end_year_t18a_ru, width = "50%", sep = "", step = 1),
                            #                          withSpinner(plotOutput("t18a_ru_barchart", height = "500px"), type = 5),
                            #                          p("Source: "), 
                            #                          a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                            #                          p("*NA = Data not available")
                            #                   )
                            #          )
                            # )
               ),
               h1("Full data set"),
               tabsetPanel(
                 # SUMMARY TOOL #######################################################################################################
                 tabPanel("Summary Tool",
                          p("Please choose the rural/urban areas and indicators from the checkbox inputs below (the pair: Scotland and Gross Value Added are pre-selected)."),
                          column(width = 2,
                                 checkboxGroupInput(inputId = "checked_areas_ru", label = "Rural/urban area", width = NULL, choices = vector_of_ruandurb, selected = "Scotland")      
                          ),
                          column(width = 3,
                                 checkboxGroupInput(inputId = "checked_indicators_ru", label = "Indicator", width = NULL, choices = vector_of_indicators, selected = "Gross Value Added")      
                          ),
                          column(width= 7,
                                 # p("Table here"),
                                 DTOutput("ruandurb_table")
                                 # img(src = "aaa.png", height = "60%"),
                          )
                 ),
                 # TIME SERIES ####
                 tabPanel("Time Series",
                          navlistPanel(widths=c(3,9),
                                       tabPanel("Gross Value Added",
                                                fluidRow(width = 12,
                                                         column(width=3,
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t1_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t1_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t1_ru_wide)))),
                                                                    selected = positions_t1_ru_wide
                                                                  )
                                                                )
                                                         ),
                                                         column(width=9,
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t1_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t1_ru_graph"),
                                                                  p("Source: "),
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE registered enterprises",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t2a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t2a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t2a_ru_wide)))),
                                                                    selected = positions_t2a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t2a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t2a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       ### tutaj
                                       tabPanel("Small businesses",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t2b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t2b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t2b_ru_wide)))),
                                                                    selected = positions_t2b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t2b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t2b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Births)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t3a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t3a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t3a_ru_wide)))),
                                                                    selected = positions_t3a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t3a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t3a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Births) per 10,000 adults",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t3b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t3b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t3b_ru_wide)))),
                                                                    selected = positions_t3b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t3b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t3b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Deaths)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t4a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t4a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t4a_ru_wide)))),
                                                                    selected = positions_t4a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t4a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t4a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Deaths) per 10,000 adults",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t4b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t4b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t4b_ru_wide)))),
                                                                    selected = positions_t4b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t4b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t4b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("3-year business survival",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t5a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t5a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t5a_ru_wide)))),
                                                                    selected = positions_t5a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t5a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t5a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("3-year business survival rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t5b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t5b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t5b_ru_wide)))),
                                                                    selected = positions_t5b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t5b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t5b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned enterprises",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t6a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t6a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t6a_ru_wide)))),
                                                                    selected = positions_t6a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t6a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t6a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned enterprises (EU)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t6b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t6b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t6b_ru_wide)))),
                                                                    selected = positions_t6b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t6b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t6b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned business jobs",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t7a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t7a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t7a_ru_wide)))),
                                                                    selected = positions_t7a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t7a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t7a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned business jobs (EU)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t7b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t7b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t7b_ru_wide)))),
                                                                    selected = positions_t7b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t7b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t7b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("International Exports Value",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t8a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t8a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t8a_ru_wide)))),
                                                                    selected = positions_t8a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t8a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t8a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("International Exports Value (EU)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t8b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t8b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t8b_ru_wide)))),
                                                                    selected = positions_t8b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t8b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t8b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("BERD",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t9_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t9_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t9_ru_wide)))),
                                                                    selected = positions_t9_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t9_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t9_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("BERD Jobs",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t10_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t10_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t10_ru_wide)))),
                                                                    selected = positions_t10_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t10_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t10_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("High growth enterprises",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t11_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t11_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t11_ru_wide)))),
                                                                    selected = positions_t11_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t11_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t11_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE Businesses Jobs",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t12_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t12_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t12_ru_wide)))),
                                                                    selected = positions_t12_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t12_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t12_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Employment",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t13_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t13_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t13_ru_wide)))),
                                                                    selected = positions_t13_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t13_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t13_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Working age employment rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t14a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t14a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t14a_ru_wide)))),
                                                                    selected = positions_t14a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t14a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t14a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Working age employment rate (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t14b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t14b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t14b_ru_wide)))),
                                                                    selected = positions_t14b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t14b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t14b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t15a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t15a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t15a_ru_wide)))),
                                                                    selected = positions_t15a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t15a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t15a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t15b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t15b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t15b_ru_wide)))),
                                                                    selected = positions_t15b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t15b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t15b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t15c_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t15c_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t15c_ru_wide)))),
                                                                    selected = positions_t15c_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t15c_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t15c_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed Females",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t16a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t16a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t16a_ru_wide)))),
                                                                    selected = positions_t16a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t16a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t16a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed Females rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t16b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t16b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t16b_ru_wide)))),
                                                                    selected = positions_t16b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t16b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t16b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed Females rate (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t16c_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t16c_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t16c_ru_wide)))),
                                                                    selected = positions_t16c_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t16c_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t16c_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Unemployment",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t17a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t17a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t17a_ru_wide)))),
                                                                    selected = positions_t17a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t17a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t17a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Unemployment rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t17b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t17b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t17b_ru_wide)))),
                                                                    selected = positions_t17b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t17b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t17b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Unemployment rate (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t17c_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t17c_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t17c_ru_wide)))),
                                                                    selected = positions_t17c_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t17c_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t17c_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (RA)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t18a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t18a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t18a_ru_wide)))),
                                                                    selected = positions_t18a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t18a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t18a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (RA) (SE)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t18b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t18b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t18b_ru_wide)))),
                                                                    selected = positions_t18b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t18b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t18b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (WE)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t19a_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t19a_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t19a_ru_wide)))),
                                                                    selected = positions_t19a_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t19a_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t19a_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (WE) (SE)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t19b_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t19b_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t19b_ru_wide)))),
                                                                    selected = positions_t19b_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t19b_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t19b_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Population",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t20_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t20_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t20_ru_wide)))),
                                                                    selected = positions_t20_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t20_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t20_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Population (16+)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t21_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t21_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t21_ru_wide)))),
                                                                    selected = positions_t21_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t21_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t21_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Population (16-64)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t22_ru_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t22_ru_wide),
                                                                    choiceValues = c(seq(1:length(names(t22_ru_wide)))),
                                                                    selected = positions_t22_ru_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t22_ru_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t22_ru_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       )
                          )
                 )
               )
             ),
             # ENTERPRISE REGION AREAS ##########################################################################################################################
             tabPanel(
               value = "EnterpriseRegionAreasTab",
               title = tags$div(icon("map-marked-alt", lib = "font-awesome"), "Enterprise Region Areas"),
               h1("Highlighted indicators"),
               navlistPanel(widths=c(3,9),
                            tabPanel(" GVA per head",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t1_e_input", label = "", min = start_year_t1_e , max = end_year_t1_e, value = end_year_t1_e, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t1_e_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Business registrations per 10,000 adults",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t3b_e_input", label = "", min = start_year_t3b_e , max = end_year_t3b_e, value = end_year_t3b_e, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t3b_e_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Business de-registrations per 10,000 adults",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t4b_e_input", label = "", min = start_year_t4b_e , max = end_year_t4b_e, value = end_year_t4b_e, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t4b_e_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Business 3-year survival rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t5b_e_input", label = "", min = start_year_t5b_e , max = end_year_t5b_e, value = end_year_t5b_e, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t5b_e_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("International exports per head",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t8a_e_input", label = "", min = start_year_t8a_e , max = end_year_t8a_e, value = end_year_t8a_e, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t8a_e_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            # NO DATA AVALIABLE
                            # tabPanel("BERD per head",
                            #          fluidRow(width = 12,
                            #                   column(width=12,
                            #                          sliderInput("t9_e_input", label = "", min = start_year_t9_e , max = end_year_t9_e, value = end_year_t9_e, width = "50%", sep = "", step = 1),
                            #                          withSpinner(plotOutput("t9_e_barchart", height = "500px"), type = 5),
                            #                          p("NOTE: Where bar is 0, data is not available for chosen combination of region and year."),
                            #                          p("Source: "), 
                            #                          a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                            #                          p("*NA = Data not available")
                            #                   )
                            #          )
                            # ),
                            tabPanel("Employment rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t14a_e_input", label = "", min = start_year_t14a_e , max = end_year_t14a_e, value = end_year_t14a_e, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t14a_e_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Self-employment rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t15b_e_input", label = "", min = start_year_t15b_e , max = end_year_t15b_e, value = end_year_t15b_e, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t15b_e_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            ),
                            tabPanel("Unemployment rate",
                                     fluidRow(width = 12,
                                              column(width=12,
                                                     sliderInput("t17b_e_input", label = "", min = start_year_t17b_e , max = end_year_t17b_e, value = end_year_t17b_e, width = "50%", sep = "", step = 1),
                                                     withSpinner(plotOutput("t17b_e_barchart", height = "500px"), type = 5),
                                                     p("Source: "), 
                                                     a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                     p("*NA = Data not available")
                                              )
                                     )
                            )
                            # NO DATA AVALIABLE
                            # tabPanel("Median gross weekly pay", 
                            #          fluidRow(width = 12,
                            #                   column(width=12,
                            #                          sliderInput("t18a_e_input", label = "", min = start_year_t18a_e , max = end_year_t18a_e, value = end_year_t18a_e, width = "50%", sep = "", step = 1),
                            #                          withSpinner(plotOutput("t18a_e_barchart", height = "500px"), type = 5),
                            #                          p("Source: "), 
                            #                          a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                            #                          p("*NA = Data not available")
                            #                   )
                            #          )
                            # )
               ),
               h1("Full data set"),
               tabsetPanel(
                 # SUMMARY TOOL #######################################################################################################
                 tabPanel("Summary Tool",
                          p("Please choose the enterprise region and indicators from the checkbox inputs below (the pair: Scotland and Gross Value Added are pre-selected)."),
                          column(width = 2,
                                 checkboxGroupInput(inputId = "checked_areas_e", label = "Enterprise Region", width = NULL, choices = vector_of_enterprise, selected = "Scotland")      
                          ),
                          column(width = 3,
                                 checkboxGroupInput(inputId = "checked_indicators_e", label = "Indicator", width = NULL, choices = vector_of_indicators, selected = "Gross Value Added")      
                          ),
                          column(width= 7,
                                 # p("Table here"),
                                 DTOutput("enterprise_table")
                                 # img(src = "aaa.png", height = "60%"),
                          )
                 ),
                 # TIME SERIES ####
                 tabPanel("Time Series",
                          navlistPanel(widths=c(3,9),
                                       tabPanel("Gross Value Added",
                                                fluidRow(width = 12,
                                                         column(width=3,
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t1_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t1_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t1_e_wide)))),
                                                                    selected = positions_t1_e_wide
                                                                  )
                                                                )
                                                         ),
                                                         column(width=9,
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t1_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t1_e_graph"),
                                                                  p("Source: "),
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE registered enterprises",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t2a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t2a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t2a_e_wide)))),
                                                                    selected = positions_t2a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t2a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t2a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       ### tutaj
                                       tabPanel("Small businesses",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t2b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t2b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t2b_e_wide)))),
                                                                    selected = positions_t2b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t2b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t2b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Births)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t3a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t3a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t3a_e_wide)))),
                                                                    selected = positions_t3a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t3a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t3a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Births) per 10,000 adults",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t3b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t3b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t3b_e_wide)))),
                                                                    selected = positions_t3b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t3b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t3b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Deaths)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t4a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t4a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t4a_e_wide)))),
                                                                    selected = positions_t4a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t4a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t4a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE (Business Deaths) per 10,000 adults",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t4b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t4b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t4b_e_wide)))),
                                                                    selected = positions_t4b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t4b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t4b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("3-year business survival",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t5a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t5a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t5a_e_wide)))),
                                                                    selected = positions_t5a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t5a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t5a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("3-year business survival rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t5b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t5b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t5b_e_wide)))),
                                                                    selected = positions_t5b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t5b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t5b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned enterprises",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t6a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t6a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t6a_e_wide)))),
                                                                    selected = positions_t6a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t6a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t6a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned enterprises (EU)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t6b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t6b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t6b_e_wide)))),
                                                                    selected = positions_t6b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t6b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t6b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned business jobs",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t7a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t7a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t7a_e_wide)))),
                                                                    selected = positions_t7a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t7a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t7a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Foreign-owned business jobs (EU)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t7b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t7b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t7b_e_wide)))),
                                                                    selected = positions_t7b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t7b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t7b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("International Exports Value",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t8a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t8a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t8a_e_wide)))),
                                                                    selected = positions_t8a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t8a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t8a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("International Exports Value (EU)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t8b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t8b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t8b_e_wide)))),
                                                                    selected = positions_t8b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t8b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t8b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("BERD",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t9_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t9_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t9_e_wide)))),
                                                                    selected = positions_t9_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t9_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t9_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("BERD Jobs",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t10_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t10_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t10_e_wide)))),
                                                                    selected = positions_t10_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t10_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t10_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("High growth enterprises",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t11_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t11_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t11_e_wide)))),
                                                                    selected = positions_t11_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t11_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t11_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("VAT/PAYE Businesses Jobs",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t12_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t12_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t12_e_wide)))),
                                                                    selected = positions_t12_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t12_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t12_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Employment",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t13_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t13_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t13_e_wide)))),
                                                                    selected = positions_t13_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t13_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t13_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Working age employment rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t14a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t14a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t14a_e_wide)))),
                                                                    selected = positions_t14a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t14a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t14a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Working age employment rate (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t14b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t14b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t14b_e_wide)))),
                                                                    selected = positions_t14b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t14b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t14b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t15a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t15a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t15a_e_wide)))),
                                                                    selected = positions_t15a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t15a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t15a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t15b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t15b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t15b_e_wide)))),
                                                                    selected = positions_t15b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t15b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t15b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t15c_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t15c_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t15c_e_wide)))),
                                                                    selected = positions_t15c_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t15c_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t15c_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed Females",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t16a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t16a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t16a_e_wide)))),
                                                                    selected = positions_t16a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t16a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t16a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed Females rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t16b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t16b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t16b_e_wide)))),
                                                                    selected = positions_t16b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t16b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t16b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Self-employed Females rate (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t16c_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t16c_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t16c_e_wide)))),
                                                                    selected = positions_t16c_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t16c_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t16c_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Unemployment",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t17a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t17a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t17a_e_wide)))),
                                                                    selected = positions_t17a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t17a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t17a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Unemployment rate",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t17b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t17b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t17b_e_wide)))),
                                                                    selected = positions_t17b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t17b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t17b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Unemployment rate (95% CI)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t17c_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t17c_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t17c_e_wide)))),
                                                                    selected = positions_t17c_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t17c_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t17c_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (RA)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t18a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t18a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t18a_e_wide)))),
                                                                    selected = positions_t18a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t18a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t18a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (RA) (SE)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t18b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t18b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t18b_e_wide)))),
                                                                    selected = positions_t18b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t18b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t18b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (WE)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t19a_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t19a_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t19a_e_wide)))),
                                                                    selected = positions_t19a_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t19a_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t19a_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Median gross weekly pay (WE) (SE)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t19b_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t19b_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t19b_e_wide)))),
                                                                    selected = positions_t19b_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t19b_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t19b_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Population",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t20_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t20_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t20_e_wide)))),
                                                                    selected = positions_t20_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t20_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t20_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Population (16+)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t21_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t21_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t21_e_wide)))),
                                                                    selected = positions_t21_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t21_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t21_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       tabPanel("Population (16-64)",
                                                fluidRow(width = 12,
                                                         column(width=3, 
                                                                wellPanel(
                                                                  checkboxGroupInput(
                                                                    inputId = "t22_e_input_time_series",
                                                                    label = "",
                                                                    choiceNames = names(t22_e_wide),
                                                                    choiceValues = c(seq(1:length(names(t22_e_wide)))),
                                                                    selected = positions_t22_e_wide
                                                                  )
                                                                )
                                                         ),      
                                                         column(width=9, 
                                                                fluidRow(
                                                                  withSpinner(dygraphOutput("t22_e_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_t22_e_graph"),
                                                                  p("Source: "), 
                                                                  a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"),
                                                                  p("*NA = Data not available"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       )
                          )
                 )
               )
             )
    ),
  # FOOTER ##########################################################################################################################################
  fluidRow(
    br(),
    wellPanel(
      fluidRow(
        # FOOTER - ABOUT
        column(width = 3,
               icon("info", lib = "font-awesome"),
               strong("ABOUT"),
               p("The Economy Board requested that OCEA lead on developing a performance framework for the Economy Board, taking into account the framework developed for the Enterprise and Skills Strategic Board."),
        ),
        # FOOTER - COPYRIGHT NOTICE
        column(width = 3,
               icon("copyright", lib = "font-awesome"),
               strong("COPYRIGHT NOTICE"),
               p("You may use or re-use this information (not including logos) free of charge in any format or medium, under the terms of the ",
                 a("Open Government Licence", href = "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"), ".")
        ),
        # FOOTER - CONTACT DETAILS
        column(width = 3,
               icon("at", lib = "font-awesome"),
               strong("CONTACT DETAILS"),
               p("We welcome your feedback:"),
               p("SCRIG@gov.scot", style = "line-height: 0%;")
        ),
        # FOOTER - EXTERNAL LINKS
        column(width = 3,
               icon("external-link", lib = "font-awesome"),
               strong("EXTERNAL LINKS"),
               p(a("Open Data Platfrom", href = "https://statistics.gov.scot/data/search")),
               p(a("National Performance Framework", href = "https://nationalperformance.gov.scot/")),
               p(a("SCRIG ", href = "https://www.inclusivegrowth.scot/")),
               p(a("SCRIG dashboard ", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"))
        )
      ),
      fluidRow(
        p("Reload the page should you experience any issues."),
        style = "text-align: center; outline: 0px;"
      )
    )
  ) # Navbar page ends here
)
) # UI ends here

