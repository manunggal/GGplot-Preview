library(shiny)
library(bs4Dash)
library(colourpicker)
library(rclipboard)


# Reference:
# 
# - Icon, https://fontawesome.com/icons


header = dashboardHeader("Plot Preview")

sidebar = dashboardSidebar(
  skin = "dark", status = "info", collapsed = FALSE,
  
  sidebarUserPanel(
    name = "Plot parameters"
  ),
  
  sidebarMenu(
    id = "sidebarmenu",
    # sidebarHeader("Header 1"),
    menuItem(
      "Title Elements", 
      tabName = "title_element", 
      icon = icon("fa-solid fa-pen-to-square")
    ),
    menuItem(
      "Panel Elements",
      tabName = "panel_element",
      icon = icon("fa-solid fa-chalkboard")
    ),
    menuItem(
      "Axis Elements",
      tabName = "axis_element",
      icon = icon("fa-solid fa-chart-line")
    ),
    menuItem(
      "Legend Elements",
      tabName = "legend_element",
      icon = icon("fa-solid fa-sheet-plastic")
    ),
    menuItem(
      "Color Scale",
      tabName = "color_scale",
      icon = icon("fa-solid fa-brush")
    )
  )
)

body = dashboardBody(
  fluidRow(
    box(
      width = 4,
      title = "Plot Parameter Input",
      # Title, subtitle, text adjustment
      tabItems(
        tabItem(
          tabName = "title_element",
          tabsetPanel(
            #title input
            tabPanel("Title Input",
              textInput("plot_title", "Plot Title:", value = "Mtcars Exploration"),
              textInput("plot_subtitle", "Plot Subtitle:", value = "Weight vs Miles/Gallon"),
              textInput("plot_x_title", "Plot X Axis Title:", value = "Weight (lb/1000)"),
              textInput("plot_y_title", "Plot Y Axis Title:", value = "Miles / US gallon"),
              textInput("plot_caption", "Plot Caption:", value = "Caption"),
              textInput("plot_tag", "Plot Tag:", value = "Tag")
            ),
            # title parameter
            tabPanel("Title Size etc",
              sliderInput("plot_title_size", "Plot Title Size", min = 0, max = 10, value = 1, step = 0.5),
              sliderInput("plot_title_hjust", "Plot Title hjust", min = 0, max = 1, value = 0.5, step = 0.1),
              sliderInput("plot_title_vjust", "Plot Title vjust", min = 0, max = 1, value = 0.5, step = 0.1),
              colourInput("plot_title_color", "Plot Title Color", "#e6e1e1")
            ),
            # subtitle, etc
            tabPanel("Subtitle Size etc",
              sliderInput("plot_subtitle_size", "Plot Subtitle Size", min = 0, max = 10, value = 1, step = 0.5),
              sliderInput("plot_subtitle_hjust", "Plot Subtitle hjust", min = 0, max = 1, value = 0.5, step = 0.1),
              sliderInput("plot_subtitle_vjust", "Plot Subtitle vjust", min = 0, max = 1, value = 0.5, step = 0.1),
              colourInput("plot_subtitle_color", "Plot Subtitle Color", "#e6e1e1"),
              colourInput("plot_caption_color", "Plot caption Color", "#e6e1e1"),
              colourInput("plot_tag_color", "Plot Tag Color", "#e6e1e1")
            )
          )

          
        ),
        
        # plot panel background, grid lines adjustment
        tabItem(
          tabName = "panel_element",
          tabsetPanel(
            # plot panel background
            tabPanel("Plot & Panel",
             colourInput("plot_background", "Plot Background Colour", "#282C33"),
             colourInput("panel_background", "Panel Background Colour", "#282C33")
            ),
            tabPanel("Grid Lines",
             # Grid Major
             sliderInput("panel_grid_major_size", "Grid Major Size", min = 0, max = 10, value = 1, step = 0.5),
             selectInput("panel_grid_major_linetype", "Grid Major Linetype", choices = list("blank" = "blank", "solid" = "solid", "dashed" = "dashed", "dotted" = "dotted", "dotdash" = "dotdash", "longdash" = "longdash", 'twodash' = "twodash"), selected = "solid"),
             colourInput("panel_grid_major_color", "Panel Grid Major Colour", "#3c4046"),
             tags$hr(), # separator
             #Grid Minor
             sliderInput("panel_grid_minor_size", "Grid Minor Size", min = 0, max = 10, value = 1, step = 0.5),
             selectInput("panel_grid_minor_linetype", "Grid Minor Linetype", choices = list("blank" = "blank", "solid" = "solid", "dashed" = "dashed", "dotted" = "dotted", "dotdash" = "dotdash", "longdash" = "longdash", 'twodash' = "twodash"), selected = "solid"),
             colourInput("panel_grid_minor_color", "Panel Grid Minor Colour", "#303338")
            )
          )
        ),
        
        # Axis element
        tabItem(
          tabName = "axis_element",
          tabsetPanel(
            tabPanel("Axis Title & Text",
              sliderInput("axis_title_size", "Axis Title Size", min = 0, max = 10, value = 1, step = 0.5),
              colourInput("axis_title_color", "Axis Title Color", "#e6e1e1"),
              sliderInput("axis_text_size", "Axis Text Size", min = 0, max = 10, value = 1, step = 0.5),
              colourInput("axis_text_color", "Axis Text Color", "#e6e1e1")
            ),
            tabPanel("Lines",
              sliderInput("axis_line_size", "AXis Line Size", min = 0, max = 10, value = 1, step = 0.5),
              colourInput("axis_line_color", "Axis Line Colour", "#303338"),
              sliderInput("axis_ticks_size", "AXis Ticks Size", min = 0, max = 10, value = 1, step = 0.5),
              colourInput("axis_ticks_color", "Axis Ticks Colour", "#303338"),
              
            )
          )
          
        ),
        
        # legend element
        tabItem(
          tabName = "legend_element",
          tabsetPanel(
            tabPanel("Panel",
              colourInput("legend_background", "Legend Background Colour", "#282C33"),
              colourInput("legend_key", "Legend Key Colour", "#282C33")
            ),
            tabPanel("Position",
              selectInput("legend_position_option", "Legend Positioning Method:",
                list("Outside Plot Area" = "outside_plot_area",
                     "inside Plot Area" = "inside_plot_area")
              ),
              conditionalPanel(
                condition = "input.legend_position_option == 'outside_plot_area'",
                radioButtons(
                  "outside_legend", "Outside Plot Area",
                  choices = list("none" = "none", "left" = "left", "right" = "right", "top" = "top", "bottom" = "bottom"),
                  selected = "right"
                )
              ),
              conditionalPanel(
                condition = "input.legend_position_option == 'inside_plot_area'",
                sliderInput("legend_x_post", "Legend X Position", min = 0, max = 1, value = 0.8, step = 0.05),
                sliderInput("legend_y_post", "Legend Y Position", min = 0, max = 1, value = 0.5, step = 0.05)
              )
            ),
            tabPanel("Text",
              sliderInput("legend_title_size", "Legend Title Size", min = 0, max = 10, value = 1, step = 0.5),
              colourInput("legend_title_color", "Legend Title Color", "#e6e1e1"),
              sliderInput("legend_title_align", "Legend Title Align", min = 0, max = 10, value = 1, step = 0.5),
              
              tags$hr(style="border-color: #706969;"),
              
              sliderInput("legend_text_size", "Legend Text Size", min = 0, max = 10, value = 1, step = 0.5),
              colourInput("legend_text_color", "Legend Text Color", "#e6e1e1"),
              sliderInput("legend_text_align", "Legend Text Align", min = 0, max = 10, value = 1, step = 0.5)
            ),
            tabPanel("Key Size, Color etc",
              colourInput("legend_size_color", "Legend Size Color (Horsepower)", "#e6e1e1"),
              colourInput("legend_shape_color", "Legend Shape Color (Gear)", "#e6e1e1"),
              sliderInput("legend_shape_size", "Legend Shape Size (Gear", min = 0, max = 10, value = 4, step = 0.5),
              sliderInput("legend_color_size", "Legend Color Size (Cylinder)", min = 0, max = 10, value = 5, step = 0.5)
            )
          )
        ),
        
        # color scale
        tabItem(
          tabName = "color_scale",
          tabsetPanel(
            tabPanel("Color Scale Palettes",
              DT::dataTableOutput("palettes_d")),
            tabPanel("Scale Transparency & Size",
              sliderInput("scatter_transparency", "Scatter Transparency", min = 0, max = 1, value = 1, step = 0.1),
              sliderInput("scatter_min_size", "Scatter Min Size", min = 0, max = 10, value = 1, step = 0.5),
              sliderInput("scatter_max_size", "Scatter Max Size", min = 0, max = 10, value = 10, step = 0.5)
            )
          )
        )
        
      )
    ),
    box(
      title = "Plot Output and Source Code",
      width = 8,
      tabsetPanel(
        tabPanel("Plot Preview",
          plotOutput("plot")
        ),
        tabPanel("GGPlot Code to Copy",
          rclipboardSetup(),
          uiOutput("clip"),
          verbatimTextOutput ('code_snippet')
        )
      )
    )
  )
)

dashboardPage(
  dark = TRUE,
  header, sidebar, body)

