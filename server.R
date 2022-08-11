library(shiny)
library(bs4Dash)
library(ggplot2)
library(tidyverse)
library(paletteer)
library(DT)
library(knitr)
library(markdown)
library(glue)
source("code_snippet_strings.r")

server = function(input, output) {
  
  # render data table for palette selection
  output$palettes_d <- DT::renderDataTable({
    
    DT::datatable(
      palettes_d_names %>% select(package, palette, type),
      selection = 'single',
      style = "bootstrap4",
      options = list(
        dom = '<"top"if<"clear">>rt<"bottom"p<"clear">>'
      )
      # options = list(   initComplete = JS(
      #   "function(settings, json) {",
      #   "$(this.api().table().header()).css({'font-size': '100%'});",
      #   "}")
      #   )
    )
  })
  
  # selected palette
  selected_color = reactive({
    
    # take input from palette dataframe
    selected_package = palettes_d_names[input$palettes_d_rows_selected, 1]
    selected_palette = palettes_d_names[input$palettes_d_rows_selected, 2]
    
    # create strings for ggplot color scale function
    paste0("scale_color_paletteer_d(\"",
           selected_package,
           "::",
           selected_palette,
           "\")")
  })
  
  # output to plot
  output$plot <- renderPlot({
    
    ggplot(mtcars, aes(x=mpg, y=disp, size=hp, shape=as.factor(gear), col=as.factor(cyl))) +
      geom_point(alpha = input$scatter_transparency) +
      
      
      # plot title, etc info
      labs(
        title = input$plot_title,
        subtitle = input$plot_subtitle,
        caption = input$plot_caption,
        tag = input$plot_tag,
        size = "horsepower",
        col = "Numbers of cylinders",
        shape = "Numbers of gears"
      ) +
      
      # plot axis info
      xlab(input$plot_x_title) + ylab(input$plot_y_title) +
      
      # plot theme, panel color, line size, etc (non data aspects)
      theme(
        # plot title etc
        plot.title = element_text(
          face = "bold", 
          size = rel(input$plot_title_size),
          colour = input$plot_title_color,
          hjust = input$plot_title_hjust,
          vjust = input$plot_title_vjust),
        plot.subtitle = element_text(
          face = "bold", 
          size = rel(input$plot_subtitle_size),
          colour = input$plot_subtitle_color,
          hjust = input$plot_subtitle_hjust,
          vjust = input$plot_subtitle_vjust),
        plot.caption = element_text(colour = input$plot_caption_color),
        plot.tag = element_text(colour = input$plot_tag_color),
        
        # plot panel
        plot.background = element_rect(
          colour = input$plot_background,
          fill = input$plot_background),
        panel.background = element_rect(
          colour = NA,
          fill = input$panel_background),
        panel.grid.major = element_line(
          colour = input$panel_grid_major_color,
          size = rel(input$panel_grid_major_size),
          linetype = input$panel_grid_major_linetype),
        panel.grid.minor = element_line(
          colour = input$panel_grid_minor_color,
          size = rel(input$panel_grid_minor_size),
          linetype = input$panel_grid_minor_linetype),
        
        # axis
        axis.title = element_text(
          face = "bold", 
          size = rel(input$axis_title_size), 
          colour = input$axis_title_color),
        axis.text = element_text(
          face = "bold", 
          size = rel(input$axis_text_size), 
          colour = input$axis_text_color),
        axis.line = element_line(colour = input$axis_line_color, size = input$axis_line_size),
        axis.ticks = element_line(colour = input$axis_ticks_color, size = input$axis_ticks_size),
        
        # legend
        legend.background = element_rect(colour = NA, fill = input$legend_background),
        legend.key = element_rect(colour = input$legend_key, fill = NA),
        
        # legend.key = element_blank(),
        legend.title = element_text(
          face = "bold",
          size = rel(input$legend_title_size),
          colour = input$legend_title_color),
        legend.text = element_text(
          face = "bold",
          size = rel(input$legend_text_size),
          colour = input$legend_text_color),
        legend.position = if(input$legend_position_option == "outside_plot_area") {
          input$outside_legend
        } else {
          c(input$legend_x_post, input$legend_y_post)
        }
        
        
      ) +
      
      guides(
        size = guide_legend(override.aes = list(color = input$legend_size_color)),
        shape = guide_legend(override.aes = list(color = input$legend_shape_color, size = input$legend_shape_size)),
        color = guide_legend(override.aes = list(size = input$legend_color_size))
      ) +
      
      scale_size_continuous(range = c(input$scatter_min_size, input$scatter_max_size)) +
      
      #color scale ggplot
      if(str_detect(selected_color(), "character")){ # check if palette already selected, if not use default
        # provide default color scale if no color palette is selected
        scale_fill_discrete()
      } else {
        # use strings from selected_color() to generate scale color function
        eval(parse(text = selected_color()))
      }
    
  })
  
  
  legend_position = reactive({
    legend_position_strings(
      input$legend_position_option,
      input$outside_legend,
      input$legend_x_post,
      input$legend_y_post)
  })
  
  scale_color = reactive({
    scale_color_strings(
      selected_color()
    )
  })
  
  
  ggplot_code_to_copy = reactive({
    
    code_snipet_strings(
      scatter_transparency = input$scatter_transparency,
      plot_title = input$plot_title, plot_subtitle = input$plot_subtitle, plot_caption = input$plot_caption, plot_tag = input$plot_tag,
      horsepower = "horsepower", cylinders = "Numbers of cylinders", gears = "Numbers of gears",
      plot_x_title = input$plot_x_title, plot_y_title = input$plot_y_title,
      bold = "bold",
      plot_title_size = input$plot_title_size, plot_title_color = input$plot_title_color, plot_title_hjust = input$plot_title_hjust, plot_title_vjust = input$plot_title_vjust,
      plot_subtitle_size = input$plot_subtitle_size, plot_subtitle_color = input$plot_subtitle_color, plot_subtitle_hjust = input$plot_subtitle_hjust, plot_subtitle_vjust = input$plot_subtitle_vjust,
      plot_caption_color = input$plot_caption_color, plot_tag_color = input$plot_tag_color,
      plot_background = input$plot_background, panel_background = input$panel_background,
      panel_grid_major_color = input$panel_grid_major_color, panel_grid_major_size = input$panel_grid_major_size, panel_grid_major_linetype = input$panel_grid_major_linetype,
      panel_grid_minor_color = input$panel_grid_minor_color, panel_grid_minor_size = input$panel_grid_minor_size, panel_grid_minor_linetype = input$panel_grid_minor_linetype,
      axis_title_size = input$axis_title_size, axis_title_color = input$axis_title_color,
      axis_text_size = input$axis_text_size, axis_text_color = input$axis_text_color,
      axis_line_color = input$axis_line_color, axis_line_size = input$axis_line_size,
      axis_ticks_color = input$axis_ticks_color, axis_ticks_size = input$axis_ticks_size,
      legend_background = input$legend_background, legend_key = input$legend_key,
      legend_title_size = input$legend_title_size, legend_title_color = input$legend_title_color,
      legend_text_size = input$legend_text_size, legend_text_color = input$legend_text_color,
      legend_position = legend_position(),
      legend_size_color = input$legend_size_color, legend_shape_color = input$legend_shape_color, legend_shape_size = input$legend_shape_size, legend_color_size = input$legend_color_size,
      scatter_min_size = input$scatter_min_size, scatter_max_size = input$scatter_max_size,
      scale_color = scale_color()
    )
    
    
  })
  
  output$code_snippet <- renderText({ggplot_code_to_copy()})
  
  output$clip = renderUI({
    rclipButton(
      inputId = "clipbutton",
      label = "Copy Code",
      clipText = ggplot_code_to_copy(),
      icon = icon("clipboard")
    )
  })
  
  observeEvent(input$clipbutton, {
    showNotification("Code copied!", type = "message")
  })
  
  
}