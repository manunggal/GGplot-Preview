library(glue)


scale_color_strings = function(
    
  selected_color
) {
  
  if(str_detect(selected_color, "character")) {
    glue("scale_fill_discrete()")
  } else {
    selected_color
  }
  
}

legend_position_strings = function(
    legend_position_option, outside_legend, legend_x_post,  legend_y_post
) {
  
  if(legend_position_option == "outside_plot_area") {
    glue("{outside_legend}")
  } else{
    glue("{c({legend_x_post}, {legend_y_post})}")
  }
  
}

code_snipet_strings = function(
    scatter_transparency,
    plot_title, plot_subtitle, plot_caption, plot_tag,
    horsepower, cylinders, gears,
    plot_x_title, plot_y_title,
    bold,
    plot_title_size, plot_title_color, plot_title_hjust, plot_title_vjust,
    plot_subtitle_size, plot_subtitle_color, plot_subtitle_hjust, plot_subtitle_vjust,
    plot_caption_color, plot_tag_color,
    plot_background, panel_background,
    panel_grid_major_color, panel_grid_major_size, panel_grid_major_linetype,
    panel_grid_minor_color, panel_grid_minor_size, panel_grid_minor_linetype,
    axis_title_size, axis_title_color,
    axis_text_size, axis_text_color,
    axis_line_color, axis_line_size,
    axis_ticks_color, axis_ticks_size,
    legend_background, legend_key,
    legend_title_size, legend_title_color,
    legend_text_size, legend_text_color,
    legend_position,
    legend_size_color, legend_shape_color, legend_shape_size, legend_color_size,
    scatter_min_size, scatter_max_size,
    scale_color
    
) {
  
  glue(
    "
    # Required libraries
    library(ggplot2)
    library(paletteer)
    
    ggplot(mtcars, aes(x=mpg, y=disp, size=hp, shape=as.factor(gear), col=as.factor(cyl))) +
      geom_point(alpha = {scatter_transparency}) +
      
      # plot title, etc text
      labs(
        title = {double_quote({plot_title})}, 
        subtitle = {double_quote({plot_subtitle})},
        caption = {double_quote({plot_caption})},
        tag = {double_quote({plot_tag})},
        size = {double_quote({horsepower})}, 
        col = {double_quote({cylinders})}, 
        shape = {double_quote({gears})}
        ) +
      
      # plot axis text
      xlab({double_quote({plot_x_title})}) + ylab({double_quote({plot_y_title})}) +
      
      # plot theme, panel color, line size, etc (non data aspects)
      theme(
        # plot title, etc modification
        plot.title = element_text(
          face = {double_quote({bold})}, 
          size = rel({plot_title_size}),
          colour = {double_quote({plot_title_color})},
          hjust = {plot_title_hjust},
          vjust = {plot_title_vjust}),
        plot.subtitle = element_text(
          face = {double_quote({bold})}, 
          size = rel({plot_subtitle_size}),
          colour = {double_quote({plot_subtitle_color})},
          hjust = {plot_subtitle_hjust},
          vjust = {plot_subtitle_vjust}),
        plot.caption = element_text(colour = {double_quote({plot_caption_color})}),
        plot.tag = element_text(colour = {double_quote({plot_tag_color})}),
        
        # plot panel modification
        plot.background = element_rect(
          colour = {double_quote({plot_background})},
          fill = {double_quote({plot_background})}),
        panel.background = element_rect(
          colour = NA,
          fill = {double_quote({plot_background})}),
        panel.grid.major = element_line(
          colour = {double_quote({panel_grid_major_color})},
          size = rel({panel_grid_major_size}),
          linetype = {double_quote({panel_grid_major_linetype})}),
        panel.grid.minor = element_line(
          colour = {double_quote({panel_grid_minor_color})},
          size = rel({panel_grid_minor_size}),
          linetype = {double_quote({panel_grid_minor_linetype})}),
        
        # axis modification
        axis.title = element_text(
          face = {double_quote({bold})}, 
          size = rel({axis_title_size}), 
          colour = {double_quote({axis_title_color})}),
        axis.text = element_text(
          face = {double_quote({bold})}, 
          size = rel({axis_text_size}), 
          colour = {double_quote({axis_text_color})}),
        axis.line = element_line(colour = {double_quote({axis_line_color})}, size = {axis_line_size}),
        axis.ticks = element_line(colour = {double_quote({axis_ticks_color})}, size = {axis_ticks_size}),
        
        # legend modification
        legend.background = element_rect(colour = NA, fill = {double_quote({legend_background})}),
        legend.key = element_rect(colour = {double_quote({legend_key})}, fill = NA),
        legend.title = element_text(
          face = {double_quote({bold})},
          size = rel({legend_title_size}),
          colour = {double_quote({legend_title_color})}),
        legend.text = element_text(
          face = {double_quote({bold})},
          size = rel({legend_text_size}),
          colour = {double_quote({legend_text_color})}),
        legend.position = {double_quote(legend_position)}
      ) +
      guides(
        size = guide_legend(override.aes = list(color = {double_quote({legend_size_color})})),
        shape = guide_legend(override.aes = list(color = {double_quote({legend_shape_color})}, size = {legend_shape_size})),
        color = guide_legend(override.aes = list(size = {legend_color_size}))
      ) +
      
      # scale scatter size
      scale_size_continuous(range = c({scatter_min_size}, {scatter_max_size})) +
      
      # scale color
      {scale_color}  

    "
  )
  
}
