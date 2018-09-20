## -- Function to plot % complete for a set of individual assessments ----------
## Creates a ggplot2 object, with intention to pipe to ggplotly()

## This function assumes that df includes the columns
## - asmt_type: name of assessment (eg, RBANS)
## - prop_comp: proportion of this assessment counted as complete
## - htext: text for tooltip (eg, "RBANS: 98%")
## - comp_ok: factor categorizing % complete; used to color lollipops
plot_asmts_comp <- function(df, ybreaks){
  p <- ggplot(
    data = df
  ) +
    aes(x = asmt_type, y = prop_comp, text = htext) +
    scale_y_continuous(breaks = ybreaks, label = scales::percent) +
    geom_pointrange(
      aes(ymin = 0, ymax = prop_comp, color = comp_ok), size = 3
    ) +
    scale_colour_manual(
      values = c(mosaic_col("green4"), mosaic_col("orange3"), mosaic_col("red5"))
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
  
  return(p)
  
}