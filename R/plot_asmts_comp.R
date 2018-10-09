## -- Function to plot % complete for a set of individual assessments ----------
## Creates a ggplot2 object, with intention to pipe to ggplotly()

## This function assumes that df includes the columns
## - asmt_type: name of assessment (eg, RBANS)
## - prop_comp: proportion of this assessment counted as complete
## - htext: text for tooltip (eg, "RBANS: 98%")
## - comp_ok: factor categorizing % complete; used to color lollipops
plot_asmts_comp <- function(df, ybreaks, order_desc = TRUE){
  ## Reorder assessment types if requested; otherwise, default is alphabetical
  if(order_desc){
    df <- df %>%
      mutate(asmt_type = fct_reorder(asmt_type, prop_comp, .desc = TRUE))
  }
  
  p <- ggplot(
    data = df
  ) +
    aes(x = asmt_type, y = prop_comp, text = htext) +
    scale_y_continuous(
      limits = c(0, 1.05), breaks = ybreaks, label = scales::percent
    ) +
    geom_pointrange(
      aes(ymin = 0, ymax = prop_comp, color = comp_ok), size = 3
    ) +
    scale_colour_manual(
      values = asmt_values
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
  
  return(p)
  
}