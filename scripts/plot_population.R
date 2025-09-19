## Plots growth rates for individual cities or global

plot_demography <- function(df, country_name) {
  df$Nodes_ID <- as.factor(df$Nodes_ID)
  
  ggplot(df, aes(x = Year, y = Inhabitants, group = Nodes_ID, colour = Nodes_ID)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    theme(legend.position = 'none') +
    labs(
      title = paste('Demographic trends in', country_name),
      x = 'Year',
      y = 'Inhabitants'
    )
}

plot_growth_rate <- function(df, country_name) {
  df <- df |>
    arrange(Nodes_ID, Year) |>
    group_by(Nodes_ID) |>
    mutate(
      previous = lag(Inhabitants),
      year_diff = Year - lag(Year),
      growth_rate = ifelse(!is.na(previous) & previous > 0,
                           ((Inhabitants - previous) / previous) / year_diff, # Yearly growth
                           NA_real_)
    ) |>
    ungroup()
  
  summary_growth <- df |>
    filter(!is.na(growth_rate)) |>
    group_by(Year) |>
    summarise(
      mean_growth = mean(growth_rate, na.rm = TRUE),
      sd = sd(growth_rate, na.rm = TRUE),
      n = n(),
      se = sd / sqrt(n),
      lower = mean_growth - 1.96 * se,
      upper = mean_growth + 1.96 * se
    )
  
  ggplot(summary_growth, aes(x = Year, y = mean_growth)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'skyblue', alpha = 0.3) +
    geom_line(color = 'steelblue', size = 1) +
    geom_point(color = 'steelblue') +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste('Average population growth rate in', country_name),
      y = 'Mean growth rate (±95% CI)',
      x = 'Year'
    )
}

plot_country_population_trend <- function(df, country_name) {
  if (nrow(df) == 0) return(NULL)
  
  # Aggregate population per year
  agg_pop <- tapply(df$Inhabitants * 1000, df$Year, sum)
  
  # Skip empty plots
  if (length(agg_pop) < 2) return(NULL)
  
  # Prepare axis labels
  # y_breaks <- seq(round(min(agg_pop), -4), round(max(agg_pop), -4), 2e4)
  # y_labels <- paste0(round(y_breaks / 1e3, 1), ' k')
  
  # Barplot with midpoint capture
  bp_x <- barplot(
    agg_pop,
    col = 'lightblue',
    border = NA,
    xlab = 'Years AD',
    # ylab = 'Population',
    # yaxt = 'n',
    main = paste('Population trends in', country_name),
    names.arg = names(agg_pop),
    las = 2,
    ylim = c(0, round(max(agg_pop), -3) + 1e4)
  )
  
  # Y-axis
  # axis(2, at = y_breaks, labels = y_labels)
  
  # Line over bars
  lines(bp_x, agg_pop, type = 'b', lwd = 2, col = 'steelblue', pch = 19)
}