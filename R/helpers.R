# ==============================
# Shared Chart Helpers (look only)
# ==============================

# Common look for every chart
apply_theme <- function(p, margin = list(l = 60, r = 30, b = 50, t = 20)) {
  p %>%
    layout(
      font          = list(family = "Inter, Segoe UI, sans-serif", color = "#475569"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      showlegend    = FALSE,
      margin        = margin,
      hoverlabel    = list(
        bgcolor = "#0f172a",
        font = list(color = "white", family = "Inter"),
        bordercolor = "rgba(0,0,0,0)"
      )
    ) %>%
    config(displayModeBar = FALSE)
}

grad_scale <- list(c(0, "#C7C3FF"), c(1, "#6C63FF"))

# Vertical bars (categories sorted low -> high, same as before)
pretty_vbar <- function(d, cat, val, ylab, hover, labels, tickangle = -30) {
  d <- d[order(d[[val]]), ]
  d[[cat]] <- factor(d[[cat]], levels = d[[cat]])
  
  plot_ly(
    x = d[[cat]], y = d[[val]], type = "bar",
    text = labels(d[[val]]), textposition = "outside", cliponaxis = FALSE,
    marker = list(color = d[[val]], colorscale = grad_scale,
                  line = list(width = 0)),
    hovertemplate = hover
  ) %>%
    layout(
      xaxis = list(title = "", tickangle = tickangle, showgrid = FALSE,
                   linecolor = "#e2e8f0"),
      yaxis = list(title = ylab, gridcolor = "#eef2f7", zeroline = FALSE,
                   tickformat = ".2s"),
      bargap = 0.35
    ) %>%
    apply_theme(list(l = 60, r = 20, b = 70, t = 30))
}

# Horizontal bars (highest at the top, same as before)
pretty_hbar <- function(d, cat, val, xlab, hover, labels) {
  d <- d[order(d[[val]]), ]
  d[[cat]] <- factor(d[[cat]], levels = d[[cat]])
  
  plot_ly(
    x = d[[val]], y = d[[cat]], type = "bar", orientation = "h",
    text = labels(d[[val]]), textposition = "outside", cliponaxis = FALSE,
    marker = list(color = d[[val]], colorscale = grad_scale,
                  line = list(width = 0)),
    hovertemplate = hover
  ) %>%
    layout(
      xaxis = list(title = xlab, gridcolor = "#eef2f7", zeroline = FALSE),
      yaxis = list(title = "", showgrid = FALSE, linecolor = "#e2e8f0"),
      bargap = 0.35
    ) %>%
    apply_theme(list(l = 100, r = 60, b = 50, t = 20))
}

# Smooth line with soft fill and highlighted peak
pretty_line <- function(d, x, y, xlab, ylab, hover,
                        dtick = NULL, tickangle = 0,
                        peak_label = function(v) format(round(v), big.mark = ",")) {
  
  p <- plot_ly() %>%
    add_trace(
      x = d[[x]], y = d[[y]],
      type = "scatter", mode = "lines+markers",
      fill = "tozeroy", fillcolor = "rgba(108, 99, 255, 0.10)",
      line = list(color = "#6C63FF", width = 3, shape = "spline"),
      marker = list(size = 8, color = "white",
                    line = list(color = "#6C63FF", width = 2.5)),
      hovertemplate = hover
    )
  
  # highlight the highest point (skipped if the filter leaves no data)
  if (nrow(d) > 0) {
    peak <- d[which.max(d[[y]]), ]
    p <- p %>%
      add_trace(
        x = peak[[x]], y = peak[[y]],
        type = "scatter", mode = "markers+text",
        marker = list(size = 14, color = "#FF5C8A",
                      line = list(color = "white", width = 3)),
        text = paste0("Peak: ", peak_label(peak[[y]])),
        textposition = "top center",
        textfont = list(color = "#FF5C8A", size = 12),
        hoverinfo = "skip", cliponaxis = FALSE
      )
  }
  
  xaxis <- list(title = xlab, showgrid = FALSE, zeroline = FALSE,
                linecolor = "#e2e8f0", tickangle = tickangle)
  if (!is.null(dtick)) xaxis$dtick <- dtick
  
  p %>%
    layout(
      xaxis = xaxis,
      yaxis = list(title = ylab, gridcolor = "#eef2f7", zeroline = FALSE,
                   tickformat = ".2s", rangemode = "tozero")
    ) %>%
    apply_theme(list(l = 60, r = 30, b = if (tickangle != 0) 90 else 50, t = 40))
}

# Label formatters used on the bars
fmt_short <- function(v) {
  ifelse(v >= 1e6, paste0(round(v / 1e6, 2), "M"),
         ifelse(v >= 1e3, paste0(round(v / 1e3, 1), "K"), round(v, 1)))
}
fmt_int  <- function(v) format(round(v), big.mark = ",")
fmt_1dp  <- function(v) sprintf("%.1f", v)
fmt_pct  <- function(v) sprintf("%.2f%%", v)