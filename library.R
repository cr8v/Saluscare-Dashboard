# Library ####
library(tidyverse)
library(reactable)
library(plotly)
library(hrbrthemes)
#library(data.table)
#library(bslib)
#library(htmltools)
#library(shiny)


# Function ####

## Reactable

f_reactable <- function(x, f_select = TRUE, searchable = TRUE, filterable = TRUE, ...){
  args <- list(
    x,
    defaultColDef = colDef(align = "right", headerVAlign = "center"),
    highlight = TRUE,
    resizable = TRUE,
    fullWidth = FALSE,
    borderless = TRUE,
    searchable = searchable,
    filterable = filterable
  )
  
  if (f_select == TRUE){
    args$onClick <- "select"
    args$selection <- "multiple"
  }
  
  do.call(reactable, c(args, list(...)))
}

### style ####
f_style_sticky <- function(value) { list(fontWeight = "bold",
                                         borderRight = "1px solid #eee") }
f_style_bold <- function(value) { list(fontWeight = "bold") }

f_style1 <- function(value) { if ((value %% 64 == 0) & (value != 0)) { list(fontWeight = "bold") } else if (value != 0) { list() } else { list() } }
f_style2 <- function(value) { if (value == 64) { list(color = "green", fontWeight = "bold") } else if (value != 0) { list(color = "red") } else { list() } }

f_style_colors <- function(value) {
  if (typeof(value)=="character"){
    value <- as.numeric(str_extract(value, "\\d+"))
    normalized <- (value - colors[1]) / (colors[2] - colors[1])
    color <- palette_blue(normalized)
  } else {
    normalized <- (value - colors[1]) / (colors[2] - colors[1])
    color <- palette_orange(normalized)
  }
  list(background = color)
}

f_colors <- function(var){ 
  if (typeof(var) == "character"){
    var <- as.numeric(str_extract(var, "\\d+"))
  }
  c(min(var), max(var))
}

### header
f_header = function(x) {
  paste0('function(column) {
        return column.name + `<div style="color: #737373"; font-size: 0.6em;>(',x,')</div>`}')
}

### palette

palette_orange <- function(x) rgb(colorRamp(c("#ffe4cc", "#ffb54d"))(x), maxColorValue = 255)
palette_blue <- function(x) {
  rgb(colorRamp(c("aliceblue", "lightsteelblue"))(x), maxColorValue = 255)
}



