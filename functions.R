library(purrr)
library(ggplot2)
library(dplyr)
library(farver)

# Functions ---------------------------------------------------------------

# From https://github.com/jgm/skylighting/blob/a1d02a0db6260c73aaf04aae2e6e18b569caacdc/skylighting-core/src/Skylighting/Format/HTML.hs#L117-L147
abbr <- c(
  "Keyword"        = "kw",
  "DataType"       = "dt",
  "DecVal"         = "dv",
  "BaseN"          = "bn",
  "Float"          = "fl",
  "Char"           = "ch",
  "String"         = "st",
  "Comment"        = "co",
  "Other"          = "ot",
  "Alert"          = "al",
  "Function"       = "fu",
  "RegionMarker"   = "re",
  "Error"          = "er",
  "Constant"       = "cn",
  "SpecialChar"    = "sc",
  "VerbatimString" = "vs",
  "SpecialString"  = "ss",
  "Import"         = "im",
  "Documentation"  = "do",
  "Annotation"     = "an",
  "CommentVar"     = "cv",
  "Variable"       = "va",
  "ControlFlow"    = "cf",
  "Operator"       = "op",
  "BuiltIn"        = "bu",
  "Extension"      = "ex",
  "Preprocessor"   = "pp",
  "Attribute"      = "at",
  "Information"    = "in",
  "Warning"        = "wa",
  "Normal"         = ""
)

read_theme <- function(path) {
  theme <- jsonlite::read_json(path)
  
  as_row <- function(x) {
    x %>%
      purrr::modify_if(is.null, ~ NA) %>%
      tibble::as_tibble()
  }
  
  df <- purrr::map_df(theme$`text-styles`, as_row, .id = "name")
  names(df)[names(df) == "text-color"] <- "fg"
  names(df)[names(df) == "background-color"] <- "bg"
  df$abbr <- unname(abbr[df$name])
  
  df[c("abbr", "fg", "bg", "bold", "italic", "underline")]
}

# From https://accessible-colors.com
rel_l <- function(x) {
  scale <- function(x) {
    ifelse(x <= 0.03928, 
           x / 12.92, 
           ((x + 0.055) / 1.055)^2.4)
  }
  rgb <- farver::decode_colour(x) / 255
  0.2126 * scale(rgb[, 1]) + 0.7152 * scale(rgb[, 2]) + 0.0722 * scale(rgb[, 3])
}

contrast_ratio <- function(x, y) {
  x_l <- rel_l(x)
  y_l <- rel_l(y)
  
  (pmax(x_l, y_l) + 0.05) / (pmin(x_l, y_l) + 0.05)
}

modify_hcl <- function(x, h, c, l) {
  hcl <- as.data.frame(farver::decode_colour(x, to = "hcl"))
  
  if (!missing(h)) {
    h <- eval(substitute(h), envir = hcl)
  } else {
    h <- hcl$h
  }
  if (!missing(c)) {
    c <- eval(substitute(c), envir = hcl)
  } else {
    c <- hcl$c
  }
  if (!missing(l)) {
    l <- eval(substitute(l), envir = hcl)
  } else {
    l <- hcl$l
  }
  
  farver::encode_colour(cbind(h, c, l), from = "hcl")
}

Luv <- function(x) {
  mat <- decode_colour(x, to = "Luv")
  set_names(as.data.frame(mat), c("L", "u", "v"))
}

hcl <- function(x) {
  mat <- decode_colour(x, to = "hcl")
  as.data.frame(mat)
}

# Specialised plots -------------------------------------------------------

plot_cl <- function(df) {
  df %>%
    mutate(hcl(fg)) %>%
    ggplot(aes(l, c)) +
    geom_point(aes(colour = fg), size = 10) +
    scale_colour_identity() +
    geom_text(aes(label = abbr), nudge_y = -5) +
    geom_vline(xintercept = 50) +  # guess
    theme(plot.background = element_rect(fill = bg)) +
    labs(x = "luminance", y = "chroma")
}
  
plot_uv <- function(df) {
  df %>%
    mutate(Luv(fg)) %>%
    ggplot(aes(u, v)) +
    geom_point(aes(colour = fg), size = 10) +
    geom_text(aes(label = abbr), nudge_y = -5) +
    scale_colour_identity() +
    coord_equal(xlim = c(-150, 150), ylim = c(-100, 100)) +
    labs(x = NULL, y = NULL) +
    theme(plot.background = element_rect(fill = bg)) +
    labs(x = "u = green/red", y = "v = blue/yellow") 
}




