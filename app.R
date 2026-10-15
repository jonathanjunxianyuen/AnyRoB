# =============================================================================
#  AnyRoB
#  Highly customizable traffic light and summary plots for any risk of bias
#  tool.
#
#  Install once:
#    install.packages(c("shiny", "bslib", "rhandsontable", "colourpicker",
#                       "ggplot2", "openxlsx", "ragg"))
#  Run:
#    shiny::runApp("path/to/folder/containing/app.R")
# =============================================================================

required <- c("shiny", "bslib", "rhandsontable", "colourpicker", "ggplot2", "gtable", "openxlsx")
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Please install the missing packages first:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))", call. = FALSE)
}

library(shiny)
library(bslib)
library(rhandsontable)
library(colourpicker)
library(ggplot2)
library(grid)

has_ragg <- requireNamespace("ragg", quietly = TRUE)
if (has_ragg) options(shiny.useragg = TRUE)

# -----------------------------------------------------------------------------
# Citation: shown on the home page and next to the downloads, and offered as
# .ris / .nbib / .bib files for reference managers.
# -----------------------------------------------------------------------------
APP_AUTHOR <- "Jonathan Jun Xian, Yuen"     # as displayed in the citation
APP_FAMILY <- "Yuen"                         # used by reference managers
APP_GIVEN <- "Jonathan Jun Xian"
APP_INITIALS <- "JJX"
APP_TITLE <- "AnyRoB: A highly customizable R Shiny web application for generating risk-of-bias traffic-light and summary plots for any assessment tool"
APP_VERSION <- "1.0"
APP_YEAR <- "2026"
APP_URL <- "https://jonathanyuen.shinyapps.io/AnyRoB/"
app_citation <- function() {
  paste0(APP_AUTHOR, ". ", APP_TITLE, " [computer software]. Version ", APP_VERSION, ". ", APP_YEAR, ".",
         if (nzchar(APP_URL)) paste0(" Available at: ", APP_URL) else "")
}
citation_file <- function(fmt) {
  author <- paste0(APP_FAMILY, ", ", APP_GIVEN)
  lines <- switch(fmt,
                  ris = c("TY  - COMP", paste0("AU  - ", author), paste0("TI  - ", APP_TITLE), paste0("PY  - ", APP_YEAR),
                          paste0("ET  - Version ", APP_VERSION), "M3  - Computer software (R Shiny web application)",
                          if (nzchar(APP_URL)) paste0("UR  - ", APP_URL), "ER  - "),
                  nbib = c(paste0("TI  - ", APP_TITLE), paste0("FAU - ", author), paste0("AU  - ", APP_FAMILY, " ", APP_INITIALS),
                           paste0("DP  - ", APP_YEAR), "PT  - Software", "LA  - eng",
                           paste0("SO  - ", APP_TITLE, " [computer software]. Version ", APP_VERSION, ". ", APP_YEAR, ".",
                                  if (nzchar(APP_URL)) paste0(" Available at: ", APP_URL) else "")),
                  bib = c(paste0("@misc{", tolower(APP_FAMILY), APP_YEAR, "anyrob,"),
                          paste0("  author = {", APP_FAMILY, ", ", APP_GIVEN, "},"),
                          paste0("  title = {{", APP_TITLE, "}},"),
                          paste0("  year = {", APP_YEAR, "},"),
                          paste0("  note = {Computer software, version ", APP_VERSION, "},"),
                          if (nzchar(APP_URL)) paste0("  howpublished = {\\url{", APP_URL, "}},"),
                          "}"))
  paste0(paste(lines, collapse = "\r\n"), "\r\n")
}

PX_PER_CM <- 96 / 2.54          # screen preview is drawn at true size (96 dpi)
CM_PER_PT <- 2.54 / 72

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

nz <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) NULL else x
}

blank_str <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

to_hex <- function(x, default = "#BDBDBD") {
  vapply(as.character(x), function(v) {
    if (is.na(v) || !nzchar(trimws(v))) return(default)
    tryCatch(grDevices::rgb(t(grDevices::col2rgb(trimws(v))), maxColorValue = 255),
             error = function(e) default)
  }, character(1), USE.NAMES = FALSE)
}

wrap_text <- function(x, width) {
  vapply(x, function(s) paste(strwrap(s, width = max(4, width)), collapse = "\n"),
         character(1), USE.NAMES = FALSE)
}

# Approximate width of text in cm (average glyph is about half the font size)
text_w_cm <- function(txt, fs) {
  if (!length(txt)) return(0)
  max(nchar(strsplit(paste(txt, collapse = "\n"), "\n", fixed = TRUE)[[1]]), 0) * fs * 0.56 * CM_PER_PT
}

make_domains <- function(group, group_name, label, name) {
  data.frame(Group = group, GroupName = group_name, Label = label, Name = name,
             stringsAsFactors = FALSE)
}
empty_domains <- function() make_domains(character(0), character(0), character(0), character(0))

make_judgements <- function(name, fill, symbol, symbol_colour, in_legend = rep(TRUE, length(name))) {
  data.frame(Name = name, Fill = fill, Symbol = symbol,
             SymbolColour = symbol_colour, InLegend = as.logical(in_legend), stringsAsFactors = FALSE)
}

# Accepts the 4-column layout, or the older 3-column one (group, label, name)
clean_domains <- function(d) {
  if (is.null(d) || !nrow(d) || ncol(d) < 3) return(empty_domains())
  d <- if (ncol(d) >= 4) {
    make_domains(blank_str(d[[1]]), blank_str(d[[2]]), blank_str(d[[3]]), blank_str(d[[4]]))
  } else {
    make_domains(blank_str(d[[1]]), "", blank_str(d[[2]]), blank_str(d[[3]]))
  }
  d$Label <- ifelse(d$Label == "", d$Name, d$Label)   # no label? use the name
  d[d$Label != "", , drop = FALSE]
}

# The domain editor works on a tree: a list of domains, each with a label,
# a title and a list of subdomains. Everything else uses the flat table
# (Group = domain label, GroupName = domain title, Label/Name = the column).
tree_from_domains <- function(d, next_id) {
  if (!nrow(d)) return(list())
  gk <- ifelse(nzchar(d$Group), d$Group, d$GroupName)
  r <- rle(gk)
  starts <- cumsum(r$lengths) - r$lengths + 1
  out <- list()
  for (i in seq_along(r$values)) {
    idx <- starts[i]:(starts[i] + r$lengths[i] - 1)
    if (nzchar(r$values[i])) {
      nm <- d$GroupName[idx][nzchar(d$GroupName[idx])]
      subs <- lapply(idx, function(k) list(id = next_id(), label = d$Label[k], title = d$Name[k]))
      out[[length(out) + 1]] <- list(id = next_id(), label = d$Group[idx[1]],
                                     title = if (length(nm)) nm[1] else "", subs = subs)
    } else {
      for (k in idx) out[[length(out) + 1]] <- list(id = next_id(), label = d$Label[k], title = d$Name[k], subs = list())
    }
  }
  out
}

domains_from_tree <- function(tree, use_sub) {
  rows <- lapply(tree, function(dm) {
    subs <- Filter(function(x) nzchar(x$label) || nzchar(x$title), dm$subs)
    if (use_sub && length(subs)) {
      make_domains(rep(dm$label, length(subs)), rep(dm$title, length(subs)),
                   vapply(subs, `[[`, "", "label"), vapply(subs, `[[`, "", "title"))
    } else {
      make_domains("", "", dm$label, dm$title)
    }
  })
  if (!length(rows)) return(empty_domains())
  clean_domains(do.call(rbind, rows))
}

# Suggest the next free label, e.g. D5, or 3.4 under a domain labelled D3
next_label <- function(prefix, taken, start) {
  k <- start
  while (paste0(prefix, k) %in% taken) k <- k + 1
  paste0(prefix, k)
}

# Every column needs a unique ID. Subdomain labels only have to be unique within
# their domain, so the ID combines the domain and subdomain labels (e.g. "D2::1").
domain_keys <- function(d) {
  gk <- ifelse(nzchar(d$Group), d$Group, d$GroupName)
  ifelse(nzchar(gk), paste0(gk, "::", d$Label), d$Label)
}

validate_domains <- function(d) {
  d$Key <- domain_keys(d)
  d <- d[!(d$Key %in% c("Study", ".overall")), , drop = FALSE]
  d <- d[!duplicated(d$Key), , drop = FALSE]
  # A group name typed once applies to the whole group
  key <- ifelse(nzchar(d$Group), d$Group, d$GroupName)
  for (g in unique(key[nzchar(key)])) {
    nm <- d$GroupName[key == g & nzchar(d$GroupName)]
    if (length(nm)) d$GroupName[key == g] <- nm[1]
  }
  d
}

clean_judgements <- function(j) {
  if (is.null(j) || !nrow(j)) return(make_judgements(character(0), character(0), character(0), character(0)))
  make_judgements(blank_str(j$Name), to_hex(j$Fill),
                  blank_str(j$Symbol), to_hex(j$SymbolColour, "#000000"),
                  if (is.null(j$InLegend)) rep(TRUE, nrow(j)) else !(j$InLegend %in% FALSE))
}

blank_rows <- function(cols, n) {
  df <- as.data.frame(matrix(NA_character_, nrow = n, ncol = length(cols)),
                      stringsAsFactors = FALSE)
  names(df) <- cols
  df
}

# Keep existing judgements when the domain structure changes.
# Matching is by label; if the number of domains is unchanged, a renamed
# domain keeps its data by position.
conform_assess <- function(a, labels, overall) {
  if (is.null(a) || !ncol(a)) a <- data.frame(Study = character(0), stringsAsFactors = FALSE)
  old_cols <- setdiff(names(a), c("Study", ".overall"))
  out <- data.frame(Study = as.character(a$Study), stringsAsFactors = FALSE)
  for (k in seq_along(labels)) {
    lab <- labels[k]
    v <- if (lab %in% old_cols) {
      a[[lab]]
    } else if (length(old_cols) == length(labels) && !(old_cols[k] %in% labels)) {
      a[[old_cols[k]]]
    } else {
      rep(NA_character_, nrow(a))
    }
    out[[lab]] <- as.character(v)
  }
  if (overall) {
    out[[".overall"]] <- if (".overall" %in% names(a)) as.character(a[[".overall"]]) else rep(NA_character_, nrow(a))
  }
  out
}

col_header <- function(group, label) {
  esc <- htmltools::htmlEscape
  sprintf("<div class='hdr-group' title='%s'>%s</div><div class='hdr-label'>%s</div>",
          esc(group, attribute = TRUE), if (nzchar(group)) esc(group) else "&nbsp;", esc(label))
}

# Colours each spreadsheet cell by its judgement; unknown values turn red.
cell_renderer <- function(jv) {
  json <- if (nrow(jv)) {
    as.character(jsonlite::toJSON(as.list(setNames(paste0(jv$Fill, "66"), jv$Name)), auto_unbox = TRUE))
  } else "{}"
  sprintf("function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.AutocompleteRenderer.apply(this, arguments);
    var map = %s;
    td.style.background = '';
    td.style.color = '';
    if (value !== null && value !== undefined && value !== '') {
      if (Object.prototype.hasOwnProperty.call(map, value)) {
        td.style.background = map[value]; td.style.color = '#1d2328';
      } else {
        td.style.background = '#F8D7DA'; td.style.color = '#842029';
      }
    }
    return td;
  }", json)
}

# -----------------------------------------------------------------------------
# Plot data: one tidy structure shared by the live preview and final plots
# -----------------------------------------------------------------------------
build_plot_data <- function(d, jv, a, overall_label = "Overall") {
  a <- a[!is.na(a$Study) & nzchar(trimws(a$Study)), , drop = FALSE]
  a$Study <- make.unique(trimws(a$Study), sep = " #")
  keys <- c(d$Key[d$Key %in% names(a)], if (".overall" %in% names(a)) ".overall")
  m <- match(keys, d$Key)
  is_ov <- keys == ".overall"
  info <- data.frame(
    key = keys,
    Label = ifelse(is_ov, overall_label, d$Label[m]),
    Name = ifelse(is_ov, "", d$Name[m]),
    Group = ifelse(is_ov, "", d$Group[m]),
    GroupName = ifelse(is_ov, "", d$GroupName[m]),
    stringsAsFactors = FALSE)
  info$gkey <- ifelse(nzchar(info$Group), info$Group, info$GroupName)
  long <- do.call(rbind, lapply(keys, function(k) {
    data.frame(Study = a$Study, key = k, Judgement = as.character(a[[k]]), stringsAsFactors = FALSE)
  }))
  long$Judgement[!(long$Judgement %in% jv$Name)] <- NA
  list(long = long, info = info, studies = a$Study, jv = jv,
       has_groups = any(nzchar(info$gkey)))
}

placeholder_assess <- function(d, jv, n, overall) {
  a <- data.frame(Study = paste("Study", seq_len(n)), stringsAsFactors = FALSE)
  nj <- max(1, nrow(jv))
  for (k in seq_along(d$Key)) {
    a[[d$Key[k]]] <- if (nrow(jv)) jv$Name[((seq_len(n) * 5 + k * 3 + (seq_len(n) * k) %% 5) %% nj) + 1] else NA
  }
  if (overall) a[[".overall"]] <- if (nrow(jv)) jv$Name[(seq_len(n) %% nj) + 1] else NA
  a
}

# -----------------------------------------------------------------------------
# Drawing: traffic light table + domain key + judgement legend
# All positions are in centimetres so the preview matches the exported file.
# -----------------------------------------------------------------------------
# Build ggplot layouts on a private, freshly started off-screen device, then
# switch back to whatever device was current. Shiny asks for a plot's size
# before it opens its own device, so otherwise the figure can be built on
# whatever device happens to be current (e.g. RStudio's plot pane), whose grid
# state R 4.6 may not have set up ("VECTOR_ELT() can only be applied to a
# 'list', not a 'NULL'").
with_build_device <- function(expr) {
  old <- grDevices::dev.cur()
  if (has_ragg) {
    ragg::agg_png(tempfile(fileext = ".png"), width = 20, height = 20, units = "cm", res = 72)
  } else {
    grDevices::pdf(NULL)
  }
  dev <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(dev)
    if (old > 1 && old %in% grDevices::dev.list()) grDevices::dev.set(old)
  }, add = TRUE)
  grid::grid.newpage()
  force(expr)
}

# A legend marker; x, y and r are plain numbers in cm
marker_grob <- function(shape, x, y, r, fill, col, lwd = 0.6) {
  gp <- gpar(fill = fill, col = if (is.na(col)) NA else col, lwd = if (is.na(col)) 0 else lwd * .stroke / 2)
  cm <- function(v) unit(v, "cm")
  switch(as.character(shape),
         "22" = rectGrob(cm(x), cm(y), width = cm(2 * r), height = cm(2 * r), gp = gp),
         "23" = polygonGrob(x = cm(c(x, x + r, x, x - r)), y = cm(c(y + r, y, y - r, y)), gp = gp),
         circleGrob(cm(x), cm(y), r = cm(r), gp = gp))
}

key_lines <- function(info, title) {
  dom <- info[info$key != ".overall", , drop = FALSE]
  out <- data.frame(text = character(0), face = character(0), indent = numeric(0), stringsAsFactors = FALSE)
  add <- function(t, f, i) out <<- rbind(out, data.frame(text = t, face = f, indent = i, stringsAsFactors = FALSE))
  if (nzchar(title %||% "")) add(title, "bold", 0)
  last <- NULL
  for (i in seq_len(nrow(dom))) {
    g <- dom$gkey[i]
    if (nzchar(g)) {
      if (!identical(g, last)) {
        gl <- if (nzchar(dom$Group[i]) && nzchar(dom$GroupName[i])) paste0(dom$Group[i], ": ", dom$GroupName[i]) else g
        add(gl, "plain", 0)
      }
      add(if (nzchar(dom$Name[i])) paste(dom$Label[i], dom$Name[i]) else dom$Label[i], "plain", 0.4)
    } else {
      add(if (nzchar(dom$Name[i])) paste0(dom$Label[i], ": ", dom$Name[i]) else dom$Label[i], "plain", 0)
    }
    last <- g
  }
  out
}

# Judgements to list in the traffic light legend
legend_judgements <- function(jv) {
  if (is.null(jv$InLegend)) jv else jv[!(jv$InLegend %in% FALSE), , drop = FALSE]
}

# Estimated widths (cm) of the domain key and the judgement legend
footer_widths <- function(pd, s) {
  fs <- s$base_size * 0.92
  key_w <- 0
  if (isTRUE(s$show_key)) {
    kl <- key_lines(pd$info, s$key_title)
    if (nrow(kl)) key_w <- max(vapply(seq_len(nrow(kl)), function(i) text_w_cm(kl$text[i], fs) + kl$indent[i], numeric(1)))
  }
  lh <- fs * 1.55 * CM_PER_PT
  lj <- legend_judgements(pd$jv)
  leg_w <- if (s$legend_pos != "none" && nrow(lj)) 2 * min(0.21, lh * 0.42) + 0.2 + text_w_cm(c(lj$Name, s$legend_title), fs) else 0
  list(key = key_w, legend = leg_w)
}

footer_parts <- function(pd, s, ox = 0.3, right = NULL) {
  # Everything is laid out as plain numbers (cm from the top-left of the footer).
  # The only thing measured from the real font is the legend's width when it is
  # lined up with the table's right edge; that happens at draw time in
  # makeContent.anyrob_footer, so no compound grid units are needed.
  fs <- s$base_size * 0.92
  lh <- fs * 1.55 * CM_PER_PT
  gp <- function(face = "plain", f = fs, col = "black") gpar(fontsize = f, fontface = face, fontfamily = s$family, col = col)
  pad <- 0.3
  fixed <- list()    # items at a fixed position: list(kind, x, y, ...)
  legend <- list()   # legend items: x is an offset from the legend's left edge
  key_w <- 0; key_h <- 0
  if (isTRUE(s$show_key)) {
    kl <- key_lines(pd$info, s$key_title)
    for (i in seq_len(nrow(kl))) {
      fixed[[length(fixed) + 1]] <- list(kind = "text", label = kl$text[i], x = ox + kl$indent[i],
                                         y = pad + (i - 1) * lh, just = c("left", "top"), gp = gp(kl$face[i]))
    }
    if (nrow(kl)) {
      key_w <- max(vapply(seq_len(nrow(kl)), function(i) text_w_cm(kl$text[i], fs) + kl$indent[i], numeric(1)))
      key_h <- nrow(kl) * lh
    }
  }
  leg_w <- 0; leg_h <- 0; lx_cm <- ox; ly <- 0; est_w <- 0; align_right <- FALSE
  jv <- legend_judgements(pd$jv)   # only judgements ticked "Show in legend"
  measure <- list()
  if (s$legend_pos != "none" && nrow(jv)) {
    r <- min(0.21, lh * 0.42)
    step <- lh * 1.2
    has_title <- nzchar(s$legend_title %||% "")
    est_w <- 2 * r + 0.2 + text_w_cm(c(jv$Name, s$legend_title), fs)
    beside <- s$legend_pos == "beside"
    ly <- if (!beside && key_h > 0) key_h + 0.35 else 0
    # Beside the key, the legend's right edge lines up with the table's right edge
    align_right <- beside && key_w > 0 && !is.null(right) && (right - est_w) >= (ox + key_w + 0.8 - 1e-6)
    lx_cm <- if (!beside || key_w == 0) ox else if (align_right) right - est_w else ox + key_w + 1.2
    row <- 0
    if (has_title) {
      legend[[length(legend) + 1]] <- list(kind = "text", label = s$legend_title, x = 0, y = pad + ly,
                                           just = c("left", "top"), gp = gp("bold"))
      measure[[length(measure) + 1]] <- list(label = s$legend_title, gp = gp("bold"), extra = 0)
      row <- 1
    }
    for (k in seq_len(nrow(jv))) {
      yc <- pad + ly + row * lh + (k - 1) * step + step / 2
      legend[[length(legend) + 1]] <- list(kind = "marker", x = r, y = yc, r = r, fill = jv$Fill[k])
      if (nzchar(jv$Symbol[k])) {
        legend[[length(legend) + 1]] <- list(kind = "text", label = jv$Symbol[k], x = r, y = yc, just = "centre",
                                             gp = gp("bold", fs * 0.85, jv$SymbolColour[k]))
      }
      legend[[length(legend) + 1]] <- list(kind = "text", label = jv$Name[k], x = 2 * r + 0.2, y = yc,
                                           just = c("left", "centre"), gp = gp())
      measure[[length(measure) + 1]] <- list(label = jv$Name[k], gp = gp(), extra = 2 * r + 0.2)
    }
    leg_w <- (lx_cm - ox) + est_w
    leg_h <- ly + row * lh + nrow(jv) * step
  }
  extra_h <- 0
  if (!is.null(nz(s$footer))) {
    fixed[[length(fixed) + 1]] <- list(kind = "text", label = s$footer, x = ox, y = pad + max(key_h, leg_h) + 0.3,
                                       just = c("left", "top"), gp = gp())
    extra_h <- 0.3 + length(strsplit(s$footer, "\n", fixed = TRUE)[[1]]) * lh
  }
  h <- max(key_h, leg_h) + extra_h
  height <- if (h > 0) h + pad + 0.25 else 0
  key_box <- if (key_h > 0) c(l = ox, t = pad, r = ox + key_w, b = pad + key_h) else NULL
  leg_box <- if (leg_h > 0) c(l = lx_cm, t = pad + ly, r = lx_cm + est_w, b = pad + leg_h) else NULL
  grob <- gTree(fixed = fixed, legend = legend, measure = measure, lx = lx_cm,
                right = if (align_right) right else NA_real_, height = height,
                shape = s$shape, outline = s$outline, outline_lwd = s$outline_lwd %||% 0.6,
                cl = "anyrob_footer")
  list(grob = grob,
       w = ox + max(key_w, leg_w, if (extra_h > 0) text_w_cm(s$footer, fs) else 0),
       h = height, key_box = key_box, leg_box = leg_box)
}

# Draw the footer. Runs on the device being drawn to, so the right-aligned
# legend is placed using that device's real font widths (as plain numbers).
makeContent.anyrob_footer <- function(x) {
  H <- x$height
  lx <- x$lx
  if (!is.na(x$right) && length(x$measure)) {
    widths <- vapply(x$measure, function(m) {
      convertWidth(grobWidth(textGrob(m$label, gp = m$gp)), "cm", valueOnly = TRUE) + m$extra
    }, numeric(1))
    lx <- x$right - max(widths)
  }
  draw_item <- function(it, x0) {
    cx <- unit(x0 + it$x, "cm"); cy <- unit(H - it$y, "cm")
    if (it$kind == "text") return(textGrob(it$label, x = cx, y = cy, just = it$just, gp = it$gp))
    marker_grob(x$shape, x0 + it$x, H - it$y, it$r, it$fill, x$outline, x$outline_lwd)
  }
  kids <- c(lapply(x$fixed, draw_item, x0 = 0), lapply(x$legend, draw_item, x0 = lx))
  setChildren(x, do.call(gList, kids))
}
registerS3method("makeContent", "anyrob_footer", makeContent.anyrob_footer, envir = asNamespace("grid"))

draw_traffic <- function(pd, s) {
  info <- pd$info; jv <- pd$jv; n <- length(pd$studies)
  fs <- s$base_size
  has_ov <- ".overall" %in% info$key
  m <- sum(info$key != ".overall")
  cw <- s$col_w; rh <- s$row_h; hh <- s$header_h
  gap <- 0
  sw <- if (is.na(s$study_w %||% NA)) max(1.6, text_w_cm(pd$studies, fs) + 0.6) else s$study_w
  ov_w <- max(cw, text_w_cm(info$Label[info$key == ".overall"], fs * 1.05) + 0.4)
  # Optionally widen domain columns so the legend can sit flush with the table's right edge
  if (isTRUE(s$fit_footer) && s$legend_pos == "beside" && isTRUE(s$show_key) && m > 0) {
    fw <- footer_widths(pd, s)
    need <- fw$key + 0.9 + fw$legend
    have <- sw + m * cw + if (has_ov) ov_w else 0
    if (fw$key > 0 && have < need) {
      ov_text <- if (has_ov) text_w_cm(info$Label[info$key == ".overall"], fs * 1.05) + 0.4 else 0
      while (cw < 4 && sw + m * cw + (if (has_ov) max(cw, ov_text) else 0) < need) cw <- cw + 0.01
      ov_w <- max(cw, ov_text)
    }
  }
  # Column widths: widen a domain's columns if its heading would not fit above them
  widths <- ifelse(info$key == ".overall", ov_w, cw)
  if (pd$has_groups) {
    r <- rle(info$gkey)
    ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
    for (k in seq_along(r$values)) if (nzchar(r$values[k]) && r$values[k] != " ") {
      idx <- starts[k]:ends[k]
      need <- text_w_cm(r$values[k], fs * 0.95 * 1.12) + 0.35     # bold heading plus padding
      have <- sum(widths[idx])
      if (have < need && !grepl(" ", r$values[k])) widths[idx] <- widths[idx] + (need - have) / length(idx)
    }
  }
  info$x0 <- c(0, cumsum(widths))[seq_along(widths)]
  info$x1 <- info$x0 + widths
  top_body <- n * rh
  n_hdr <- if (pd$has_groups) 2 else 1
  top_hdr <- top_body + n_hdr * hh
  hdr_fs <- fs * 0.95
  fit <- function(txt, span) wrap_text(txt, floor((span - 0.1) / (hdr_fs * 0.55 * CM_PER_PT)))
  
  # Rectangles: body cells, study cells, header cells
  long <- pd$long
  long$row <- match(long$Study, pd$studies)
  long$x0 <- info$x0[match(long$key, info$key)]
  long$x1 <- info$x1[match(long$key, info$key)]
  long$cx <- (long$x0 + long$x1) / 2
  long$cy <- (n - long$row + 0.5) * rh
  body <- data.frame(x0 = long$x0, x1 = long$x1, y0 = (n - long$row) * rh,
                     y1 = (n - long$row + 1) * rh, fill = s$cell_fill)
  studies <- data.frame(x0 = -sw, x1 = 0, y0 = (n - seq_len(n)) * rh, y1 = (n - seq_len(n) + 1) * rh,
                        fill = s$header_fill)
  spans_both <- pd$has_groups & !nzchar(info$gkey)
  sub <- data.frame(x0 = info$x0, x1 = info$x1, y0 = top_body,
                    y1 = ifelse(spans_both, top_hdr, top_body + hh), fill = s$header_fill,
                    label = fit(info$Label, info$x1 - info$x0))
  sub$ly <- (sub$y0 + sub$y1) / 2
  grp <- NULL
  if (pd$has_groups) {
    r <- rle(info$gkey)
    ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
    keep <- nzchar(r$values)
    if (any(keep)) {
      grp <- data.frame(x0 = info$x0[starts[keep]], x1 = info$x1[ends[keep]],
                        y0 = top_body + hh, y1 = top_hdr, fill = s$header_fill)
      grp$label <- fit(r$values[keep], grp$x1 - grp$x0)
    }
  }
  rects <- rbind(body, studies, sub[, c("x0", "x1", "y0", "y1", "fill")],
                 if (!is.null(grp)) grp[, c("x0", "x1", "y0", "y1", "fill")])
  
  filled <- long[!is.na(long$Judgement), , drop = FALSE]
  filled$fillcol <- jv$Fill[match(filled$Judgement, jv$Name)]
  filled$symcol <- jv$SymbolColour[match(filled$Judgement, jv$Name)]
  filled$sym <- jv$Symbol[match(filled$Judgement, jv$Name)]
  empty <- long[is.na(long$Judgement), , drop = FALSE]
  tsize <- fs / .pt
  outline <- if (is.na(s$outline)) "transparent" else s$outline
  ytitle <- !is.null(nz(s$ylab))
  head_title <- !is.null(nz(s$header_title))
  xmin <- -sw - if (ytitle) 0.75 else 0.05
  xmax <- max(info$x1) + 0.05
  ymax <- top_hdr + if (head_title) 0.65 else 0.05
  
  p <- ggplot() +
    geom_rect(data = rects, aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
              fill = rects$fill, colour = s$grid_col, linewidth = 0.35) +
    geom_text(data = sub, aes(x = (x0 + x1) / 2, y = ly, label = label),
              size = hdr_fs / .pt, fontface = "bold", family = s$family, lineheight = 0.9) +
    geom_text(data = data.frame(y = (n - seq_len(n) + 0.5) * rh, label = pd$studies),
              aes(x = -sw + 0.25, y = y, label = label), hjust = 0,
              size = tsize, family = s$family)
  if (!is.null(grp)) {
    p <- p + geom_text(data = grp, aes(x = (x0 + x1) / 2, y = (y0 + y1) / 2, label = label),
                       size = hdr_fs / .pt, fontface = "bold", family = s$family, lineheight = 0.9)
  }
  if (nrow(empty) && isTRUE(s$show_empty)) {
    p <- p + geom_point(data = empty, aes(cx, cy), shape = s$shape, size = s$point_size,
                        fill = NA, colour = "grey80", stroke = 0.5)
  }
  if (nrow(filled)) {
    p <- p +
      geom_point(data = filled, aes(cx, cy, fill = fillcol), shape = s$shape, size = s$point_size,
                 colour = outline, stroke = if (is.na(s$outline)) 0 else (s$outline_lwd %||% 0.6)) +
      geom_text(data = filled, aes(cx, cy, label = sym, colour = symcol),
                size = s$symbol_size, fontface = "bold", family = s$family)
  }
  if (head_title) {
    p <- p + annotate("text", x = (0 + max(info$x1)) / 2, y = top_hdr + 0.32, label = s$header_title,
                      size = (fs + 1) / .pt, fontface = "bold", family = s$family)
  }
  if (ytitle) {
    p <- p + annotate("text", x = -sw - 0.4, y = top_body / 2, label = s$ylab, angle = 90,
                      size = fs / .pt, fontface = "bold", family = s$family)
  }
  p <- p +
    scale_fill_identity() + scale_colour_identity() +
    coord_fixed(ratio = 1, xlim = c(xmin, xmax), ylim = c(-0.05, ymax), expand = FALSE, clip = "off") +
    labs(title = nz(s$title), tag = nz(s$tag)) +
    theme_void(base_size = fs, base_family = s$family) +
    theme(plot.margin = margin(0.15, 0.2, 0.1, 0.2, "cm"),
          plot.title = element_text(face = "bold", size = fs + 2, hjust = 0.5, margin = margin(b = 6)),
          plot.tag = element_text(face = "bold", size = fs * 2.6, hjust = 0, vjust = 1),
          plot.tag.position = c(0.004, 0.996),
          plot.background = element_rect(fill = "white", colour = NA))
  
  title_h <- if (!is.null(nz(s$title))) (fs + 2) * 1.6 * CM_PER_PT else 0
  tw <- (xmax - xmin) + 0.4
  th <- (ymax + 0.05) + 0.25 + title_h + 0.05   # a little slack so width, not height, sets the scale
  left_edge <- 0.2 + (-sw - xmin)                # left edge of the study column
  right_edge <- 0.2 + (max(info$x1) - xmin)      # right edge of the last column
  ft <- footer_parts(pd, s, ox = left_edge, right = right_edge)
  W <- max(tw, ft$w + 0.2)
  H <- th + ft$h
  gt <- gtable::gtable(widths = unit(c(tw, max(0.001, W - tw)), "cm"),
                       heights = unit(c(th, max(0.001, ft$h)), "cm"))
  gt <- gtable::gtable_add_grob(gt, rectGrob(gp = gpar(fill = "white", col = NA)), t = 1, l = 1, b = 2, r = 2,
                                name = "bg")
  # Fix the table panel to its exact size in cm and left/top-align it, so the
  # footer can line up with the table edges precisely
  g <- with_build_device(ggplotGrob(p))
  pan <- g$layout[g$layout$name == "panel", ]
  g$widths[pan$l] <- unit(xmax - xmin, "cm")
  g$heights[pan$t] <- unit(ymax + 0.05, "cm")
  g$respect <- FALSE
  g <- gtable::gtable_add_cols(g, unit(1, "null"))
  g <- gtable::gtable_add_rows(g, unit(1, "null"))
  gt <- gtable::gtable_add_grob(gt, g, t = 1, l = 1, r = 2, name = "table")
  if (ft$h > 0) gt <- gtable::gtable_add_grob(gt, ft$grob, t = 2, l = 1, r = 2, name = "footer")
  # Regions (cm from the figure's top-left) used by the help pictures
  ytop0 <- 0.15 + title_h
  to_x <- function(x) 0.2 + (x - xmin)
  to_y <- function(y) ytop0 + (ymax - y)
  shift <- function(b) if (is.null(b)) NULL else b + c(0, th, 0, th)
  regions <- list(
    headers = c(l = to_x(0), t = to_y(ymax), r = to_x(max(info$x1)), b = to_y(top_body)),
    cells = c(l = to_x(0), t = to_y(top_body), r = to_x(max(info$x1)), b = to_y(0)),
    studies = c(l = to_x(-sw), t = to_y(top_body), r = to_x(0), b = to_y(0)),
    key = shift(ft$key_box),
    legend = shift(ft$leg_box))
  list(grob = gt, w = W, h = H, regions = regions)
}

# Draw a figure with numbered highlight boxes over chosen regions.
# A margin of `m` cm is left around the figure so the badges are never clipped.
HELP_MARGIN <- 0.5
draw_highlight <- function(fig, boxes, m = HELP_MARGIN) {
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  pushViewport(viewport(x = unit(m, "cm"), y = unit(fig$h + m, "cm"),
                        width = unit(fig$w, "cm"), height = unit(fig$h, "cm"), just = c("left", "top")))
  grid.draw(fig$grob)
  for (i in seq_along(boxes)) {
    b <- boxes[[i]]
    if (is.null(b)) next
    pad <- 0.1
    grid.rect(x = unit(b[["l"]] - pad, "cm"), y = unit(fig$h - b[["b"]] - pad, "cm"),
              width = unit(b[["r"]] - b[["l"]] + 2 * pad, "cm"),
              height = unit(b[["b"]] - b[["t"]] + 2 * pad, "cm"), just = c("left", "bottom"),
              gp = gpar(fill = grDevices::adjustcolor("#FFB000", 0.12), col = "#E8590C", lwd = 2.5))
    cx <- unit(b[["l"]] - pad, "cm"); cy <- unit(fig$h - b[["t"]] + pad, "cm")
    grid.circle(cx, cy, r = unit(0.3, "cm"), gp = gpar(fill = "#E8590C", col = "white", lwd = 2))
    grid.text(as.character(i), cx, cy, gp = gpar(col = "white", fontface = "bold", fontsize = 11))
  }
  popViewport()
}

# A ggplot wrapped so that its layout is worked out when it is drawn, using the
# fonts of the device it is drawn on (PNG, TIFF, PDF or screen). Building it
# earlier measures text with whatever device happens to be open, which can
# leave legend labels overlapping their neighbours.
lazy_ggplot_grob <- function(p) gTree(plot = p, cl = "lazy_ggplot")
registerS3method("makeContent", "lazy_ggplot", function(x) {
  setChildren(x, gList(ggplotGrob(x$plot)))
}, envir = asNamespace("grid"))

# -----------------------------------------------------------------------------
# Drawing: summary bar chart
# -----------------------------------------------------------------------------
draw_summary <- function(pd, s, width) {
  jv <- pd$jv; info <- pd$info; long <- pd$long
  if (!isTRUE(s$include_overall)) {
    info <- info[info$key != ".overall", , drop = FALSE]
    long <- long[long$key != ".overall", , drop = FALSE]
  }
  long <- long[!is.na(long$Judgement), , drop = FALSE]
  if (!nrow(long)) return(NULL)
  fs <- s$base_size
  tab <- as.data.frame(table(key = factor(long$key, levels = info$key),
                             Judgement = factor(long$Judgement, levels = jv$Name)),
                       stringsAsFactors = FALSE)
  tab <- tab[tab$Freq > 0, , drop = FALSE]
  tab$prop <- tab$Freq / ave(tab$Freq, tab$key, FUN = sum)
  tab <- tab[order(match(tab$key, info$key), match(tab$Judgement, jv$Name)), , drop = FALSE]
  tab$mid <- ave(tab$prop, tab$key, FUN = cumsum) - tab$prop / 2
  tab$symcol <- jv$SymbolColour[match(tab$Judgement, jv$Name)]
  
  # Repeated subdomain labels (e.g. "1" under two domains) get their domain added
  dup <- if (any(duplicated(info$Label))) nzchar(info$gkey) else rep(FALSE, nrow(info))
  info$Label[dup] <- paste(info$gkey[dup], info$Label[dup])
  nm <- wrap_text(ifelse(nzchar(info$Name), info$Name, info$Label), s$sum_wrap)
  disp <- switch(s$label_type,
                 stack = ifelse(nzchar(info$Name), paste0(info$Label, "\n", nm), info$Label),
                 both = ifelse(nzchar(info$Name), wrap_text(paste0(info$Label, ": ", info$Name), s$sum_wrap), info$Label),
                 short = info$Label,
                 full = nm)
  tab$y <- factor(tab$key, levels = rev(info$key))
  tab$Group <- factor(info$gkey[match(tab$key, info$key)], levels = unique(info$gkey))
  tab$Judgement <- factor(tab$Judgement, levels = jv$Name)
  # Legend lists only judgements ticked "Legend" on the Set up page
  lj <- legend_judgements(jv)
  n_leg <- nrow(lj)
  leg_pos <- if (n_leg == 0) "none" else s$sum_legend_pos
  horiz <- leg_pos %in% c("bottom", "top")
  # Make the plot wide enough for its legend and labels. ggplot gives every legend
  # entry the width of the longest label, so size the entries on that.
  item_w <- if (n_leg) text_w_cm(lj$Name, fs * 0.9) + 1.4 else 0       # key + gap + label + spacing
  leg_title_w <- if (!is.null(nz(s$sum_legend_title))) text_w_cm(s$sum_legend_title, fs) + 0.4 else 0
  axis_w <- text_w_cm(disp, fs * 0.85) + 0.3                            # y-axis label column
  min_panel <- 5                                                        # narrowest sensible bar area (cm)
  if (leg_pos %in% c("left", "right")) {
    width <- max(width, axis_w + min_panel + max(item_w, leg_title_w) + 1.2)
    per_row <- 1
  } else {
    width <- max(width, axis_w + min_panel + 1, (if (n_leg) item_w + leg_title_w else 0) + 1)
    # Widen (up to 18 cm) so the legend needs at most two rows
    if (n_leg) width <- max(width, min(18, ceiling(n_leg / 2) * item_w + leg_title_w + 1))
    per_row <- if (n_leg) max(1, min(n_leg, floor((width - 1 - leg_title_w) / item_w))) else 1
  }
  
  p <- ggplot(tab, aes(x = prop, y = y, fill = Judgement)) +
    geom_col(width = 0.75, colour = if (is.na(s$outline)) NA else s$outline,
             linewidth = (s$outline_lwd %||% 0.6) * 0.5,
             position = position_stack(reverse = TRUE),
             show.legend = TRUE) +   # draw keys even for judgements no study uses yet
    scale_fill_manual(values = setNames(jv$Fill, jv$Name), limits = jv$Name, breaks = lj$Name,
                      drop = FALSE, name = nz(s$sum_legend_title)) +
    guides(fill = guide_legend(nrow = if (horiz) ceiling(n_leg / per_row) else max(1, n_leg), byrow = TRUE)) +
    scale_x_continuous(labels = function(x) round(x * 100), breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_y_discrete(labels = setNames(disp, info$key)) +
    labs(title = nz(s$sum_title), x = nz(s$sum_xlab), y = NULL, caption = nz(s$sum_footer),
         tag = nz(s$sum_tag))
  if (s$values != "none") {
    lab <- if (s$values == "count") as.character(tab$Freq) else paste0(round(tab$prop * 100), "%")
    show <- tab$prop >= if (s$values == "count") 0.04 else 0.08
    p <- p + geom_text(data = transform(tab, lab = lab)[show, , drop = FALSE],
                       aes(x = mid, y = y, label = lab, colour = symcol), inherit.aes = FALSE,
                       size = fs / .pt, family = s$family) +
      scale_colour_identity()
  }
  grouped <- isTRUE(s$sum_groups) && pd$has_groups
  if (grouped) {
    p <- p + facet_grid(Group ~ ., scales = "free_y", space = "free_y", switch = "y")
  }
  p <- p +
    theme_minimal(base_size = fs, base_family = s$family) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text.y = element_text(colour = "grey30", hjust = 1, lineheight = 0.95),
          axis.title.x = element_text(margin = margin(t = 8)),
          legend.position = leg_pos,
          legend.title = element_text(face = "bold"),
          legend.text = element_text(margin = margin(r = 12, l = 3)),
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
          plot.title = element_text(size = fs + 3, margin = margin(b = 8)),
          plot.title.position = "panel",
          plot.caption = element_text(hjust = 0, colour = "grey30"),
          plot.caption.position = "plot",
          plot.tag = element_text(face = "bold", size = fs * 2.6, hjust = 0, vjust = 1),
          plot.tag.position = c(0.004, 0.996),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(0.3, 0.4, 0.3, 0.3, "cm"))
  # ggplot2 3.5+ spaces legend items with legend.key.spacing.x (older versions use the text margin)
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    p <- p + theme(legend.key.spacing.x = unit(12, "pt"), legend.key.spacing.y = unit(4, "pt"))
  }
  label_lines <- sum(vapply(strsplit(disp, "\n", fixed = TRUE), length, integer(1)))
  rows_h <- max(nrow(info) * s$sum_row_h, label_lines * fs * 1.25 * CM_PER_PT)
  h <- 0.6 + rows_h +
    (if (!is.null(nz(s$sum_title)) || !is.null(nz(s$sum_tag))) (fs + 3) * 2 * CM_PER_PT else 0) +
    (if (!is.null(nz(s$sum_xlab))) fs * 3 * CM_PER_PT else fs * 1.8 * CM_PER_PT) +
    (if (horiz) ceiling(n_leg / per_row) * fs * 2.2 * CM_PER_PT + 0.3 else 0) +
    (if (!is.null(nz(s$sum_footer))) length(strsplit(s$sum_footer, "\n")[[1]]) * fs * 1.5 * CM_PER_PT else 0)
  if (leg_pos %in% c("left", "right")) h <- max(h, n_leg * fs * 2 * CM_PER_PT + 1.5)
  list(plot = p, grob = lazy_ggplot_grob(p), w = width, h = h)
}

combine_figures <- function(a, b) {
  W <- max(a$w, b$w)
  gt <- gtable::gtable(widths = unit(c(a$w, max(0.001, W - a$w)), "cm"),
                       heights = unit(c(a$h, 0.3, b$h), "cm"))
  gt <- gtable::gtable_add_grob(gt, rectGrob(gp = gpar(fill = "white", col = NA)), t = 1, l = 1, b = 3, r = 2,
                                name = "bg")
  gt <- gtable::gtable_add_grob(gt, a$grob, t = 1, l = 1, name = "panel_a")
  gt <- gtable::gtable_add_grob(gt, b$grob, t = 3, l = 1, r = 2, name = "panel_b")
  list(grob = gt, w = W, h = a$h + 0.3 + b$h)
}

# -----------------------------------------------------------------------------
# Templates
# -----------------------------------------------------------------------------
robvis_cols <- c(low = "#02C100", some = "#E2DF07", high = "#BF0000",
                 critical = "#820000", ni = "#4EA1F7")

templates <- list(
  rob2 = list(
    title = "RoB 2 (randomised trials)",
    description = "Five domains plus an overall judgement, using the robvis colour scheme.",
    header = "Risk of bias domains", sum_title = "Risk of bias summary",
    overall = TRUE, use_sub = FALSE,
    domains = make_domains(
      rep("", 5), rep("", 5), paste0("D", 1:5),
      c("Bias arising from the randomization process",
        "Bias due to deviations from intended interventions",
        "Bias due to missing outcome data", "Bias in measurement of the outcome",
        "Bias in selection of the reported result")),
    judgements = make_judgements(
      c("Low", "Some concerns", "High", "No information"),
      robvis_cols[c("low", "some", "high", "ni")], c("+", "-", "X", "?"), rep("#000000", 4))
  ),
  robins = list(
    title = "ROBINS-I (non-randomised studies)",
    description = "Seven domains plus an overall judgement, using the robvis colour scheme.",
    header = "Risk of bias domains", sum_title = "Risk of bias summary",
    overall = TRUE, use_sub = FALSE,
    domains = make_domains(
      rep("", 7), rep("", 7), paste0("D", 1:7),
      c("Bias due to confounding", "Bias due to selection of participants",
        "Bias in classification of interventions",
        "Bias due to deviations from intended interventions", "Bias due to missing data",
        "Bias in measurement of outcomes", "Bias in selection of the reported result")),
    judgements = make_judgements(
      c("Low", "Moderate", "Serious", "Critical", "No information"),
      robvis_cols[c("low", "some", "high", "critical", "ni")],
      c("+", "-", "X", "!", "?"), c(rep("#000000", 3), "#FFFFFF", "#000000"))
  ),
  robins_e = list(
    title = "ROBINS-E (non-randomised studies of exposures)",
    description = "Seven domains plus an overall judgement, for observational studies of the effects of exposures (Higgins et al., 2024). Uses the robvis colour scheme.",
    header = "Risk of bias domains", sum_title = "Risk of bias summary",
    overall = TRUE, use_sub = FALSE,
    domains = make_domains(
      rep("", 7), rep("", 7), paste0("D", 1:7),
      c("Risk of bias due to confounding",
        "Risk of bias arising from measurement of the exposure",
        "Risk of bias in selection of participants into the study (or into the analysis)",
        "Risk of bias due to post-exposure interventions",
        "Risk of bias due to missing data",
        "Risk of bias arising from measurement of the outcome",
        "Risk of bias in selection of the reported result")),
    judgements = make_judgements(
      c("Low", "Some concerns", "High", "Very high"),
      robvis_cols[c("low", "some", "high", "critical")],
      c("+", "-", "X", "!"), c(rep("#000000", 3), "#FFFFFF"))
  ),
  quadas = list(
    title = "QUADAS-2 (diagnostic accuracy)",
    description = "Four risk-of-bias domains with low, high and unclear judgements.",
    header = "QUADAS-2 domains", sum_title = "QUADAS-2 summary",
    overall = FALSE, use_sub = FALSE,
    domains = make_domains(
      rep("", 4), rep("", 4), paste0("D", 1:4),
      c("Patient selection", "Index test", "Reference standard", "Flow and timing")),
    judgements = make_judgements(
      c("Low", "High", "Unclear"), robvis_cols[c("low", "high", "some")],
      c("+", "X", "?"), rep("#000000", 3))
  ),
  robdemat = list(
    title = "RoBDEMAT (pre-clinical dental materials)",
    description = "Four domain groups (D1 to D4) with nine signalling questions. RoBDEMAT does not use an overall judgement.",
    header = "RoBDEMAT domains", sum_title = "RoBDEMAT summary plot",
    overall = FALSE, use_sub = TRUE,
    domains = make_domains(
      group = rep(c("D1", "D2", "D3", "D4"), c(3, 2, 2, 2)),
      group_name = rep(c("Bias in planning and allocation", "Bias in sample/specimen preparation",
                         "Bias in outcome assessment", "Bias in data treatment and outcome reporting"),
                       c(3, 2, 2, 2)),
      label = c("1.1", "1.2", "1.3", "2.1", "2.2", "3.1", "3.2", "4.1", "4.2"),
      name = c("Control group", "Randomization of samples", "Sample size rationale and reporting",
               "Standardization of samples and materials",
               "Identical experimental conditions across groups",
               "Adequate and standardized testing procedures and outcomes",
               "Blinding of test operator", "Statistical analysis", "Reporting study outcomes")),
    judgements = make_judgements(
      c("Sufficiently reported / adequate", "Insufficiently reported",
        "Not reported / not adequate", "Not applicable"),
      c("#2E7D32", "#F9A825", "#C62828", "#9E9E9E"),
      c("+", "-", "x", ""), rep("#000000", 4))
  ),
  blank = list(
    title = "Blank (build your own)",
    description = "Two placeholder domains with subdomains and three placeholder judgements to edit freely.",
    header = "Domains", sum_title = "Summary",
    overall = FALSE, use_sub = TRUE,
    domains = make_domains(c("D1", "D1", "D2"), c("First domain group", "", "Second domain group"),
                           c("1.1", "1.2", "2.1"), c("First item", "Second item", "Third item")),
    judgements = make_judgements(
      c("Low", "Some concerns", "High"), robvis_cols[c("low", "some", "high")],
      c("+", "-", "X"), rep("#000000", 3))
  )
)
template_choices <- setNames(names(templates), vapply(templates, `[[`, "", "title"))

example_assess <- function(domains, judgements, overall) {
  studies <- c("Anderson et al. 2018", "Baker et al. 2019", "Chen et al. 2020",
               "Dias et al. 2021", "Evans et al. 2022", "Fujita et al. 2023")
  nj <- nrow(judgements)
  a <- data.frame(Study = studies, stringsAsFactors = FALSE)
  domains <- validate_domains(domains)
  for (k in seq_along(domains$Key)) {
    a[[domains$Key[k]]] <- judgements$Name[((seq_along(studies) * 5 + k * 3 + (seq_along(studies) * k) %% 5) %% nj) + 1]
  }
  if (overall) a[[".overall"]] <- judgements$Name[((seq_along(studies) + 1) %% max(1, nj - 1)) + 1]
  a
}

# -----------------------------------------------------------------------------
# Excel template: write one matching the current domains, and read it back
# -----------------------------------------------------------------------------
write_assessment_template <- function(file, d, jv, a, overall_label = NULL,
                                      structure = NULL, settings = NULL) {
  wb <- openxlsx::createWorkbook()
  d <- validate_domains(d)
  cols <- c(d$Label, if (!is.null(overall_label)) overall_label)
  groups <- c(ifelse(nzchar(d$Group), d$Group, d$GroupName), if (!is.null(overall_label)) "")
  has_groups <- any(nzchar(groups))
  n_rows <- max(200, nrow(a) + 100)
  
  hdr <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center",
                               fgFill = "#D9D9D9", border = "TopBottomLeftRight", borderColour = "#A6A6A6",
                               wrapText = TRUE)
  study_hdr <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9D9D9", border = "TopBottomLeftRight",
                                     borderColour = "#A6A6A6", valign = "center")
  cell <- openxlsx::createStyle(border = "TopBottomLeftRight", borderColour = "#D0D0D0", halign = "center")
  study_cell <- openxlsx::createStyle(border = "TopBottomLeftRight", borderColour = "#D0D0D0")
  
  # Sheet 1: the assessments
  openxlsx::addWorksheet(wb, "Assessments", gridLines = FALSE)
  top <- if (has_groups) 2 else 1
  if (has_groups) {
    openxlsx::writeData(wb, 1, t(c("", groups)), startRow = 1, colNames = FALSE)
    r <- rle(groups)
    ends <- cumsum(r$lengths) + 1; starts <- ends - r$lengths + 1
    for (k in seq_along(r$values)) if (r$lengths[k] > 1 && nzchar(r$values[k])) {
      openxlsx::mergeCells(wb, 1, cols = starts[k]:ends[k], rows = 1)
    }
    openxlsx::addStyle(wb, 1, hdr, rows = 1, cols = 1:(length(cols) + 1), gridExpand = TRUE)
  }
  openxlsx::writeData(wb, 1, t(c("Study", cols)), startRow = top, colNames = FALSE)
  openxlsx::addStyle(wb, 1, hdr, rows = top, cols = 2:(length(cols) + 1), gridExpand = TRUE)
  openxlsx::addStyle(wb, 1, study_hdr, rows = top, cols = 1)
  if (nrow(a)) {
    out <- a[, c("Study", intersect(c(d$Key, if (!is.null(overall_label)) ".overall"), names(a))), drop = FALSE]
    openxlsx::writeData(wb, 1, out, startRow = top + 1, colNames = FALSE)
  }
  data_rows <- (top + 1):(top + n_rows)
  openxlsx::addStyle(wb, 1, study_cell, rows = data_rows, cols = 1, gridExpand = TRUE)
  if (length(cols)) openxlsx::addStyle(wb, 1, cell, rows = data_rows, cols = 2:(length(cols) + 1), gridExpand = TRUE)
  openxlsx::setColWidths(wb, 1, cols = 1, widths = 30)
  if (length(cols)) openxlsx::setColWidths(wb, 1, cols = 2:(length(cols) + 1),
                                           widths = max(12, min(34, max(nchar(jv$Name), 8) + 2)))
  openxlsx::freezePane(wb, 1, firstActiveRow = top + 1, firstActiveCol = 2)
  
  # Sheet 2: the judgement list that feeds the drop-downs
  openxlsx::addWorksheet(wb, "Judgements")
  in_leg <- if (is.null(jv$InLegend)) rep(TRUE, nrow(jv)) else !(jv$InLegend %in% FALSE)
  openxlsx::writeData(wb, "Judgements", data.frame(
    Judgement = jv$Name, Symbol = jv$Symbol, Colour = jv$Fill, `Symbol colour` = jv$SymbolColour,
    `Show in legend` = ifelse(in_leg, "Yes", "No"), check.names = FALSE))
  openxlsx::addStyle(wb, "Judgements", openxlsx::createStyle(textDecoration = "bold"), rows = 1, cols = 1:5)
  for (k in seq_len(nrow(jv))) {
    openxlsx::addStyle(wb, "Judgements", openxlsx::createStyle(fgFill = jv$Fill[k], fontColour = jv$SymbolColour[k]),
                       rows = k + 1, cols = 1)
  }
  openxlsx::setColWidths(wb, "Judgements", cols = 1:5, widths = c(36, 10, 12, 14, 15))
  if (length(cols) && nrow(jv)) {
    # A named range keeps the drop-down a standard Excel list validation
    openxlsx::createNamedRegion(wb, "Judgements", cols = 1, rows = 2:(nrow(jv) + 1), name = "JudgementList")
    # (openxlsx 4.2.5 emits a harmless internal sprintf warning here)
    suppressWarnings(openxlsx::dataValidation(wb, 1, cols = 2:(length(cols) + 1), rows = data_rows,
                                              type = "list", value = "JudgementList"))
    # Colour each filled cell like its judgement
    first <- paste0(openxlsx::int2col(2), top + 1)
    for (k in seq_len(nrow(jv))) {
      openxlsx::conditionalFormatting(
        wb, 1, cols = 2:(length(cols) + 1), rows = data_rows, type = "expression",
        rule = sprintf('EXACT(%s,"%s")', first, gsub('"', '""', jv$Name[k])),
        style = openxlsx::createStyle(bgFill = jv$Fill[k], fontColour = jv$SymbolColour[k]))
    }
  }
  
  # Sheet 3: instructions
  openxlsx::addWorksheet(wb, "How to fill in")
  steps <- c("How to fill in this template",
             "",
             "1. Go to the 'Assessments' sheet.",
             "2. Type one study per row in the 'Study' column.",
             "3. Click a cell under a domain and pick the judgement from the drop-down list.",
             "   The list comes from the 'Judgements' sheet, so the spelling always matches the app.",
             "4. Leave a cell empty if it has not been assessed yet.",
             "5. Do not rename, add or delete the domain columns; change domains in the app instead.",
             "6. Save the file, then either upload it on the Assessments page of AnyRoB,",
             "   or open it from the AnyRoB home page ('Open a saved project') to restore everything.",
             "",
             "This file is also an AnyRoB project file: the 'Domains' and 'Settings' sheets store your",
             "set-up and plot settings. You can edit domain names or judgement colours there, but keep the",
             "column headings as they are.",
             "",
             "Domains in this template:",
             paste0("   ", sub("::", " ", d$Key, fixed = TRUE), ifelse(nzchar(d$Name), paste0(": ", d$Name), "")),
             if (!is.null(overall_label)) paste0("   ", overall_label, ": overall judgement"))
  openxlsx::writeData(wb, "How to fill in", data.frame(x = steps), colNames = FALSE)
  openxlsx::addStyle(wb, "How to fill in", openxlsx::createStyle(textDecoration = "bold", fontSize = 13), rows = 1, cols = 1)
  openxlsx::setColWidths(wb, "How to fill in", cols = 1, widths = 100)
  
  # Sheets 4 and 5: the full project set-up, so the file can be reopened later
  bold <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9D9D9")
  if (!is.null(structure)) {
    has_sub <- nzchar(structure$Group)
    dom <- data.frame(
      `Domain label` = ifelse(has_sub, structure$Group, structure$Label),
      `Domain name` = ifelse(has_sub, structure$GroupName, structure$Name),
      `Subdomain label` = ifelse(has_sub, structure$Label, ""),
      `Subdomain name` = ifelse(has_sub, structure$Name, ""), check.names = FALSE)
    openxlsx::addWorksheet(wb, "Domains")
    openxlsx::writeData(wb, "Domains", dom)
    openxlsx::addStyle(wb, "Domains", bold, rows = 1, cols = 1:4)
    openxlsx::setColWidths(wb, "Domains", cols = 1:4, widths = c(14, 45, 16, 55))
    openxlsx::freezePane(wb, "Domains", firstRow = TRUE)
  }
  if (!is.null(settings)) {
    openxlsx::addWorksheet(wb, "Settings")
    openxlsx::writeData(wb, "Settings", data.frame(Setting = names(settings),
                                                   Value = vapply(settings, function(v) paste(v, collapse = ","), "")))
    openxlsx::addStyle(wb, "Settings", bold, rows = 1, cols = 1:2)
    openxlsx::setColWidths(wb, "Settings", cols = 1:2, widths = c(24, 60))
  }
  openxlsx::activeSheet(wb) <- 1
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# Read a filled template. Returns the assessments (named by domain label, with
# the overall column as ".overall") plus notes on anything that did not match.
read_assessment_upload <- function(path, d, jv, overall_label = NULL) {
  d <- validate_domains(d)
  sheets <- openxlsx::getSheetNames(path)
  sheet <- if ("Assessments" %in% sheets) "Assessments" else sheets[1]
  raw <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE, skipEmptyRows = FALSE,
                             skipEmptyCols = FALSE,
                             fillMergedCells = TRUE)
  if (is.null(raw) || !nrow(raw)) stop("the file has no data.")
  raw[] <- lapply(raw, function(x) trimws(as.character(x)))
  # The header row is the one whose first cell is "Study"
  hr <- which(tolower(raw[[1]]) == "study")[1]
  if (is.na(hr)) stop("no 'Study' column was found. Please use the downloaded template.")
  header <- unlist(raw[hr, ], use.names = FALSE)
  header[is.na(header)] <- ""
  # Domain headings sit in the row above; merged heading cells are read as
  # filled across every column they span
  groups <- rep("", length(header))
  if (hr > 1) {
    g <- unlist(raw[hr - 1, ], use.names = FALSE); g[is.na(g)] <- ""
    groups <- ifelse(nzchar(header), g, "")
    groups[1] <- ""
  }
  col_key <- ifelse(nzchar(groups), paste0(groups, "::", header), header)
  if (!is.null(overall_label)) col_key[tolower(header) == tolower(overall_label) & !nzchar(groups)] <- ".overall"
  body <- raw[-seq_len(hr), , drop = FALSE]
  wanted <- c(d$Key, if (!is.null(overall_label)) ".overall")
  used <- integer(0)
  out <- data.frame(Study = body[[1]], stringsAsFactors = FALSE)
  for (w in wanted) {
    j <- match(w, col_key)
    if (is.na(j) && w != ".overall") {
      # Fall back to the plain label when it is unique (e.g. the heading row was removed)
      lab <- d$Label[d$Key == w]
      if (sum(d$Label == lab) == 1) j <- which(header == lab & !(seq_along(header) %in% used))[1]
    }
    if (!is.na(j)) used <- c(used, j)
    out[[w]] <- if (is.na(j)) NA_character_ else body[[j]]
  }
  out[] <- lapply(out, function(x) { x[!is.na(x) & x == ""] <- NA; x })
  out <- out[rowSums(!is.na(out)) > 0, , drop = FALSE]
  # Match judgements regardless of capitalisation
  canon <- setNames(jv$Name, tolower(jv$Name))
  for (w in wanted) {
    v <- out[[w]]
    hit <- !is.na(v) & tolower(v) %in% names(canon)
    v[hit] <- canon[tolower(v[hit])]
    out[[w]] <- v
  }
  vals <- unlist(out[wanted], use.names = FALSE)
  shown <- function(k) sub("::", " ", k, fixed = TRUE)
  missing <- setdiff(wanted, col_key[used])
  extra_idx <- setdiff(which(nzchar(header))[-1], used)
  list(assess = out,
       missing = shown(ifelse(missing == ".overall", overall_label %||% "Overall", missing)),
       extra = shown(col_key[extra_idx]),
       invalid = unique(vals[!is.na(vals) & !(vals %in% jv$Name)]))
}

# Read a saved project (or a downloaded template): set-up, judgements, settings
# and assessments. Stops with a plain-language message if it is not one.
read_project_file <- function(path) {
  sheets <- tryCatch(openxlsx::getSheetNames(path), error = function(e) stop("this is not an Excel (.xlsx) file."))
  if (!all(c("Domains", "Judgements") %in% sheets)) {
    stop("it does not contain an AnyRoB project (no 'Domains' sheet). To load studies into an existing set-up, use 'Upload filled template' on the Assessments page instead.")
  }
  rd <- function(sh) {
    x <- openxlsx::read.xlsx(path, sheet = sh, colNames = TRUE, skipEmptyRows = TRUE, check.names = FALSE,
                             sep.names = " ")
    if (is.null(x)) x <- data.frame()
    names(x) <- tolower(trimws(names(x)))
    x[] <- lapply(x, function(v) { v <- trimws(as.character(v)); v[is.na(v)] <- ""; v })
    x
  }
  col <- function(x, nm) if (nm %in% names(x)) x[[nm]] else rep("", nrow(x))
  
  dom <- rd("Domains")
  if (!nrow(dom)) stop("the 'Domains' sheet is empty.")
  has_sub <- nzchar(col(dom, "subdomain label")) | nzchar(col(dom, "subdomain name"))
  structure <- clean_domains(make_domains(
    ifelse(has_sub, col(dom, "domain label"), ""), ifelse(has_sub, col(dom, "domain name"), ""),
    ifelse(has_sub, col(dom, "subdomain label"), col(dom, "domain label")),
    ifelse(has_sub, col(dom, "subdomain name"), col(dom, "domain name"))))
  if (!nrow(structure)) stop("no domains were found in the 'Domains' sheet.")
  
  jd <- rd("Judgements")
  jd <- jd[nzchar(col(jd, "judgement")), , drop = FALSE]
  if (!nrow(jd)) stop("no judgements were found in the 'Judgements' sheet.")
  palette <- c("#2E7D32", "#F9A825", "#C62828", "#9E9E9E", "#4EA1F7", "#8E44AD", "#E67E22")
  fill <- col(jd, "colour"); fill[!nzchar(fill)] <- palette[(which(!nzchar(fill)) - 1) %% length(palette) + 1]
  judgements <- clean_judgements(data.frame(
    Name = jd$judgement, Fill = fill, Symbol = col(jd, "symbol"),
    SymbolColour = ifelse(nzchar(col(jd, "symbol colour")), col(jd, "symbol colour"), "#000000"),
    InLegend = !(tolower(col(jd, "show in legend")) %in% c("no", "false", "0")), stringsAsFactors = FALSE))
  judgements <- judgements[!duplicated(judgements$Name), , drop = FALSE]
  
  settings <- list()
  if ("Settings" %in% sheets) {
    st <- rd("Settings")
    if (nrow(st) && all(c("setting", "value") %in% names(st))) settings <- as.list(setNames(st$value, st$setting))
  }
  flag <- function(nm, default) if (is.null(settings[[nm]]) || !nzchar(settings[[nm]])) default else
    toupper(settings[[nm]]) %in% c("TRUE", "YES", "1")
  use_sub <- flag("use_sub", any(nzchar(structure$Group)))
  use_overall <- flag("use_overall", FALSE)
  overall_label <- if (nzchar(settings$overall_label %||% "")) settings$overall_label else "Overall"
  
  # The plot columns, worked out the same way the app does
  k <- 0; next_id <- function() { k <<- k + 1; k }
  d <- validate_domains(domains_from_tree(tree_from_domains(structure, next_id), use_sub))
  res <- if ("Assessments" %in% sheets) {
    read_assessment_upload(path, d, judgements, if (use_overall) overall_label)
  } else list(assess = data.frame(Study = character(0)), missing = character(0), extra = character(0), invalid = character(0))
  list(structure = structure, judgements = judgements, settings = settings, use_sub = use_sub,
       use_overall = use_overall, overall_label = overall_label, upload = res)
}

# Default values of every plot customisation input, used by "Reset to default".
# Inputs whose default depends on the template (tl_header, sum_title) are
# filled in from the template that was last loaded.
plot_defaults <- list(
  text = list(tl_tag = "A", tl_title = "", tl_ylab = "Study", tl_key_title = "Domains:",
              legend_title = "Judgements:", sum_tag = "B", sum_xlab = "% of studies", sum_legend_title = ""),
  area = list(tl_footer = "", sum_footer = ""),
  check = list(tl_show_key = TRUE, show_empty = TRUE, sum_groups = FALSE, sum_include_overall = TRUE),
  select = list(tl_legend_pos = "beside", font_family = "sans", sum_label_type = "stack", sum_values = "count",
                sum_legend_pos = "bottom", sum_font_family = "sans"),
  slider = list(base_size = 10, col_w = 1.4, row_h = 0.95, header_h = 0.55, point_size = 6.5, symbol_size = 3,
                sum_base_size = 10, sum_row_h = 0.85, sum_wrap = 40),
  colour = list(grid_col = "#A6A6A6", header_fill = "#D9D9D9")
)

# Draw a figure (list with grob, w, h in cm) into the current device
draw_fig <- function(fig) {
  grid.newpage()
  grid.draw(fig$grob)
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
app_css <- "
body { font-size: 0.95rem; }
.help-note { color: #5b6770; font-size: 0.9rem; max-width: 80ch; }
.step-num { display: inline-flex; align-items: center; justify-content: center;
  width: 1.7rem; height: 1.7rem; border-radius: 50%; background: var(--bs-primary);
  color: #fff; font-weight: 700; margin-right: 0.55rem; font-size: 0.85rem; }
.j-row { display: grid; gap: 0.5rem; align-items: center; margin-bottom: 0.4rem;
  grid-template-columns: minmax(0, 2fr) minmax(0, 1.25fr) minmax(0, 0.55fr) minmax(0, 1.25fr) 52px 104px; }
.j-row > * { min-width: 0; }
.j-row input { min-width: 0; width: 100%; }
.j-legend { display: flex; justify-content: center; }
.j-legend .checkbox, .j-legend .form-check { margin: 0; }
.j-row .form-group, .j-row .shiny-input-container { margin-bottom: 0 !important; width: 100% !important; }
.j-head { font-size: 0.82rem; font-weight: 600; color: #5b6770; }
.j-btns { display: flex; gap: 0.25rem; }
.j-btns .btn { padding: 0.2rem 0.45rem; }
.hdr-group { font-size: 10px; color: #5b6770; white-space: nowrap; overflow: hidden;
  text-overflow: ellipsis; max-width: 100%; line-height: 1.3; }
.hdr-label { font-weight: 600; line-height: 1.3; }
.handsontable thead th { vertical-align: middle; }
.nav-buttons { display: flex; justify-content: space-between; gap: 1rem; margin: 1rem 0 2rem; }
.lead-intro { max-width: 70ch; }
.hero { max-width: 760px; margin: 0 auto; padding: 3rem 0 1rem; text-align: center; }
.hero h1 { font-weight: 800; letter-spacing: -0.02em; font-size: 2.6rem; margin-bottom: 0.4rem; }
.hero h1 .accent { color: var(--bs-primary); }
.hero .lead { color: #4a5660; }
.start-card { max-width: 1040px; margin: 1.5rem auto 2rem; }
.start-split { display: grid; grid-template-columns: minmax(0, 1fr) auto minmax(0, 1fr); gap: 1.25rem; align-items: stretch; }
.start-option { display: flex; flex-direction: column; background: #f8fafb; border: 1px solid #e3e8ec; border-radius: 10px;
  padding: 1rem 1.1rem 0.9rem; }
.start-option .shiny-input-container { margin-bottom: 0.4rem; }
.split-or { display: flex; flex-direction: column; align-items: center; justify-content: center; color: #7a8790;
  font-weight: 700; font-size: 0.85rem; letter-spacing: 0.08em; }
.split-or::before, .split-or::after { content: ''; flex: 1; border-left: 1px solid #dde2e6; margin: 0.4rem 0; }
@media (max-width: 860px) {
  .start-split { grid-template-columns: 1fr; }
  .split-or { flex-direction: row; }
  .split-or::before, .split-or::after { border-left: none; border-top: 1px solid #dde2e6; margin: 0 0.6rem; }
}
.cite-section { max-width: 1040px; margin: 0.5rem auto 3rem; padding: 0 0.25rem; }
.cite-title { font-weight: 500; font-size: 1.6rem; margin-bottom: 0.35rem; }
.cite-intro { color: #3d4850; margin-bottom: 0.8rem; }
.cite-quote { background: #f2f4f5; border-left: 5px solid #c8cdd1; padding: 0.85rem 1.1rem; margin: 0 0 0.9rem;
  font-size: 0.98rem; color: #2b3640; user-select: all; }
.cite-actions { display: flex; flex-wrap: wrap; gap: 0.5rem; }
.cite-help { color: #7a8790; font-size: 0.8rem; margin-top: 0.5rem; }
.cite-compact { display: flex; gap: 0.75rem; align-items: flex-start; justify-content: space-between;
  background: #f2f4f5; border-left: 4px solid #c8cdd1; padding: 0.55rem 0.8rem; margin-bottom: 0.9rem; font-size: 0.83rem; }
.cite-compact .btn { flex-shrink: 0; font-size: 0.78rem; padding: 0.1rem 0.5rem; }
.save-btn { margin: 0.25rem 0.5rem; }
.step-title { font-weight: 700; font-size: 1.1rem; }
.or-divider { display: flex; align-items: center; gap: 0.9rem; margin: 1.4rem 0 1.1rem; color: #7a8790;
  font-weight: 700; font-size: 0.85rem; letter-spacing: 0.08em; }
.or-divider::before, .or-divider::after { content: ''; flex: 1; border-top: 1px solid #dde2e6; }
.lights { display: inline-flex; gap: 6px; margin-bottom: 1rem; }
.lights span { width: 14px; height: 14px; border-radius: 50%; display: inline-block; }
.help-btn { border: none; background: none; padding: 0 0.35rem; color: var(--bs-primary); font-size: 1.05rem;
  line-height: 1; cursor: pointer; }
.help-btn:hover { color: #E8590C; }
.sticky-preview { position: sticky; top: 72px; align-self: start; height: fit-content; display: block !important; }
.figure-frame.fit img { max-width: 100%; height: auto !important; }
.figure-frame.fit .shiny-plot-output { max-width: 100%; }
/* Domain editor: domains as cards, subdomains nested underneath */
.dom-card { border: 1px solid #dfe4e8; border-left: 4px solid var(--bs-primary); border-radius: 8px;
  padding: 0.7rem 0.8rem 0.75rem; margin-bottom: 0.7rem; background: #fff; }
.dom-kicker { font-size: 0.72rem; font-weight: 700; letter-spacing: 0.06em; text-transform: uppercase;
  color: var(--bs-primary); margin-bottom: 0.35rem; display: flex; justify-content: space-between; }
.dom-kicker .count { color: #7a8790; font-weight: 600; text-transform: none; letter-spacing: 0; }
.tree-row { display: grid; grid-template-columns: 84px minmax(0, 1fr) auto; gap: 0.45rem; align-items: center; }
.tree-row .form-group, .tree-row .shiny-input-container { margin-bottom: 0 !important; width: 100% !important; }
.tree-row input.form-control { padding: 0.3rem 0.55rem; }
.tree-row .lbl input { font-weight: 700; text-align: center; }
.tree-btns { display: flex; gap: 0.2rem; }
.tree-btns .btn { padding: 0.15rem 0.4rem; font-size: 0.8rem; }
.sub-list { margin: 0.55rem 0 0 0.9rem; padding-left: 1.1rem; border-left: 2px solid #dfe4e8; }
.sub-list .tree-row { position: relative; margin-bottom: 0.35rem; grid-template-columns: 70px minmax(0, 1fr) auto; }
.sub-list .tree-row::before { content: ''; position: absolute; left: -1.1rem; top: 50%; width: 0.85rem;
  border-top: 2px solid #dfe4e8; }
.sub-list .tree-row input.form-control { font-size: 0.9rem; }
.sub-caption { font-size: 0.75rem; color: #7a8790; margin: 0 0 0.3rem; }
.add-sub { font-size: 0.85rem; padding: 0.1rem 0.2rem; }
.tree-head { display: grid; grid-template-columns: 84px minmax(0, 1fr) 96px; gap: 0.45rem; font-size: 0.75rem;
  color: #7a8790; font-weight: 600; margin: 0 0 0.3rem 0.85rem; }
.figure-frame { overflow: auto; background: #eef1f3; border-radius: 6px; padding: 14px; }
.figure-frame .shiny-plot-output { width: fit-content !important; margin: 0 auto; box-shadow: 0 1px 3px rgba(20, 40, 60, 0.18); background: #fff; }
.side-head { font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.05em; color: #5b6770;
  font-weight: 700; margin: 1rem 0 0.6rem; padding-top: 0.75rem; border-top: 1px solid #e3e7ea; }
.accordion-body > .side-head:first-child { margin-top: 0; padding-top: 0; border-top: none; }
/* Plots page: the Customise panel scrolls on its own while the plots stay in view */
.bslib-sidebar-layout:has(> .customise-sidebar) { overflow: visible !important; }
.bslib-sidebar-layout > .customise-sidebar { position: sticky; top: 64px; align-self: start;
  height: calc(100vh - 80px); max-height: calc(100vh - 80px); }
.customise-sidebar > .sidebar-content { height: 100%; overflow-y: auto; overscroll-behavior: contain;
  padding-top: 0.6rem !important; }
.customise-title { display: flex; align-items: center; justify-content: space-between; gap: 0.5rem;
  padding-right: 1.6rem; }
.customise-title .btn { font-size: 0.8rem; padding: 0.15rem 0.55rem; font-weight: 500; }
.customise-sidebar .sidebar-title { font-size: 1.05rem; font-weight: 600; margin: 0 0 0.5rem !important;
  padding: 0 !important; line-height: 1.6; }
.help-note.wide { max-width: none; }
/* Assessment spreadsheet: show every study, no inner scrollbar */
#grid_wrap .html-widget, #grid_wrap .shiny-html-output { height: auto !important; }
.excel-box { display: flex; flex-wrap: wrap; gap: 0.75rem 1.5rem; align-items: center; justify-content: space-between;
  background: #f1f7f3; border: 1px solid #cfe5d6; border-radius: 8px; padding: 0.75rem 1rem; margin-bottom: 1rem; }
.excel-box .excel-text { flex: 1 1 380px; color: #2f4a3a; }
.excel-box .excel-text .fa-file-excel { color: #1d6f42; }
.excel-actions { display: flex; flex-wrap: wrap; gap: 0.6rem; align-items: center; }
.excel-actions .form-group, .excel-actions .shiny-input-container { margin-bottom: 0 !important; }
.excel-actions .shiny-file-input-progress { display: none !important; }
.export-bar { background: #f7f9fa; padding: 0.9rem 1.1rem 1rem; }
.export-head { font-weight: 700; font-size: 0.85rem; letter-spacing: 0.04em; text-transform: uppercase;
  color: #5b6770; margin-bottom: 0.5rem; }
.export-row { display: flex; flex-wrap: wrap; gap: 0.4rem 1.2rem; align-items: flex-end; }
.export-row .form-group, .export-row .shiny-input-container { margin-bottom: 0.6rem; }
.export-dims { display: flex; gap: 0.8rem; }
.export-check { padding-bottom: 0.55rem; }
.size-note { color: #5b6770; font-size: 0.85rem; margin-top: 0.5rem; }
"

# Help icons sit inside accordion headers. Bootstrap toggles panels from a
# capture-phase listener on the document, so intercept on the window first.
help_js <- "
(function() {
  function fire(e) {
    var el = e.target.closest && e.target.closest('[data-help]');
    if (!el) return;
    if (e.type === 'keydown' && e.key !== 'Enter' && e.key !== ' ') return;
    e.stopPropagation(); e.preventDefault();
    if (window.Shiny) Shiny.setInputValue(el.getAttribute('data-help'), Date.now(), {priority: 'event'});
  }
  window.addEventListener('click', fire, true);
  window.addEventListener('keydown', fire, true);
})();
"

# rhandsontable does nothing when its container changes width, so the body
# and its separately drawn header can drift out of line (for example when a
# scrollbar appears, a section expands or the Overall label box opens).
# Re-fit every spreadsheet whenever its container width changes.
table_fit_js <- "
(function() {
  function refit(el) {
    var w = window.HTMLWidgets && HTMLWidgets.find('#' + el.id);
    if (!w || !w.hot || el.offsetWidth === 0) return;
    if (w.hot.refreshDimensions) w.hot.refreshDimensions(); else w.hot.render();
  }
  function schedule(el) {
    clearTimeout(el._refitTimer);
    el._refitTimer = setTimeout(function() { refit(el); }, 60);
  }
  var ro = new ResizeObserver(function(entries) {
    entries.forEach(function(e) {
      var el = e.target.querySelector('.rhandsontable') || e.target;
      var w = Math.round(e.contentRect.width);
      if (el._lastW !== w) { el._lastW = w; schedule(el); }
    });
  });
  function watchAll() {
    document.querySelectorAll('.rhandsontable.html-widget').forEach(function(el) {
      if (!el._refitWatched && el.parentElement) { el._refitWatched = true; ro.observe(el.parentElement); }
    });
  }
  function refitAll() {
    document.querySelectorAll('.rhandsontable.html-widget').forEach(function(el) { schedule(el); });
  }
  $(document).on('shiny:value', function(e) {
    setTimeout(function() { watchAll(); var el = document.getElementById(e.name);
      if (el && el.classList.contains('rhandsontable')) schedule(el); }, 50);
  });
  document.addEventListener('shown.bs.collapse', refitAll);
  document.addEventListener('shown.bs.tab', refitAll);
  $(document).on('shiny:connected', function() { setTimeout(watchAll, 500); });
})();
"

# Handsontable (the spreadsheet) takes over the mouse wheel when the browser is
# not Chrome or the screen is scaled above 100% (common on Windows laptops), and
# then cancels the page scroll. The assessment table has no scrollbar of its
# own, so hand vertical wheel movement back to the page before the table sees it.
# Sideways movement is left to the table.
wheel_js <- "
(function() {
  function release(e) {
    var dx = Math.abs(e.deltaX || 0), dy = Math.abs(e.deltaY || 0);
    if (dy > dx && !e.shiftKey) e.stopPropagation();   // no preventDefault: the page scrolls normally
  }
  function attach() {
    var w = document.getElementById('grid_wrap');
    if (w && !w._wheelFix) { w._wheelFix = true; w.addEventListener('wheel', release, {capture: true, passive: true}); }
  }
  document.addEventListener('DOMContentLoaded', attach);
  $(document).on('shiny:connected shiny:value', attach);
})();
"

figure_output <- function(id, fit = FALSE) {
  div(class = paste("figure-frame", if (fit) "fit"), plotOutput(id, width = "auto", height = "auto"))
}

# A clickable help icon for an accordion header (does not toggle the panel)
help_button <- function(id, title) {
  # A span (not a button): buttons cannot be nested inside the accordion header
  # button. Clicks are handled by help_js below, before Bootstrap sees them.
  tags$span(class = "help-btn", role = "button", tabindex = "0", title = title, `aria-label` = title,
            `data-help` = id, icon("circle-question"))
}

# "Cite AnyRoB" section: the full version (home page) has reference-manager
# downloads; the compact one (next to the figure downloads) just has copy.
copy_button <- function(target) {
  tags$button(type = "button", class = "btn btn-outline-secondary btn-sm",
              onclick = sprintf(
                "var t=document.getElementById('%s').innerText; var b=this; navigator.clipboard.writeText(t).then(function(){b.innerHTML='Copied!';setTimeout(function(){b.innerHTML='Copy citation'},1500)});",
                target),
              "Copy citation")
}
cite_box <- function(class = NULL, compact = FALSE) {
  id <- if (compact) "cite_text_export" else "cite_text_home"
  if (compact) {
    return(div(class = paste("cite-compact", class),
               div(class = "cite-compact-text",
                   strong("Using these figures in a publication? Please cite: "),
                   span(id = id, app_citation())),
               copy_button(id)))
  }
  div(class = paste("cite-section", class),
      h3(class = "cite-title", "Cite AnyRoB"),
      p(class = "cite-intro", "If AnyRoB was useful for your research, please consider citing the tool in your publication."),
      tags$blockquote(class = "cite-quote", id = id, app_citation()),
      div(class = "cite-actions",
          copy_button(id),
          downloadButton("cite_ris", "Download citation (.ris)", class = "btn-outline-secondary btn-sm"),
          downloadButton("cite_nbib", "Download citation (.nbib)", class = "btn-outline-secondary btn-sm"),
          downloadButton("cite_bib", "Download citation (.bib)", class = "btn-outline-secondary btn-sm")),
      p(class = "cite-help", ".ris works with EndNote, Zotero and Mendeley; .nbib with PubMed-style importers; .bib with LaTeX/BibTeX."))
}

start_page <- nav_panel(
  title = "Start", value = "start", icon = icon("house"),
  div(class = "container-lg",
      div(class = "hero",
          div(class = "lights",
              span(style = "background:#2E7D32"), span(style = "background:#F9A825"), span(style = "background:#C62828")),
          h1(HTML('Any<span class="accent">RoB</span>')),
          p(class = "lead", "Highly customizable traffic light and summary plots for any risk of bias tool")),
      card(
        class = "start-card",
        card_body(
          actionButton("use_blank", "Start building with a blank template", class = "btn-primary btn-lg w-100",
                       icon = icon("pen-ruler")),
          div(class = "or-divider", span("OR")),
          div(class = "start-split",
              div(class = "start-option",
                  div(class = "step-title mb-2", icon("table-list"), " Choose a template to edit"),
                  selectInput("template", NULL, choices = template_choices[template_choices != "blank"], width = "100%"),
                  uiOutput("template_info"),
                  div(class = "d-flex gap-2 flex-wrap mt-auto pt-2",
                      actionButton("use_template", "Start", class = "btn-outline-primary", icon = icon("play")),
                      actionButton("use_example", "Start with example studies", class = "btn-outline-primary",
                                   icon = icon("flask")))),
              div(class = "split-or", span("OR")),
              div(class = "start-option",
                  div(class = "step-title mb-2", icon("folder-open"), " Continue a saved project"),
                  p(class = "help-note mb-2",
                    "Open an Excel file saved with 'Save progress' (or a downloaded Excel template). Your domains, judgements, assessments and plot settings are restored."),
                  div(class = "mt-auto",
                      fileInput("open_project", NULL, accept = ".xlsx", buttonLabel = list(icon("folder-open"), " Open saved project"),
                                placeholder = "No file chosen", width = "100%"))))
        )
      ),
      cite_box(class = "start-cite")
  )
)

setup_page <- nav_panel(
  title = "1. Set up", value = "setup", icon = icon("sliders"),
  div(class = "container-fluid py-3",
      layout_columns(
        col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
        div(
          accordion(
            id = "setup_sections", multiple = TRUE, open = c("Domains", "Judgements"),
            accordion_panel(
              title = span("Domains", help_button("help_domains", "What are domains? Click for an illustrated guide")),
              value = "Domains", icon = icon("table-columns"),
              div(class = "d-flex gap-4 flex-wrap",
                  checkboxInput("use_sub", "Use subdomains", TRUE),
                  checkboxInput("use_overall", "Add an overall judgement column", FALSE)),
              uiOutput("domain_help"),
              uiOutput("domain_editor"),
              div(class = "d-flex gap-2 mt-1",
                  actionButton("add_domain", "Add domain", icon = icon("plus"), class = "btn-outline-primary btn-sm")),
              conditionalPanel("input.use_overall",
                               textInput("overall_label", "Overall column label", "Overall", width = "260px"))
            ),
            accordion_panel(
              title = span("Judgements", help_button("help_judgements", "What are judgements? Click for an illustrated guide")),
              value = "Judgements", icon = icon("circle-half-stroke"),
              p(class = "help-note",
                "List every judgement you will use. Each becomes an option in the spreadsheet drop-downs, and the order here is the legend order. A symbol can be any short text, such as +, -, x, ? or !, or left empty. Click a colour box to pick a colour, or type a hex code such as #2E7D32. Untick 'Legend' to leave a judgement out of the plot legends; it can still be used in the table and bars."),
              uiOutput("judgement_ui"),
              div(actionButton("add_judgement", "Add judgement", icon = icon("plus"),
                               class = "btn-outline-primary btn-sm mt-1")),
              hr(),
              layout_columns(
                col_widths = c(6, 6),
                selectInput("marker_shape", "Marker shape",
                            c("Circle" = "21", "Square" = "22", "Diamond" = "23"), width = "100%"),
                checkboxInput("use_outline", "Outline the markers", FALSE)
              ),
              conditionalPanel(
                "input.use_outline",
                layout_columns(
                  col_widths = c(6, 6),
                  sliderInput("outline_lwd", "Outline thickness", 0.2, 3, 0.8, 0.1, width = "100%"),
                  colourInput("outline_col", "Outline colour", "#4D4D4D", showColour = "both")
                )
              )
            )
          ),
          uiOutput("setup_check")
        ),
        div(class = "sticky-preview",
            card(
              card_header(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                          span("Live preview"),
                          conditionalPanel(
                            "!output.preview_real",
                            div(class = "d-flex align-items-center gap-2",
                                span(class = "small text-muted", "Placeholder studies"),
                                div(style = "width: 90px;",
                                    numericInput("preview_n", NULL, 4, min = 1, max = 30, step = 1)))),
                          conditionalPanel(
                            "output.preview_real",
                            span(class = "badge text-bg-success", icon("table"), " ",
                                 textOutput("preview_count", inline = TRUE)))),
              uiOutput("preview_note"),
              figure_output("setup_preview", fit = TRUE)
            )
        )
      ),
      div(class = "nav-buttons",
          actionButton("back_start", "Back to start", class = "btn-outline-secondary", icon = icon("arrow-left")),
          actionButton("to_assess", "Next: enter assessments", class = "btn-primary", icon = icon("arrow-right")))
  )
)

assess_page <- nav_panel(
  title = "2. Assessments", value = "assess", icon = icon("table"),
  div(class = "container-fluid py-3",
      card(
        card_header(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                    span("Assessment spreadsheet"),
                    span(class = "text-muted small", textOutput("assess_progress", inline = TRUE))),
        div(class = "excel-box",
            div(class = "excel-text",
                strong(icon("file-excel"), " Prefer Excel?"),
                span(" Click 'Save progress' (top right) for an Excel file with your domains as columns and drop-down lists of your judgements. Fill in its Assessments sheet, then upload it here, or open it from the home page.")),
            div(class = "excel-actions",
                fileInput("upload_template", NULL, accept = ".xlsx", buttonLabel = "Upload filled template",
                          placeholder = "No file chosen", width = "340px"))),
        p(class = "help-note wide",
          "Type study names in the first column; a new row appears automatically. Click a cell and choose a judgement from the drop-down, or start typing it. Paste blocks of cells from Excel with Ctrl+V (Cmd+V on Mac). Right-click to insert or delete rows. Cells shaded red hold a value that is not in your judgement list."),
        div(class = "d-flex gap-2 align-items-end flex-wrap",
            selectInput("fill_value", "Set every empty cell to", choices = NULL, width = "280px"),
            actionButton("fill_empty", "Fill empty cells", class = "btn-outline-secondary mb-3")),
        div(id = "grid_wrap", rHandsontableOutput("grid", height = "auto"))
      ),
      uiOutput("assess_check"),
      div(class = "nav-buttons",
          actionButton("back_setup", "Back to set up", class = "btn-outline-secondary", icon = icon("arrow-left")),
          actionButton("to_plots", "Next: make plots", class = "btn-primary", icon = icon("arrow-right")))
  )
)

plots_page <- nav_panel(
  title = "3. Plots and export", value = "plots", icon = icon("chart-simple"),
  layout_sidebar(
    class = "plots-layout",
    sidebar = sidebar(
      width = 370, class = "customise-sidebar",
      title = div(class = "customise-title",
                  span("Customise"),
                  actionButton("reset_plots", "Reset to default", icon = icon("rotate-left"),
                               class = "btn-outline-secondary btn-sm")),
      accordion(
        open = FALSE,   # both sections start closed, so users see there are two to edit
        accordion_panel(
          "Traffic light plot (A)", icon = icon("table-cells"),
          h6(class = "side-head", "Text"),
          layout_columns(col_widths = c(4, 8),
                         textInput("tl_tag", "Panel letter", "A"),
                         textInput("tl_title", "Title above everything", "")),
          textInput("tl_header", "Heading above the domains", "RoBDEMAT domains"),
          textInput("tl_ylab", "Study column title", "Study"),
          checkboxInput("tl_show_key", "Show the domain key below the table", TRUE),
          textInput("tl_key_title", "Domain key title", "Domains:"),
          selectInput("tl_legend_pos", "Judgement legend",
                      c("Beside the domain key" = "beside", "Below the domain key" = "below", "Hidden" = "none")),
          textInput("legend_title", "Judgement legend title", "Judgements:"),
          textAreaInput("tl_footer", "Extra footnote text", "", rows = 2,
                        placeholder = "For example: Assessed with RoBDEMAT (Delgado et al., 2022)"),
          layout_columns(col_widths = c(6, 6),
                         sliderInput("base_size", "Text size (pt)", 7, 16, 10, 0.5),
                         selectInput("font_family", "Font", c("Sans-serif" = "sans", "Serif" = "serif", "Monospace" = "mono"))),
          h6(class = "side-head", "Table layout"),
          sliderInput("col_w", "Domain column width (cm)", 0.8, 3, 1.4, 0.05),
          sliderInput("row_h", "Study row height (cm)", 0.5, 2, 0.95, 0.05),
          sliderInput("header_h", "Header row height (cm)", 0.3, 1.5, 0.55, 0.05),
          layout_columns(col_widths = c(6, 6),
                         sliderInput("point_size", "Marker size", 2, 15, 6.5, 0.25),
                         sliderInput("symbol_size", "Symbol size", 1, 8, 3, 0.25)),
          checkboxInput("show_empty", "Show empty cells as faint outlines", TRUE),
          h6(class = "side-head", "Table colours"),
          colourInput("grid_col", "Table line colour", "#A6A6A6", showColour = "both"),
          colourInput("header_fill", "Header and study cell colour", "#D9D9D9", showColour = "both")
        ),
        accordion_panel(
          "Summary plot (B)", icon = icon("chart-bar"),
          h6(class = "side-head", "Text"),
          layout_columns(col_widths = c(4, 8),
                         textInput("sum_tag", "Panel letter", "B"),
                         textInput("sum_title", "Title", "RoBDEMAT summary plot")),
          textInput("sum_xlab", "Axis title", "% of studies"),
          selectInput("sum_label_type", "Domain labels",
                      c("Label above name" = "stack", "Label: name" = "both",
                        "Short label only" = "short", "Full name only" = "full")),
          selectInput("sum_values", "Numbers in the bars",
                      c("Number of studies" = "count", "Percentage" = "percent", "None" = "none")),
          selectInput("sum_legend_pos", "Legend position",
                      c("Bottom" = "bottom", "Top" = "top", "Right" = "right", "Hidden" = "none")),
          textInput("sum_legend_title", "Legend title", ""),
          textAreaInput("sum_footer", "Footnote text", "", rows = 2),
          layout_columns(col_widths = c(6, 6),
                         sliderInput("sum_base_size", "Text size (pt)", 7, 16, 10, 0.5),
                         selectInput("sum_font_family", "Font", c("Sans-serif" = "sans", "Serif" = "serif", "Monospace" = "mono"))),
          h6(class = "side-head", "Layout"),
          sliderInput("sum_row_h", "Bar row height (cm)", 0.5, 2, 0.85, 0.05),
          sliderInput("sum_wrap", "Wrap domain labels after (characters)", 15, 100, 40, 1),
          checkboxInput("sum_groups", "Group bars under domain headings", FALSE),
          checkboxInput("sum_include_overall", "Include the overall column", TRUE)
        )
      )
    ),
    navset_card_tab(
      id = "plot_tabs",
      nav_panel("Traffic light plot (A)", value = "tl", figure_output("tl_plot")),
      nav_panel("Summary plot (B)", value = "sum", figure_output("sum_plot")),
      nav_panel("Combined figure (A + B)", value = "both", figure_output("both_plot"))
    ),
    card(
      class = "export-bar",
      div(class = "export-head", icon("download"), " Export"),
      div(class = "export-row",
          selectInput("exp_format", "File format",
                      c("PNG" = "png", "TIFF (uncompressed)" = "tiff", "TIFF (LZW compressed)" = "tiff_lzw",
                        "JPEG" = "jpeg", "PDF (vector)" = "pdf"),
                      width = "210px"),
          selectInput("exp_dpi", "Resolution (dpi)", c("300", "600", "1200"), "300", width = "150px"),
          div(class = "export-check", checkboxInput("exp_custom", "Use custom dimensions", FALSE)),
          conditionalPanel("input.exp_custom", class = "export-dims",
                           numericInput("exp_width", "Width (cm)", 18, 5, 60, 0.5, width = "110px"),
                           numericInput("exp_height", "Height (cm)", 20, 3, 80, 0.5, width = "110px"))),
      conditionalPanel("input.exp_custom",
                       p(class = "help-note mb-2", "The figure keeps its proportions and is centred on a page of this size.")),
      div(class = "d-flex align-items-center gap-3 flex-wrap",
          conditionalPanel("input.plot_tabs == 'tl'",
                           downloadButton("dl_tl", "Download traffic light plot", class = "btn-primary")),
          conditionalPanel("input.plot_tabs == 'sum'",
                           downloadButton("dl_sum", "Download summary plot", class = "btn-primary")),
          conditionalPanel("input.plot_tabs == 'both'",
                           downloadButton("dl_both", "Download combined figure", class = "btn-primary")),
          span(class = "text-muted", textOutput("export_size", inline = TRUE)))
    ),
    div(class = "nav-buttons",
        actionButton("back_assess", "Back to assessments", class = "btn-outline-secondary", icon = icon("arrow-left")),
        span())
  )
)

ui <- page_navbar(
  title = span(icon("traffic-light"), " AnyRoB"),
  id = "nav",
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#1F5F7A", secondary = "#5B6770"),
  header = tags$head(tags$style(HTML(app_css)), tags$script(HTML(help_js)), tags$script(HTML(table_fit_js)), tags$script(HTML(wheel_js))),
  start_page, setup_page, assess_page, plots_page,
  nav_spacer(),
  nav_item(downloadButton("save_project", "Save progress", class = "btn-success btn-sm save-btn",
                          icon = icon("floppy-disk")))
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    domains = templates$robdemat$domains,
    tree = list(), tree_ver = 0, node_counter = 0,
    j_ids = numeric(0), j_init = list(), j_counter = 0, j_prev = character(0),
    use_overall = templates$robdemat$overall,
    use_sub = templates$robdemat$use_sub,
    assess = data.frame(Study = character(0), stringsAsFactors = FALSE),
    grid_ver = 0,
    pending = NULL
  )
  
  alert <- function(type, ...) div(class = paste0("alert alert-", type, " py-2 mb-2"), ...)
  
  # ---- Judgements: dynamic editor -------------------------------------------
  set_judgements <- function(df) {
    df <- clean_judgements(df)
    n <- nrow(df)
    start <- isolate(rv$j_counter)
    ids <- start + seq_len(n)
    rv$j_counter <- start + n
    init <- lapply(seq_len(n), function(i) as.list(df[i, c("Name", "Fill", "Symbol", "SymbolColour", "InLegend")]))
    names(init) <- as.character(ids)
    rv$j_init <- init
    rv$j_prev <- setNames(df$Name, ids)
    rv$j_ids <- ids
  }
  set_judgements(templates$robdemat$judgements)
  
  current_judgements <- reactive({
    ids <- rv$j_ids
    init <- rv$j_init
    if (!length(ids)) {
      return(cbind(id = numeric(0), make_judgements(character(0), character(0), character(0), character(0))))
    }
    pick <- function(prefix, field) vapply(ids, function(i) {
      v <- input[[paste0(prefix, i)]]
      if (is.null(v)) v <- init[[as.character(i)]][[field]]
      if (is.null(v) || length(v) == 0 || is.na(v)) "" else as.character(v)
    }, character(1))
    data.frame(id = ids,
               Name = trimws(pick("jn_", "Name")),
               Fill = to_hex(pick("jf_", "Fill")),
               Symbol = pick("js_", "Symbol"),
               SymbolColour = to_hex(pick("jc_", "SymbolColour"), "#000000"),
               InLegend = vapply(ids, function(i) {
                 v <- input[[paste0("jl_", i)]]
                 if (is.null(v)) v <- init[[as.character(i)]][["InLegend"]]
                 !isFALSE(v)
               }, logical(1)),
               stringsAsFactors = FALSE)
  })
  judgements <- debounce(current_judgements, 400)
  j_valid <- reactive({
    j <- judgements()
    j[nzchar(j$Name) & !duplicated(j$Name), , drop = FALSE]
  })
  
  snapshot_judgements <- function() {
    j <- isolate(current_judgements())
    init <- lapply(seq_len(nrow(j)), function(i) {
      list(Name = j$Name[i], Fill = j$Fill[i], Symbol = j$Symbol[i], SymbolColour = j$SymbolColour[i],
           InLegend = j$InLegend[i])
    })
    names(init) <- as.character(j$id)
    rv$j_init <- init
  }
  
  output$judgement_ui <- renderUI({
    ids <- rv$j_ids
    init <- isolate(rv$j_init)
    if (!length(ids)) {
      return(p(class = "help-note", "No judgements yet. Click 'Add judgement' to create one."))
    }
    js_btn <- function(cls, title, js, ico) {
      tags$button(type = "button", class = paste("btn btn-sm", cls), title = title,
                  `aria-label` = title, onclick = js, icon(ico))
    }
    rows <- lapply(ids, function(id) {
      v <- init[[as.character(id)]]
      div(class = "j-row",
          textInput(paste0("jn_", id), NULL, v$Name, placeholder = "Judgement name"),
          colourInput(paste0("jf_", id), NULL, v$Fill, showColour = "both"),
          textInput(paste0("js_", id), NULL, v$Symbol, placeholder = "e.g. +"),
          colourInput(paste0("jc_", id), NULL, v$SymbolColour, showColour = "both"),
          div(class = "j-legend", title = "Show this judgement in the plot legends",
              checkboxInput(paste0("jl_", id), NULL, !isFALSE(v$InLegend))),
          div(class = "j-btns",
              js_btn("btn-outline-secondary", "Move up",
                     sprintf("Shiny.setInputValue('j_move', {id: %d, dir: 'up'}, {priority: 'event'})", id), "arrow-up"),
              js_btn("btn-outline-secondary", "Move down",
                     sprintf("Shiny.setInputValue('j_move', {id: %d, dir: 'down'}, {priority: 'event'})", id), "arrow-down"),
              js_btn("btn-outline-danger", "Delete judgement",
                     sprintf("Shiny.setInputValue('j_delete', %d, {priority: 'event'})", id), "trash")))
    })
    tagList(
      div(class = "j-row j-head", span("Judgement name"), span("Colour"), span("Symbol"),
          span("Symbol colour"), span(class = "text-center", title = "Show in the plot legends", "Legend"), span("")),
      rows
    )
  })
  
  observeEvent(input$add_judgement, {
    snapshot_judgements()
    id <- rv$j_counter + 1
    rv$j_counter <- id
    palette <- c("#2E7D32", "#F9A825", "#C62828", "#4EA1F7", "#9E9E9E", "#8E44AD", "#E67E22")
    init <- rv$j_init
    init[[as.character(id)]] <- list(Name = paste("New judgement", length(rv$j_ids) + 1),
                                     Fill = palette[(length(rv$j_ids) %% length(palette)) + 1],
                                     Symbol = "", SymbolColour = "#000000", InLegend = TRUE)
    rv$j_init <- init
    rv$j_ids <- c(rv$j_ids, id)
  })
  
  observeEvent(input$j_delete, {
    snapshot_judgements()
    rv$j_ids <- setdiff(rv$j_ids, input$j_delete)
  })
  
  observeEvent(input$j_move, {
    snapshot_judgements()
    ids <- rv$j_ids
    i <- match(input$j_move$id, ids)
    j <- if (identical(input$j_move$dir, "up")) i - 1 else i + 1
    if (!is.na(i) && j >= 1 && j <= length(ids)) {
      ids[c(i, j)] <- ids[c(j, i)]
      rv$j_ids <- ids
    }
  })
  
  # Renaming a judgement updates every cell that used the old name
  observeEvent(judgements(), {
    j <- judgements()
    prev <- rv$j_prev
    a <- rv$assess
    cols <- setdiff(names(a), "Study")
    changed <- FALSE
    for (k in seq_len(nrow(j))) {
      key <- as.character(j$id[k])
      new <- j$Name[k]
      old <- if (key %in% names(prev)) prev[[key]] else NA_character_
      if (!nzchar(new) || new %in% j$Name[-k]) next
      if (!is.na(old) && nzchar(old) && old != new && nrow(a) && length(cols)) {
        for (cc in cols) {
          hit <- !is.na(a[[cc]]) & a[[cc]] == old
          if (any(hit)) { a[[cc]][hit] <- new; changed <- TRUE }
        }
      }
      prev[key] <- new
    }
    rv$j_prev <- prev
    if (changed) {
      rv$assess <- a
      rv$grid_ver <- rv$grid_ver + 1
    }
  })
  
  # ---- Domains ---------------------------------------------------------------
  output$domain_help <- renderUI({
    p(class = "help-note", if (isTRUE(rv$use_sub)) {
      "Each domain becomes a shaded heading in the plot, and its subdomains become the columns underneath it. Labels (such as D1 or 1.1) appear on the plot; full names appear in the key below it. A domain with no subdomains becomes a single column."
    } else {
      "Each domain becomes one column of the plot. Labels (such as D1) appear on the plot; full names appear in the key below it."
    })
  })
  
  # ---- Domain tree editor ----
  new_id <- function() { id <- isolate(rv$node_counter) + 1; rv$node_counter <- id; id }
  set_tree <- function(d) {
    rv$tree <- tree_from_domains(d, new_id)
    rv$tree_ver <- isolate(rv$tree_ver) + 1
  }
  
  # The tree with whatever the user has typed so far
  current_tree <- reactive({
    rv$tree_ver
    tree <- rv$tree
    val <- function(prefix, id, fallback) {
      v <- input[[paste0(prefix, id)]]
      if (is.null(v)) fallback else trimws(v)
    }
    out <- lapply(tree, function(dm) {
      dm$label <- val("dl_", dm$id, dm$label)
      dm$title <- val("dt_", dm$id, dm$title)
      dm$subs <- lapply(dm$subs, function(sb) {
        sb$label <- val("sl_", sb$id, sb$label)
        sb$title <- val("st_", sb$id, sb$title)
        sb
      })
      dm
    })
    attr(out, "ver") <- rv$tree_ver   # lets stale (debounced) copies be recognised
    out
  })
  tree_d <- debounce(current_tree, 400)
  snapshot_tree <- function() rv$tree <- isolate(current_tree())
  set_tree(templates$robdemat$domains)
  
  observeEvent(list(tree_d(), rv$use_sub), {
    t <- tree_d()
    # Ignore an out-of-date copy, e.g. the previous template's domains still
    # waiting in the 400 ms typing delay when a new template is loaded
    if (!identical(attr(t, "ver"), rv$tree_ver)) return()
    rv$domains <- domains_from_tree(t, isTRUE(rv$use_sub))
  })
  
  output$domain_editor <- renderUI({
    rv$tree_ver
    use_sub <- isTRUE(rv$use_sub)
    tree <- isolate(rv$tree)
    if (!length(tree)) return(p(class = "help-note", "No domains yet. Click 'Add domain' to create one."))
    act <- function(a, id, ico, title, cls = "btn-outline-secondary") {
      tags$button(type = "button", class = paste("btn btn-sm", cls), title = title, `aria-label` = title,
                  onclick = sprintf("Shiny.setInputValue('dom_action', {act: '%s', id: %d}, {priority: 'event'})", a, id),
                  icon(ico))
    }
    move_btns <- function(kind, id, del_title) {
      div(class = "tree-btns",
          act(paste0(kind, "_up"), id, "arrow-up", "Move up"),
          act(paste0(kind, "_down"), id, "arrow-down", "Move down"),
          act(paste0(kind, "_del"), id, "trash", del_title, "btn-outline-danger"))
    }
    cards <- lapply(seq_along(tree), function(i) {
      dm <- tree[[i]]
      n_sub <- length(dm$subs)
      div(class = "dom-card",
          div(class = "dom-kicker", span(paste("Domain", i)),
              if (use_sub) span(class = "count", if (n_sub == 1) "1 subdomain" else sprintf("%d subdomains", n_sub))),
          div(class = "tree-row",
              div(class = "lbl", textInput(paste0("dl_", dm$id), NULL, dm$label, placeholder = "D1")),
              textInput(paste0("dt_", dm$id), NULL, dm$title, placeholder = "Full domain name, e.g. Bias in planning and allocation"),
              move_btns("dom", dm$id, "Delete this domain and its subdomains")),
          if (use_sub) div(class = "sub-list",
                           if (n_sub) p(class = "sub-caption", "Subdomains (one column each)"),
                           lapply(dm$subs, function(sb) {
                             div(class = "tree-row",
                                 div(class = "lbl", textInput(paste0("sl_", sb$id), NULL, sb$label, placeholder = "1.1")),
                                 textInput(paste0("st_", sb$id), NULL, sb$title, placeholder = "Full subdomain name"),
                                 move_btns("sub", sb$id, "Delete this subdomain"))
                           }),
                           tags$button(type = "button", class = "btn btn-link add-sub",
                                       onclick = sprintf("Shiny.setInputValue('dom_action', {act: 'sub_add', id: %d}, {priority: 'event'})", dm$id),
                                       icon("plus"), " Add subdomain"))
      )
    })
    tagList(div(class = "tree-head", span("Label"), span("Full name (shown in the key)"), span("")), cards)
  })
  
  observeEvent(input$add_domain, {
    snapshot_tree()
    tree <- rv$tree
    taken <- vapply(tree, `[[`, "", "label")
    lab <- next_label("D", taken, length(tree) + 1)
    num <- sub("^[^0-9]*", "", lab)
    subs <- if (isTRUE(rv$use_sub)) list(list(id = new_id(), label = paste0(num, ".1"), title = "")) else list()
    tree[[length(tree) + 1]] <- list(id = new_id(), label = lab, title = "", subs = subs)
    rv$tree <- tree
    rv$tree_ver <- rv$tree_ver + 1
  })
  
  observeEvent(input$dom_action, {
    a <- input$dom_action
    snapshot_tree()
    tree <- rv$tree
    swap <- function(x, i, j) { if (j >= 1 && j <= length(x)) x[c(i, j)] <- x[c(j, i)]; x }
    di <- which(vapply(tree, function(dm) dm$id == a$id, logical(1)))
    if (length(di)) {
      tree <- switch(a$act,
                     dom_up = swap(tree, di, di - 1),
                     dom_down = swap(tree, di, di + 1),
                     dom_del = tree[-di],
                     sub_add = {
                       dm <- tree[[di]]
                       num <- sub("^[^0-9]*", "", dm$label)
                       # D3 -> 3.1, 3.2 ...; a label without a number (e.g. COMPAR) -> 1, 2 ...
                       prefix <- if (nzchar(num)) paste0(num, ".") else ""
                       dm$subs[[length(dm$subs) + 1]] <- list(
                         id = new_id(), label = next_label(prefix, vapply(dm$subs, `[[`, "", "label"), length(dm$subs) + 1),
                         title = "")
                       tree[[di]] <- dm
                       tree
                     },
                     tree)
    } else {
      for (k in seq_along(tree)) {
        si <- which(vapply(tree[[k]]$subs, function(sb) sb$id == a$id, logical(1)))
        if (length(si)) {
          tree[[k]]$subs <- switch(a$act,
                                   sub_up = swap(tree[[k]]$subs, si, si - 1),
                                   sub_down = swap(tree[[k]]$subs, si, si + 1),
                                   sub_del = tree[[k]]$subs[-si],
                                   tree[[k]]$subs)
          break
        }
      }
    }
    rv$tree <- tree
    rv$tree_ver <- rv$tree_ver + 1
  })
  
  observeEvent(input$use_sub, {
    want <- isTRUE(input$use_sub)
    if (want == isTRUE(rv$use_sub)) return()
    snapshot_tree()
    has_subs <- any(vapply(rv$tree, function(dm) length(dm$subs) > 0, logical(1)))
    if (!want && has_subs && has_work()) {
      showNotification("Subdomain columns were merged into their domains, and judgements entered for subdomains were cleared. Your subdomain list is kept: switch subdomains back on to see it again.",
                       type = "warning", duration = 8)
    }
    rv$use_sub <- want
    rv$tree_ver <- rv$tree_ver + 1
  }, ignoreInit = TRUE)
  
  valid_domains <- reactive(validate_domains(rv$domains))
  
  observeEvent(input$use_overall, rv$use_overall <- isTRUE(input$use_overall), ignoreInit = TRUE)
  
  # Reshape the spreadsheet whenever the domain list changes
  observeEvent(list(valid_domains()$Key, rv$use_overall), {
    rv$assess <- conform_assess(rv$assess, valid_domains()$Key, rv$use_overall)
    rv$grid_ver <- rv$grid_ver + 1
  })
  
  output$setup_check <- renderUI({
    raw <- rv$domains
    d <- valid_domains()
    j <- judgements()
    jv <- j_valid()
    msgs <- list()
    raw_keys <- domain_keys(raw)
    dup <- unique(sub("::", " ", raw_keys[duplicated(raw_keys)], fixed = TRUE))
    if (length(dup)) msgs <- c(msgs, list(alert("warning", "Labels must be unique within a domain (the same subdomain label can be reused under different domains). Repeated, so only the first is kept: ", paste(dup, collapse = ", "), ".")))
    if ("Study" %in% raw$Label) msgs <- c(msgs, list(alert("warning", "'Study' cannot be used as a domain label. Rename that domain.")))
    gk <- ifelse(nzchar(d$Group), d$Group, d$GroupName)
    split_groups <- unique(gk[nzchar(gk)][duplicated(rle(gk[nzchar(gk)])$values)])
    if (length(split_groups)) msgs <- c(msgs, list(alert("warning", "These domains are split by other rows, so they appear as separate headings: ",
                                                         paste(split_groups, collapse = ", "), ". Keep each domain's subdomains in consecutive rows.")))
    if (!nrow(d)) msgs <- c(msgs, list(alert("danger", "Add at least one domain.")))
    if (any(!nzchar(j$Name))) msgs <- c(msgs, list(alert("warning", "One or more judgements have no name and will be ignored until you name them.")))
    jd <- unique(j$Name[duplicated(j$Name) & nzchar(j$Name)])
    if (length(jd)) msgs <- c(msgs, list(alert("warning", "Judgement names must be unique. Repeated: ", paste(jd, collapse = ", "), ".")))
    if (!nrow(jv)) msgs <- c(msgs, list(alert("danger", "Add at least one judgement.")))
    groups <- unique(gk[nzchar(gk)])
    s <- function(n, one, many) if (n == 1) one else many
    summary <- alert("info", icon("circle-info"), sprintf(
      " %s, %d %s%s.",
      if (isTRUE(rv$use_sub) && length(groups)) {
        sprintf("%d %s with %d %s", length(groups), s(length(groups), "domain", "domains"),
                nrow(d), s(nrow(d), "subdomain", "subdomains"))
      } else sprintf("%d %s", nrow(d), s(nrow(d), "domain", "domains")),
      nrow(jv), s(nrow(jv), "judgement", "judgements"),
      if (isTRUE(rv$use_overall)) ", plus an overall column" else ""))
    tagList(summary, msgs)
  })
  
  # ---- Assessment spreadsheet -----------------------------------------------
  output$grid <- renderRHandsontable({
    rv$grid_ver
    d <- valid_domains()
    jv <- j_valid()
    ol <- nz(input$overall_label) %||% "Overall"
    a <- isolate(rv$assess)
    cols <- setdiff(names(a), "Study")
    if (!nrow(a)) a <- blank_rows(names(a), 3)
    df <- a
    df[] <- lapply(df, as.character)
    names(df) <- c("Study", if (length(cols)) paste0("c", seq_along(cols)))
    rownames(df) <- NULL
    headers <- c("Study", vapply(cols, function(cc) {
      if (cc == ".overall") return(col_header("", ol))
      i <- match(cc, d$Key)
      g <- if (is.na(i)) "" else if (nzchar(d$Group[i])) d$Group[i] else d$GroupName[i]
      col_header(g, if (is.na(i)) cc else d$Label[i])
    }, character(1), USE.NAMES = FALSE))
    hot <- rhandsontable(df, colHeaders = headers, rowHeaders = TRUE, stretchH = "all",
                         minSpareRows = 1,   # no fixed height: the table grows with the studies
                         columnHeaderHeight = 44, manualColumnResize = TRUE) |>
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
      hot_col(1, type = "text")
    if (length(cols)) {
      hot <- hot |>
        hot_col(seq_along(cols) + 1, type = "dropdown", source = I(jv$Name), strict = TRUE,
                allowInvalid = TRUE, renderer = cell_renderer(jv)) |>
        hot_cols(colWidths = c(220, rep(110, length(cols))))
    }
    hot
  })
  
  observeEvent(input$grid, {
    df <- hot_to_r(input$grid)
    a <- rv$assess
    cols <- setdiff(names(a), "Study")
    if (is.null(df) || ncol(df) != length(cols) + 1) return()
    df <- as.data.frame(lapply(df, function(x) {
      x <- trimws(as.character(x)); x[!is.na(x) & x == ""] <- NA; x
    }), stringsAsFactors = FALSE)
    names(df) <- c("Study", cols)
    keep <- rowSums(!is.na(df)) > 0
    out <- df[keep, , drop = FALSE]
    rownames(out) <- NULL
    rv$assess <- out
  })
  
  # ---- Excel template download / upload ----
  overall_lab <- function() if (isTRUE(rv$use_overall)) (nz(input$overall_label) %||% "Overall") else NULL
  
  
  pending_upload <- reactiveVal(NULL)
  apply_upload <- function(res) {
    d <- valid_domains()
    rv$assess <- conform_assess(res$assess, d$Key, isTRUE(rv$use_overall))
    rv$grid_ver <- rv$grid_ver + 1
    n <- sum(!is.na(res$assess$Study))
    notes <- c(
      if (length(res$missing)) paste0("Columns not found in the file (left empty): ", paste(res$missing, collapse = ", "), "."),
      if (length(res$extra)) paste0("Columns in the file that don't match a domain (ignored): ", paste(res$extra, collapse = ", "), "."),
      if (length(res$invalid)) paste0("Values that aren't one of your judgements (shaded red): ", paste(res$invalid, collapse = ", "), "."))
    showNotification(tagList(strong(sprintf("Loaded %d %s from Excel.", n, if (n == 1) "study" else "studies")),
                             if (length(notes)) tags$ul(class = "mb-0 mt-1 ps-3", lapply(notes, tags$li))),
                     type = if (length(notes)) "warning" else "message", duration = if (length(notes)) 12 else 5)
  }
  observeEvent(input$upload_template, {
    res <- tryCatch(read_assessment_upload(input$upload_template$datapath, valid_domains(),
                                           j_valid(), overall_lab()),
                    error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("This file could not be read:", conditionMessage(res)), type = "error", duration = 10)
      return()
    }
    if (has_work()) {
      pending_upload(res)
      showModal(modalDialog(
        title = "Replace your current assessments?",
        "The uploaded file will replace the studies and judgements currently in the spreadsheet.",
        footer = tagList(modalButton("Cancel"),
                         actionButton("confirm_upload", "Replace with the uploaded file", class = "btn-danger")),
        easyClose = TRUE))
    } else {
      apply_upload(res)
    }
  })
  observeEvent(input$confirm_upload, {
    removeModal()
    if (!is.null(pending_upload())) apply_upload(pending_upload())
    pending_upload(NULL)
  })
  
  observe({
    jv <- j_valid()
    sel <- isolate(input$fill_value)
    updateSelectInput(session, "fill_value", choices = jv$Name,
                      selected = if (!is.null(sel) && sel %in% jv$Name) sel else NULL)
  })
  
  observeEvent(input$fill_empty, {
    a <- rv$assess
    val <- input$fill_value %||% ""
    cols <- setdiff(names(a), "Study")
    if (!nzchar(val) || !nrow(a) || !length(cols)) {
      showNotification("Add at least one study and one judgement first.", type = "warning")
      return()
    }
    n <- 0
    for (cc in cols) {
      empty <- is.na(a[[cc]]) | a[[cc]] == ""
      n <- n + sum(empty)
      a[[cc]][empty] <- val
    }
    rv$assess <- a
    rv$grid_ver <- rv$grid_ver + 1
    showNotification(sprintf("Filled %d empty %s with '%s'.", n, if (n == 1) "cell" else "cells", val),
                     type = "message")
  })
  
  output$assess_progress <- renderText({
    a <- rv$assess
    cols <- setdiff(names(a), "Study")
    n <- sum(!is.na(a$Study) & nzchar(trimws(a$Study)))
    total <- nrow(a) * length(cols)
    done <- if (total) sum(!is.na(as.matrix(a[cols]))) else 0
    sprintf("%d %s, %d of %d cells filled", n, if (n == 1) "study" else "studies", done, total)
  })
  
  output$assess_check <- renderUI({
    a <- rv$assess
    jv <- j_valid()
    cols <- setdiff(names(a), "Study")
    msgs <- list()
    if (!nrow(jv)) msgs <- c(msgs, list(alert("warning", "No judgements are defined. Add them on the Set up page.")))
    if (!length(cols)) msgs <- c(msgs, list(alert("warning", "No domains are defined. Add them on the Set up page.")))
    if (nrow(a)) {
      nm <- a$Study
      unnamed <- sum(is.na(nm) | !nzchar(trimws(nm)))
      if (unnamed) msgs <- c(msgs, list(alert("warning", sprintf(
        "%d %s judgements but no study name. Rows without a name are left out of the plots.",
        unnamed, if (unnamed == 1) "row has" else "rows have"))))
      named <- nm[!is.na(nm) & nzchar(trimws(nm))]
      dups <- unique(named[duplicated(named)])
      if (length(dups)) msgs <- c(msgs, list(alert("warning", "These study names appear more than once and will be numbered in the plots: ",
                                                   paste(dups, collapse = ", "), ".")))
      if (length(cols)) {
        vals <- unlist(a[cols], use.names = FALSE)
        bad <- unique(vals[!is.na(vals) & !(vals %in% jv$Name)])
        if (length(bad)) msgs <- c(msgs, list(alert("danger", "Some cells (shaded red) hold values that are not in your judgement list: ",
                                                    paste(bad, collapse = ", "),
                                                    ". Correct them, or add them as judgements on the Set up page. Until then they are shown as empty.")))
        empty <- sum(is.na(vals))
        if (empty) msgs <- c(msgs, list(alert("info", sprintf(
          "%d empty %s. Empty cells show as faint outlines in the traffic light plot and are left out of the summary plot.",
          empty, if (empty == 1) "cell" else "cells"))))
      }
    }
    if (!length(msgs)) msgs <- list(alert("success", icon("check"), " Every cell is filled with a valid judgement."))
    tagList(msgs)
  })
  
  # ---- Plot settings and data -----------------------------------------------
  plot_settings <- reactive({
    list(
      base_size = input$base_size %||% 10, family = input$font_family %||% "sans",
      col_w = input$col_w %||% 1.4, row_h = input$row_h %||% 0.95, header_h = input$header_h %||% 0.55,
      study_w = NA,                    # study column always fits the longest name
      shape = as.integer(input$marker_shape %||% "21"),
      fit_footer = TRUE,               # legend always lines up with the table's right edge
      point_size = input$point_size %||% 6.5, symbol_size = input$symbol_size %||% 3,
      outline = if (isTRUE(input$use_outline)) (input$outline_col %||% "#4D4D4D") else NA,
      outline_lwd = input$outline_lwd %||% 0.8,
      grid_col = input$grid_col %||% "#A6A6A6", header_fill = input$header_fill %||% "#D9D9D9",
      cell_fill = "#FFFFFF", show_empty = isTRUE(input$show_empty %||% TRUE),
      header_title = input$tl_header %||% "", ylab = input$tl_ylab %||% "Study",
      title = input$tl_title %||% "", tag = input$tl_tag %||% "",
      show_key = isTRUE(input$tl_show_key %||% TRUE), key_title = input$tl_key_title %||% "Domains:",
      legend_pos = input$tl_legend_pos %||% "beside", legend_title = input$legend_title %||% "Judgements:",
      footer = input$tl_footer %||% "",
      include_overall = isTRUE(input$sum_include_overall %||% TRUE),
      label_type = input$sum_label_type %||% "stack", sum_wrap = input$sum_wrap %||% 40,
      sum_legend_pos = input$sum_legend_pos %||% "bottom", sum_legend_title = input$sum_legend_title %||% "",
      values = input$sum_values %||% "count", sum_title = input$sum_title %||% "",
      sum_xlab = input$sum_xlab %||% "", sum_footer = input$sum_footer %||% "",
      sum_tag = input$sum_tag %||% "", sum_groups = isTRUE(input$sum_groups),
      sum_row_h = input$sum_row_h %||% 0.85
    )
  })
  settings_d <- debounce(plot_settings, 350)
  
  plot_data <- reactive({
    d <- valid_domains()
    jv <- j_valid()
    a <- rv$assess
    validate(
      need(nrow(d) > 0, "Add at least one domain on the Set up page."),
      need(nrow(jv) > 0, "Add at least one named judgement on the Set up page."),
      need(nrow(a) > 0 && any(!is.na(a$Study) & nzchar(trimws(a$Study))),
           "Enter at least one study on the Assessments page to see your plots.")
    )
    build_plot_data(d, jv, a, nz(input$overall_label) %||% "Overall")
  })
  
  fig_tl <- reactive(draw_traffic(plot_data(), settings_d()))
  fig_sum <- reactive({
    s <- settings_d()
    s$base_size <- input$sum_base_size %||% s$base_size
    s$family <- input$sum_font_family %||% s$family
    f <- draw_summary(plot_data(), s, fig_tl()$w)
    validate(need(!is.null(f), "No judgements have been entered yet."))
    f
  })
  fig_both <- reactive(combine_figures(fig_tl(), fig_sum()))
  
  px <- function(cm) max(50, round(cm * PX_PER_CM))
  safe_dim <- function(fig, dim, fallback) function() tryCatch(px(fig()[[dim]]), error = function(e) fallback)
  
  # Live preview on the Set up page, filled with placeholder studies
  # Once at least one study is named on the Assessments page, the preview shows
  # the real assessments; until then it uses placeholder studies.
  real_studies <- reactive({
    a <- rv$assess
    if (!nrow(a)) return(0L)
    sum(!is.na(a$Study) & nzchar(trimws(a$Study)))
  })
  output$preview_real <- reactive(real_studies() > 0)
  outputOptions(output, "preview_real", suspendWhenHidden = FALSE)
  output$preview_count <- renderText({
    n <- real_studies()
    sprintf("Your %d %s", n, if (n == 1) "study" else "studies")
  })
  output$preview_note <- renderUI({
    p(class = "help-note", if (real_studies() > 0) {
      "Your domains and judgements laid out as the final traffic light plot, using the studies you entered on the Assessments page. Text, size and colour settings come from step 3."
    } else {
      "Your domains and judgements laid out as the final traffic light plot, filled with placeholder studies until you enter your own on the Assessments page. Text, size and colour settings come from step 3."
    })
  })
  
  fig_preview <- reactive({
    d <- valid_domains()
    jv <- j_valid()
    validate(need(nrow(d) > 0, "Add a domain to see the preview."),
             need(nrow(jv) > 0, "Name a judgement to see the preview."))
    a <- if (real_studies() > 0) {
      rv$assess
    } else {
      # An empty or invalid box (e.g. while retyping the number) falls back to 4
      n <- suppressWarnings(as.integer(input$preview_n))
      if (!length(n) || is.na(n)) n <- 4L
      n <- max(1L, min(30L, n))
      placeholder_assess(d, jv, n, isTRUE(rv$use_overall))
    }
    draw_traffic(build_plot_data(d, jv, a, nz(input$overall_label) %||% "Overall"), settings_d())
  })
  
  output$setup_preview <- renderPlot(draw_fig(fig_preview()),
                                     width = safe_dim(fig_preview, "w", 600),
                                     height = safe_dim(fig_preview, "h", 200), res = 96)
  output$tl_plot <- renderPlot(draw_fig(fig_tl()), width = safe_dim(fig_tl, "w", 600),
                               height = safe_dim(fig_tl, "h", 200), res = 96)
  output$sum_plot <- renderPlot(draw_fig(fig_sum()), width = safe_dim(fig_sum, "w", 600),
                                height = safe_dim(fig_sum, "h", 200), res = 96)
  output$both_plot <- renderPlot(draw_fig(fig_both()), width = safe_dim(fig_both, "w", 600),
                                 height = safe_dim(fig_both, "h", 200), res = 96)
  
  active_fig <- function() switch(input$plot_tabs %||% "tl", sum = fig_sum(), both = fig_both(), fig_tl())
  output$export_size <- renderText({
    f <- tryCatch(active_fig(), error = function(e) NULL)
    if (is.null(f)) return("")
    if (!isTRUE(input$exp_custom)) sprintf("Exports at %.1f x %.1f cm", f$w, f$h)
    else sprintf("Exports at %.1f x %.1f cm (custom size)", input$exp_width, input$exp_height)
  })
  # Start custom dimensions from the current figure's size
  observeEvent(input$exp_custom, {
    if (!isTRUE(input$exp_custom)) return()
    f <- tryCatch(active_fig(), error = function(e) NULL)
    if (is.null(f)) return()
    updateNumericInput(session, "exp_width", value = round(f$w, 1))
    updateNumericInput(session, "exp_height", value = round(f$h, 1))
  })
  
  # ---- Downloads -------------------------------------------------------------
  file_ext <- function() switch(input$exp_format, jpeg = "jpg", tiff_lzw = "tiff", input$exp_format)
  
  save_fig <- function(file, fig) {
    fmt <- input$exp_format
    natural <- !isTRUE(input$exp_custom)
    w <- if (natural) fig$w else input$exp_width
    h <- if (natural) fig$h else input$exp_height
    grob <- fig$grob
    if (!natural) {
      # Keep the figure's own proportions, centred on the requested page
      sc <- min(w / fig$w, h / fig$h)
      grob <- gTree(children = gList(rectGrob(gp = gpar(fill = "white", col = NA)),
                                     gTree(children = gList(fig$grob),
                                           vp = viewport(width = unit(fig$w * sc, "cm"),
                                                         height = unit(fig$h * sc, "cm")))))
    }
    dpi <- as.numeric(input$exp_dpi)
    tiff_comp <- if (identical(fmt, "tiff_lzw")) "lzw" else "none"
    if (fmt == "tiff_lzw") fmt <- "tiff"
    if (fmt == "jpeg") {
      # R's own JPEG device stores the dpi in the file (ragg's does not), which
      # journal figure checkers read
      if (capabilities("cairo")) {
        grDevices::jpeg(file, width = w, height = h, units = "cm", res = dpi, quality = 95, type = "cairo")
      } else {
        grDevices::jpeg(file, width = w, height = h, units = "cm", res = dpi, quality = 95)
      }
    } else if (fmt == "pdf") {
      dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
      dev(file, width = w / 2.54, height = h / 2.54)
    } else if (has_ragg) {
      switch(fmt,
             png = ragg::agg_png(file, width = w, height = h, units = "cm", res = dpi),
             tiff = ragg::agg_tiff(file, width = w, height = h, units = "cm", res = dpi, compression = tiff_comp))
    } else {
      switch(fmt,
             png = grDevices::png(file, width = w, height = h, units = "cm", res = dpi),
             tiff = grDevices::tiff(file, width = w, height = h, units = "cm", res = dpi, compression = tiff_comp))
    }
    on.exit(grDevices::dev.off(), add = TRUE)
    grid.newpage()
    grid.draw(grob)
  }
  
  output$dl_tl <- downloadHandler(
    filename = function() paste0("traffic-light-plot.", file_ext()),
    content = function(file) save_fig(file, fig_tl()))
  output$dl_sum <- downloadHandler(
    filename = function() paste0("summary-plot.", file_ext()),
    content = function(file) save_fig(file, fig_sum()))
  output$dl_both <- downloadHandler(
    filename = function() paste0("risk-of-bias-figure.", file_ext()),
    content = function(file) save_fig(file, fig_both()))
  
  # ---- Templates, example data and projects ---------------------------------
  output$template_info <- renderUI({
    tpl <- templates[[input$template]]
    p(class = "help-note", tpl$description)
  })
  
  apply_template <- function(key, example = FALSE) {
    tpl <- templates[[key]]
    rv$tpl_key <- key
    rv$domains <- tpl$domains
    set_tree(tpl$domains)
    rv$use_sub <- tpl$use_sub
    updateCheckboxInput(session, "use_sub", value = tpl$use_sub)
    set_judgements(tpl$judgements)
    rv$use_overall <- tpl$overall
    updateCheckboxInput(session, "use_overall", value = tpl$overall)
    a <- if (example) example_assess(tpl$domains, tpl$judgements, tpl$overall) else NULL
    rv$assess <- conform_assess(a, validate_domains(tpl$domains)$Key, tpl$overall)
    rv$grid_ver <- rv$grid_ver + 1
    updateTextInput(session, "tl_header", value = tpl$header)
    updateTextInput(session, "sum_title", value = tpl$sum_title)
    if (example) {
      showNotification("Example studies loaded. Edit anything on the Set up and Assessments pages.",
                       type = "message", duration = 6)
      nav_select("nav", "plots")
    } else {
      nav_select("nav", "setup")
    }
  }
  
  has_work <- function() {
    a <- rv$assess
    nrow(a) > 0 && any(!is.na(as.matrix(a)))
  }
  
  ask_replace <- function(action) {
    if (!has_work()) return(apply_template(action$key, action$example))
    rv$pending <- action
    showModal(modalDialog(
      title = "Replace the current project?",
      "This replaces your current domains, judgements and assessments. Use 'Save progress' first if you want to keep them.",
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_replace", "Replace project", class = "btn-danger")),
      easyClose = TRUE))
  }
  
  observeEvent(input$use_blank, ask_replace(list(key = "blank", example = FALSE)))
  observeEvent(input$use_template, ask_replace(list(key = input$template, example = FALSE)))
  observeEvent(input$use_example, ask_replace(list(key = input$template, example = TRUE)))
  observeEvent(input$confirm_replace, {
    removeModal()
    if (!is.null(rv$pending)) apply_template(rv$pending$key, rv$pending$example)
  })
  
  # ---- Illustrated help for the Set up sections -----------------------------
  help_topic <- reactiveVal("domains")
  help_text <- list(
    domains = list(
      title = "Domains",
      labels = c("Domain and subdomain labels", "Domain key (footnotes)"),
      body = tagList(
        p("Domains are the columns of the traffic light table, one per item of your risk of bias tool."),
        tags$ol(
          tags$li(strong("Labels: "), "the domain label (such as D1) becomes the shaded heading. With subdomains switched on, subdomain labels (such as 1.1) form the row beneath it, and the heading spans all of its subdomains."),
          tags$li(strong("Footnotes: "), "the full names you type in the footnote columns are listed in the domain key below the table.")),
        p(class = "help-note", "Switch off 'Use subdomains' for tools with a single level of domains, such as RoB 2."))),
    judgements = list(
      title = "Judgements",
      labels = c("Judgement markers", "Judgement legend"),
      body = tagList(
        p("Judgements are the possible answers for each study and domain, such as low, some concerns or high risk."),
        tags$ol(
          tags$li(strong("Markers: "), "each cell of the table shows the judgement as a coloured marker with its symbol. You choose the colour, symbol, symbol colour, marker shape and outline."),
          tags$li(strong("Legend: "), "judgements are listed in the legend in the order you set here; untick 'Legend' for any you want to leave out. Every judgement is an option in the assessment spreadsheet.")),
        p(class = "help-note", "Tip: pick colours that stay distinguishable when printed in greyscale, and use symbols so the figure still reads without colour.")))
  )
  show_help <- function(topic) {
    help_topic(topic)
    h <- help_text[[topic]]
    showModal(modalDialog(
      title = span(icon("circle-question"), " ", h$title),
      div(class = "figure-frame fit mb-3", plotOutput("help_fig", width = "auto", height = "auto")),
      h$body,
      size = "xl", easyClose = TRUE, footer = modalButton("Got it")))
  }
  observeEvent(input$help_domains, show_help("domains"))
  observeEvent(input$help_judgements, show_help("judgements"))
  output$help_fig <- renderPlot({
    f <- fig_preview()
    topic <- help_topic()
    boxes <- if (topic == "domains") list(f$regions$headers, f$regions$key) else list(f$regions$cells, f$regions$legend)
    draw_highlight(f, boxes)
  }, width = function() tryCatch(px(fig_preview()$w + 2 * HELP_MARGIN), error = function(e) 600),
  height = function() tryCatch(px(fig_preview()$h + 2 * HELP_MARGIN), error = function(e) 300), res = 96)
  
  # ---- Save progress / open a saved project ---------------------------------
  # Inputs stored in the project's "Settings" sheet, grouped by how to restore them
  project_inputs <- list(
    text = c("overall_label", "tl_header", "sum_title", names(plot_defaults$text)),
    area = names(plot_defaults$area),
    check = c("use_outline", "exp_custom", names(plot_defaults$check)),
    select = c("marker_shape", "exp_format", "exp_dpi", names(plot_defaults$select)),
    slider = c("outline_lwd", names(plot_defaults$slider)),
    numeric = c("exp_width", "exp_height", "preview_n"),
    colour = c("outline_col", names(plot_defaults$colour)))
  
  project_settings <- function() {
    ids <- unlist(project_inputs, use.names = FALSE)
    vals <- lapply(ids, function(id) { v <- input[[id]]; if (is.null(v)) "" else as.character(v) })
    c(list(app = "AnyRoB project", saved = format(Sys.time(), "%Y-%m-%d %H:%M"),
           template = rv$tpl_key %||% "", use_sub = isTRUE(rv$use_sub), use_overall = isTRUE(rv$use_overall)),
      setNames(vals, ids))
  }
  
  # One file format for both "Save progress" and the Excel template
  write_project <- function(file) {
    write_assessment_template(file, valid_domains(), current_judgements(), rv$assess, overall_lab(),
                              structure = domains_from_tree(current_tree(), TRUE),
                              settings = project_settings())
  }
  
  output$save_project <- downloadHandler(
    filename = function() paste0("AnyRoB-project-", format(Sys.Date()), ".xlsx"),
    content = function(file) write_project(file))
  
  restore_inputs <- function(st) {
    get <- function(id) { v <- st[[id]]; if (is.null(v) || is.na(v)) NULL else v }
    num <- function(v) suppressWarnings(as.numeric(v))
    for (id in project_inputs$text) if (!is.null(v <- get(id))) updateTextInput(session, id, value = v)
    for (id in project_inputs$area) if (!is.null(v <- get(id))) updateTextAreaInput(session, id, value = v)
    for (id in project_inputs$check) if (!is.null(v <- get(id)) && nzchar(v)) updateCheckboxInput(session, id, value = toupper(v) == "TRUE")
    for (id in project_inputs$select) if (!is.null(v <- get(id)) && nzchar(v)) updateSelectInput(session, id, selected = v)
    for (id in project_inputs$slider) if (!is.null(v <- get(id)) && !is.na(num(v))) updateSliderInput(session, id, value = num(v))
    for (id in project_inputs$numeric) if (!is.null(v <- get(id)) && !is.na(num(v))) updateNumericInput(session, id, value = num(v))
    for (id in project_inputs$colour) if (!is.null(v <- get(id)) && nzchar(v)) updateColourInput(session, id, value = to_hex(v, "#4D4D4D"))
  }
  
  pending_project <- reactiveVal(NULL)
  apply_project <- function(pr) {
    key <- pr$settings$template %||% ""
    rv$tpl_key <- if (key %in% names(templates)) key else "blank"
    set_tree(pr$structure)
    rv$use_sub <- pr$use_sub
    updateCheckboxInput(session, "use_sub", value = pr$use_sub)
    rv$domains <- domains_from_tree(isolate(rv$tree), pr$use_sub)
    set_judgements(pr$judgements)
    rv$use_overall <- pr$use_overall
    updateCheckboxInput(session, "use_overall", value = pr$use_overall)
    d <- validate_domains(rv$domains)
    rv$assess <- conform_assess(pr$upload$assess, d$Key, pr$use_overall)
    rv$grid_ver <- rv$grid_ver + 1
    restore_inputs(pr$settings)
    updateTextInput(session, "overall_label", value = pr$overall_label)
    n <- sum(!is.na(pr$upload$assess$Study) & nzchar(pr$upload$assess$Study))
    notes <- c(
      if (length(pr$upload$missing)) paste0("Columns not found in the Assessments sheet (left empty): ", paste(pr$upload$missing, collapse = ", "), "."),
      if (length(pr$upload$extra)) paste0("Columns in the Assessments sheet that don't match a domain (ignored): ", paste(pr$upload$extra, collapse = ", "), "."),
      if (length(pr$upload$invalid)) paste0("Values that aren't one of your judgements (shaded red): ", paste(pr$upload$invalid, collapse = ", "), "."))
    showNotification(tagList(strong(sprintf("Project opened: %d %s, %d %s, %d %s.",
                                            nrow(d), if (nrow(d) == 1) "column" else "columns",
                                            nrow(pr$judgements), if (nrow(pr$judgements) == 1) "judgement" else "judgements",
                                            n, if (n == 1) "study" else "studies")),
                             if (length(notes)) tags$ul(class = "mb-0 mt-1 ps-3", lapply(notes, tags$li))),
                     type = if (length(notes)) "warning" else "message", duration = if (length(notes)) 12 else 6)
    nav_select("nav", if (n > 0) "assess" else "setup")
  }
  
  observeEvent(input$open_project, {
    pr <- tryCatch(read_project_file(input$open_project$datapath), error = function(e) e)
    if (inherits(pr, "error")) {
      showNotification(paste("This file could not be opened:", conditionMessage(pr)), type = "error", duration = 12)
      return()
    }
    if (has_work()) {
      pending_project(pr)
      showModal(modalDialog(
        title = "Replace the current project?",
        "Opening this file replaces your current domains, judgements, assessments and plot settings. Use 'Save progress' first if you want to keep them.",
        footer = tagList(modalButton("Cancel"),
                         actionButton("confirm_open_project", "Open the saved project", class = "btn-danger")),
        easyClose = TRUE))
    } else {
      apply_project(pr)
    }
  })
  observeEvent(input$confirm_open_project, {
    removeModal()
    if (!is.null(pending_project())) apply_project(pending_project())
    pending_project(NULL)
  })
  
  # ---- Citation files ---------------------------------------------------------
  for (fmt in c("ris", "nbib", "bib")) local({
    f <- fmt
    output[[paste0("cite_", f)]] <- downloadHandler(
      filename = function() paste0("AnyRoB-citation.", f),
      content = function(file) writeLines(citation_file(f), file, sep = "", useBytes = TRUE))
  })
  
  # ---- Reset plot customisation ----------------------------------------------
  observeEvent(input$reset_plots, {
    showModal(modalDialog(
      title = "Reset plot settings?",
      "All traffic light and summary plot settings (text, sizes, fonts and colours) go back to their defaults. Your domains, judgements and assessments are not changed.",
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_reset_plots", "Reset to default", class = "btn-danger")),
      easyClose = TRUE))
  })
  observeEvent(input$confirm_reset_plots, {
    removeModal()
    tpl <- templates[[rv$tpl_key %||% "robdemat"]]
    d <- plot_defaults
    for (id in names(d$text)) updateTextInput(session, id, value = d$text[[id]])
    updateTextInput(session, "tl_header", value = tpl$header)
    updateTextInput(session, "sum_title", value = tpl$sum_title)
    for (id in names(d$area)) updateTextAreaInput(session, id, value = d$area[[id]])
    for (id in names(d$check)) updateCheckboxInput(session, id, value = d$check[[id]])
    for (id in names(d$select)) updateSelectInput(session, id, selected = d$select[[id]])
    for (id in names(d$slider)) updateSliderInput(session, id, value = d$slider[[id]])
    for (id in names(d$colour)) updateColourInput(session, id, value = d$colour[[id]])
    showNotification("Plot settings reset to default.", type = "message")
  })
  
  # ---- Navigation buttons ----------------------------------------------------
  observeEvent(input$back_start, nav_select("nav", "start"))
  observeEvent(input$to_assess, nav_select("nav", "assess"))
  observeEvent(input$back_setup, nav_select("nav", "setup"))
  observeEvent(input$to_plots, nav_select("nav", "plots"))
  observeEvent(input$back_assess, nav_select("nav", "assess"))
}

shinyApp(ui, server)