# ##' a shadow version of geom_text
# ##'
# ##'
# ##' @title geom_shadowtext
# ##' @param mapping aesthetic mapping
# ##' @param data the data to be displayed
# ##' @param stat statistical transformation
# ##' @param position position adjustment
# ##' @param ... additional parameter
# ##' @param parse whether parse text as expression
# ##' @param nudge_x horizontal adjustment of text
# ##' @param nudge_y vertical adjustment of text
# ##' @param check_overlap whether check overlap
# ##' @param na.rm whether remove NA values
# ##' @param show.legend whether show legend
# ##' @param inherit.aes whether inherit aes from ggplot
# ##' @return layer
# ##' @importFrom ggplot2 layer
# ##' @importFrom ggplot2 position_nudge
# ##' @author guangchuang yu
# ##' @export
# ##' @examples
# ##' library(ggplot2)
# ##' d <- data.frame(x = rnorm(3), y=rnorm(3), label = c('hello', 'world', '!!!'))
# ##' ggplot(d, aes(x,y)) + geom_shadowtext(aes(label=label, color=label), bg.colour='firebrick')
# ##' @export
# geom_shadowtext <- function(mapping = NULL,
#                             data = NULL,
#                             stat = "identity",
#                             position = "identity",
#                             ...,
#                             parse = FALSE,
#                             nudge_x = 0,
#                             nudge_y = 0,
#                             check_overlap = FALSE,
#                             na.rm = FALSE,
#                             show.legend = NA,
#                             inherit.aes = TRUE) {
#   if (!missing(nudge_x) || !missing(nudge_y)) {
#     if (!missing(position)) {
#       stop("Specify either `position` or `nudge_x`/`nudge_y`",
#            call. = FALSE)
#     }
#     
#     position <- position_nudge(nudge_x, nudge_y)
#   }
#   
#   layer(
#     data = data,
#     mapping = mapping,
#     stat = stat,
#     geom = GeomShadowText,
#     position = position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       parse = parse,
#       check_overlap = check_overlap,
#       na.rm = na.rm,
#       ...
#     ),
#     check.param = FALSE
#   )
# }
# 
# ##' @importFrom ggplot2 ggproto
# ##' @importFrom ggplot2 aes
# ##' @importFrom ggplot2 draw_key_text
# ##' @importFrom ggplot2 Geom
# ##' @importFrom ggplot2 .pt
# ##' @importFrom scales alpha
# GeomShadowText <- ggproto(
#   "GeomShadowText",
#   Geom,
#   required_aes = c("x", "y", "label"),
#   
#   default_aes = aes(
#     colour = "white",
#     size = 3.88,
#     angle = 0,
#     hjust = 0.5,
#     vjust = 0.5,
#     alpha = NA,
#     family = "",
#     fontface = 1,
#     lineheight = 1.2,
#     bg.colour = "black",
#     bg.r = 0.1
#   ),
#   
#   draw_panel = function(data,
#                         panel_params,
#                         coord,
#                         parse = FALSE,
#                         na.rm = FALSE,
#                         check_overlap = FALSE) {
#     lab <- data$label
#     if (parse) {
#       lab <- parse(text = as.character(lab))
#     }
#     
#     data <-
#       coord$transform(data, panel_params)
#     if (is.character(data$vjust)) {
#       data$vjust <- compute_just(data$vjust, data$y)
#     }
#     if (is.character(data$hjust)) {
#       data$hjust <- compute_just(data$hjust, data$x)
#     }
#     
#     shadowtextGrob(
#       lab,
#       data$x,
#       data$y,
#       default.units = "native",
#       hjust = data$hjust,
#       vjust = data$vjust,
#       rot = data$angle,
#       bg.colour = alpha(data$bg.colour, data$alpha),
#       bg.r = data$bg.r,
#       gp = gpar(
#         col = alpha(data$colour, data$alpha),
#         fontsize = data$size * ggplot2::.pt,
#         fontfamily = data$family,
#         fontface = data$fontface,
#         lineheight = data$lineheight
#       ),
#       check.overlap = check_overlap
#     )
#   },
#   
#   draw_key = draw_key_text
# )
# 
# compute_just <- getFromNamespace("compute_just", "ggplot2")
# just_dir <- getFromNamespace("just_dir", "ggplot2")
# 
# 
# 
# 
# 
# ##' shadown text element for ggplot theme system
# ##'
# ##'
# ##' @title element_shadowtext
# ##' @param family Font family
# ##' @param face Font face ("plain", "italic", "bold", "bold.italic")
# ##' @param colour text colour
# ##' @param size text size in pts
# ##' @param hjust horizontal justification (in [0, 1])
# ##' @param vjust vertical justification (in [0, 1])
# ##' @param angle text angle
# ##' @param lineheight line height
# ##' @param color aliase to colour
# ##' @param margin margins around the text, see also 'margin()' for more details
# ##' @param debug if 'TRUE', aids visual debugging by drawing a solic rectangle behind the complete text area, and a point where each label is anchored.
# ##' @param inherit.blank whether inherit 'element_blank'
# ##' @return element_shadowtext object
# ##' @export
# ##' @author Guangchuang Yu and xmarti6@github
# element_shadowtext <-
#   function (family = NULL,
#             face = NULL,
#             colour = NULL,
#             size = NULL,
#             hjust = NULL,
#             vjust = NULL,
#             angle = NULL,
#             lineheight = NULL,
#             color = NULL,
#             margin = NULL,
#             debug = NULL,
#             inherit.blank = FALSE) {
#     if (!is.null(color))
#       colour <- color
#     ## Create a new "subclass" from "element_text"
#     structure(
#       list(
#         family = family,
#         face = face,
#         colour = colour,
#         size = size,
#         hjust = hjust,
#         vjust = vjust,
#         angle = angle,
#         lineheight = lineheight,
#         margin = margin,
#         debug = debug,
#         inherit.blank = inherit.blank
#       ),
#       class = c("element_shadowtext", "element_text",  "element")
#     )
#   }
# 
# ##' @importFrom ggplot2 element_grob
# ##' @method element_grob element_shadowtext
# ##' @export
# element_grob.element_shadowtext <-
#   function (element,
#             label = "",
#             x = NULL,
#             y = NULL,
#             family = NULL,
#             face = NULL,
#             colour = NULL,
#             size = NULL,
#             hjust = NULL,
#             vjust = NULL,
#             angle = NULL,
#             lineheight = NULL,
#             margin = NULL,
#             margin_x = FALSE,
#             margin_y = FALSE,
#             ...) {
#     if (is.null(label))
#       return(zeroGrob())
#     vj <-
#       vjust %||% element$vjust
#     hj <-
#       hjust %||% element$hjust
#     margin <-
#       margin %||% element$margin
#     angle <-
#       angle %||% element$angle %||% 0
#     gp <-
#       gpar(
#         fontsize = size,
#         col = colour,
#         fontfamily = family,
#         fontface = face,
#         lineheight = lineheight
#       )
#     element_gp <-
#       gpar(
#         fontsize = element$size,
#         col = element$colour,
#         fontfamily = element$family,
#         fontface = element$face,
#         lineheight = element$lineheight
#       )
#     shadow.titleGrob(
#       label,
#       x,
#       y,
#       hjust = hj,
#       vjust = vj,
#       angle = angle,
#       gp = modify_list(element_gp, gp),
#       margin = margin,
#       margin_x = margin_x,
#       margin_y = margin_y,
#       debug = element$debug
#     )
#   }
# 
# ##' @importFrom grid rectGrob
# ##' @importFrom grid pointsGrob
# shadow.titleGrob <-
#   function (label,
#             x,
#             y,
#             hjust,
#             vjust,
#             angle = 0,
#             gp = gpar(),
#             margin = NULL,
#             margin_x = FALSE,
#             margin_y = FALSE,
#             debug = FALSE) {
#     if (is.null(label))
#       return(zeroGrob())
#     grob_details <-
#       shadow.title_spec(
#         label,
#         x = x,
#         y = y,
#         hjust = hjust,
#         #<<<<
#         vjust = vjust,
#         angle = angle,
#         gp = gp,
#         debug = debug
#       )
#     add_margins(
#       grob = grob_details$text_grob,
#       height = grob_details$text_height,
#       width = grob_details$text_width,
#       gp = gp,
#       margin = margin,
#       margin_x = margin_x,
#       margin_y = margin_y
#     )
#   }
# 
# 
# shadow.title_spec <-
#   function (label,
#             x,
#             y,
#             hjust,
#             vjust,
#             angle,
#             gp = gpar(),
#             debug = FALSE) {
#     if (is.null(label))
#       return(zeroGrob())
#     just <-
#       rotate_just(angle, hjust, vjust)
#     n <-
#       max(length(x), length(y), 1)
#     x <-
#       x %||% unit(rep(just$hjust, n), "npc")
#     y <-
#       y %||% unit(rep(just$vjust, n), "npc")
#     #text_grob <- textGrob(label, x, y, hjust = hjust, vjust = vjust,   #<<<<TARGET
#     #    rot = angle, gp = gp)
#     text_grob <-
#       shadowtextGrob(
#         label,
#         x,
#         y,
#         hjust = hjust,
#         #<<<<TARGET_EDITED
#         vjust = vjust,
#         rot = angle,
#         gp = gp
#       )
#     
#     
#     descent <-
#       font_descent(gp$fontfamily, gp$fontface, gp$fontsize, gp$cex)
#     #The "grobheight/width" are only extracted from a "textGrob" but not from
#     #a "gTree" which is a list of textGrobs generated by "shadowtext".
#     #It is enough if we get the height/width from the 1st element textGrob from gTree.
#     #To access textGrobs from gTree we use "$children".
#     #text_height <- unit(1, "grobheight", text_grob) + abs(cos(angle/180 *     #<<<<EDITED
#     text_height <-
#       unit(1, "grobheight", text_grob) + abs(cos(angle / 180 *
#                                                    pi)) * descent
#     #text_width <- unit(1, "grobwidth", text_grob) + abs(sin(angle/180 *      #<<<<EDITED
#     text_width <-
#       unit(1, "grobwidth", text_grob) + abs(sin(angle / 180 *
#                                                   pi)) * descent
#     if (isTRUE(debug)) {
#       children <-
#         gList(rectGrob(gp = gpar(fill = "cornsilk",
#                                  col = NA)),
#               pointsGrob(x, y, pch = 20, gp = gpar(col = "gold")),
#               text_grob)
#     }
#     else {
#       children <-
#         gList(text_grob)
#     }
#     list(text_grob = children,
#          text_height = text_height,
#          text_width = text_width)
#   }
# 
# "%||%" <- getFromNamespace("%||%", "ggplot2")
# zeroGrob <- getFromNamespace("zeroGrob", "ggplot2")
# modify_list <- getFromNamespace("modify_list", "ggplot2")
# add_margins <- getFromNamespace("add_margins", "ggplot2")
# rotate_just <- getFromNamespace("rotate_just", "ggplot2")
# font_descent <- getFromNamespace("font_descent", "ggplot2")
# 
# 
# 
# ##' create and draw text
# ##'
# ##'
# ##' @title shadowtextGrob
# ##' @param label text labels
# ##' @param x x position
# ##' @param y y position
# ##' @param just The justification of the text, can be 'left', 'right', 'center', 'bottom' and 'top'
# ##' @param hjust horizontal adjustment
# ##' @param vjust vertical adjustment
# ##' @param rot angle to rotate the text
# ##' @param check.overlap whether check for and omit overlapping text
# ##' @param default.units default unit of x and y
# ##' @param name identifier
# ##' @param gp gpar object
# ##' @param vp viewport object
# ##' @param bg.colour background color of shadow text
# ##' @param bg.r background ratio of shadow text
# ##' @return grob object
# ##' @importFrom grid textGrob
# ##' @importFrom grid unit
# ##' @importFrom grid gpar
# ##' @importFrom grid gList
# ##' @importFrom grid gTree
# ##' @importFrom grid is.unit
# ##' @export
# ##' @author guangchuang yu
# shadowtextGrob <-
#   function(label,
#            x = unit(0.5, "npc"),
#            y = unit(0.5, "npc"),
#            just = "centre",
#            hjust = NULL,
#            vjust = NULL,
#            rot = 0,
#            check.overlap = FALSE,
#            default.units = "npc",
#            name = NULL,
#            gp = gpar(col = "white"),
#            vp = NULL,
#            bg.colour = "black",
#            bg.r = 0.1) {
#     upperGrob <-
#       textGrob(
#         label = label,
#         x = x,
#         y = y,
#         just = just,
#         hjust = hjust,
#         vjust = vjust,
#         rot = rot,
#         default.units = default.units,
#         check.overlap = check.overlap,
#         name = name,
#         gp = gp,
#         vp = vp
#       )
#     
#     if (is.null(bg.colour))
#       return(upperGrob)
#     
#     
#     gp$col <- bg.colour
#     
#     theta <- seq(pi / 8, 2 * pi, length.out = 16)
#     char <- "X"
#     r <- bg.r[1]
#     
#     bgList <- lapply(theta, function(i) {
#       if (!is.unit(x))
#         x <- unit(x, default.units)
#       if (!is.unit(y))
#         y <- unit(y, default.units)
#       
#       x <- x + unit(cos(i) * r, "strheight", data = char)
#       y <- y + unit(sin(i) * r, "strheight", data = char)
#       textGrob(
#         label = label,
#         x = x,
#         y = y,
#         just = just,
#         hjust = hjust,
#         vjust = vjust,
#         rot = rot,
#         default.units = default.units,
#         check.overlap = check.overlap,
#         name = name,
#         gp = gp,
#         vp = vp
#       )
#     })
#     
#     bgGrob <- do.call(gList, bgList)
#     grobs <- gList(bgGrob, upperGrob)
#     gTree(children = grobs)
#   }
# 
# ##' @rdname shadowtextGrob
# ##' @param draw whether draw the grob
# ##' @importFrom grid grid.draw
# ##' @export
# grid.shadowtext <-
#   function(label,
#            x = unit(0.5, "npc"),
#            y = unit(0.5, "npc"),
#            just = "centre",
#            hjust = NULL,
#            vjust = NULL,
#            rot = 0,
#            check.overlap = FALSE,
#            default.units = "npc",
#            name = NULL,
#            gp = gpar(col = "white"),
#            vp = NULL,
#            bg.colour = "black",
#            bg.r = 0.1,
#            draw = TRUE) {
#     stg <-
#       shadowtextGrob(
#         label = label,
#         x = x,
#         y = y,
#         just = just,
#         hjust = hjust,
#         vjust = vjust,
#         rot = rot,
#         default.units = default.units,
#         check.overlap = check.overlap,
#         name = name,
#         gp = gp,
#         vp = vp,
#         bg.colour = bg.colour,
#         bg.r = bg.r
#       )
#     if (draw)
#       grid.draw(stg)
#     invisible(stg)
#   }
# 
# ##' @importFrom grid gpar
# ##' @export
# grid::gpar