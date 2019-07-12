titler <- function(title, width = 58) {
  
  if (width == FALSE) {
    textGrob(title, 
             x = unit(0, "npc"), 
             y = unit(1, "npc"), 
             hjust = 0, 
             vjust = 1,
             gp = gpar(fontsize = 18, 
                       fontfamily = "Lato", 
                       lineheight = 1))    
  } else {
    textGrob(stringr::str_wrap(title, width = width), 
             x = unit(0, "npc"), 
             y = unit(1, "npc"), 
             hjust = 0, 
             vjust = 1,
             gp = gpar(fontsize = 18, 
                       fontfamily = "Lato", 
                       lineheight = 1))
  }
}

subtitler <- function(subtitle) {
  textGrob(subtitle, 
           x = unit(0, "npc"),
           y = unit(1, "npc"),
           hjust = 0, 
           vjust = 1,
           gp = gpar(fontsize = 12, 
                     fontfamily = "Lato"))
}

sourcer <- function(source) {
  grobTree(
    textGrob("Source: ", 
             name = "source1",
             x = unit(0, "npc"), 
             y = unit(1, "npc"), 
             hjust = 0, 
             vjust = 1,
             gp = gpar(fontsize = 8, fontfamily = "Lato", fontface = "bold")),
    textGrob(source, 
             x = unit(0, "npc") + grobWidth("source1"),
             y = unit(1, "npc"),             
             hjust = 0, 
             vjust = 1,
             gp = gpar(fontsize = 8, fontfamily = "Lato"))
  )
}

noter <- function(note, width = 132) {
  
  # get the shorter first line
  wrapped_lines <- stringr::str_wrap(note, width = width - 6)
  
  
  line1 <- paste0(stringr::str_split(wrapped_lines, "\n", n = 2)[[1]][1], "\n")  
  
  multiline <- length(stringr::str_split(wrapped_lines, "\n", n = 2)[[1]]) > 1
  
  if (multiline) {
  
    lines <- stringr::str_replace_all(stringr::str_split(wrapped_lines, "\n", n = 2)[[1]][2], "\n", " ")
  
    lines <- stringr::str_wrap(lines, width = width)
  
  }
  
  grob1 <- textGrob("Note: ", 
                    name = "note1",
                    x = unit(0, "npc"), 
                    y = unit(1, "npc"), 
                    hjust = 0, 
                    vjust = 1,
                    gp = gpar(fontsize = 8, fontfamily = "Lato", fontface = "bold", lineheight = 1))
  
  grob2 <- textGrob(line1,
                    x = unit(0, "npc") + grobWidth("note1"),
                    y = unit(1, "npc"), 
                    hjust = 0,
                    vjust = 1,
                    gp = gpar(fontsize = 8, fontfamily = "Lato", lineheight = 1))

  
  if (multiline) {
    
    grob3 <- textGrob(lines,
                      x = unit(0, "npc"),
                      y = unit(1, "npc") - 1.5 * grobHeight("note1"),
                      hjust = 0,
                      vjust = 1,
                      gp = gpar(fontsize = 8, fontfamily = "Lato", lineheight = 1))
    
    
    grobTree(grob1, grob2, grob3)
  } else {
    grobTree(grob1, grob2)
  }
}

caption <- grobTree(
  gp = gpar(fontsize = 7, hjust = 1), 
  textGrob(label = "I N S T I T U T E", 
           name = "caption1",
           x = unit(1, "npc"),  
           y = unit(1, "npc"),
           hjust = 1, 
           vjust = 1,
           gp = gpar(fontface = "bold", fontfamily = "Lato")),
  textGrob(label = "U R B A N  ", 
           x = unit(1, "npc") - grobWidth("caption1") - unit(0.01, "lines"),         
           y = unit(1, "npc"),
           hjust = 1, 
           vjust = 1, 
           gp = gpar(col = "#1696d2", fontface = "bold", fontfamily = "Lato"))
)

plotr <- function(..., heights) {
  grid.arrange(...,
               heights = heights) 
}
