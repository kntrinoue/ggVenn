#ggVenn 

ggVenn <- function(x, na = "stop", label.size=5, cex=5, fill=NULL, alpha=0.2, ...){
  
  ne <- length(x)
  if(length(x)==1 || 5 < length(x) || is.null(x) || !is.list(x)){
    stop("ggVenn is required for a list of two-five vectors")
  }
  
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger") # don't output log
  venn.plot <- VennDiagram::venn.diagram(x, filename=NULL, na = na, euler.d = FALSE, scaled =FALSE)
  #venn.summary <- summary(venn.plot)

  if(is.null(fill)){
    fill  <- RColorBrewer::brewer.pal(5,"Set1")[1:ne]
  }
  
  if(ne==2) nl <- 5
  else if(ne==3) nl <- 10
  else if(ne==4) nl <- 19
  else if(ne==5) nl <- 36
  
  g <- ggplot2::ggplot() + ggplot2::theme_void()
  for(i in 1:ne){
    data.Coord <- data.frame(xcoord=as.numeric(venn.plot[[i]]$x), ycoord=as.numeric(venn.plot[[i]]$y))
    
    g <- g + 
      ggplot2::layer(data.Coord, 
                     ggplot2::aes(x=xcoord, y=ycoord), geom="polygon", 
                     position="identity", stat="identity",
                     params=list(fill=fill[i], alpha=alpha, colour="black")
      )
  }
  
  
  for(i in 1:nl){
    data.label <- data.frame(x=as.numeric(venn.plot[[ne*2+i]]$x), y=as.numeric(venn.plot[[ne*2+i]]$y), label=venn.plot[[ne*2+i]]$label)
    
    if(nl-ne<i){
      size <- label.size
    }else{
      size <- cex
    }
    g <- g + 
      ggplot2::layer(data.label,
                     ggplot2::aes(x=x, y=y, label=label), geom="text", 
                     position="identity", stat="identity",
                     params = list(size=size)
      )
  }
  
  return(g)
}