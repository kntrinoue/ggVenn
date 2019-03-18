#ggVenn 

ggVenn <- function(x, na = "stop", label.size=5, cex=5, fill=NULL, alpha=0.2, ...){
  
  nc <- length(x) # Number of category
  if(length(x)==1 | 5 < length(x) | is.null(x) | !is.list(x)){
    warning("ggVenn is required for a list of two-five vectors")
    return(NULL)
  }
  
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger") # don't output log
  venn.plot <- VennDiagram::venn.diagram(x, filename=NULL, na = na, euler.d = FALSE, scaled =FALSE)
  #venn.summary <- summary(venn.plot)

  if(is.null(fill)){
    fill  <- RColorBrewer::brewer.pal(5,"Set1")[1:nc]
  }
  
  #Number of labels
  if(nc==2) nl <- 5 
  else if(nc==3) nl <- 10
  else if(nc==4) nl <- 19
  else if(nc==5) nl <- 36
  
  g <- ggplot2::ggplot() + ggplot2::theme_void()
  for(i in 1:nc){
    data.Coord <- data.frame(xcoord=as.numeric(venn.plot[[i]]$x),
                             ycoord=as.numeric(venn.plot[[i]]$y))
    
    g <- g + 
      ggplot2::layer(data.Coord, 
                     ggplot2::aes(x=xcoord, y=ycoord), geom="polygon", 
                     position="identity", stat="identity",
                     params=list(fill=fill[i], alpha=alpha, colour="black")
      )
  }
  
  
  for(i in 1:nl){
    data.label <- data.frame(x=as.numeric(venn.plot[[nc*2+i]]$x), 
                             y=as.numeric(venn.plot[[nc*2+i]]$y), 
                             label=venn.plot[[nc*2+i]]$label)
    
    if(nl-nc<i){
      size <- label.size # title label
      if(nc==2){
        data.label$y <- data.label$y + 0.1
      }else if(nc==3){
        if((i-(nl-nc))==1 || (i-(nl-nc))==2){
          data.label$y <- data.label$y + 0.1
        }else{
          data.label$y <- data.label$y - 0.1
        }
        
      }else if(nc==5){
        if((i-(nl-nc))==1){
          data.label$y <- data.label$y + 0.05
        }else if((i-(nl-nc))==2){
          data.label$x <- data.label$x + 0.1
          data.label$y <- data.label$y + 0.15
        }else if( (i-(nl-nc))==5){
          data.label$x <- data.label$x - 0.1
          data.label$y <- data.label$y + 0.15
        }else{
          data.label$y <- data.label$y - 0.05
        }
        
      }
       
    }else{
      size <- cex # numerical label
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


### venn.overlaplist
# return matrix of element names in overlap
# VennDiagram::calculate.overlap(x) returns names in the number in venn diagram
# This matrix returns names in overlap
venn.overlaplist <- function(x, fileName=NA){
  
  nc <- length(x) # Number of category
  if(length(x)==1 | 5 < length(x) | is.null(x) | !is.list(x)){
    warning("ggVenn is required for a list of two-five vectors")
    return(NULL)
  }
  
  overlap<-VennDiagram::calculate.overlap(x) # independent overlap list
  no<-array(NA,length(overlap)) #number of name in overlap
  maxol <- 0 # maximum number in overlap
  for(i in 1:length(overlap)){
    overlap[[i]]<-unique(overlap[[i]])
    no[i] <- length(overlap[[i]])
    if(i==1){
      el <- overlap[[i]] # element list in overlap
    }else{
      el <- c(el,overlap[[i]])
    }
  }
  el <- unique(el)
  
  addData <- function(i, overlapDup, range, no, addol){
    if(no==0){
      return (overlapDup)
    }else{
      overlapDup[range,i] <- addol
    }
    return (overlapDup)
  }
  
  overlapDup<-matrix(NA,nrow=length(el),ncol=length(overlap))
  for(i in 1:length(overlap)){
    overlapDup <- addData(i, overlapDup, 1:no[i], no[i], as.character(overlap[[i]])) # overlap in all category
    if(2<=i){
      addi <- 1; s <- (no[i]+1); e <- (no[i]+no[addi])
      overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
    }
    
    if(length(x)==3){
      if(i==5){ # 1
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==6){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==7){
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
    }else if(length(x)==4){
      if(i==6){ # 2
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==7){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==8){
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==9){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==10){
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==11){
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==12){ # 1
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 7; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 8; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==13){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 9; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 10; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==14){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 7; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 9; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 11; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==15){
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 8; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 10; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 11; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
    }else if(length(x)==5){
      if(i==7){ # 3
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==8){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==9){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==10){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==11){
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==12){
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==13){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==14){
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==15){
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==16){
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==17){#2
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 7; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 14; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 16; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==18){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 7; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 8; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 13; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==19){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 8; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 9; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 16; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==20){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 9; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 10; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 15; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==21){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 8; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 10; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 11; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==22){
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 7; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 11; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 12; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==23){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 10; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 12; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 13; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==24){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 9; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 13; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 14; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==25){
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 12; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 14; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 15; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==26){
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 11; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 15; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 16; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==27){ # 1
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 7; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 11; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 12; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 14; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 15; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 16; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 17; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 22; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 25; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 26; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==28){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 7; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 8; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 9; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 13; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 14; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 16; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 17; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 18; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 19; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 24; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==29){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 8; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 9; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 10; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 11; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 15; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 16; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 19; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 20; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 21; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 26; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==30){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 6; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 7; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 8; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 10; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 11; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 12; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 13; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 18; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 21; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 22; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 23; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
      else if(i==31){
        addi <- 2; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 3; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 4; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 5; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 9; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 10; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 12; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 13; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 14; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 15; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 20; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 23; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 24; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
        addi <- 25; s <- e+1; e <- s-1+no[addi]
        overlapDup <- addData(i, overlapDup, s:e, no[addi], overlapDup[1:no[addi], addi])
      }
    }

    
  }
  
  if(length(x)==2){
    colnames(overlapDup) <- c(paste0(names(x)[1],"&",names(x)[2]),
                      names(x)[1], names(x)[2])
    names(overlap) <- c(paste0(names(x)[1],"&",names(x)[2]),
                          names(x)[1], names(x)[2])
  }else if(length(x)==3){
    colnames(overlapDup) <- c(paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3]),
                      paste0(names(x)[1],"&",names(x)[2]),paste0(names(x)[1],"&",names(x)[3]),paste0(names(x)[2],"&",names(x)[3]),
                      names(x)[1], names(x)[2], names(x)[3])
    names(overlap) <- c(paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3]),
                      paste0(names(x)[1],"&",names(x)[2]),paste0(names(x)[1],"&",names(x)[3]),paste0(names(x)[2],"&",names(x)[3]),
                      names(x)[1], names(x)[2], names(x)[3])
  }else if(length(x)==4){
    colnames(overlapDup) <- c(paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3]),paste0(names(x)[1],"&",names(x)[2],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[3],"&",names(x)[4]),paste0(names(x)[2],"&",names(x)[3],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[2]),paste0(names(x)[1],"&",names(x)[3]),paste0(names(x)[1],"&",names(x)[4]),
                      paste0(names(x)[2],"&",names(x)[3]),paste0(names(x)[2],"&",names(x)[4]),paste0(names(x)[3],"&",names(x)[4]),
                      names(x)[1], names(x)[2], names(x)[3], names(x)[4])
    names(overlap) <- c(paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3]),paste0(names(x)[1],"&",names(x)[2],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[3],"&",names(x)[4]),paste0(names(x)[2],"&",names(x)[3],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[2]),paste0(names(x)[1],"&",names(x)[3]),paste0(names(x)[1],"&",names(x)[4]),
                      paste0(names(x)[2],"&",names(x)[3]),paste0(names(x)[2],"&",names(x)[4]),paste0(names(x)[3],"&",names(x)[4]),
                      names(x)[1], names(x)[2], names(x)[3], names(x)[4])
  }else if(length(x)==5){
    colnames(overlapDup) <- c(paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3],"&",names(x)[5]),
                      paste0(names(x)[1],"&",names(x)[2],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[1],"&",names(x)[3],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[2],"&",names(x)[3],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[2],"&",names(x)[4],"&",names(x)[5]),paste0(names(x)[2],"&",names(x)[3],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[3],"&",names(x)[4]),paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3]),
                      paste0(names(x)[2],"&",names(x)[3],"&",names(x)[4]),paste0(names(x)[1],"&",names(x)[2],"&",names(x)[5]),
                      paste0(names(x)[1],"&",names(x)[2],"&",names(x)[4]),paste0(names(x)[1],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[1],"&",names(x)[3],"&",names(x)[5]),paste0(names(x)[3],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[4],"&",names(x)[5]),paste0(names(x)[2],"&",names(x)[4]),paste0(names(x)[3],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[3]),
                      paste0(names(x)[2],"&",names(x)[3]),paste0(names(x)[2],"&",names(x)[5]),paste0(names(x)[1],"&",names(x)[2]),
                      paste0(names(x)[1],"&",names(x)[4]),paste0(names(x)[1],"&",names(x)[5]),paste0(names(x)[3],"&",names(x)[5]),
                      names(x)[5], names(x)[4], names(x)[3], names(x)[2], names(x)[1])
    names(overlap) <- c(paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3],"&",names(x)[5]),
                      paste0(names(x)[1],"&",names(x)[2],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[1],"&",names(x)[3],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[2],"&",names(x)[3],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[2],"&",names(x)[4],"&",names(x)[5]),paste0(names(x)[2],"&",names(x)[3],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[3],"&",names(x)[4]),paste0(names(x)[1],"&",names(x)[2],"&",names(x)[3]),
                      paste0(names(x)[2],"&",names(x)[3],"&",names(x)[4]),paste0(names(x)[1],"&",names(x)[2],"&",names(x)[5]),
                      paste0(names(x)[1],"&",names(x)[2],"&",names(x)[4]),paste0(names(x)[1],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[1],"&",names(x)[3],"&",names(x)[5]),paste0(names(x)[3],"&",names(x)[4],"&",names(x)[5]),
                      paste0(names(x)[4],"&",names(x)[5]),paste0(names(x)[2],"&",names(x)[4]),paste0(names(x)[3],"&",names(x)[4]),
                      paste0(names(x)[1],"&",names(x)[3]),
                      paste0(names(x)[2],"&",names(x)[3]),paste0(names(x)[2],"&",names(x)[5]),paste0(names(x)[1],"&",names(x)[2]),
                      paste0(names(x)[1],"&",names(x)[4]),paste0(names(x)[1],"&",names(x)[5]),paste0(names(x)[3],"&",names(x)[5]),
                      names(x)[5], names(x)[4], names(x)[3], names(x)[2], names(x)[1])
  }
  
  if(!is.na(fileName)){
    write.table(overlapDup, file=fileName)
  }
  
  return (list(overlapDup=overlapDup,overlap=overlap)) # matrix

}
