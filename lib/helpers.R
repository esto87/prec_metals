get_p <- function(w){
  return(c(w)*c(v1)+c(1-w)*c(v2))
}

SR_simp <- function(v){
  return(mean(v)/sd(v))
}

get_sr <- function(w){
  return(SR_simp(v = get_p(w)))
}
