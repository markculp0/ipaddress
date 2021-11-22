


#' Convert decimal ip to dot decimal string
#'
#' @param ip_dec_vect Decimal IP to convert
#' @return ip_dot_vect Dot decimal string
#' @export
dec2dot <- function(ip_dec_vect) {
  ip_dot_vect <- character(length = length(ip_dec_vect))

  for(i in 3:0) {
    ip_dot_vect <- paste(ip_dot_vect,
                         as.character(ip_dec_vect %/% 256^i),
                         sep = "")
    ip_dec_vect <- ip_dec_vect %% 256^i
    if (i > 0) ip_dot_vect <- paste(ip_dot_vect,
                                    ".", sep = "")
  }
  ip_dot_vect
}

#' Calculate hosts and last ip from cidr
#'
#' @param ip_dec_vect Vector of decimal IP addresses
#' @param prefix_vect Vector of cidr network prefixes
#' @return last_ip_dec_vector Vector of decimal last ips
#' @export
calc_last_ip_dec <- function(ip_dec_vect, prefix_vect){
  n <- 32 -prefix_vect
  hosts <- 2^n
  last_ip_dec_vector <- ip_dec_vect + hosts - 1

  last_ip_dec_vector
}
