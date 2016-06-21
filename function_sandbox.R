fahr_to_celsius <- function(temp) {
     (temp - 32) * 5/9
     
}

#Don't forget to re evaluate your function


fahr_to_celsius(46)
#A little better and more flexible

convert_fahr <- function(temp, to) {
    #what this is litearlly doing is saying in all cases, unless you give me kelvin, you get celsius
      res <- (temp - 32) *5/9
     if (to == "kelvin") {
          res <- res + 273.15
     }
     res
}

convert_fahr(46, "celsius")

convert_fahr2 <- function(temp, to = c("celsius", "kelvin")) {
     #this lets you get more specfic and get kinda close
     to <- tolower(to)
     to <- match.arg(to)
     res <- (temp - 32) *5/9
     if (to == "kelvin") {
          res <- res + 273.15
     }
     res
}

convert_fahr2(47, "C")
convert_fahr2(50, "k")


###make expandable!!!

fahr_to_celsius <- function(temp) {
     (temp - 32) * 5/9
}

fahr_to_kelvin <- 
     
     
     convert_fahr2 <- function(temp, to = c("celsius", "kelvin")) {
          #this lets you get more specfic and get kinda close
          to <- tolower(to)
          to <- match.arg(to)
          res <- fahr_to_celsius(temp)
          if (to == "kelvin") {
               res <- celsius_to_kelvin(res)
          }
          res
     }


#lbs to kilogram

lb_to_kg <- function(weight) {
   weight / 2.2
}


lb_to_kg(200)

#stretch....expandable funciton and grams

kg_to_g <-  function(weight) {
     weight * 1000
}

convert_weight <- function(weight, to = c("kilograms", "grams")) {
     to <- tolower(to)
     to <- match.arg(to)
     res <- lb_to_kg(weight)
     if (to == "grams") {
          res <- kg_to_g(res)
     }
     res
}

lb_to_kg(100)
kg_to_g(45.45)
convert_weight(100, "grams")










