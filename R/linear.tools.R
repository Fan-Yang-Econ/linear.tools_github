# put this file in R folder of the package
# run  roxygen2::roxygenise()

### Test ---------------------

# source("/Users/yangguodaxia/Dropbox/Tech/R/attach_Load_First.R")
# source("S:/Users/Fan Yang/Research/Core_Code/attach_Load_First.R")
# source("H:/jpmDesk/Desktop/Personal/Research_personal/Core_Code/linear_tools.R"
# source("/Users/yangguodaxia/Dropbox/Tech/R/linear_tools_origin.R")


# key problem is predict() cannot handle dirty formula, when the newdata does not contain empty vars in dirty formula
# key finding is the name class() that can be transfered to call.

#
library(magrittr)
library(ggplot2)
library(pryr)
library(plyr)
library(stringr)
library(scales)
#  data.table(ggplot2::diamonds)


###  Not Exported ==================



#' check whether list names (tobechecked) is within specified sets ('checking')
#' @export
#' @keywords internal
#' @param tobechecked   a named list / character vector to be checked
#' @param checking   a character vector, the names in `tobechecked` is supposed to show up here.
#' @param STOP  STOP if there are names in `tobechecked` not showing up in `checking`.
#' If FALSE, then we will delete the names not showing up in `checking`.
#' @param tobechecked_name  a character, name of 'tobechecked' for printing.
#' @param checking_name  a character, name of 'checking' for printing.
#' @param default_value  value to be returned if all names in tobechecked are not in 'checking'.
#' @param PRINT  whether print diagnostic information.
#' @return a 'purified' named list / character vector , with all names not showing up in `checking` deleted.
#' @examples
#' 
#' tobechecked = list('cut' = 1, 'dwewsdfds' = 2); checking = 'cut'
#' 
#' result =   check_names_delete(tobechecked, checking, STOP = FALSE,
#'                               tobechecked_name = 'focus_var',
#'                               checking_name = 'all_raw_vars')
#' result
#' 
#' result = check_names_delete(tobechecked, checking, STOP = FALSE)
#' result
#' 
#' tobechecked = c('cut', 'dsfsf'); checking = 'cut'
#' result = check_names_delete(tobechecked, checking, STOP = FALSE)
#' result
#' 
check_names_delete = function(tobechecked, checking,
                              STOP = TRUE,
                              tobechecked_name = 'tobechecked' , checking_name = 'checking',
                              default_value = NULL,
                              PRINT = TRUE){

  ## check whether list names (tobechecked) is within specified sets ('checking')
  ## @export
  ## @keywords internal
  ## @param tobechecked   a named list / character vector to be checked
  ## @param checking   a character vector, the names in `tobechecked` is supposed to show up here.
  ## @param STOP  STOP if there are names in `tobechecked` not showing up in `checking`.
  ## If FALSE, then we will delete the names not showing up in `checking`.
  ## @param tobechecked_name  a character, name of 'tobechecked' for printing.
  ## @param checking_name  a character, name of 'checking' for printing.
  ## @param default_value  value to be returned if all names in tobechecked are not in 'checking'.
  ## @param PRINT  whether print diagnostic information.
  ## @return a 'purified' named list / character vector , with all names not showing up in `checking` deleted.

  if ('character' %in% class(tobechecked)) names(tobechecked) = tobechecked

  if (STOP) {
    sanity_check(names(tobechecked), exact_in_match = checking)
    }

  # tobechecked = list('cut' = 1, 'dwewsdfds' = 2); checking = 'cut'
  # tobechecked = list('cudasdst' = 1, 'dwewsdfds' = 2); checking = 'cut'

  tobechecked_unmatched = names(tobechecked)[!names(tobechecked) %in% c(checking)]

  if (PRINT && length(tobechecked_unmatched)){
    cat('\nfollowing names in ',tobechecked_name,' cannot be found ', checking_name, ', so delete them\n',sep='')
    print(tobechecked_unmatched)
  }

  tobechecked2 = tobechecked[names(tobechecked) %in% c(checking)]

  if (length(tobechecked2) == 0) {
    if (PRINT) cat('names in ', tobechecked_name,' cannot be matched with ',checking_name, ' NULL is returned.\n', sep='')
    tobechecked2 = default_value
  }

  return(tobechecked2)



  if (FALSE && TRUE){
    tobechecked = list('cut' = 1, 'dwewsdfds' = 2); checking = 'cut'

    result =   check_names_delete(tobechecked, checking, STOP = FALSE,
                                  tobechecked_name = 'focus_var',
                                  checking_name = 'all_raw_vars')
    result

    result = check_names_delete(tobechecked, checking, STOP = FALSE)
    result

    tobechecked = c('cut', 'dsfsf'); checking = 'cut'
    result = check_names_delete(tobechecked, checking, STOP = FALSE)
    result
  }

}





#' get raw data from lm or glm
#' @export
#' @keywords internal
#' @description get raw data from lm or glm
#' @param modle  a lm or glm.
#' @return a data.frame used as raw data for the model.
#' @examples
#' 
#' data_used = ggplot2::diamonds[0:10,]
#' model = lm(price~ cut + carat + I(carat^2) + I(carat^3) +
#'              I(carat  * depth) + cut:depth, data_used) # a GLM
#' get_data_from_lm(model)
#' 
#' data_used = 'data_used is deleted in this environment'
#' get_data_from_lm(model)
#' 
get_data_from_lm = function(model){

  ## get raw data from lm or glm
  ## @export
  ## @keywords internal
  ## @description get raw data from lm or glm
  ## @param modle  a lm or glm.
  ## @return a data.frame used as raw data for the model.

  if (sum(c('lm','glm') %in% class(model)) == 0 ){
    stop('model must be in the class lm or glm')
  }

  data1 = eval(model$call$data, environment(model$terms) ) # NOT works here

  if (!'data.frame' %in% class(data1)){
    data1 = model$data # NOT works here
  }

  return(data1)

  if (FALSE && TRUE){

    data_used = ggplot2::diamonds[0:10,]
    model = lm(price~ cut + carat + I(carat^2) + I(carat^3) +
                 I(carat  * depth) + cut:depth, data_used) # a GLM
    get_data_from_lm(model)

    data_used = 'data_used is deleted in this environment'
    get_data_from_lm(model)
  }
}



#' Enter_to_Continue: wait your response to continue
#' @description wait your response to continue
#' @export
#' @param df_input_output  data.frame.
#' df_input_output shall be either NULL or a two column data.frame with characters as values, with first column as what you want to type, and second column as what you want to return.
#' If it is NULL, then it will return ' Press [enter] to continue; Type [s] to stop'.
#' See the sample code for the df case.
#'
#' @return Type through keyboard to continue in console.
#'
#' @examples
#' 
#' Enter_to_Continue(rbind(c('small','small data'),c('n','normal'),c('w','weird curve')))
#' 
Enter_to_Continue = function(df_input_output = NULL){

  ## Enter_to_Continue: wait your response to continue
  ## @description wait your response to continue
  ## @export
  ## @param df_input_output  data.frame.
  ## df_input_output shall be either NULL or a two column data.frame with characters as values, with first column as what you want to type, and second column as what you want to return.
  ## If it is NULL, then it will return ' Press [enter] to continue; Type [s] to stop'.
  ## See the sample code for the df case.
  ##
  ## @return Type through keyboard to continue in console.
  ##

  Return=NA

  cat ("\n .... Press [enter] to continue; Type [s] to stop ....")

  if (!is.null(df_input_output) && !is.na(df_input_output) &&
      nrow(df_input_output)>0){
    for (pair_row in 1:nrow(df_input_output)) {
      # pair = List[[1]]
      cat("\n .... Type [ ",paste(df_input_output[pair_row,1])," ]"," to return '",
          paste(df_input_output[pair_row,2]),"' as object 'Return' ....",sep='')
    }
  }

  line <- readline()
  if (line=='s' | line=='S') stop("Stop ! ")

  # line = 'small'
  if (!is.null(df_input_output) && !is.na(df_input_output) &&
      nrow(df_input_output)>0 ) {

    Match = match(line,paste(df_input_output[,1]))
    Return = df_input_output[Match,2]
  }

  return(Return)

  if (FALSE && TRUE){

    Enter_to_Continue(rbind(c('small','small data'),c('n','normal'),c('w','weird curve')))

  } # END TRUE

}



#' a wrap up of \code{expand.grid()} that takes list
#' @export
#' @keywords internal
#' @description  a wrap up of \code{expand.grid()} that takes list
#' @param List  a list same as '...' in \code{expand_grid()},
#' @param KEEP.OUT.ATTRS  stringsAsFactors, stringsAsFactors see \code{expand_grid()}
#' @return Tsee \code{expand_grid()}
#' @examples
#' expand_grid(list(A= c(1:3),b= letters[1:4]))
#' expand_grid(List = list(c(1:3)))
expand_grid = function (List, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE) {

  ## a wrap up of \code{expand.grid()} that takes list
  ## @export
  ## @keywords internal
  ## @description  a wrap up of \code{expand.grid()} that takes list
  ## @param List  a list same as '...' in \code{expand_grid()},
  ## @param KEEP.OUT.ATTRS  stringsAsFactors, stringsAsFactors see \code{expand_grid()}
  ## @return Tsee \code{expand_grid()}
  ## @examples
  ## expand_grid(list(A= c(1:3),b= letters[1:4]))
  ## expand_grid(List = list(c(1:3)))


  nargs <- length(args <- List)
  if (!nargs)
    return(as.data.frame(list()))
  if (nargs == 1L && is.list(a1 <- args[[1L]]))
    nargs <- length(args <- a1)
  if (nargs == 0L)
    return(as.data.frame(list()))
  cargs <- vector("list", nargs)
  iArgs <- seq_len(nargs)
  nmc <- paste0("Var", iArgs)
  nm <- names(args)
  if (is.null(nm))
    nm <- nmc
  else if (any(ng0 <- nzchar(nm)))
    nmc[ng0] <- nm[ng0]
  names(cargs) <- nmc
  rep.fac <- 1L
  d <- sapply(args, length)
  if (KEEP.OUT.ATTRS) {
    dn <- vector("list", nargs)
    names(dn) <- nmc
  }
  orep <- prod(d)
  if (orep == 0L) {
    for (i in iArgs) cargs[[i]] <- args[[i]][FALSE]
  }
  else {
    for (i in iArgs) {
      x <- args[[i]]
      if (KEEP.OUT.ATTRS)
        dn[[i]] <- paste0(nmc[i], "=", if (is.numeric(x))
          format(x)
          else x)
      nx <- length(x)
      orep <- orep/nx
      x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac,
                                                  nx)), orep)]
      if (stringsAsFactors && !is.factor(x) && is.character(x))
        x <- factor(x, levels = unique(x))
      cargs[[i]] <- x
      rep.fac <- rep.fac * nx
    }
  }
  if (KEEP.OUT.ATTRS)
    attr(cargs, "out.attrs") <- list(dim = d, dimnames = dn)
  rn <- .set_row_names(as.integer(prod(d)))
  structure(cargs, class = "data.frame", row.names = rn)

  # expand_grid(list(A= c(1:3),b= letters[1:4]))
  # expand_grid(List = list(c(1:3)))

}



#' get mode number from a numeric vector
#' @export
#' @keywords internal
#' @param x a numeric vector
#' @return a numeric value mode number of x
#' @examples
#' Mode(c(1,1,1,10,10))
Mode <- function(x) {

  ## get mode number from a numeric vector
  ## @export
  ## @keywords internal
  ## @param x a numeric vector
  ## @return a numeric value mode number of x
  ## @examples
  ## Mode(c(1,1,1,10,10))

  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



#' check whether the vector is increasing
#' @export
#' @keywords internal
#' @param x a numeric vector
#' @return \code{TRUE} if increasing, \code{FALSE} if not
#' @examples
#' is_increase(c(1,1,1,10,10))
#' is_increase(c(1,2,1,10,10))
is_increase = function(x){

  ## check whether the vector is increasing
  ## @export
  ## @keywords internal
  ## @param x a numeric vector
  ## @return \code{TRUE} if increasing, \code{FALSE} if not
  ## @examples
  ## is_increase(c(1,1,1,10,10))
  ## is_increase(c(1,2,1,10,10))

  all(x == cummax(x))
}



#' check whether the vector is decreasing
#' @export
#' @keywords internal
#' @param x a numeric vector
#' @return \code{TRUE} if decreasing, \code{FALSE} if npt.
#' @examples
#' is_decrease(rev(c(1,1,1,10,10)))
#' is_decrease(c(1,2,1,10,10))
is_decrease = function(x){

  ## check whether the vector is decreasing
  ## @export
  ## @keywords internal
  ## @param x a numeric vector
  ## @return \code{TRUE} if decreasing, \code{FALSE} if npt.
  ## @examples
  ## is_decrease(rev(c(1,1,1,10,10)))
  ## is_decrease(c(1,2,1,10,10))

  all(x == cummin(x))
}




#' check x is a meaningful vector
#' @export
#' @keywords internal
#' @param x any object
#' @return If it is a meaningful vector, the return \code{TRUE}, otherwise \code{FALSE}
#' @details we define "a meaningful vector" as
#' 1. cannot be a 'list'
#' 2. with positive length,
#' 3. with valid common one-dimensional slicing method, like x[1] etc, # so factor, character, numeric will pass the check
#' 4. not all elements being NA.
#' @examples
#' 
#' check_vec_meaningful(c(NA,NA)) # NOT PASS
#' 
#' tryCatch(check_vec_meaningful(x=list(NA,NaN)),
#'          error = function(err){
#'            print(err)
#'            }
#'          )# NOT PASS
#' 
#' check_vec_meaningful(c(NA,1)) # PASS
#' check_vec_meaningful(c(NULL,1)) # PASS
#' check_vec_meaningful(factor(c(NA,1,1))) # PASS
#' check_vec_meaningful(1) # PASS
#' check_vec_meaningful('1') # PASS
#' 
check_vec_meaningful = function (x){

  # an attach_Load_First.R function

  ## check x is a meaningful vector
  ## @export
  ## @keywords internal
  ## @param x any object
  ## @return If it is a meaningful vector, the return \code{TRUE}, otherwise \code{FALSE}
  ## @details we define "a meaningful vector" as
  ## 1. cannot be a 'list'
  ## 2. with positive length,
  ## 3. with valid common one-dimensional slicing method, like x[1] etc, # so factor, character, numeric will pass the check
  ## 4. not all elements being NA.

  if ('factor' %in% class(x)) x = as.character(x) #is.vector will think factor as FALSE

  if (is.null(x) || length(x)==0) { # is.null will check the object as a whole, not each single element
    y = 0
  } else if (!is.vector(x) || 'list' %in% class(x)) { # is.vector will return true for list and vector!
    stop('x is not a vector, it might be a list or data.frame or matrix ....')
  } else if (sum(is.na(x) + is.nan(x)  )==length(x) ){ # this method will allow logic(0) and integer(0)
    y = 0
  } else {
    y = 1
  }

  return(y)

  if (FALSE && TRUE){

    check_vec_meaningful(c(NA,NA)) # NOT PASS

    tryCatch(check_vec_meaningful(x=list(NA,NaN)),
             error = function(err){
               print(err)
               }
             )# NOT PASS

    check_vec_meaningful(c(NA,1)) # PASS
    check_vec_meaningful(c(NULL,1)) # PASS
    check_vec_meaningful(factor(c(NA,1,1))) # PASS
    check_vec_meaningful(1) # PASS
    check_vec_meaningful('1') # PASS

  }
}






#' check x with various characters
#' @export
#' @keywords internal
#' @param x an object to be checked.,
#' @param NULL_allowed whether a x == NULL is allowed,
#' @param Class  the correct class of x
#' @param min_oberserv the min number of row / length that x should have,
#' @param exact_in_match
#' each element of x shall be found in the exact_in_match vector
#' if exact_in_match is a data.frame, we will get the colnames of it as exact_in_match
#' @param fuzzy_match TEST feature
#' @param exact_length  can let you test the exact length of a vector, OR exact number of colnames
#' @param complete_cases TRUE or NULL # check whether a dataframe have NA values
#' @param STOP whether to stop if there is an error.
#' @param message_provided replace the default error message
#'
#' @return return nothing if x passes checks, otherwise an error message
#'
#' @description a single function that can do several different types of sanity checks at the same time
#' @details See sample code
#' @examples
#' 
#' ###_____ unit test ____
#' 
#' 
#' data = ggplot2::diamonds
#' # sanity_check(dasfdfdsfsgre)
#' null_checl = NULL ;
#' tryCatch( sanity_check(null_checl),
#'           error = function(err) print(err))
#' tryCatch( sanity_check(null_checl,NULL_allowed = TRUE),
#'           error = function(err) print(err))
#' 
#' 
#' tryCatch( sanity_check(c('x','y'),min_oberserv = 3),
#'           error = function(err) print(err))
#' tryCatch( sanity_check(c('x','y'),min_oberserv = 2),
#'           error = function(err) print(err))
#' 
#' tryCatch( sanity_check(data,Class = 'data.frame2'),
#'           error = function(err) print(err))
#' tryCatch( sanity_check(data,min_oberserv = 3000000),
#'           error = function(err) print(err))
#' 
#' tryCatch( sanity_check(data,exact_length = ncol(data) ),
#'           error = function(err) print(err))
#' tryCatch( sanity_check(
#'   data.frame(data,NA,stringsAsFactors = FALSE)[1:10,],
#'                        complete_cases = TRUE ),
#'           error = function(err) print(err))
#' 
#' 
#' colnames(data)
#' tryCatch( sanity_check(x= c('carat') ,exact_in_match = data),
#'           error = function(err) print(err))
#' tryCatch( sanity_check(x= c('carat') ,exact_in_match = colnames(data)) ,
#'           error = function(err) print(err))
#' tryCatch( sanity_check(x= c('carat2') ,exact_in_match = data),
#'           error = function(err) print(err))
#' tryCatch(  sanity_check(x= c('carat','carat2') ,exact_in_match = data),
#'            error = function(err) print(err))
#' 
#' tryCatch(  sanity_check(x=colnames(data),exact_in_match = c('carat')),
#'            error = function(err) print(err))
#' 
#' tryCatch(   sanity_check(x=data,exact_in_match = c('carat2')),
#'             error = function(err) print(err))
#' 
sanity_check = function(x ,
                        NULL_allowed = FALSE, # whether is.NULL is allowed
                        Class = NULL, # Class is the correct class that class(x) should contains
                        min_oberserv = NULL, # min_oberserv is the min number of row / length that x should have
                        exact_in_match = NULL, # each element of x shall be found in the exact_in_match vector
                        # if exact_in_match is a data.frame, we will get the colnames of it as exact_in_match
                        fuzzy_match = NULL,
                        exact_length = NULL, # exact_length can let you test the exact length of a vector, OR exact number of colnames
                        complete_cases = NULL, # TRUE or NULL # check whether a dataframe have NA values
                        STOP = 1, # STOP: whether to stop if there is an error.
                        message_provided = ''
){
  # from attach_load_first.R

  # Goal: check whether the inout is in the right class, or right length , or right values....
  # x is the object you want to check


  ## check x with various characters
  ## @export
  ## @keywords internal
  ## @param x an object to be checked.,
  ## @param NULL_allowed whether a x == NULL is allowed,
  ## @param Class  the correct class of x
  ## @param min_oberserv the min number of row / length that x should have,
  ## @param exact_in_match
  ## each element of x shall be found in the exact_in_match vector
  ## if exact_in_match is a data.frame, we will get the colnames of it as exact_in_match
  ## @param fuzzy_match TEST feature
  ## @param exact_length  can let you test the exact length of a vector, OR exact number of colnames
  ## @param complete_cases TRUE or NULL # check whether a dataframe have NA values
  ## @param STOP whether to stop if there is an error.
  ## @param message_provided replace the default error message
  ##
  ## @return return nothing if x passes checks, otherwise an error message
  ##
  ## @description a single function that can do several different types of sanity checks at the same time
  ## @details See sample code


  Show_Name = deparse(substitute(x)) # to let the code print the original name of x

  Message = NULL

  if ('factor' %in% class(x)) x = as.character(x)

  ## --------------  check null
  if (NULL_allowed == FALSE && is.null(x)) stop(Show_Name, ' is NULL, which is not allowed')

  ## --------------  check class

  if (check_vec_meaningful(Class)){

    if (class(Class)!='character') stop('Class must be an character')

    Input_Class=class(x)
    #  if integer and numeric class, we treat them as same
    if (sum(c('integer','numeric','logical') %in% Class)) {
      # Class = 'test'
      Class=c(Class,c('numeric','integer','logical'))
    }

    # check class

    if (sum(Class %in% Input_Class)==0){
      cat("\n \n------------------- \n")

      Message =
        paste("\n",Show_Name," is in ",class(x), "\n","It shall be in class ",paste(Class,collapse = ' or '),"\n")
    }
  }

  ## --------------  check exact values:
  if ('factor' %in% class(exact_in_match)) exact_in_match = as.character(exact_in_match)

  if ("data.frame" %in% class(exact_in_match)) exact_in_match = colnames(exact_in_match)


  if (check_vec_meaningful(exact_in_match) ){

    if ( !is.vector(x) & !sum(c('data.frame',"matrix") %in% class(x) )) {
      stop("x must be a vector or matrix or data.frame when you want to check its match")
    }


    if (is.vector(x) && sum(x %in% exact_in_match)!=length(x)  ){
      # if x is the vector, then try to match the value
      cat("\n \n------------------- \n")
      print(exact_in_match)
      Message = paste("\n",Show_Name," shall have values above \n \n")
    }
    if (
      # if x is a data.frame, then try to match the colnames
      sum(c('data.frame',"matrix") %in% class(x) ) && sum(colnames(x) %in% exact_in_match)!=ncol(x)
    ){

      cat("\n \n------------------- \n")
      print(exact_in_match)
      Message = paste("\n",Show_Name," shall have colnames above \n \n")
    }
  }

  ## -------------- check fuzzy values

  # each element of x shall be fuzzy matched in the fuzzy_match vector

  if ("data.frame" %in% class(fuzzy_match)) fuzzy_match = colnames(fuzzy_match)
  if (check_vec_meaningful(fuzzy_match) ){

    if ( !is.vector(x) & !sum(c('data.frame',"matrix") %in% class(x) )) {
      stop("x must be a vector or matrix or data.frame when you want to check its match")
    }

    # if x is the vector, then try to match the value
    if (is.vector(x)) x_match = x
    if (sum(c('data.frame',"matrix") %in% class(x))) x_match = colnames(x)

    for (M in x_match) {

      if (sum(str_detect(M,fuzzy_match))==0){

        cat("\n \n------------------- \n")
        print(fuzzy_match)
        Message = paste("\n",Show_Name," shall have values / colnames above \n \n")
        print(Message)
      }
    }
  }

  ## -------------- check lengths
  if (  check_vec_meaningful(exact_length) ) {

    if (!check_single_numeric(exact_length)) {
      stop ('exact_length must be numeric and has single value larger than 1')
    }

    if ( !is.vector(x) & !sum(c('data.frame',"matrix") %in% class(x) )) {
      stop("x must be a vector or matrix or data.frame when you want to check its length or number of cols")
    }

    if (
      (is.vector(x) && length(x)!=exact_length) |
      (sum(c('data.frame',"matrix") %in% class(x) ) && ncol(x)!=exact_length)
    ){
      cat("\n \n------------------- \n")
      Message = paste("\n",Show_Name," shall have the length / number of columns as ", exact_length,"\n")
    }
  }

  # -------------  check min_oberserv, if you defined min_oberserv
  if (check_vec_meaningful(min_oberserv)){

    if (!check_single_numeric(min_oberserv)){
      stop ("min_oberserv shall have a single numeric positive values" )
    }

    if (
      (class(x) %in% c("data.frame","matrix") && nrow(x)<min_oberserv) ||
      (is.null(nrow(x)) && length(x)<min_oberserv)
    ){
      cat("\n \n------------------- \n")

      Message =
        paste(
          "\n",
          Show_Name, " shall have more observations than ", min_oberserv,
          "\n"
        )
    }
  }

  ## ---------- check min_oberserv
  if ((check_vec_meaningful(complete_cases)) && complete_cases &&
      !is.null(nrow(x)) &&
      !sum( complete.cases(x) )==nrow(x) ){
    Message =
      paste("\n \n------------------- \n")

    Rows_NA=which(complete.cases(x)==0)
    cat(Rows_NA)

    Message =
      paste("\n","Rows above has NA","\n")
  }

  if (length(Message)){
    cat(message_provided)

    if (STOP == 1) {
      stop(Message)
    } else {
      warning(Message)
    }
  }

  if (FALSE && TRUE){

    ###_____ unit test ____


    data = ggplot2::diamonds
    # sanity_check(dasfdfdsfsgre)
    null_checl = NULL ;
    tryCatch( sanity_check(null_checl),
              error = function(err) print(err))
    tryCatch( sanity_check(null_checl,NULL_allowed = TRUE),
              error = function(err) print(err))


    tryCatch( sanity_check(c('x','y'),min_oberserv = 3),
              error = function(err) print(err))
    tryCatch( sanity_check(c('x','y'),min_oberserv = 2),
              error = function(err) print(err))

    tryCatch( sanity_check(data,Class = 'data.frame2'),
              error = function(err) print(err))
    tryCatch( sanity_check(data,min_oberserv = 3000000),
              error = function(err) print(err))

    tryCatch( sanity_check(data,exact_length = ncol(data) ),
              error = function(err) print(err))
    tryCatch( sanity_check(
      data.frame(data,NA,stringsAsFactors = FALSE)[1:10,],
                           complete_cases = TRUE ),
              error = function(err) print(err))


    colnames(data)
    tryCatch( sanity_check(x= c('carat') ,exact_in_match = data),
              error = function(err) print(err))
    tryCatch( sanity_check(x= c('carat') ,exact_in_match = colnames(data)) ,
              error = function(err) print(err))
    tryCatch( sanity_check(x= c('carat2') ,exact_in_match = data),
              error = function(err) print(err))
    tryCatch(  sanity_check(x= c('carat','carat2') ,exact_in_match = data),
               error = function(err) print(err))

    tryCatch(  sanity_check(x=colnames(data),exact_in_match = c('carat')),
               error = function(err) print(err))

    tryCatch(   sanity_check(x=data,exact_in_match = c('carat2')),
                error = function(err) print(err))







  }

}




#' check whether an object is a single numeric number
#' @export
#' @keywords internal
#' @param x  the object to be checked
#' @param sign the correct sign: 1 or -1
#' @return a boolean, whehter x is a valid numeric.
#' @examples
#' 
#' check_single_numeric(x = nrow(ggplot2::diamonds))
#' 
check_single_numeric = function(x, sign = 1){

  # an attach_Load_First.R function
  ## check whether an object is a single numeric number
  ## @export
  ## @keywords internal
  ## @param x  the object to be checked
  ## @param sign the correct sign: 1 or -1
  ## @return a boolean, whehter x is a valid numeric.

  if (sum(c('numeric','integer','logical') %in% class(x)) && length(x)==1 && x*sign>0) {
    x= 1
  } else {x = 0}

  return(x)

  if (FALSE && TRUE){
    check_single_numeric(x = nrow(ggplot2::diamonds))
  }
}



### Unfished =====================



### From Others --------------------




#' make the lm or glm thin
#' @keywords internal
#' @param model glm or lm.
#' @return a thinner model
#' @author  Nina Zumel
stripGlmLR = function(model) {

  # from attach_load_first.R

  # Trimming the Fat from glm() Models in R: reduce the size of it
  # http://www.r-bloggers.com/trimming-the-fat-from-glm-models-in-r/

  ## make the lm or glm thin
  ## @keywords internal
  ## @param model glm or lm.
  ## @return a thinner model
  ## @author  Nina Zumel

  model$y = c()
  model$model = c()

  model$residuals = c()
  model$fitted.values = c()
  model$effects = c()
  model$qr$qr = c()
  model$linear.predictors = c()
  model$weights = c()
  model$prior.weights = c()
  model$data = c()

  model$family$variance = c()
  model$family$dev.resids = c()
  model$family$aic = c()
  model$family$validmu = c()
  model$family$simulate = c()
  attr(model$terms,".Environment") = c()
  attr(model$formula,".Environment") = c()

  model$striped = TRUE



  return(model)


  if( FALSE && FALSE) {


    model = lm(price~  I(carat^   2) + cut  - carat:table - cut ,ggplot2::diamonds)

    model = stripGlmLR(model)

    model$residuals

    for (i in attributes(model)$names){
      print(model[[i]])
    }
  }

}




###  Formula Functions  -------------------

# NOT supposed to show to public





#' the underlying function of \code{\link{get_x}}
#'
#' @export
#' @keywords internal
#'
#' @details
#' This is the one of many underlying functions that powers \code{\link{get_x}}.
#' The major difference is: \code{\link{get_x}} can deal with dirty formula,
#' but \code{get_x_hidden} cannot. 'dirty formula' means a formula with redundant terms,
#' such as \code{y ~ x1 + x2 -x1}.
#'
#' @param model method,data  \code{\link{get_x}}
#' @seealso \code{\link{get_x}}
#' @return  \code{\link{get_x}}
#' @examples
#' 
#' #
#' data = ggplot2::diamonds
#' diamond_lm  =  lm(price~  I(carat^   2) + cut  + carat*table ,ggplot2::diamonds)
#' 
#' #_________ input as model
#' get_x_hidden(model = diamond_lm,method = 'raw')
#' get_x_hidden(diamond_lm,method = 'model')
#' get_x_hidden(diamond_lm,method = 'coeff')
#' 
#' #_______ input as formula
#' get_x_hidden(formula(diamond_lm),method = 'model')
#' # data is required when input is formula
#' get_x_hidden(formula(diamond_lm), data = ggplot2::diamonds, method = 'coeff')
#' 
#' tryCatch(
#'   get_x_hidden(formula(diamond_lm),method = 'coeff'),
#'   error =function(err){
#'     print(err)
#'   }
#' )
#' 
#' 
#' 
#' 
#' 
#' #________ irregular formulas __________
#' 
#' model_dirty = model = lm(price~  I(carat^   2) + cut  -
#'                            carat:table - cut ,ggplot2::diamonds)
#' 
#' # WRONG for raw vars
#' get_x_hidden(model_dirty)
#' 
#' # correct for model vars
#' get_x_hidden(price~  I(carat^2) + cut  -
#'                carat:table - cut,
#'              data = ggplot2::diamonds, method ='model')
#' 
#' get_x_hidden(model_dirty,method = 'model')
#' get_x_hidden(model_dirty,data = ggplot2::diamonds, method = 'model')
#' get_x_hidden(model_dirty, method = 'model')
#' 
#' #___________ coeff vars __________
#' 
#' # clean
#' get_x_hidden(model_dirty, data = ggplot2::diamonds, method = 'coeff')
#' get_x_hidden(formula(model_dirty),data = ggplot2::diamonds, method = 'coeff')
#' 
#' 
#' #
#' # # dirty
#' # attr(terms((price~  I(carat^2) + cut  + carat:table - cut)),"factors") %>% colnames()
#' #
#' # #______________ test: how to get variables
#' # model.matrix(formula(model_dirty),data = ggplot2::diamonds) %>% colnames
#' # terms(formula(diamond_lm)) %>% attr(.,"factors") %>% colnames()
#' # terms(formula(model_dirty)) %>% attr(.,"factors") %>% colnames()
#' # terms(formula(model_dirty)) %>% attr(.,"factors") %>% rownames()
#' #
#' #
#' # # clean method for model vars
#' # terms((price~  I(carat^2) + cut  - carat:table - cut)) %>% attr(.,"factors") %>% colnames()
#' # model_dirty %>% terms %>% attr(.,"factors") %>% colnames()
#' # formula(model_dirty) %>% terms %>% attr(.,"factors") %>% colnames()
#' 
get_x_hidden = function(model,
                        method = c("raw","model","coeff"),
                        data = NULL){
  # from attach_load_first.R

  ## the underlying function of \code{\link{get_x}}
  ##
  ## @export
  ## @keywords internal
  ##
  ## @details
  ## This is the one of many underlying functions that powers \code{\link{get_x}}.
  ## The major difference is: \code{\link{get_x}} can deal with dirty formula,
  ## but \code{get_x_hidden} cannot. 'dirty formula' means a formula with redundant terms,
  ## such as \code{y ~ x1 + x2 -x1}.
  ##
  ## @param model method,data  \code{\link{get_x}}
  ## @seealso \code{\link{get_x}}
  ## @return  \code{\link{get_x}}


  # NOT supposed to show to public

  # if method = "raw": only get the raw var: you will get "x" instead of "log(x)" from formula y~log(x).
  # if method = "model": only get the raw var: you will get "x" instead of "log(x)" from formula y~log(x).
  # if method = "coeff": used for categorical variables, you will get
  # "cut.L"       "cut.Q"       "cut.C"       "cut^4"

  # instead of just cut
  # from lm(price ~ cut,data = ggplot2::diamonds)

  method = match.arg(method)

  #    if (is.null(data)) data=data.frame(DELETE_LATER.....123.= 0)
  #       # some formulas contain "." to represent all other vars in data.
  #       # if data is null, then "." has no meaning at all
  #       # so we want to delete it
  #       # just replace "." by "DELETE_LATER.....123." temporarily, later we will delete it.
  #       # so this "DELETE_LATER.....123." is just a temp placeholder


  sanity_check(model, Class = c("lm","glm",'formula','character') ,
               message_provided = 'model must be a lm, glm, formula or string or formula'    )

  if (method == "raw") {
    # all.vars() cannot deal with dirty formula
    # it has to depend on coeff or model vars to get cleaned raw vars
    var = all.vars(formula (model))

    # var = all.vars(formula (model))
    # var = all.vars(formula (model.frame(model)))
    # var = all.vars(formula (model.matrix(model))) # invalid

    var = var[-1]
  }

  if (method == "model") {
    # this method is able to deal with dirty formulas
    var = terms(formula(model)) %>% attr(.,"factors") %>% colnames()

    #     if (sum(c("lm","glm") %in% class(model)) ) {
    #
    #       var = model$terms %>% attr(.,"factors") %>% colnames()
    #
    #     }
    #     if (sum(c("formula","character") %in% class(model)) ) {
    #       if (is.null(data)) {
    #         stop("data must be provided when 'model' argument is just a formula and
    #              you want to get 'model' vars")
    #       }
    #       var = colnames(model.frame(formula(model),data))[-1]
    #       }

  }

  if (method == "coeff") {
    # works in dirty formulas

    if (sum(c("lm","glm") %in% class(model))
    )  {
      var = names(model$coeff)
    }
    if (sum(c("formula","character") %in% class(model))) {

      if (is.null(data)) {
        stop("data must be provided when 'model' argument is just a formula and
             you want to get 'coeff' vars")
      }
      data = data.frame(data)

      var = colnames(model.matrix(formula(model),data))
      }
    var = var["(Intercept)" !=var]
  }

  var = gsub(" ","",var)



  return(var[var != "DELETE_LATER.....123."])



  if ( FALSE && TRUE) {

    #
    data = ggplot2::diamonds
    diamond_lm  =  lm(price~  I(carat^   2) + cut  + carat*table ,ggplot2::diamonds)

    #_________ input as model
    get_x_hidden(model = diamond_lm,method = 'raw')
    get_x_hidden(diamond_lm,method = 'model')
    get_x_hidden(diamond_lm,method = 'coeff')

    #_______ input as formula
    get_x_hidden(formula(diamond_lm),method = 'model')
    # data is required when input is formula
    get_x_hidden(formula(diamond_lm), data = ggplot2::diamonds, method = 'coeff')

    tryCatch(
      get_x_hidden(formula(diamond_lm),method = 'coeff'),
      error =function(err){
        print(err)
      }
    )





    #________ irregular formulas __________

    model_dirty = model = lm(price~  I(carat^   2) + cut  -
                               carat:table - cut ,ggplot2::diamonds)

    # WRONG for raw vars
    get_x_hidden(model_dirty)

    # correct for model vars
    get_x_hidden(price~  I(carat^2) + cut  -
                   carat:table - cut,
                 data = ggplot2::diamonds, method ='model')

    get_x_hidden(model_dirty,method = 'model')
    get_x_hidden(model_dirty,data = ggplot2::diamonds, method = 'model')
    get_x_hidden(model_dirty, method = 'model')

    #___________ coeff vars __________

    # clean
    get_x_hidden(model_dirty, data = ggplot2::diamonds, method = 'coeff')
    get_x_hidden(formula(model_dirty),data = ggplot2::diamonds, method = 'coeff')


    #
    # # dirty
    # attr(terms((price~  I(carat^2) + cut  + carat:table - cut)),"factors") %>% colnames()
    #
    # #______________ test: how to get variables
    # model.matrix(formula(model_dirty),data = ggplot2::diamonds) %>% colnames
    # terms(formula(diamond_lm)) %>% attr(.,"factors") %>% colnames()
    # terms(formula(model_dirty)) %>% attr(.,"factors") %>% colnames()
    # terms(formula(model_dirty)) %>% attr(.,"factors") %>% rownames()
    #
    #
    # # clean method for model vars
    # terms((price~  I(carat^2) + cut  - carat:table - cut)) %>% attr(.,"factors") %>% colnames()
    # model_dirty %>% terms %>% attr(.,"factors") %>% colnames()
    # formula(model_dirty) %>% terms %>% attr(.,"factors") %>% colnames()




  }
}





#' paste a formula as string
#'
#' @details
#' a pasted formula in string, with all spaces deleted.
#' This function uses \code{\link{get_y}} and \code{\link{get_x}} behind the scene.
#'
#' @param Formula  a formula to be pasted.
#' @param exclude_y  a boolean, whether to exclude y when paste. Default is FALSE.
#' @param clean  a boolean, whether to clean dirty formula: for example -- price ~ cut + carat - cut
#' will be cleaned into price ~ carat. Default is FALSE.
#' @export
#' @return  a pasted formula in string, with all spaces deleted.
#' @examples
#' 
#' paste_formula(price~carat +cut)
#' paste_formula(price~carat + cut)
#' 
#' paste_formula(price~carat +cut,exclude_y = TRUE)
#' paste_formula(Formula = price ~ cut + carat, clean = TRUE)
#' 
#' paste_formula(price~carat +cut - cut, clean = TRUE)
#' 
#' # irregular formulas: cross lines
#' paste_formula(price~carat +
#'                 cut ~ dsad)
#' 
#' paste_formula(price~carat +
#'                 cut ~ dsad,exclude_y = TRUE)
#' 
paste_formula = function(Formula, # a formula, or a model (like glm/lm) so that formula(model) can give a formula
                         exclude_y = FALSE, # whether to exclude y in the pasted formula
                         clean = FALSE
){
  # from attach_load_first.R
  # paste a formula into text in the same line.
  # note that all spaces will be deleted

  ## paste a formula as string
  ##
  ## @details
  ## a pasted formula in string, with all spaces deleted.
  ## This function uses \code{\link{get_y}} and \code{\link{get_x}} behind the scene.
  ##
  ## @param Formula  a formula to be pasted.
  ## @param exclude_y  a boolean, whether to exclude y when paste. Default is FALSE.
  ## @param clean  a boolean, whether to clean dirty formula: for example -- price ~ cut + carat - cut
  ## will be cleaned into price ~ carat. Default is FALSE.

  ## @export
  ## @return  a pasted formula in string, with all spaces deleted.

  y = get_y(Formula,"model")
  ## max_length of formula = 500, a numeric, max length of the formula in one line.
  ##  If the formula is longer than this number, then the pasted formula will be showed in multiple lines.

  if (clean){
    # Formula = y ~ x - x
    Formula_x_part = paste(get_x(Formula,'model'),collapse = '+',sep='')
    if (str_length(Formula_x_part)==0) Formula_x_part = 1
    Formula = paste(y ,'~',Formula_x_part, sep='')
  } else {
    Formula = gsub(" ","",deparse(formula(Formula),500))
  }

  if (exclude_y) {
    Formula = gsub(paste(y,"~",sep=''),"",Formula,fixed = TRUE)
  }
  return(Formula)


  if( FALSE && TRUE){

    paste_formula(price~carat +cut)
    paste_formula(price~carat + cut)

    paste_formula(price~carat +cut,exclude_y = TRUE)
    paste_formula(Formula = price ~ cut + carat, clean = TRUE)

    paste_formula(price~carat +cut - cut, clean = TRUE)

    # irregular formulas: cross lines
    paste_formula(price~carat +
                    cut ~ dsad)

    paste_formula(price~carat +
                    cut ~ dsad,exclude_y = TRUE)

  }
}






#' get y (right hand of var)
#' @export
#' @details
#' What do 'raw' variable, 'model' variable, and 'coeff' variable mean?
#'
#'\itemize{
#'\item raw var is the underlying variable without any calculation or transformation.
#'\item model var is the underlying variable with calculations or transformation.
#'\item coeff var is the coefficient variable in the model output. So only evaluated model has coeff var.
#' }
#'
#' In the formula, \code{log(y) ~ x1 + x2}, we have:
#' 'raw' variable for \code{y}: \code{y}
#' 'model' variable for \code{y}: \code{log(y)}
#' 'coeff' variable for \code{y}: \code{log(y)}
#'
#' More examples see the sample code below.
#' @param Formula  a formula to be paste.
#' @param method  either \code{'raw','model'}, or \code{'coeff'}, to decide what kind variables to show.
#' Default is 'raw'. See section Detials below.
#' @return y in formula
#' @examples
#' 
#' get_y(log(price) ~sdfsf + dsa )
#' get_y(log(price) ~ sdfsf + dsa, method = "model")
#' get_y(log(price) ~ sdfsf + dsa, method = "coeff") # same as model var in the get_y() case
#' 
#' # can deal with un-regular formula
#' get_y(log(price) ~sdfsf + dsa ~ dsad)
#' get_y(log(price) ~ sdfsf + dsa ~ dsad, method = "coeff")
#' get_y(log(price) ~ sdfsf + dsa ~ dsad, method = "model")
#' 
#' model_dirty = model = lm(price~  I(carat^   2) + cut  - carat:table - cut ,ggplot2::diamonds)
#' get_y(model_dirty)
#' 
get_y = function(Formula,method = c("raw","model","coeff")){
  # from attach_load_first.R

  # depend on get_x_hidden in the 'raw' case, so no need for the data in the 'raw' case.
  # in "model" and "coeff" case, we use regular expression to get y, so no need for data either

  ## get y (right hand of var)
  ## @export
  ## @details
  ## What do 'raw' variable, 'model' variable, and 'coeff' variable mean?
  ##
  ##\itemize{
  ##\item raw var is the underlying variable without any calculation or transformation.
  ##\item model var is the underlying variable with calculations or transformation.
  ##\item coeff var is the coefficient variable in the model output. So only evaluated model has coeff var.
  ## }
  ##
  ## In the formula, \code{log(y) ~ x1 + x2}, we have:
  ## 'raw' variable for \code{y}: \code{y}
  ## 'model' variable for \code{y}: \code{log(y)}
  ## 'coeff' variable for \code{y}: \code{log(y)}
  ##
  ## More examples see the sample code below.

  ## @param Formula  a formula to be paste.
  ## @param method  either \code{'raw','model'}, or \code{'coeff'}, to decide what kind variables to show.
  ## Default is 'raw'. See section Detials below.
  ## @return y in formula



  method = match.arg(method)

  sanity_check(Formula, Class = c("lm","glm",'formula','character') ,
               message_provided = 'model must be a lm, glm, formula or string or formula'    )

  if (method == "raw") {

    var = all.vars(formula (Formula))[1]
  } else {
    Formula = gsub(" ","",deparse(formula(Formula),500))
    var = gsub("\\~.*","",Formula,perl = TRUE)
  }

  return(var)

  if ( FALSE && TRUE) {
    get_y(log(price) ~sdfsf + dsa )
    get_y(log(price) ~ sdfsf + dsa, method = "model")
    get_y(log(price) ~ sdfsf + dsa, method = "coeff") # same as model var in the get_y() case

    # can deal with un-regular formula
    get_y(log(price) ~sdfsf + dsa ~ dsad)
    get_y(log(price) ~ sdfsf + dsa ~ dsad, method = "coeff")
    get_y(log(price) ~ sdfsf + dsa ~ dsad, method = "model")

    model_dirty = model = lm(price~  I(carat^   2) + cut  - carat:table - cut ,ggplot2::diamonds)
    get_y(model_dirty)

  }
}






#' get contrast of categorical variables in a model
#'
#' @export
#' @details
#' When R put categorical vars in the linear model, R will transform them into set of 'contrast' using
#' certain contrast encoding schedule. See example code and the reference link below for details.
#'
#' @param model  a model, either \code{lm} or \code{glm}.
#' @param data   dataframe, to provide new data to evaluate the model. If NULL (default), then we use the default data in the model.
#' @param PRINT  a boolean, whether to print messages. Default is TRUE.
#' @param return_method  a boolean, whether to return the method of contrast, rather than the contrast itself. Default is FALSE.
#' @param delete.minus.var  a boolean. whether to delete x2 in  y ~ x1 - x2. Default is TRUE.
#' @return contrasts of the categorical vars in the model, or the contrast method if \code{return_method} is TRUE.
#' @references \url{http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm}
#' @examples
#' 
#' get_contrast(lm(price ~ carat + I(carat^2) + cut:carat +
#'                   color,ggplot2::diamonds))
#' get_contrast(lm(price ~ carat + I(carat^2) + cut:carat +
#'                   color,ggplot2::diamonds),return_method = TRUE)
#' 
#' # dirty formulas: all categorical vars are with minus sign
#' # no categorical vars, thus no contast
#' get_contrast(lm(price ~ carat + I(carat^2) ,ggplot2::diamonds))
#' 
#' model_dirty = lm(price ~ carat + I(carat^2) - cut:carat - color,
#'                  ggplot2::diamonds)
#' get_contrast(model = model_dirty )
#' 
#' diamond_lm3 = lm(price~ I(cut) + depth,ggplot2::diamonds) # a GLM
#' get_contrast(model = diamond_lm3 )
#' 
get_contrast = function(model,
                        data = NULL,
                        PRINT = TRUE,
                        return_method = FALSE,
                        # if FALSE, then return the names of contrasted vars,
                        # if TRUE, then return the name of contrast function used.
                        delete.minus.var = TRUE
                        # whether to delete x2 in  y ~ x1 - x2, default is yes.
){

  # from attach_load_first.R
  # independent from get_x_hidden

  ## get contrast of categorical variables in a model
  ##
  ## @export
  ## @details
  ## When R put categorical vars in the linear model, R will transform them into set of 'contrast' using
  ## certain contrast encoding schedule. See example code and the reference link below for details.
  ##
  ## @param model  a model, either \code{lm} or \code{glm}.
  ## @param data   dataframe, to provide new data to evaluate the model. If NULL (default), then we use the default data in the model.
  ## @param PRINT  a boolean, whether to print messages. Default is TRUE.
  ## @param return_method  a boolean, whether to return the method of contrast, rather than the contrast itself. Default is FALSE.
  ## @param delete.minus.var  a boolean. whether to delete x2 in  y ~ x1 - x2. Default is TRUE.
  ## @return contrasts of the categorical vars in the model, or the contrast method if \code{return_method} is TRUE.
  ## @references \url{http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm}

  # input is a model or a model.matrix
  # if there is categorical var, output is a list,
  # where names of the list are the names of raw categorical vars
  # where values of the list are the names of contrasted coeffs of the corresponding categorical vars
  # if no categorical var, then return NULL

  #   if ( sum(c('lm','glm') %in% class(model))
  #        ){
  #     mm = model.matrix(model)
  #   } else if ('matrix' %in% model){
  #     mm = model
  #   } else {
  #     stop('class of input shall be model.matrix or a model')
  #     }


  Contrast = model$contrasts # a list, names as model var
  if (delete.minus.var) Contrast = Contrast[names(Contrast) %in% get_x(model,'model')]

  if (length(Contrast)==0){
    if (PRINT) message('no contrasts are found')
    result = NULL
  } else {
    if (is.null(data)) {
      data = model.frame(model)
      if (is.null(data))  stop("data must be provided, or the 'model' argument must be lm/glm")
    } else {
      data = data.frame(data)

      data = model.frame(formula = model, data = data)


    }

    result = list()


    for (i in names(Contrast)){
      # i = names(Contrast)[1]
      if (is.null(data[,i])) {
        if (is.null(data)) stop(i, ' can not be found in the data')
      }

      if ( return_method) {
        result[[i]] = as.name( Contrast[[i]] )
      } else {
        result[[i]] =

          list(
            as.name( Contrast[[i]] ), # this is contrast method
            unique(data[,i])
          ) %>% as.call %>% eval %>% colnames %>% paste(i,.,sep='')
      }
    }
  }

  return(result)

  if (FALSE && TRUE){
    get_contrast(lm(price ~ carat + I(carat^2) + cut:carat +
                      color,ggplot2::diamonds))
    get_contrast(lm(price ~ carat + I(carat^2) + cut:carat +
                      color,ggplot2::diamonds),return_method = TRUE)

    # dirty formulas: all categorical vars are with minus sign
    # no categorical vars, thus no contast
    get_contrast(lm(price ~ carat + I(carat^2) ,ggplot2::diamonds))

    model_dirty = lm(price ~ carat + I(carat^2) - cut:carat - color,
                     ggplot2::diamonds)
    get_contrast(model = model_dirty )

    diamond_lm3 = lm(price~ I(cut) + depth,ggplot2::diamonds) # a GLM
    get_contrast(model = diamond_lm3 )


  }

}






#' get a list of model vars with their corresponding coeff vars or raw vars.
#' @export
#' @details get a list of model vars with their corresponding coeff vars or raw vars.
#' See \code{\link{get_x}} for the meaning of model var, coeff var and raw var.
#'
#' @param model  a lm or glm output
#' @param data  NULL (default) or data.frame, a new dataset to evaluate the categorical variables.
#' If NULL, then use the data used in model itself.
#' @param pair_with  either 'raw' (default) or 'coeff', to decide the elements of list are raw vars or coeff vars.
#' See \code{\link{get_x}} for the meaning of model var, coeff var and raw var.
#' @return a list with names as model vars and elements as their corresponding coeff/raw vars
#' @examples
#' 
#' # return coeff
#' get_model_pair(model = price~  I(carat^2) + cut  + carat*table, data = ggplot2::diamonds)
#' # return raw vars
#' get_model_pair(price~  I(carat^2) + cut  + carat*table, data= ggplot2::diamonds, pair_with = 'raw')
#' 
#' # correctly deal with irregular formulas
#' model_dirty = lm(price~  I(carat^   2) + cut  - carat:table - cut ,ggplot2::diamonds)
#' get_model_pair(model_dirty,pair_with = 'raw')
#' 
get_model_pair = function(model,data = NULL,pair_with = c('coeff','raw')){

  # from attach_load_first.R

  # this function will create links between raw, model and coeffs.

  # output is a list
  # list names are model vars
  # value of list names are the coeffs or raws

  # will be used in get_x, to link to raw vars to model vars
  # pair_with = 'coeff'

  ## get a list of model vars with their corresponding coeff vars or raw vars.
  ## @export
  ## @details get a list of model vars with their corresponding coeff vars or raw vars.
  ## See \code{\link{get_x}} for the meaning of model var, coeff var and raw var.
  ##
  ## @param model  a lm or glm output
  ## @param data  NULL (default) or data.frame, a new dataset to evaluate the categorical variables.
  ## If NULL, then use the data used in model itself.
  ## @param pair_with  either 'raw' (default) or 'coeff', to decide the elements of list are raw vars or coeff vars.
  ## See \code{\link{get_x}} for the meaning of model var, coeff var and raw var.
  ## @return a list with names as model vars and elements as their corresponding coeff/raw vars


  # pair_with = 'coeff'
  pair_with = match.arg(pair_with)
  y = get_y(model,'model')
  result = list()
  model_var = get_x_hidden(model,data = data, 'model')

  for (i in model_var){
    # i = model_var[1]
    if ( pair_with == 'raw') {
      result[[i]] = get_x_hidden (paste( y,'~', i),data = data, method  = pair_with)
    }
    if ( pair_with == 'coeff'){
      if (is.null(data)) {
        data = get_data_from_lm(model)
        if (is.null(data)) stop ('data is not provided and cannot be found in the model')
      }
      data = data.frame(data)

      result[[i]] = get_x_hidden (paste( y,'~', i),data = data, method  = pair_with)
    }

  }

  return(result)

  if(FALSE && TRUE){
    # return coeff
    get_model_pair(model = price~  I(carat^2) + cut  + carat*table, data = ggplot2::diamonds)
    # return raw vars
    get_model_pair(price~  I(carat^2) + cut  + carat*table, data= ggplot2::diamonds, pair_with = 'raw')

    # correctly deal with irregular formulas
    model_dirty = lm(price~  I(carat^   2) + cut  - carat:table - cut ,ggplot2::diamonds)
    get_model_pair(model_dirty,pair_with = 'raw')
  }
}





#' get x (left hand of var) from model or formula
#' @export
#' @details
#' What do 'raw' variable, 'model' variable, and 'coeff' variable mean?
#'
#'\itemize{
#'\item raw var is the underlying variable without any calculation or transformation.
#'\item model var is the underlying variable with calculations or transformation.
#'\item coeff var is the coefficient variable in the model output.
#'      So only evaluated model has coeff vars.
#'      Most of the time one categorical variable will have several coeff vars according to their contrast encoding. see \code{\link{get_contrast}}
#' }
#'
#' Example:
#'
#' In the model, \code{log(price) ~ cut + I(carat^2)} in \code{diamonds} data, we have:
#' \itemize{
#' \item 'raw' variables of x: \code{carat} and \code{cut}.
#' \item 'model' variables of x: \code{I(carat^2)} and \code{cut}.
#' \item 'coeff' variables of x: \code{cut.L,"cut.Q","cut.C","cut^4"} and \code{I(carat^2)}.
#' }
#'
#' See the sample code below for more examples.
#' @param model  a formula or a model.
#' @param method  either \code{'raw','model'}, or \code{'coeff'}, to decide what kind variables to show.
#' Default is 'raw'. See section Detials below.
#' @param data  a dataframe, to provide new data to evaluate the model. If NULL (default), then we use the default data in the model.
#' @return x variables in the formula or model
#' @import utils
#' @import ggplot2
#' @import plyr
#' @import pryr
#' @import stringr
#' @import stats
#' @import magrittr
#' @import scales
#' @examples
#' 
#' # use the sample code from get_x_hidden
#' #
#' data = ggplot2::diamonds
#' diamond_lm  =  lm(price~  I(carat^   2) + cut  + carat*table ,ggplot2::diamonds)
#' 
#' #_________ input as model
#' get_x(model = diamond_lm,method = 'raw')
#' get_x(diamond_lm,method = 'model')
#' get_x(diamond_lm,method = 'coeff')
#' 
#' #_______ input as formula
#' get_x(formula(diamond_lm),method = 'model')
#' # data is required when input is formula
#' get_x(formula(diamond_lm), data = ggplot2::diamonds, method = 'coeff')
#' 
#' tryCatch(
#'   get_x(formula(diamond_lm),method = 'coeff'),
#'   error =function(err){
#'     print(err)
#'   }
#' )
#' 
#' 
#' 
#' #________ irregular formulas __________
#' 
#' model_dirty = model = lm(price~  I(carat^   2) + cut  - carat:table - cut ,ggplot2::diamonds)
#' 
#' # CORRECT for raw vars
#' get_x(model_dirty)
#' 
#' # correct for model vars
#' get_x(price~  I(carat^2) + cut  - carat:table - cut,data = ggplot2::diamonds, method ='model')
#' get_x(model_dirty,method = 'model')
#' get_x(model_dirty,data = ggplot2::diamonds, method = 'model')
#' get_x(model_dirty, method = 'model')
#' 
#' # clean method for model vars
#' # terms((price~  I(carat^2) + cut  - carat:table - cut)) %>% attr(.,"factors") %>% colnames()
#' # model_dirty %>% terms %>% attr(.,"factors") %>% colnames()
#' # formula(model_dirty) %>% terms %>% attr(.,"factors") %>% colnames()
#' 
get_x = function(model ,
                 method = c("raw","model","coeff"),
                 data = NULL){

  # a function that shall be exported

  ## get x (left hand of var) from model or formula
  ## @export
  ## @details
  ## What do 'raw' variable, 'model' variable, and 'coeff' variable mean?
  ##
  ##\itemize{
  ##\item raw var is the underlying variable without any calculation or transformation.
  ##\item model var is the underlying variable with calculations or transformation.
  ##\item coeff var is the coefficient variable in the model output.
  ##      So only evaluated model has coeff vars.
  ##      Most of the time one categorical variable will have several coeff vars according to their contrast encoding. see \code{\link{get_contrast}}
  ## }
  ##
  ## Example:
  ##
  ## In the model, \code{log(price) ~ cut + I(carat^2)} in \code{diamonds} data, we have:
  ## \itemize{
  ## \item 'raw' variables of x: \code{carat} and \code{cut}.
  ## \item 'model' variables of x: \code{I(carat^2)} and \code{cut}.
  ## \item 'coeff' variables of x: \code{cut.L,"cut.Q","cut.C","cut^4"} and \code{I(carat^2)}.
  ## }
  ##
  ## See the sample code below for more examples.

  ## @param model  a formula or a model.
  ## @param method  either \code{'raw','model'}, or \code{'coeff'}, to decide what kind variables to show.
  ## Default is 'raw'. See section Detials below.
  ## @param data  a dataframe, to provide new data to evaluate the model. If NULL (default), then we use the default data in the model.
  ## @return x variables in the formula or model
  ## @import utils
  ## @import ggplot2
  ## @import plyr
  ## @import pryr
  ## @import stringr
  ## @import stats
  ## @import magrittr
  ## @import scales


  method = match.arg(method)

  if (method == 'raw'){
    var =  get_model_pair(model = model,data = data, pair_with = 'raw') %>% unique
    names(var) = NULL
    var = var %>% unlist %>% unique
  } else {
    var = get_x_hidden(model = model,
                       method = method,
                       data = data)
  }

  return(var)

  if(FALSE && TRUE){
    # use the sample code from get_x_hidden
    #
    data = ggplot2::diamonds
    diamond_lm  =  lm(price~  I(carat^   2) + cut  + carat*table ,ggplot2::diamonds)

    #_________ input as model
    get_x(model = diamond_lm,method = 'raw')
    get_x(diamond_lm,method = 'model')
    get_x(diamond_lm,method = 'coeff')

    #_______ input as formula
    get_x(formula(diamond_lm),method = 'model')
    # data is required when input is formula
    get_x(formula(diamond_lm), data = ggplot2::diamonds, method = 'coeff')

    tryCatch(
      get_x(formula(diamond_lm),method = 'coeff'),
      error =function(err){
        print(err)
      }
    )



    #________ irregular formulas __________

    model_dirty = model = lm(price~  I(carat^   2) + cut  - carat:table - cut ,ggplot2::diamonds)

    # CORRECT for raw vars
    get_x(model_dirty)

    # correct for model vars
    get_x(price~  I(carat^2) + cut  - carat:table - cut,data = ggplot2::diamonds, method ='model')
    get_x(model_dirty,method = 'model')
    get_x(model_dirty,data = ggplot2::diamonds, method = 'model')
    get_x(model_dirty, method = 'model')

    # clean method for model vars
    # terms((price~  I(carat^2) + cut  - carat:table - cut)) %>% attr(.,"factors") %>% colnames()
    # model_dirty %>% terms %>% attr(.,"factors") %>% colnames()
    # formula(model_dirty) %>% terms %>% attr(.,"factors") %>% colnames()

  }
}





#' get a list of model variables with their corresponding coeff vars.
#'
#' @export
#' @description a wrap up function of \code{\link{get_model_pair}}
#' @details See \code{\link{get_model_pair}}
#'
#' @param model See \code{\link{get_model_pair}}
#' @param data See \code{\link{get_model_pair}}
#' @return a list with names as model vars and elements as their corresponding coeff
#' @examples
#' 
#' get_model_with_coeff(price~  I(carat^   2) + cut  + carat*table, data= ggplot2::diamonds)
#' 
get_model_with_coeff = function(model,data = NULL){
  # depend on get_model_pair


  ## get a list of model variables with their corresponding coeff vars.
  ##
  ## @export
  ## @description a wrap up function of \code{\link{get_model_pair}}
  ## @details See \code{\link{get_model_pair}}
  ##
  ## @param model See \code{\link{get_model_pair}}
  ## @param data See \code{\link{get_model_pair}}
  ## @return a list with names as model vars and elements as their corresponding coeff


  return(get_model_pair(model = model, data=data, pair_with = 'coeff'))

  if(FALSE && TRUE){
    get_model_with_coeff(price~  I(carat^   2) + cut  + carat*table, data= ggplot2::diamonds)
  }
}





#' get a list of model vars with their corresponding raw vars.
#'
#' @export
#' @description a warp up function of \code{\link{get_model_pair}}
#' @details See \code{\link{get_model_pair}}
#'
#' @param model, See \code{\link{get_model_pair}}
#' @param data, See \code{\link{get_model_pair}}
#' @return a list with names as model vars and elements as their raw coeff
#' @examples
#' 
#' get_model_with_raw(price~  I(carat^   2) + cut  + carat*table, data= ggplot2::diamonds)
#' 
get_model_with_raw = function(model,data = NULL){
  # depend on get_model_pair

  ## get a list of model vars with their corresponding raw vars.
  ##
  ## @export
  ## @description a warp up function of \code{\link{get_model_pair}}
  ## @details See \code{\link{get_model_pair}}
  ##
  ## @param model, See \code{\link{get_model_pair}}
  ## @param data, See \code{\link{get_model_pair}}
  ## @return a list with names as model vars and elements as their raw coeff



  return(get_model_pair(model = model, data=data, pair_with = 'raw'))

  if(FALSE && TRUE){
    get_model_with_raw(price~  I(carat^   2) + cut  + carat*table, data= ggplot2::diamonds)
  }
}





#' a unique combinations of model vars, coeff vars and raw vars
#'
#' @export
#' @details For the differences between raw var, model var, and coeff var: see \code{\link{get_x}}
#'
#' @param model \code{lm} or \code{glm}
#' @param data  NULL (default) or data.frame, a new dataset to evaluate the categorical variables.
#' If NULL, then use the data used in model itself.
#' @return a data.frame, a unique combinations of model vars, coeff vars and raw vars
#' See \code{\link{get_x}} for the meaning of \code{model var}, \code{coeff var} or \code{raw var}.
#'
#'
#' The column \code{'n_raw_in_model'} is a numeric field showing how many raw variables are in the corresponding model variables.
#' For example, the model variable 'I(carat*table)' contains two raw variables: 'carat' and 'table'. See example code for details.
#' @examples
#' 
#' get_x_all(model = price~  I(carat^   2) + cut  + I(carat*table),data = ggplot2::diamonds)
#' 
#' 
#' #________ irregular formulas
#' model_dirty = lm(price~  I(carat^   2) + cut  - carat:table - cut ,ggplot2::diamonds)
#' test = get_x_all(model_dirty)
#' 
#' test
#' test$coeff
#' # ______  errors _______________
#' 
#' tryCatch(get_x_all(model = price~  I(carat^   2) + cut  + I(carat*table)),
#'          error = function(x){
#'            print(x)
#'          })
#' 
get_x_all = function(model,
                     data = NULL
){

  # from attach_load_first.R

  # depend on get_model_pair
  # output is a data.frame, a unique combinations of model vars, coeff vars and raw vars

  ## a unique combinations of model vars, coeff vars and raw vars
  ##
  ## @export
  ## @details For the differences between raw var, model var, and coeff var: see \code{\link{get_x}}
  ##
  ## @param model \code{lm} or \code{glm}
  ## @param data  NULL (default) or data.frame, a new dataset to evaluate the categorical variables.
  ## If NULL, then use the data used in model itself.
  ## @return a data.frame, a unique combinations of model vars, coeff vars and raw vars
  ## See \code{\link{get_x}} for the meaning of \code{model var}, \code{coeff var} or \code{raw var}.
  ##
  ##
  ## The column \code{'n_raw_in_model'} is a numeric field showing how many raw variables are in the corresponding model variables.
  ## For example, the model variable 'I(carat*table)' contains two raw variables: 'carat' and 'table'. See example code for details.


  List_raw = get_model_pair(model, data, 'raw')
  List_coeff = get_model_pair(model, data, 'coeff')

  result = NULL
  for (i in names(List_raw)){

    raw =  List_raw[[i]]

    for (r in raw){
      result = rbind(result,
                     data.frame(raw = r, model = i, coeff = List_coeff[[i]],stringsAsFactors = FALSE)
      )
    }
  }
  # result$coeff
  result$raw = as.character(result$raw)
  result$model = as.character(result$model)
  result$coeff = as.character(result$coeff)


  n_raw_in_model = (result$model == result$raw)*1
  n_raw_in_coeff = (result$coeff == result$raw)*1

  if (sum(!n_raw_in_model)){
    for (i in  (result$model[!n_raw_in_model] %>% unique)
    ){
      # i =  (result$model[!n_raw_in_model] %>% unique)[1]
      position = (result$model == i)
      n_raw_in_model[position] = position %>% result$raw[.] %>% unique %>% length
    }
  }
  #
  #   if (sum(!n_raw_in_coeff)){
  #     for (i in  (result$coeff[!n_raw_in_coeff] %>% unique)
  #     ){
  #       # i =  (result$coeff[!n_raw_in_coeff] %>% unique)[1]
  #       position = (result$coeff == i)
  #       n_raw_in_coeff[position] = position %>% result$raw[.] %>% unique %>% length
  #     }
  #   }

  return(cbind(result,n_raw_in_model))

  if(FALSE && TRUE){
    get_x_all(model = price~  I(carat^   2) + cut  + I(carat*table),data = ggplot2::diamonds)


    #________ irregular formulas
    model_dirty = lm(price~  I(carat^   2) + cut  - carat:table - cut ,ggplot2::diamonds)
    test = get_x_all(model_dirty)

    test
    test$coeff
    # ______  errors _______________

    tryCatch(get_x_all(model = price~  I(carat^   2) + cut  + I(carat*table)),
             error = function(x){
               print(x)
             })

  }

}





###  Model Functions  -------------------





#' identify missing rows for model/formula.
#'
#' @export
#' @details Data often contains missing values and \code{lm()} or \code{glm()} often skip those rows.
#' This function is to identify which rows that \code{lm()} or \code{glm()} skips.
#'
#' @param model  a formula or an output of lm or glm
#' @param data  the data.frame supposed to be used in modelling
#' @return a boolean vector with same length as the number of rows of data, with TRUE if a row has full data for the modelling and FALSE if not.
#' @examples
#' 
#' model = lm(price ~ carat, head(ggplot2::diamonds,1000))
#' data = head(ggplot2::diamonds,10)
#' 
#' # so observation 1, 4, 7 will be not valid rows
#' data[c(1,4,7),"price"] = NA
#' data
#' get_valid_rows(model,data)
#' 
#' # error message as no "price" is found in the data
#' data[,"price"] = NULL
#' tryCatch(get_valid_rows(model,data),
#'          error = function(x){
#'            print(x)
#'          })
#' 
get_valid_rows = function(model, # glm or lm or a formula
                          data # data.frame
){
  # from attach_load_first.R

  # this function will return the TRUE or FALSE for each observation in the dataset
  # that can be used in the model fit/prediction.

  ## identify missing rows for model/formula.
  ##
  ## @export
  ## @details Data often contains missing values and \code{lm()} or \code{glm()} often skip those rows.
  ## This function is to identify which rows that \code{lm()} or \code{glm()} skips.
  ##
  ## @param model  a formula or an output of lm or glm
  ## @param data  the data.frame supposed to be used in modelling
  ## @return a boolean vector with same length as the number of rows of data, with TRUE if a row has full data for the modelling and FALSE if not.


  vars = c(get_x(model,method = 'raw'), get_y(model,method = 'raw'))
  is_in_data = vars %in% colnames(data)
  if (sum(is_in_data)<length(is_in_data)){

    stop("variables below in the model are not in the data \n  ",
         vars[!is_in_data]
    )
  }


  return(data[,vars] %>% complete.cases)

  if (FALSE && TRUE){

    model = lm(price ~ carat, head(ggplot2::diamonds,1000))
    data = head(ggplot2::diamonds,10)

    # so observation 1, 4, 7 will be not valid rows
    data[c(1,4,7),"price"] = NA
    data
    get_valid_rows(model,data)

    # error message as no "price" is found in the data
    data[,"price"] = NULL
    tryCatch(get_valid_rows(model,data),
             error = function(x){
               print(x)
             })
  }
}



###### Advanced Functions ===============




#' focusing on selected variables in the model, and eliminating impacts from other variables.
#'
#' @export
#' @details In a model \code{y ~ a + b}. Sometimes you want to fix value of \code{a} and see the variations of \code{b} in \code{y}.
#' The most straightforward way to code this, as we did in this function, is to make \code{a}'s coefficients as 0, and then use the predict().
#'
#' @param model  an output of lm or glm
#' @param data  optional, a new dataset to evaluate the categorical variables.
#' If NULL, then use the data used in model itself.
#' @param focus_var_coeff  NULL or a character vector, choose coeff vars you want to focus. The unselected vars will have coeff values as 0.
#' Default is NULL, which means to choosing nothing.
#' @param focus_var_raw  NULL or a character vector, choose raw vars you want to focus. The unselected vars will have coeff values as 0.
#' Default is NULL, which means to choosing nothing.
#' @param intercept_include a boolean, whether to include the intercept (default is TRUE).
#' @return a new model with only focused vars having coeff unchanged, and all other vars having coeff as 0.
#' @examples
#' 
#' focus_var_raw  = 'carat'
#' 
#' model = lm(price~ cut + carat + I(carat^2) + I(carat^3) +
#'              I(carat  * depth) + depth,ggplot2::diamonds)
#' # all coeffs except carat's will be 0
#' focusing_var_coeff(model, focus_var_coeff = 'carat')
#' # all coeffs except cut.L's will be 0
#' focusing_var_coeff(model, focus_var_coeff = 'cut.L')
#' # all coeffs without raw vars cut or carat will be 0
#' focusing_var_coeff(model, focus_var_raw = c('cut','carat'))
#' 
#' # if you didn't specify anything, then all vars' coeff will become 0 except intercept
#' focusing_var_coeff(model)
#' 
#' 
#' # if cannot find the focus_var_coeff or focus_var_raw in the model
#' tryCatch(focusing_var_coeff(model, focus_var_coeff = 'caratdsd'),
#'          error = function(err) warning(err))
#' tryCatch(focusing_var_coeff(model, focus_var_raw = '3213'),
#'          error = function(err) warning(err))
#' 
focusing_var_coeff = function(model,
                              focus_var_coeff = NULL,
                              focus_var_raw = NULL,
                              intercept_include = TRUE,
                              data = NULL
){

  # an attach_Load_First.R function

  # make all the coefficients into 0 except the variables you want to focus
  # used to measure impacts of certain varia


  ## focusing on selected variables in the model, and eliminating impacts from other variables.
  ##
  ## @export

  ## @details In a model \code{y ~ a + b}. Sometimes you want to fix value of \code{a} and see the variations of \code{b} in \code{y}.
  ## The most straightforward way to code this, as we did in this function, is to make \code{a}'s coefficients as 0, and then use the predict().
  ##
  ## @param model  an output of lm or glm
  ## @param data  optional, a new dataset to evaluate the categorical variables.
  ## If NULL, then use the data used in model itself.
  ## @param focus_var_coeff  NULL or a character vector, choose coeff vars you want to focus. The unselected vars will have coeff values as 0.
  ## Default is NULL, which means to choosing nothing.
  ## @param focus_var_raw  NULL or a character vector, choose raw vars you want to focus. The unselected vars will have coeff values as 0.
  ## Default is NULL, which means to choosing nothing.
  ## @param intercept_include a boolean, whether to include the intercept (default is TRUE).
  ## @return a new model with only focused vars having coeff unchanged, and all other vars having coeff as 0.


  sanity_check(model,Class = c('lm','glm'))

  if ( !is.null(focus_var_coeff) && !is.null(focus_var_raw) ){
    stop("you have to provide the var names on which you want to focus.")
  }

  # make any coeff except focus_var_coeff as 0
  replacement = model$coefficients # model = Result
  names(replacement) = gsub(" ","",names(replacement))


  if ( !is.null(focus_var_coeff)){

    sanity_check(focus_var_coeff, exact_in_match = names(replacement) )

    focus_var_coeff = gsub(" ","",focus_var_coeff)
  } else if ( !is.null(focus_var_raw)){

    sanity_check(focus_var_raw, exact_in_match = get_x(model,method = "raw") )

    focus_var_raw = gsub(" ","",focus_var_raw)

    x_pair = get_x_all(model, data = data)
    x_pair = x_pair[x_pair$raw %in% focus_var_raw,]
    focus_var_coeff = unique(x_pair$coeff)
  }



  if (intercept_include) focus_var_coeff = c(focus_var_coeff,"(Intercept)")

  # focus_var_coeff = '(Intercept)'
  replacement[!names(replacement) %in% focus_var_coeff] = 0


  model$coefficients = replacement

  return(model)

  if (FALSE && TRUE){
    focus_var_raw  = 'carat'

    model = lm(price~ cut + carat + I(carat^2) + I(carat^3) +
                 I(carat  * depth) + depth,ggplot2::diamonds)
    # all coeffs except carat's will be 0
    focusing_var_coeff(model, focus_var_coeff = 'carat')
    # all coeffs except cut.L's will be 0
    focusing_var_coeff(model, focus_var_coeff = 'cut.L')
    # all coeffs without raw vars cut or carat will be 0
    focusing_var_coeff(model, focus_var_raw = c('cut','carat'))

    # if you didn't specify anything, then all vars' coeff will become 0 except intercept
    focusing_var_coeff(model)


    # if cannot find the focus_var_coeff or focus_var_raw in the model
    tryCatch(focusing_var_coeff(model, focus_var_coeff = 'caratdsd'),
             error = function(err) warning(err))
    tryCatch(focusing_var_coeff(model, focus_var_raw = '3213'),
             error = function(err) warning(err))

  }
}









#' evaluate the marginal effects of the selected raw variable on the dependent.
#'
#' @export
#' @details This function will evaluate marginal impacts and show the monotonicity of marginal impacts of
#' a selected variable on the dependent.
#'
#' Note that the marginal impacts is not simply the sign of coeff: In a model like \code{y~ x + x^2 + p + q},
#' marginal impacts of \code{x} on \code{y} requires an evaluation of both \code{x} and \code{x^2} at the same time.
#'
#' Here the \code{focus_var_raw} is \code{x}, \code{focus_var_coeff} are \code{x} and \code{x^2}
#' \code{nonfocus_value} is \code{p} and \code{q}
#'
#' Also the monotonicity of marginal impacts of \code{x} will be different for different range of \code{x}'s values.
#'
#' Another interesting case is when \code{x} is interacting with other variables, then its marginal impacts will also
#' be dependent on the values of those interacted variables.
#'
#' Level of marginal impacts: To make the level of marginal impacts of \code{x} realistic, by default we fixed all other right-hand-side variables
#' fixed at their mean (numeric) or mode (character or factor). You can also provide fixed values for them.
#' Also by default we let the interested variable (focused raw var) \code{x} to vary between its \code{seq(0.05,0.95,by = 0.05)} quantiles.
#'
#' This function will take care those cases above and make evaluating marginal impacts easier.
#'
#'
#' @param model  an output of lm or glm
#' @param data  NULL (default) or a data.frame, a new dataset to evaluate the categorical variables.
#' If NULL, then use the data used in model itself.
#' @param focus_var_raw  NULL or a character vector with maximum length of 2, in which you can choose \code{raw vars} you want to focus.
#' See \code{\link{get_x}} for the meaning of \code{raw var}.
#'
#' \itemize{
#' \item If there is only one raw var in the vector \code{focus_var_raw}, then we will check the marginal impact of that raw var.
#' \item If there is only two raw vars in the vector \code{focus_var_raw}, then we
#' will check the marginal impact of the FIRST raw var (\code{focus_var_raw[1]}) under different values of SECOND raw var (\code{focus_var_raw[2]}).
#' }
#'
#' See the example code for details.
#'
#' @param focus_var_coeff  NULL or a character vector. Must be \code{coeff vars} containing \code{focus_var_raw[1]}.
#' See \code{\link{get_x}} for the meaning of \code{coeff var}.
#' After you set up the \code{focus_var_raw}, you can also choose to focus on effects of \code{focus_var_raw[1]} through only certain coeff vars,
#' then all other unspecified coeff vars related \code{focus_var_raw[1]} will have coeff 0
#' by default, focus_var_coeff is null, which means we will check effect of \code{focus_var_raw[1]} on all coeff vars.
#'
#' See the example code for details.
#'
#' @param focus_var_model  NULL or a character vector. Must be model vars containing \code{focus_var_raw[1]}.
#' See \code{\link{get_x}} for the meaning of \code{model var}.
#' Similar use as argument \code{focus_var_coeff}, except here you can specify which model vars you want to focus.
#'
#' See the example code for details.
#'
#' @param focus_value  NULL or a list; each element of the list must have names in focus_var_raw.
#' By default, we will check marginal effects of \code{focus_var_raw[1]} through \code{seq(0.05,0.95,by = 0.05)} quantiles of its values in the modelling data.
#' But you can also specify the values you want to check here. See the sample code.
#'
#' @param nonfocus_value  NULL or a list; each element of the list must have names in non-focused raw vars (not show up in \code{focus_var_raw})
#' The meaning of non-focus var is: When we check the marginal effect of focus var on dependent, we let the focus var vary and fix the non-focus vars.
#' By default, for non-focused raw vars, we assume their values are fixed at mean (if numeric) or mode (if factor or character) in the modelling data.
#' But you can also specify the fixed values you want. See the sample code.
#'
#' @param transform_y  NULL or a function, used only for plot. Used as a function to recalculate y (a function on y (ex. log(y) )).
#' @param PRINT  a boolean, whether to print messages AND to plot.
#' @param PLOT a bookean, whether to plot
#' @param Reverse  a boolean,  whether to use reverse order in x-axis when plot. Default is FALSE.
#' @param bar_plot  NULL or a boolean, choose bar plot or line plot. If NULL, we will choose automatically.
#'
#' @param intolerance_on_wrong_names  a boolean. If a name is wrong, either in focus_var_raw, focus_var_model, focus_var_coeff,
#' focus_value or nonfocus_value, whether we delete the wrong names and go on (default), or report an error.
#' @return a list:
#'\itemize{
#'\item Focus_values: show the values of focus_var_raw we used to evaluate the marginal effects.
#'\item data_and_predict: full dataset used to evaluate the marginal effects.
#'\item summmary_glm: a summary of lm or glm model.
#'\item Monoton_Increase: whether the marginal impact is Monotonic Increase.
#'\item Monoton_Decrease: whether the marginal impact is Monotonic Decrease.
#' }
#' @examples
#' 
#' ##___ unit test ____
#' 
#' # __________________  One Dimension: the most basic case ____________________
#' 
#' 
#' 
#' set.seed(413)
#' traing_data = ggplot2::diamonds[runif(nrow(ggplot2::diamonds))<0.05,]
#' nrow(traing_data)
#' 
#' diamond_lm3 = lm(price~ cut + carat + I(carat^2) +
#'                    I(carat^3) + I(carat  * depth) + cut:depth, traing_data) # a GLM
#' 
#' # more carats, higher price.
#' effect(model = diamond_lm3,
#'        data = traing_data,
#'        focus_var_raw = c('carat'),
#'        Reverse = TRUE) # value in x-axis is reverse
#' 
#' # focus on only 'I(carat^3)', which means we will make all other coeff,
#' # including 'carat' and 'I(carat^2)' into 0
#' effect(model = diamond_lm3,
#'        data =traing_data,
#'        focus_var_raw =c('carat'),
#'        focus_var_coeff = 'I(carat^3)')
#' # __________________  One Dimension: Categorical ____________________
#' 
#' # selected model-var to focus: here not focus on cut:depth, only focus on cut
#' suppressWarnings(
#'   effect(model = diamond_lm3,
#'          data = traing_data,
#'          focus_var_raw = c('cut'),
#'          focus_var_model = 'cut'
#'          )
#'   )
#' 
#' # __________________  Double Dimensions ____________________
#' 
#' # here focus_var_raw has two values: "carat" and "cut"
#' # that means we will evaluate impact of "carat" on "price" through different value of "cut"
#' 
#' effect(model = diamond_lm3,data = traing_data, focus_var_raw=c('carat',"cut"))
#' 
#' # __________________  Provide Values to Focused vars  ____________________
#' 
#' # when evaluating impacts,
#' # we can provide the range of values for key variables
#' 
#' effect(model = diamond_lm3,data = traing_data,
#'        focus_var_raw = c('carat',"cut"),
#'        focus_value = list(carat=seq(0.5,6,0.1)))
#' 
effect = function( model,
                   data = NULL,
                   focus_var_raw , # must be the raw vars in the model
                   focus_var_coeff = NULL,   # must be the coeff vars in the model
                   focus_var_model = NULL,   # must be the model vars in the model

                   focus_value = NULL,
                   # a list, each element of the list must have names in focus_var_raw,
                   # and contains at least 2 values of the key coeff vars
                   # at least 2 values shall be provided, as we want to get the effects of it on the dependent
                   nonfocus_value = NULL,
                   # a list, each element of the list must have names in non focus_var_raw,
                   # and contain at most 1 values of the key coeff vars
                   # only one value can be provided, as we want to fix those non focus vars.
                   transform_y = NULL, # a function on y (ex. log(y) )
                   PRINT = TRUE,
                   PLOT = TRUE,
                   Reverse = FALSE, # when plot, whether to use reverse order in x-axis (ex. for balance_left)
                   bar_plot = NULL, # choose bar plot or line plot
                   intolerance_on_wrong_names = FALSE
){

  # an attach_Load_First.R function

  # Main usage:: check the effects of the key raw variable (key_focus), focus_var_raw[1], on the dependent.
  # If focus_var_raw[2] exists, then we call it non-key focus raw var,
  # then we will check the effects of the first raw var under different values of the second raw.

  # the function will also check if the dependent vars is monotonic under different values of the key_focus
  # if focus_var_raw[2] exists, then we will check if it is monotonic under different values of focus_var_raw[2]

  # you can also focus on effects of key_focus (focus_var_raw[1]) through only certain coeff vars.
  # you need specify those coeff vars, focus_var_coeff in the arguments. then all other coeff vars unspecified will have coeff 0
  # by default, focus_var_coeff is null, which means we will check effect of key_focus on all coeff vars.

  # same as focus_var_model

  # by default, effects of key_focus through its value seq(0.05,0.95,by = 0.05) quantities will be shown.
  # you can also provide values of all raws through argument focus_value (for focus_var_raw), and nonfocus_value for non-focus var
  # by default, for all non-key non_focus raw vars, we assume their values are fixed at mean (if numeric) or mode (if factor or character) .

  # what is "raw var" / "model var" and "coeff var"
  # in price ~ I(carat * depth) + I(carat>1), carat and depth are raw, but not model var,
  # "model vars" here are "I(carat * depth)" and "I(carat>1)"
  # "coeff vars" here are "I(carat * depth)" and "I(carat>1)TRUE"
  # "coeff vars" only exist after running the model
  # "raw vars" and "model vars" exist when formula is created.



  # preg = model =  glm(case ~ I(age>35) + spontaneous, data = infert,family = "binomial")
  # data = infert


  ## evaluate the marginal effects of the selected raw variable on the dependent.
  ##
  ## @export
  ## @details This function will evaluate marginal impacts and show the monotonicity of marginal impacts of
  ## a selected variable on the dependent.
  ##
  ## Note that the marginal impacts is not simply the sign of coeff: In a model like \code{y~ x + x^2 + p + q},
  ## marginal impacts of \code{x} on \code{y} requires an evaluation of both \code{x} and \code{x^2} at the same time.
  ##
  ## Here the \code{focus_var_raw} is \code{x}, \code{focus_var_coeff} are \code{x} and \code{x^2}
  ## \code{nonfocus_value} is \code{p} and \code{q}
  ##
  ## Also the monotonicity of marginal impacts of \code{x} will be different for different range of \code{x}'s values.
  ##
  ## Another interesting case is when \code{x} is interacting with other variables, then its marginal impacts will also
  ## be dependent on the values of those interacted variables.
  ##
  ## Level of marginal impacts: To make the level of marginal impacts of \code{x} realistic, by default we fixed all other right-hand-side variables
  ## fixed at their mean (numeric) or mode (character or factor). You can also provide fixed values for them.
  ## Also by default we let the interested variable (focused raw var) \code{x} to vary between its \code{seq(0.05,0.95,by = 0.05)} quantiles.
  ##
  ## This function will take care those cases above and make evaluating marginal impacts easier.
  ##
  ##
  ## @param model  an output of lm or glm
  ## @param data  NULL (default) or a data.frame, a new dataset to evaluate the categorical variables.
  ## If NULL, then use the data used in model itself.
  ## @param focus_var_raw  NULL or a character vector with maximum length of 2, in which you can choose \code{raw vars} you want to focus.
  ## See \code{\link{get_x}} for the meaning of \code{raw var}.
  ##
  ## \itemize{
  ## \item If there is only one raw var in the vector \code{focus_var_raw}, then we will check the marginal impact of that raw var.
  ## \item If there is only two raw vars in the vector \code{focus_var_raw}, then we
  ## will check the marginal impact of the FIRST raw var (\code{focus_var_raw[1]}) under different values of SECOND raw var (\code{focus_var_raw[2]}).
  ## }
  ##
  ## See the example code for details.
  ##
  ## @param focus_var_coeff  NULL or a character vector. Must be \code{coeff vars} containing \code{focus_var_raw[1]}.
  ## See \code{\link{get_x}} for the meaning of \code{coeff var}.

  ## After you set up the \code{focus_var_raw}, you can also choose to focus on effects of \code{focus_var_raw[1]} through only certain coeff vars,
  ## then all other unspecified coeff vars related \code{focus_var_raw[1]} will have coeff 0
  ## by default, focus_var_coeff is null, which means we will check effect of \code{focus_var_raw[1]} on all coeff vars.
  ##
  ## See the example code for details.
  ##
  ## @param focus_var_model  NULL or a character vector. Must be model vars containing \code{focus_var_raw[1]}.
  ## See \code{\link{get_x}} for the meaning of \code{model var}.
  ## Similar use as argument \code{focus_var_coeff}, except here you can specify which model vars you want to focus.
  ##
  ## See the example code for details.
  ##
  ## @param focus_value  NULL or a list; each element of the list must have names in focus_var_raw.
  ## By default, we will check marginal effects of \code{focus_var_raw[1]} through \code{seq(0.05,0.95,by = 0.05)} quantiles of its values in the modelling data.
  ## But you can also specify the values you want to check here. See the sample code.
  ##
  ## @param nonfocus_value  NULL or a list; each element of the list must have names in non-focused raw vars (not show up in \code{focus_var_raw})
  ## The meaning of non-focus var is: When we check the marginal effect of focus var on dependent, we let the focus var vary and fix the non-focus vars.
  ## By default, for non-focused raw vars, we assume their values are fixed at mean (if numeric) or mode (if factor or character) in the modelling data.
  ## But you can also specify the fixed values you want. See the sample code.
  ##
  ## @param transform_y  NULL or a function, used only for plot. Used as a function to recalculate y (a function on y (ex. log(y) )).
  ## @param PRINT  a boolean, whether to print messages AND to plot.
  ## @param PLOT a bookean, whether to plot
  ## @param Reverse  a boolean,  whether to use reverse order in x-axis when plot. Default is FALSE.
  ## @param bar_plot  NULL or a boolean, choose bar plot or line plot. If NULL, we will choose automatically.
  ##
  ## @param intolerance_on_wrong_names  a boolean. If a name is wrong, either in focus_var_raw, focus_var_model, focus_var_coeff,
  ## focus_value or nonfocus_value, whether we delete the wrong names and go on (default), or report an error.

  ## @return a list:
  ##\itemize{
  ##\item Focus_values: show the values of focus_var_raw we used to evaluate the marginal effects.
  ##\item data_and_predict: full dataset used to evaluate the marginal effects.
  ##\item summmary_glm: a summary of lm or glm model.
  ##\item Monoton_Increase: whether the marginal impact is Monotonic Increase.
  ##\item Monoton_Decrease: whether the marginal impact is Monotonic Decrease.
  ## }


  ### ------------------------   prepare

  if (is.null(data)) {
    data = get_data_from_lm(model)
    if (is.null(data)) stop('data is not provided and not found in model')
  }
  data = data.frame(data)

  # get_model_pair(model)
  x_pair = get_x_all(model, data = data)

  all_raw_var = x_pair$raw %>% unique
  all_coeff = x_pair$coeff %>% unique
  all_model = x_pair$model %>% unique

  # all the coeffs that contains the focus_raw
  all_focus_coeff = x_pair[x_pair$raw %in% focus_var_raw[1],]$coeff %>% unique
  # all the model that contains the focus_raw
  all_focus_model = x_pair[x_pair$raw %in% focus_var_raw[1],]$model %>% unique

  y = get_y(model,"coeff")
  names(model$coefficients) = gsub(" ","",names(model$coefficients)) # standardized the names


  ### ------------------------   check

  # check focus_var_raw
  if (intolerance_on_wrong_names) {
    sanity_check(focus_var_raw, exact_in_match = all_raw_var)
  } else {
    focus_var_raw =  check_names_delete(focus_var_raw, all_raw_var, STOP = FALSE, PRINT = PRINT,
                                        tobechecked_name = 'focus_var_raw',
                                        checking_name = 'all_raw_var',default_value = NULL)
    if (is.null(focus_var_raw)) stop('focus_var_raw is missing!')
  }
  # check focus_var_coeff

  if (!is.null(focus_var_coeff)){

    # is.vector(focus_var_coeff)
    if (intolerance_on_wrong_names) {
      sanity_check(focus_var_coeff,exact_in_match = all_focus_coeff)
    } else {
      focus_var_coeff =  check_names_delete(focus_var_coeff, all_coeff, STOP = FALSE,PRINT = PRINT,
                                            tobechecked_name = 'focus_var_coeff',
                                            checking_name = 'coeff vars',default_value = NULL)
    }
  }

  if (!is.null(focus_var_model)){
    # is.vector(focus_var_model)
    if (intolerance_on_wrong_names) {
      sanity_check(focus_var_model,exact_in_match = all_focus_coeff)
    } else {
      focus_var_model =  check_names_delete(focus_var_model, all_model, STOP = FALSE,PRINT = PRINT,
                                            tobechecked_name = 'focus_var_model',
                                            checking_name = 'model vars',default_value = NULL)
    }
  }





  if (sum(colnames(data) %in% all_raw_var) < length(all_raw_var)) stop("data provided is missing some raw vars for this regression")


  if (length(nonfocus_value)) {
    sanity_check(nonfocus_value, Class = 'list')

    if (intolerance_on_wrong_names) {
      sanity_check(names(nonfocus_value), exact_in_match = all_raw_var)
    } else {

      # nonfocus_value = list('cut' = 1, 'dwewsdfds' = 2); all_raw_var = 'cut'
      # nonfocus_value = list('cudasdst' = 1, 'dwewsdfds' = 2); all_raw_var = 'cut'

      nonfocus_value =  check_names_delete(nonfocus_value, all_raw_var, STOP = FALSE,PRINT = PRINT,
                         tobechecked_name = 'nonfocus_value',
                         checking_name = 'all_raw_var',default_value = NULL)
    }

    for (x in nonfocus_value){
      sanity_check(x, exact_length = 1, message_provided = "Only 1 value can be provided for each non-focus vars")
    }
  }

  if (length(focus_var_raw)>2) stop("You can only focus on at most two variables")

  if (length(focus_value)) {
    sanity_check(focus_value, Class = 'list')
    if (intolerance_on_wrong_names) {
      sanity_check(names(focus_value), exact_in_match = all_raw_var )
    } else {
      focus_value =  check_names_delete(focus_value, all_raw_var, STOP = FALSE,PRINT = PRINT,
                                           tobechecked_name = 'focus_value',
                                           checking_name = 'all_raw_var',default_value = NULL)
    }

    for (x in focus_value){
      sanity_check(x,min_oberserv = 2,
                   message_provided = 'The provided values for each focus raw var shall at least have to different values to enable monoton comparison')
    }
  }

  if (length(transform_y)) sanity_check(transform_y, Class = 'function')


  ### ------------------------    values for non-focus variables

  #   llply(ggplot2::diamonds,function(x){
  #      print(paste( class(x),collapse=' '))
  #   })

  ##~~~~~~~~   get the mean or mode for all raw vars : used for prediction

  all_raw_values = list()
  for ( each_raw_var in all_raw_var) {
    x = data[,each_raw_var]
    Class = paste( class(x),collapse=' ')

    if ( Class %in% c("numeric","integer")){
      all_raw_values[[each_raw_var]] = mean(x)
    } else {
      # for factor or character, we assume Mode
      all_raw_values[[each_raw_var]] = Mode(x)
    }
  }

  # if you provide the values to the non-focus vars, then replace the mean/mode by the provided ones.
  if (length(nonfocus_value)){
    for( x in names(nonfocus_value)){
      all_raw_values[[x]] = nonfocus_value[[x]]
    }
  }

  ### get the valueS for focus raw vars
  # if numeric:
  # get seq(0.015,0.95,0.3 ) quantile values for the non-key focus vars
  # get seq(0.015,0.95,0.05 ) quantile values for the key focus vars
  # if not : like factor
  # get the unique values

  is_factor_key  = c(1,1)
  names(is_factor_key) = focus_var_raw
  i=1

  for( x in focus_var_raw){
    # x = focus_var_raw[1]

    # to see whether focus_vars are factors/characters
    is_factor_key[x] = sum(c("factor","character") %in% class(all_raw_values[[x]] ))

    if (x %in% names(focus_value)) {
      # if values are provided for focus variables
      all_raw_values[[x]] = focus_value[[x]]

    } else if ( # if not provided, and x is a factor/category
      is_factor_key[x]
    ){
      # for factors and characters, just get unique values
      all_raw_values[[x]] =  focus_value[[x]] = unique(data[,x])

    } else if ( i== 2 & is_factor_key[1]) { # for numerics
      # if the first focus var is a factor/character, and second focus var is a numeric,
      # then we just don't need a very detailed quantile for the second one
      # i = 2
      all_raw_values[[x]] =  focus_value[[x]] =   unique(quantile(data[,x],seq(0.015,0.95,0.3 )))
    } else {
      all_raw_values[[x]] =  focus_value[[x]] =   unique(quantile(data[,x],seq(0.015,0.95,0.05 )))
    }
    i = i + 1
  }



  # ________ prepare data for predict() _____________

  # this will keep the class
  modeled_data = expand_grid(all_raw_values, stringsAsFactors = FALSE)
  # modeled_data[,1] %>% class
  model_use = model
  corresponding_coeff = x_pair[x_pair$raw %in% focus_var_raw[1],"coeff"] %>% unique

  # if you only focus the effects of certain coeff vars,
  # then assign all other focus_raw related coeff vars to 0

  if (!is.null(focus_var_coeff)){

    corresponding_coeff = focus_var_coeff

    coeff_to_delete = all_focus_coeff[!all_focus_coeff %in% focus_var_coeff]
    coeff_to_include = all_coeff[!all_coeff %in% coeff_to_delete]
    model_use = focusing_var_coeff(model,coeff_to_include)
  }

  # if you only focus the effects of certain coeff vars,
  # then assign all other focus_raw related coeff vars to 0
  if (!is.null(focus_var_model)){
    corresponding_coeff = x_pair[x_pair$model %in% focus_var_model,"coeff"] %>% unique

    sanity_check(focus_var_model,exact_in_match = focus_var_model)

    model_to_delete = all_focus_model[!all_focus_model %in% focus_var_model]

    coeff_to_delete = x_pair[x_pair$model %in% model_to_delete,"coeff"] %>% unique
    coeff_to_include = x_pair[!x_pair$coeff %in% coeff_to_delete,"coeff"] %>% unique

    model_use = focusing_var_coeff(model_use,focus_var_coeff = coeff_to_include)
  }

  if (!is.null(focus_var_coeff) && !is.null(focus_var_model)){
    corresponding_coeff = x_pair[x_pair$model %in% focus_var_model,"coeff"] %>% unique
    if (length(union(corresponding_coeff,focus_var_coeff)) == 0) stop("focus_var_coeff and focus_var_model have no common variables.")
  }

  # ------------------- Prediction ~~~~~~~~~~~~~~~~~~~~~~~~```


  # if formula is dirty, then the predict will now work
  if (
    sum(
      (get_x_hidden(model_use) %in% colnames(modeled_data))==0
    )
  ){
    model_use = glm(paste_formula(model_use,clean = TRUE) %>% as.formula, data = data, family = family(model_use))
  }

  predicted =  data.frame(predict = predict(model_use,newdata = modeled_data,type='response'),
                          modeled_data,
                          stringsAsFactors = FALSE)

  ###_____  check the monotonicity ________

  # when there is only one focus var: the key
  if (length(focus_var_raw) ==1 ) {
    predicted = predicted[order(predicted[,focus_var_raw[1]]),]
    monoton_increase = is_increase(predicted$predict)
    monoton_decrease = is_decrease(predicted$predict)
  }

  # when there are focus vars: the key and non-key, we check the monotonic effect under each value of the non-key
  if (length(focus_var_raw) ==2 ) {

    predicted = predicted[
      order(
        predicted[,focus_var_raw[2]],predicted[,focus_var_raw[1]]
      ),]

    unique_key_focus = unique(predicted[,focus_var_raw[2]])

    monoton_increase = laply(unique_key_focus, function(x){
      is_increase(predicted[predicted[,focus_var_raw[2]] ==x, ]$predict)
    })

    monoton_decrease = laply(unique_key_focus, function(x){
      is_decrease(predicted[predicted[,focus_var_raw[2]] ==x, ]$predict)
    })

    names(monoton_decrease) = unique_key_focus
    names(monoton_increase) = unique_key_focus

  }


  # ------------------- For Plot ~~~~~~~~~~~~~~~~~~~~~~~~```

  plot_data = predicted

  # whether the target variable needs some transform function?
  if (!is.null(transform_y)){
    plot_data$predict =  predicted$predict = transform_y(plot_data$predict)
  }

  # initialize the graph
  graph = NULL

  # if the key focus var only has at most 10 unique values, then transfer it to factor when plot
  if (PRINT & PLOT){
    x_title = paste(corresponding_coeff,collapse = '+')
    graph_title = paste('marginal impacts of ', focus_var_raw[1], ' on ', y, sep='')

    Length_Unique = plot_data[,focus_var_raw[1]] %>% unique %>% length

    # if bar_plot is not provided
    if (is.null(bar_plot)){
      if  ("numeric" %in% class(plot_data[,focus_var_raw[1]])){
        bar_plot = FALSE
      } else {
        bar_plot = TRUE
      }
    }

    # if bar_plot is TRUE and suitable, then transform the key var into factor
    if (bar_plot) {
      plot_data[,focus_var_raw[1]] = as.factor(plot_data[,focus_var_raw[1]])
      is_factor_key[1] = 1
    }

    # plot according to number of focus variables
    if (length(focus_var_raw) ==1 ) {

      if (bar_plot && is_factor_key[1]>0){
        # if it is a character, then use bar to plot
        graph =
          ggplot(plot_data) + geom_bar(aes_string(x=focus_var_raw[1], y = 'predict'),stat = "identity")
      } else {
        graph =
          ggplot(plot_data) + geom_line(aes_string(x=focus_var_raw[1], y = 'predict'))
      }
    }
    #

    if (length(focus_var_raw)==2) {

      Class_col = paste( class(plot_data[,focus_var_raw[2]]),collapse=' ')

      if (!is_factor_key[2]) { # transfer the secondary key into factor
        plot_data[,focus_var_raw[2]] = as.factor(plot_data[,focus_var_raw[2]])
      }

      if (bar_plot && is_factor_key[1] ){
        graph = ggplot(plot_data) + geom_bar(aes_string(x=focus_var_raw[1],
                                                        fill = focus_var_raw[2],
                                                        y = 'predict'),
                                             stat = "identity")

      } else {
        graph = ggplot(plot_data) +
          geom_line(aes_string(x=focus_var_raw[1], colour = focus_var_raw[2], y = 'predict'))
      }

    }

    # if logit or probit, then y shall be percentage
    if (
      !("lm" %in% class(model)) &&
      model$family$link %in% c("logit","probit")
    ) {
      graph = graph + scale_y_continuous(labels = percent)
    }

    graph = graph + labs(y=y, x = x_title, title = graph_title)


    if (Reverse && is_factor_key[1] == 0) {graph = graph + scale_x_reverse()} # factor cannot use reverse

    print(graph)
  }



  Coeff_table = data.frame(Var = names(model_use$coefficients) ,
                           coeff_value = model_use$coefficients,
                           stringsAsFactors = FALSE)
  rownames(Coeff_table) = NULL

  return(list(
    Focus_values = focus_value,
    data_and_predict = predicted,
    summmary_glm = Coeff_table,
    Monoton_Increase = monoton_increase,
    Monoton_Decrease = monoton_decrease  ))


  if (FALSE && TRUE) {
    ##___ unit test ____

    # __________________  One Dimension: the most basic case ____________________



    set.seed(413)
    traing_data = ggplot2::diamonds[runif(nrow(ggplot2::diamonds))<0.05,]
    nrow(traing_data)

    diamond_lm3 = lm(price~ cut + carat + I(carat^2) +
                       I(carat^3) + I(carat  * depth) + cut:depth, traing_data) # a GLM

    # more carats, higher price.
    effect(model = diamond_lm3,
           data = traing_data,
           focus_var_raw = c('carat'),
           Reverse = TRUE) # value in x-axis is reverse

    # focus on only 'I(carat^3)', which means we will make all other coeff,
    # including 'carat' and 'I(carat^2)' into 0
    effect(model = diamond_lm3,
           data =traing_data,
           focus_var_raw =c('carat'),
           focus_var_coeff = 'I(carat^3)')
    # __________________  One Dimension: Categorical ____________________

    # selected model-var to focus: here not focus on cut:depth, only focus on cut
    suppressWarnings(
      effect(model = diamond_lm3,
             data = traing_data,
             focus_var_raw = c('cut'),
             focus_var_model = 'cut'
             )
      )

    # __________________  Double Dimensions ____________________

    # here focus_var_raw has two values: "carat" and "cut"
    # that means we will evaluate impact of "carat" on "price" through different value of "cut"

    effect(model = diamond_lm3,data = traing_data, focus_var_raw=c('carat',"cut"))

    # __________________  Provide Values to Focused vars  ____________________

    # when evaluating impacts,
    # we can provide the range of values for key variables

    effect(model = diamond_lm3,data = traing_data,
           focus_var_raw = c('carat',"cut"),
           focus_value = list(carat=seq(0.5,6,0.1)))

  }
}



#' check monotonicity of marginal impacts and re-estimate the model.
#' @description check monotonicity of marginal impacts and re-estimate the model (optional) until we get correct marginal impacts.
#' @export
#' @details This function first calls function \code{\link{effects}}
#' and then checks the monotonicity of marginal impacts. If the direction of marginal impacts are incorrect,
#' it can delete a model var that potentially causes the wrong marginal impacts and then re-estimate the model.
#' We will keep doing this until the correct marginal impacts are found
#'
#' Details of evaluating the marginal impacts \code{\link{effects}}
#'
#' @param model, an output of lm or glm
#' @param focus_var_raw  see \code{\link{effects}}.
#' @param focus_var_model  see \code{\link{effects}}.
#'
#'
#' @param PRINT  a boolean, whether to print messages and to plot.
#' @param PLOT  a boolean, whether to plot.
#' @param Monoton_to_Match  1 or -1. 1 means you want monotonic increasing as the correct marginal effect, -1 means negative
#' @param re_estimate  a boolean with default as TRUE. This is to decide
#' if the marginal impacts are found to be incorrect, then whether to delete a model var that
#' potentially cause the wrong marginal impacts and re-estimate the model
#' @param data  optional, a new dataset to show the marginal impacts and re-estimate the model.
#' If NULL, then use the data used in model itself.
#'
#' @param STOP  a boolean. When find a model with incorrect marginal impacts, whether to stop there and wait to continue (call the \code{\link{Enter_to_Continue}})
#' @param family  family of glm, for example, can be gaussian \code{"(link = 'identity')"}  or \code{"(link = 'logit')"}.
#' If NULL, we will use the default family of the model
#' @param ...  additional arguments going to \code{\link{effect}}
#'
#' @return a model (\code{lm} or \code{glm}).
#' \itemize{
#' \item If re_estimate == TRUE, then return will be an re-estimated model with correct marginal impacts given we can find one.
#' \item If re_estimate == FALSE, original model will be returned.
#'}
#' @examples
#' 
#' ##
#' set.seed(413)
#' traing_data = ggplot2::diamonds[runif(nrow(ggplot2::diamonds))<0.05,]
#' nrow(traing_data)
#' 
#' diamond_lm3 = lm(formula = price ~ carat + I(carat^2) + I(carat^3) + cut +
#'                    I(carat * depth) , data = traing_data)
#' 
#' 
#' test = deleting_wrongeffect(model = diamond_lm3,
#'                             focus_var_raw = 'carat',
#'                             focus_var_model = c("I(carat^3)","I(carat*depth)",
#'                                                 "I(carat^2)","I(carat)"),
#'                             focus_value = list(carat=seq(0.5,6,0.1)),
#'                             data = traing_data,
#'                             PRINT = TRUE,STOP = FALSE,
#'                             Reverse = FALSE)
#' 
#' 
#' ## two focus on vars
#' test =
#'   deleting_wrongeffect(model = diamond_lm3 ,
#'                        focus_var_raw = c('carat',"cut"),
#'                        focus_var_model = c("I(carat*depth)","I(carat^3)"),
#'                        focus_value = list(carat=seq(0.5,6,0.1)),
#'                        data = traing_data,PRINT = TRUE,STOP =FALSE)
#' 
#' diamond_lm3 = lm(formula = price ~ cut + depth +
#'                    I(carat * depth) , data = ggplot2::diamonds)
#' ##  negative signs
#' deleting_wrongeffect(model = diamond_lm3 ,
#'                      focus_var_raw = c('depth',"cut"),
#'                      focus_var_model = c("depth"),Monoton_to_Match = -1,
#'                      data = ggplot2::diamonds,PRINT = TRUE,STOP =FALSE)
#' 
#' ## wrong variables names
#' deleting_wrongeffect(diamond_lm3, focus_var_raw = 'carat',
#'                      focus_var_model = c("I(cara79t^3)"),
#'                      data = ggplot2::diamonds,PRINT = TRUE)
#' 
#' deleting_wrongeffect(diamond_lm3, focus_var_raw = 'carat890',
#'                      focus_var_model = c("I(carat^3)"),
#'                      data = ggplot2::diamonds, PRINT = TRUE)
#' 
deleting_wrongeffect = function (model , # a GLM model
                                 focus_var_raw = NULL,  # see Effect()
                                 focus_var_model= NULL,   # see Effect()
                                 Monoton_to_Match = 1,  # 1 means you want monotonic increasing marginal effect, -1 means negative
                                 # the correct signs
                                 family = NULL,
                                 re_estimate = TRUE, # if find the wrong marginal effect, whether to delete the wrong coeff and re-estimate the model
                                 data,
                                 STOP = FALSE, # stop at key checking point
                                 PRINT = TRUE,
                                 PLOT = TRUE,
                                 ...){

  # this function will check whether the marginal effect of certain raw var in a model is correct.
  # You have to read the function effect() to understand how to get the marginal effect

  # note that a raw variable. for example "carat" will wield its impact
  # through its corresponding coeff variables, like  c("I(carat^4)","I(carat^3)") etc

  # if re_estimate == TRUE, then we will
  # 1. drop the first coeff from focus_var_model
  # 2. revaluate the model, then check the marginal effect
  # repeat the two steps above until we get correct marginal effect or we delete all coeffs in focus_var_model.


  # model = diamond_lm
  # focus_var_raw = "carat"
  # focus_var_model =  c("I(carat^4)","I(carat^3)")


  ## check monotonicity of marginal impacts and re-estimate the model.
  ## @description check monotonicity of marginal impacts and re-estimate the model (optional) until we get correct marginal impacts.
  ## @export
  ## @details This function first calls function \code{\link{effects}}
  ## and then checks the monotonicity of marginal impacts. If the direction of marginal impacts are incorrect,
  ## it can delete a model var that potentially causes the wrong marginal impacts and then re-estimate the model.
  ## We will keep doing this until the correct marginal impacts are found
  ##
  ## Details of evaluating the marginal impacts \code{\link{effects}}
  ##
  ## @param model, an output of lm or glm

  ## @param focus_var_raw  see \code{\link{effects}}.
  ## @param focus_var_model  see \code{\link{effects}}.
  ##
  ##
  ## @param PRINT  a boolean, whether to print messages and to plot.
  ## @param PLOT  a boolean, whether to plot.
  ## @param Monoton_to_Match  1 or -1. 1 means you want monotonic increasing as the correct marginal effect, -1 means negative
  ## @param re_estimate  a boolean with default as TRUE. This is to decide
  ## if the marginal impacts are found to be incorrect, then whether to delete a model var that
  ## potentially cause the wrong marginal impacts and re-estimate the model

  ## @param data  optional, a new dataset to show the marginal impacts and re-estimate the model.
  ## If NULL, then use the data used in model itself.
  ##
  ## @param STOP  a boolean. When find a model with incorrect marginal impacts, whether to stop there and wait to continue (call the \code{\link{Enter_to_Continue}})
  ## @param family  family of glm, for example, can be gaussian \code{"(link = 'identity')"}  or \code{"(link = 'logit')"}.
  ## If NULL, we will use the default family of the model
  ## @param ...  additional arguments going to \code{\link{effect}}
  ##
  ## @return a model (\code{lm} or \code{glm}).
  ## \itemize{
  ## \item If re_estimate == TRUE, then return will be an re-estimated model with correct marginal impacts given we can find one.
  ## \item If re_estimate == FALSE, original model will be returned.
  ##}


  ### __________________  initialize __________________
  if (is.null(family)) family = family(model)
  if (is.null(data)) {
    data = get_data_from_lm(data)
    if (is.null(data)) stop ('data is not provided and cannot be found in the model')
  }
  data0 = data.frame(data)

  # for raw vars
  all_var_raw = get_x(model) # model= Result
  in_raw = focus_var_raw %in% all_var_raw

  if (FALSE %in% in_raw) {
    if (PRINT) cat("raw_var ", paste( focus_var_raw[!in_raw], collapse = ', '),", cannot be found in the model, so skip the check for it. Nothing changed. \n\n")
    return(model)
  }

  # for coeff vars
  if (is.null(focus_var_model)){
    # if focus_var_model NOT Provided,
    # then get all coeff-var that contains the raw-var as the focus_var_model

    x.all = get_x_all(model)
    focus_var_model = x.all[(x.all$raw == focus_var_raw),'model'] %>% unique

  } else {

    all_var_model = get_x(model,method = "model")
    focus_var_model = gsub(" ","",focus_var_model)
    focus_var_model = unique(focus_var_model)

    in_model = focus_var_model %in% all_var_model


    # check whether focus_var_raw and print_focus_car_coeff exist in the model
    if (length(focus_var_raw) ==0 ){
      cat("The updated focus_var_raw cannot be found in the model. Nothing changed\n")
      return(model)

    } else if (FALSE %in% in_model) {
      print_focus_car_coeff = focus_var_model[!in_model]
      print_focus_car_coeff = print_focus_car_coeff[!is.na(print_focus_car_coeff)]
      cat("\ncoeff var: ", paste(print_focus_car_coeff ,collapse = ', '),
              " cannot be found in the model, So not check them \n")

    }
    focus_var_model = focus_var_model[in_model]
  }
  if (length(focus_var_model)==0){
    cat('\n nothing has been checked; original model is returned.')
    return(model)
  }

  ### __________________  each time just delete one variable _________________

  control_var = 1
  focus_var_model_0 = rev(focus_var_model)

  if (re_estimate) {
    max_steps = length(focus_var_model_0)
    # stop if find the correct effect or run out of the focus_var_model_0
  } else {
    max_steps = 1 # if not re_estimate, then stop after the first step
  }

  while (control_var <= max_steps ){

    # Result$family

    var_tobe_checked = focus_var_model_0[control_var] # this is the model_var you prepare to delete if the marginal impact is wrong.


    if (PRINT)  {

      if (control_var==1) {
        cat("\ninitial model: \n")
        print(summary(model)$coefficient[,c(1,4)])
      }

      cat('\n\n')
      cat("check raw var: ",focus_var_raw[1],'\ncheck model var: ', paste(focus_var_model,collapse = ", "),'\n')
      if (Monoton_to_Match == 1) {
        Monotonicity = 'Increasing'
      } else {
        Monotonicity = 'Decreasing'
      }
      cat("Correct Monotonicity is supposed to be: ",Monotonicity,'\n')
    }

    effect_result = effect(model = model,
                           data = data0,
                           focus_var_raw = focus_var_raw,
                           focus_var_model = focus_var_model,  # be updated each time
                           PRINT = PRINT, PLOT = PLOT,
                           ...)
#
#
#     effect_result = effect(model = model,
#                            data = data0,
#                            focus_var_raw = focus_var_raw,
#                            focus_var_model = focus_var_model,  # be updated each time
#
#                            PRINT = PRINT,
#                            PLOT = PLOT,
#
#                            focus_var_coeff = focus_var_coeff,
#                            focus_value = focus_value,
#                            nonfocus_value = nonfocus_value,
#
#                            transform_y = transform_y,
#                            bar_plot = bar_plot
#                            )
#
#     sanity_check(focus_var_coeff, exact_in_match = 'carat')

    # if non-key var is provided,
    # then we will check the marginal effects of the key-var under each non-key var
    # thus we need to use all() to insure all values of Monoton_Increase/Monoton_Decrease ==1

    if (   (Monoton_to_Match == 1 & all(effect_result$Monoton_Increase==1) ) |
           (Monoton_to_Match == -1 & all(effect_result$Monoton_Decrease==1) )
    ){
      if (PRINT) cat("Monotonicity is correct \n")
      control_var = length(focus_var_model_0) + 100 # stop
      correct_effect_ind = 1
    } else {
      if (PRINT) cat("Monotonicity is incorrect \n")
      correct_effect_ind = 0
    }
    ## --------------------------  Revaluate -----------------------

    if ( correct_effect_ind ==0 & re_estimate){
      if (PRINT) cat("Variable ",  var_tobe_checked, " shall be deleted, and the model shall be re-estimated. \n")
      if (PRINT) cat("-------------------------------------------------------\n")

      model_var = get_x(model,"model")
      model_var = model_var[var_tobe_checked!=model_var]

      if (length(model_var)==0) model_var = "1" # intercept only

      Formula_new = paste( paste(get_y(model,"model"),"~"),
                           paste(model_var,collapse = '+')
                           # be careful with multiple variables to be deleted,
                           # so you have to collapse it
      ) %>% as.formula

      model = glm(Formula_new,data = data0,family = family)


      ##________  update
      focus_var_model_old = focus_var_model
      focus_var_model = focus_var_model[!(focus_var_model %in% var_tobe_checked)]
      control_var  = control_var + 1 # must do this after updating focus_var_model

      ## update focus_var_model each time, after you delete one variable
      # if the sign is correct or the variables_to_check cannot be found in the model, then just break the loop

      if ( length(focus_var_model)==0 & correct_effect_ind==0) {
        control_var = length(focus_var_model_0) + 100 # stop
        if (re_estimate) if (PRINT) cat("\nAll model vars with wrong sign have been deleted, nothing to check now. \n")
      }
      if  (STOP & correct_effect_ind == 0) {
        Enter_to_Continue()
      }

      if (PRINT) {
        cat("\nNew Model: \n")
        Coeffs = summary(model)$coefficient[,c(1,4)] %>% data.frame(.,stringsAsFactors = FALSE)
        row.names(Coeffs) = gsub(" ","",row.names(Coeffs))

        # identify the checked variables
        Coeffs[,'checked']  = row.names(Coeffs) %in% focus_var_model_old

        print(summary(model)$coefficient[,c(1,4)])
      }

    } else {
      control_var = length(focus_var_model_0) + 100 # stop
    }
  }


  model$correct_effect_ind = correct_effect_ind



  return(model)


  if (FALSE && TRUE){

    ##
    set.seed(413)
    traing_data = ggplot2::diamonds[runif(nrow(ggplot2::diamonds))<0.05,]
    nrow(traing_data)

    diamond_lm3 = lm(formula = price ~ carat + I(carat^2) + I(carat^3) + cut +
                       I(carat * depth) , data = traing_data)


    test = deleting_wrongeffect(model = diamond_lm3,
                                focus_var_raw = 'carat',
                                focus_var_model = c("I(carat^3)","I(carat*depth)",
                                                    "I(carat^2)","I(carat)"),
                                focus_value = list(carat=seq(0.5,6,0.1)),
                                data = traing_data,
                                PRINT = TRUE,STOP = FALSE,
                                Reverse = FALSE)


    ## two focus on vars
    test =
      deleting_wrongeffect(model = diamond_lm3 ,
                           focus_var_raw = c('carat',"cut"),
                           focus_var_model = c("I(carat*depth)","I(carat^3)"),
                           focus_value = list(carat=seq(0.5,6,0.1)),
                           data = traing_data,PRINT = TRUE,STOP =FALSE)

    diamond_lm3 = lm(formula = price ~ cut + depth +
                       I(carat * depth) , data = ggplot2::diamonds)
    ##  negative signs
    deleting_wrongeffect(model = diamond_lm3 ,
                         focus_var_raw = c('depth',"cut"),
                         focus_var_model = c("depth"),Monoton_to_Match = -1,
                         data = ggplot2::diamonds,PRINT = TRUE,STOP =FALSE)

    ## wrong variables names
    deleting_wrongeffect(diamond_lm3, focus_var_raw = 'carat',
                         focus_var_model = c("I(cara79t^3)"),
                         data = ggplot2::diamonds,PRINT = TRUE)

    deleting_wrongeffect(diamond_lm3, focus_var_raw = 'carat890',
                         focus_var_model = c("I(carat^3)"),
                         data = ggplot2::diamonds, PRINT = TRUE)

  }
}






#' same as \code{step()} in R, but able to check marginal effects.
#'
#' @export
#' @details For each step of regression, you can first choose the models with correct marginal effect
#' and then choose the one with highest AIC/BIC within them
#'
#' @param model an output of \code{lm} or \code{glm}
#' @param scope,trace,steps,k   see \code{step()}
#' @param family  used as the argument for \code{family} of \code{glm}, default is NULL, which means we will use the family imbedded in the model.
#' @param data  a data.frame used in regression.
#' @param IC_method  either 'AIC' or 'BIC', will overwrite the \code{k} argument above.
#' @param STOP  whether stop and wait your response for each step.
#' @param test_suit  used to specify the correct marginal effect you want to check.
#' It is a list with names as raw variable and values as arguments of the function \code{deleting_wrongeffect}
#' If NULL (default), then not check any marginal effect
#' See example code for details.
#' @return a stepwise-selected model. If \code{test_suit} is specified, then the returned model is the one with highest AIC/BIC within those that get
#' correct marginal impact.
#'
#' The silde effect is to print a data.frame containing diagnostic informations for each step. The 'correct_effect_ind' column is a boolean vector to show
#' whether the model has correct marginal effect or not.
#'
#'
#' @examples
#' 
#' # starting model:
#' # can have a dirty formula like below
#' 
#' set.seed(413)
#' traing_data = ggplot2::diamonds[runif(nrow(ggplot2::diamonds))<0.05,]
#' nrow(traing_data)
#' 
#' diamond_lm3 = lm(formula = price ~ cut + carat - cut   , data = traing_data)
#' 
#' scope = list(lower = price ~ 1,
#'              upper = price ~  I(carat^2) + I(carat^3) + I(carat * depth) + depth + carat)
#' 
#' # traditional stepwise regression with no marginal effect check
#' model1 = stepwise2(model = diamond_lm3, scope = scope,k = 2,
#'                    trace = TRUE, data = traing_data, STOP = TRUE)
#' model1
#' # result is exactly same using the default step() function.
#' model2 = suppressWarnings(step(diamond_lm3,scope = scope, k = 2))
#' model2
#' 
#' 
#' #__ How to Specify the Correct Marginal Effects in Stepwise Regression  __
#' 
#' # this test_suit means we will check the marginal effect of both 'carat' and 'depth'
#' # for 'carat', we will only focus on 4 coeff vars :
#'     # "I(carat^3)","I(carat*depth)","I(carat^2)","carat"
#' # for 'depth', as we do not specify any particular coeff vars there,
#' # we will check all coeff var related to 'depth'
#' 
#' test_suit = list(
#'   carat = list(
#'     # the list name must be the raw var
#'     focus_var_raw = "carat",
#'     # must specify the focus_var_raw (see deleting_wrongeffect() ) as the raw var
#'     focus_var_coeff = c("I(carat^3)","I(carat*depth)",
#'                         "I(carat^2)","carat") ,
#'     # optional # If not defined, then we to check all coeffs related to the raw var
#'     focus_value =list(carat = seq(0.5,6,0.1)),
#'     Monoton_to_Match = 1 # optional. Default is 1
#'   ),
#'   depth = list(
#'     focus_var_raw = "depth",
#'     Monoton_to_Match = 1
#'   )
#' )
#' 
#' model3 =  stepwise2(model = diamond_lm3, scope = scope, trace = TRUE,
#'                     data = traing_data,
#'                     STOP = FALSE, test_suit = test_suit)
#' 
#' # see the difference from model1
#' effect(model3,focus_var_raw =  "carat", focus_value =list(carat = seq(0.5,6,0.1)))
#' 
stepwise2 =function (model, # can be an glm/lm/formula,
                     scope,
                     trace = 1,
                     steps = 1000,
                     k = 2,
                     data,
                     family = NULL, # argument for familiy for glm
                     IC_method = c("AIC","BIC"),
                     test_suit = NULL, # see delete_wrongsign()
                     STOP = FALSE # see delete_wrongsign()
)
{

  ## same as \code{step()} in R, but able to check marginal effects.
  ##
  ## @export
  ## @details For each step of regression, you can first choose the models with correct marginal effect
  ## and then choose the one with highest AIC/BIC within them
  ##
  ## @param model an output of \code{lm} or \code{glm}
  ## @param scope,trace,steps,k   see \code{step()}
  ## @param family  used as the argument for \code{family} of \code{glm}, default is NULL, which means we will use the family imbedded in the model.
  ## @param data  a data.frame used in regression.
  ## @param IC_method  either 'AIC' or 'BIC', will overwrite the \code{k} argument above.
  ## @param STOP  whether stop and wait your response for each step.
  ## @param test_suit  used to specify the correct marginal effect you want to check.
  ## It is a list with names as raw variable and values as arguments of the function \code{deleting_wrongeffect}
  ## If NULL (default), then not check any marginal effect
  ## See example code for details.

  ## @return a stepwise-selected model. If \code{test_suit} is specified, then the returned model is the one with highest AIC/BIC within those that get
  ## correct marginal impact.
  ##
  ## The silde effect is to print a data.frame containing diagnostic informations for each step. The 'correct_effect_ind' column is a boolean vector to show
  ## whether the model has correct marginal effect or not.
  ##
  ##

  data = data.frame(data)
  if (is.null(family)) family = family(model)


  IC_method = match.arg(IC_method)
  if (length(IC_method)>1) IC_method = IC_method[1]
  # IC_method = "BIC"

  if (IC_method == "BIC") {
    if (k==2 & trace)  cat("k is overwritten as log(N)")
    k = log(nrow(data))
  }

  y = get_y(model, method = "model")
  x_lower = get_x(scope$lower, data = data, method = "model")
  # x_start has vars as union of the lower and start model
  x_start = get_x(model = model, data = data, method = "model") %>% union(.,x_lower)
  # x_upper has vars as union of the lower, start nad upper  model
  x_upper = get_x(scope$upper, data = data, method = "model") %>% union(.,x_start)

  step_count = 1
  x_best = NULL
  x_best_row = NULL
  x_skip = NULL
  best_model = model

  while (step_count<steps){

    list_upper = setdiff(x_upper,x_start) # vars to add
    list_lower = setdiff(x_start,x_lower) # vars to delete

    # x_delta: ~~~~~~~~~~~ denotations of the different models
    # if add a var, then denote it as "+ 'var'", otherwise as "- 'var'"
    # "---" is to denote the starting model in that step

    x_delta = c("origin",paste("+",list_upper), paste("-",list_lower))
    x_delta = x_delta [!x_delta %in% c("+ ","- ")]

    correct_effect_ind = nvar = IC = formula_new = NULL

    # within one step, try add or delete each var
    for (delta in x_delta ){
      # delta = x_delta [1]

      # create the formula
      if (delta == "origin") {
        formula_new[delta] = paste(y,"~",paste(x_start,collapse = "+"))
      } else {
        formula_new[delta] = paste(y,"~",paste(x_start,collapse = "+"),delta)
      }

      if (trace) {
        cat('\n',formula_new[delta],"\n------------------------")
        }

      formula_new_model = as.formula(formula_new[delta])
      new_formula = formula_new_model %>% paste_formula(.,clean = TRUE) %>% as.formula
      new_model = glm( new_formula,data = data, family = family)
      new_model$call$formula = new_formula

      if (delta == "origin") {
        best_model = new_model
      }
      #________________ do the test of marginal effect ________________

      correct_effect_ind[delta] = 1
      control_dw = 1

      while (!is.null(test_suit) &&
             correct_effect_ind[delta] && # as long as we can get one wrong effect, then stop
             control_dw <= length(test_suit) # stop if we run out of the test sute
      ) {

        # stop condition: encounter an incorrect sign OR run out of test suit

        test_i = test_suit[[control_dw]]

        if ( is.null(test_i$Monoton_to_Match)) test_i$Monoton_to_Match = 1
        if ( is.null(test_i$Reverse)) test_i$Reverse = FALSE

        # if (trace){
        #   if (!is.null(test_i$focus_var_coeff) ) {
        #     cat("\ncheck marginal effects of coeff vars: ", paste(test_i$focus_var_coeff,collapse = '+',sep=''))
        #   } else {
        #     cat("\ncheck marginal effects of raw vars: ", paste(test_i$focus_var_raw,collapse = '+',sep=''))
        #   }
        # }

        # formula(new_model)
        new_model =
          deleting_wrongeffect (new_model,
                                Reverse =  test_i$Reverse,
                                family = family,
                                Monoton_to_Match = test_i$Monoton_to_Match,
                                re_estimate = FALSE,

                                focus_var_raw = test_i$focus_var_raw,
                                focus_var_model = test_i$focus_var_model,

                                focus_var_coeff = test_i$focus_var_coeff,

                                PRINT =  FALSE,
                                data = data, STOP = STOP,

                                focus_value = test_i$focus_value,
                                nonfocus_value = test_i$nonfocus_value,

                                transform_y = test_i$transform_y,
                                bar_plot =  test_i$bar_plot

                                )
        #
        #
        # eval_arguments(
        #   deleting_wrongeffect (new_model,
        #                         Reverse =  test_i$Reverse,
        #                         family = family,
        #                         Monoton_to_Match = test_i$Monoton_to_Match,
        #                         re_estimate = FALSE,
        #
        #                         focus_var_raw = test_i$focus_var_raw,
        #                         focus_var_coeff = test_i$focus_var_coeff,
        #
        #                         PRINT =  FALSE,
        #                         data = data, STOP = STOP,
        #
        #                         focus_var_model = test_i$focus_var_model,
        #                         focus_value = test_i$focus_value,
        #                         nonfocus_value = test_i$nonfocus_value,
        #
        #                         transform_y = test_i$transform_y,
        #                         bar_plot =  test_i$bar_plot
        #
        #   )
        # )

        if (!is.null(new_model$correct_effect_ind)) correct_effect_ind[delta] = new_model$correct_effect_ind
        control_dw = control_dw + 1
      }

      #_________________________________________________________

      # record
      nvar[delta] = length(get_x(new_model,data = data, method = "model"))
      IC[delta] = extractAIC(new_model,k = k)[2]
    }

    # to record
    result = data.frame(IC, nvar , step_count, correct_effect_ind, formula_new,stringsAsFactors = FALSE)
    result = result[order(result$IC),]

    if (trace) {
      cat("\n")
      if (!is.null(test_suit)) {
        print(result[,c('IC','nvar','step_count','correct_effect_ind')])
      } else {
        print(result[,c('IC','nvar','step_count')])
      }
      cat("\n\n")
    }

    # this is the best model you choose this step: largest IC within those models with correct impacts

    x_best_row = result[result$correct_effect_ind>0,][1,]

    if (is.na(x_best_row$correct_effect_ind) ||
        nrow(x_best_row) ==0 ||
        (x_best_row$correct_effect_ind) ==0
    ) stop("cannot find the correct specification according to the test suit")

    if (rownames(x_best_row) == 'origin'){   # if the best model is the original model, then stop
      step_count = 1000

      # print(deparse(formula(best_model),500))
    } else {  # if the best model is a new model, then we create a new x_start from the best model
      step_count = step_count + 1
      x_start = get_x(as.formula(x_best_row$formula_new),data = data, method = "model")

      if (trace & STOP)  Enter_to_Continue()


      # print(best_model)
      # print(x_start)

      # rownames(x_best_row) %>% str_replace(x_best,pattern = fixed("+ "),"")
    }
  }

  return(best_model)


  if (FALSE && TRUE){

    # starting model:
    # can have a dirty formula like below

    set.seed(413)
    traing_data = ggplot2::diamonds[runif(nrow(ggplot2::diamonds))<0.05,]
    nrow(traing_data)

    diamond_lm3 = lm(formula = price ~ cut + carat - cut   , data = traing_data)

    scope = list(lower = price ~ 1,
                 upper = price ~  I(carat^2) + I(carat^3) + I(carat * depth) + depth + carat)

    # traditional stepwise regression with no marginal effect check
    model1 = stepwise2(model = diamond_lm3, scope = scope,k = 2,
                       trace = TRUE, data = traing_data, STOP = TRUE)
    model1
    # result is exactly same using the default step() function.
    model2 = suppressWarnings(step(diamond_lm3,scope = scope, k = 2))
    model2


    #__ How to Specify the Correct Marginal Effects in Stepwise Regression  __

    # this test_suit means we will check the marginal effect of both 'carat' and 'depth'
    # for 'carat', we will only focus on 4 coeff vars :
        # "I(carat^3)","I(carat*depth)","I(carat^2)","carat"
    # for 'depth', as we do not specify any particular coeff vars there,
    # we will check all coeff var related to 'depth'

    test_suit = list(
      carat = list(
        # the list name must be the raw var
        focus_var_raw = "carat",
        # must specify the focus_var_raw (see deleting_wrongeffect() ) as the raw var
        focus_var_coeff = c("I(carat^3)","I(carat*depth)",
                            "I(carat^2)","carat") ,
        # optional # If not defined, then we to check all coeffs related to the raw var
        focus_value =list(carat = seq(0.5,6,0.1)),
        Monoton_to_Match = 1 # optional. Default is 1
      ),
      depth = list(
        focus_var_raw = "depth",
        Monoton_to_Match = 1
      )
    )

    model3 =  stepwise2(model = diamond_lm3, scope = scope, trace = TRUE,
                        data = traing_data,
                        STOP = FALSE, test_suit = test_suit)

    # see the difference from model1
    effect(model3,focus_var_raw =  "carat", focus_value =list(carat = seq(0.5,6,0.1)))

  }

}





