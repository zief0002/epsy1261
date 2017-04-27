#' Scatterplot
#'
#' This function allows you to interactively create a scatterplot and acess the ggplot syntax.
#' @param data dataframe.
#' @keywords scatterplot
#' @export
#' @examples
#' data(cars)
#' scatterplot(cars)

scatterplot = function(data){

  # Keep name of data
  dataname = deparse(substitute(data))

  manipulate::manipulate({

    # Create and print ggplot
    p = ggplot2::ggplot(data = data, ggplot2::aes_string(x = X, y = Y)) + ggplot2::geom_point()
    print(p)

    # Print syntax for button click
    if(show){
      cat(paste('ggplot(data = ', dataname, ', aes(x = ', X, ', y = ', Y, ')) + \n geom_point() \n\n', sep = ""))
    }
  },

  # Add interactivity to plot
  X = manipulate::picker(as.list(colnames(data)), initial = as.list(colnames(data))[[1]]),
  Y = manipulate::picker(as.list(colnames(data)), initial = as.list(colnames(data))[[2]]),
  show = manipulate::button("Show ggplot syntax")

  )
}



# zScatter(data = beauty)
# zScatter(data = ecls)
#
#
#
# titanic = read.csv(file = "~/Dropbox/EPSY-1261/data/titanic.csv")
# head(titanic)
#
# zBarChart = function(data){
#
#   # Keep name of data
#   dataname = deparse(substitute(data))
#
#   manipulate::manipulate({
#
#     #legalVars = lapply(data, function(x) length(unique(x))) < 30
#     #data2 = data[legalVars]
#
#     # Create and print ggplot
#     p = ggplot(data = data, aes_string(x = X)) + geom_bar()
#     print(p)
#
#     # Print syntax for button click
#     if(show){
#       cat(paste('ggplot(data = ', dataname, ', aes(x = ', X, ')) + \n geom_bar() \n\n', sep = ""))
#     }
#   },
#
#   # Add interactivity to plot
#   X = manipulate::picker(as.list(colnames(data)), initial = as.list(colnames(data[lapply(data, function(x) length(unique(x))) < 30]))[[1]]),
#   #Y = manipulate::picker(as.list(colnames(data)), initial = as.list(colnames(data))[[2]]),
#   show = manipulate::button("Show ggplot syntax")
#
#   )
# }
#
#
# zBarChart(titanic)
#
