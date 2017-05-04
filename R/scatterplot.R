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
      cat("\014")
      cat(paste('ggplot(data = ', dataname, ', aes(x = ', X, ', y = ', Y, ')) + \n geom_point() \n\n', sep = ""))
    }
  },

  # Add interactivity to plot
  X = manipulate::picker(as.list(colnames(data)), initial = as.list(colnames(data))[[1]]),
  Y = manipulate::picker(as.list(colnames(data)), initial = as.list(colnames(data))[[2]]),
  show = manipulate::button("Show ggplot syntax")

  )
}


