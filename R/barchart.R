#' Bar Chart
#'
#' This function allows you to interactively create a scatterplot and acess the ggplot syntax.
#' @param data dataframe
#' @keywords barchart
#' @export
#' @examples
#' data(cars)
#' barchart(cars)

barchart = function(data){

  # Keep name of data
  dataname = deparse(substitute(data))

  # Make sure dataframe has categorical variables
  if( sum(sapply(data, function(x) length(unique(x)) < 30) & (sapply(data, is.character) | sapply(data, is.factor))) == 0 ){
    cat("The dataset either has no categorical variables, or the categorical variable(s) has 30 or more categories.")
  }

  # Store only the categorical variables
  temp = data[sapply(data, function(x) length(unique(x)) < 30) & (sapply(data, is.character) | sapply(data, is.factor))]

  manipulate::manipulate({

    # Create and print ggplot
    p = ggplot2::ggplot(data = temp, ggplot2::aes_string(x = X)) + ggplot2::geom_bar()


    # Change to percentage
    if(prop){
      p = ggplot2::ggplot(data = temp, ggplot2::aes_string(x = X)) + ggplot2::geom_bar(ggplot2::aes(y = (..count..)/sum(..count..))) + ggplot2::ylab("proportion") + ggplot2::ylim(0, 1)
      }

    print(p)

    # Print syntax for button click
    if(show){
      cat(paste('ggplot(data = ', dataname, ', aes(x = ', X, ')) + \n geom_bar() \n\n', sep = ""))
    }
  },

  # Add interactivity to plot
  X = manipulate::picker(as.list(colnames(temp)), initial = as.list(colnames(temp))[[1]] ),
  prop = manipulate::checkbox(label = "Proportion"),
  show = manipulate::button("Show ggplot syntax")

  )
}

