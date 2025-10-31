%let pgm=utl-altair-personal-slc-optimum-lamda-for-box-cox-regression;

%stop-submission;

Altair personal slc optimum lamda for box cox regression

Too log to post in listserv, see github

github
https://github.com/rogerjdeangelis/utl-altair-personal-slc-optimum-lamda-for-box-cox-regression

community.altair
https://community.altair.com/discussion/64845/box-cox?tab=all#latest

I suspect you acan also do this in python


/*               _     _
 _ __  _ __ ___ | |__ | | ___ _ __ ___
| `_ \| `__/ _ \| `_ \| |/ _ \ `_ ` _ \
| |_) | | | (_) | |_) | |  __/ | | | | |
| .__/|_|  \___/|_.__/|_|\___|_| |_| |_|
|_|
*/

COMPUTE OPTIMUM LAMDA (for dependent variable transformation)
=====================

[1] "Optimal Lambda: -0.454545454545455"

Optimum Y for

           --
           |    lamda - 1
           |  y
 lamda     | -----------  if lamda not = 0
y        = |   lamda
           |
           |   log(y)     lamda = 0
           --

Lambda is the transformative power parameter that defines how the Box-Cox
transformation reshapes the data distribution to improve normality and variance
stability in regression and other analyses.

/*                   _
(_)_ __  _ __  _   _| |_
| | `_ \| `_ \| | | | __|
| | | | | |_) | |_| | |_
|_|_| |_| .__/ \__,_|\__|
        |_|
*/

/* Create example dataset */
data my_data;
  input x y;
  datalines;
7 1
7 1
8 1
3 2
2 2
4 2
4 2
6 2
6 2
7 3
5 3
3 3
3 6
5 7
8 8
;
run;

lambda is the transformative power parameter that defines how the Box-Cox
transformation reshapes the data distribution to improve normality and variance
stability in regression and other analyses.


/*
 _ __  _ __ ___   ___ ___  ___ ___
| `_ \| `__/ _ \ / __/ _ \/ __/ __|
| |_) | | | (_) | (_|  __/\__ \__ \
| .__/|_|  \___/ \___\___||___/___/
|_|
*/


[1] "Optimal Lambda: -0.454545454545455"

Optimum Y for

           --
           |    lamda - 1
           |  y
 lamda     | -----------  if lamda not = =
y        = |   lamda
           |
           |   log(y)     lamda = 0
           --


options set=RHOME "D:\d451";
&_init_;
proc r;
export data=my_data r=my_data;
submit;

# Load required package
library(MASS)

# Fit a linear model (required for boxcox)
model <- lm(y ~ x, data = my_data)

# Perform Box-Cox transformation analysis
pdf("d:/pdf/boxcox_plot.pdf", width = 8, height = 6)
bc_result <- boxcox(
  model,
  lambda = seq(-3, 3, 0.25),  # Lambda range from -3 to 3
  plotit = TRUE                # Plot log-likelihood vs lambda
)
dev.off()
# Find the optimal lambda value
optimal_lambda <- bc_result$x[which.max(bc_result$y)]
print(paste("Optimal Lambda:", optimal_lambda))

# Optional: Create a data frame with detailed results
bc_table <- data.frame(
  Lambda = bc_result$x,
  Log_Likelihood = bc_result$y
)

bc_table$R_Square <- sapply(bc_result$x, function(l) {
  if(l == 0) y_trans <- log(my_data$y)
  else y_trans <- (my_data$y^l - 1)/l
  summary(lm(y_trans ~ my_data$x))$r.squared
})

max_ll    <- max(bc_table$Log_Likelihood)
ci_cutoff <- max_ll - qchisq(0.95, 1)/2
ci_lambdas <- bc_table$Lambda[bc_table$Log_Likelihood >= ci_cutoff]

max_ll
ci_cutoff
ci_lambdas

endsubmit;
import data=bc_table r=bc_table;
run;quit;

proc print data=bc_table width=min;
run;quit;

/*           _               _
  ___  _   _| |_ _ __  _   _| |_
 / _ \| | | | __| `_ \| | | | __|
| (_) | |_| | |_| |_) | |_| | |_
 \___/ \__,_|\__| .__/ \__,_|\__|
                |_|
*/


[1] "Optimal Lambda: -0.454545454545455"

Altair SLC

Obs     LAMBDA     LOG_LIKELIHOOD      R_SQUARE

  1    -3.00000       -26.4923        0.274377013
  2    -2.93939       -25.9685       0.2725452914
  3    -2.87879       -25.4497       0.2706242502
....
 98     2.87879       -37.2990       0.0353794065
 99     2.93939       -38.0331         0.03675286
100     3.00000       -38.7728       0.0381178814

/*
| | ___   __ _
| |/ _ \ / _` |
| | (_) | (_| |
|_|\___/ \__, |
         |___/
*/

856       ODS _ALL_ CLOSE;
857       ODS LISTING;
858       FILENAME WBGSF 'd:\wpswrk\_TD9616/listing_images';
859       OPTIONS DEVICE=GIF;
860       GOPTIONS GSFNAME=WBGSF;
861
862
863       options set=RHOME "D:\d451";
864       &_init_;
865       proc r;
NOTE: Using R version 4.5.1 (2025-06-13 ucrt) from d:\r451
866       export data=my_data r=my_data;
NOTE: Creating R data frame 'my_data' from data set 'WORK.my_data'

867       submit;
868
869       # Load required package
870       library(MASS)
871
872       # Fit a linear model (required for boxcox)
873       model <- lm(y ~ x, data = my_data)
874
875       # Perform Box-Cox transformation analysis
876       pdf("d:/pdf/boxcox_plot.pdf", width = 8, height = 6)
877       bc_result <- boxcox(
878         model,
879         lambda = seq(-3, 3, 0.25),  # Lambda range from -3 to 3
880         plotit = TRUE                # Plot log-likelihood vs lambda
881       )
882       dev.off()
883       # Find the optimal lambda value
884       optimal_lambda <- bc_result$x[which.max(bc_result$y)]
885       print(paste("Optimal Lambda:", optimal_lambda))
886
887       # Optional: Create a data frame with detailed results
888       bc_table <- data.frame(
889         Lambda = bc_result$x,
890         Log_Likelihood = bc_result$y
891       )
892
893       bc_table$R_Square <- sapply(bc_result$x, function(l) {
894         if(l == 0) y_trans <- log(my_data$y)
895         else y_trans <- (my_data$y^l - 1)/l
896         summary(lm(y_trans ~ my_data$x))$r.squared
897       })
898
899       max_ll    <- max(bc_table$Log_Likelihood)
900       ci_cutoff <- max_ll - qchisq(0.95, 1)/2
901       ci_lambdas <- bc_table$Lambda[bc_table$Log_Likelihood >= ci_cutoff]
902
903       max_ll
904       ci_cutoff
905       ci_lambdas
906
907       endsubmit;

NOTE: Submitting statements to R:

>
> # Load required package
> library(MASS)
>
> # Fit a linear model (required for boxcox)
> model <- lm(y ~ x, data = my_data)
>
> # Perform Box-Cox transformation analysis
> pdf("d:/pdf/boxcox_plot.pdf", width = 8, height = 6)
> bc_result <- boxcox(
+   model,
+   lambda = seq(-3, 3, 0.25),  # Lambda range from -3 to 3
+   plotit = TRUE                # Plot log-likelihood vs lambda
+ )
> dev.off()
> # Find the optimal lambda value
> optimal_lambda <- bc_result$x[which.max(bc_result$y)]
> print(paste("Optimal Lambda:", optimal_lambda))
>
> # Optional: Create a data frame with detailed results
> bc_table <- data.frame(
+   Lambda = bc_result$x,
+   Log_Likelihood = bc_result$y
+ )
>
> bc_table$R_Square <- sapply(bc_result$x, function(l) {
+   if(l == 0) y_trans <- log(my_data$y)
+   else y_trans <- (my_data$y^l - 1)/l
+   summary(lm(y_trans ~ my_data$x))$r.squared
+ })
>
> max_ll    <- max(bc_table$Log_Likelihood)
> ci_cutoff <- max_ll - qchisq(0.95, 1)/2
> ci_lambdas <- bc_table$Lambda[bc_table$Log_Likelihood >= ci_cutoff]
>
> max_ll
> ci_cutoff
> ci_lambdas

NOTE: Processing of R statements complete

>
908       import data=bc_table r=bc_table;
NOTE: Creating data set 'WORK.bc_table' from R data frame 'bc_table'
NOTE: Column names modified during import of 'bc_table'
NOTE: Data set "WORK.bc_table" has 100 observation(s) and 3 variable(s)

909       run;quit;
NOTE: Procedure r step took :
      real time : 0.804
      cpu time  : 0.000


910
911       proc print data=bc_table width=min;
912       run;quit;
NOTE: 100 observations were read from "WORK.bc_table"
NOTE: Procedure print step took :
      real time : 0.012
      cpu time  : 0.000


913       quit; run;
914       ODS _ALL_ CLOSE;
915       FILENAME WBGSF CLEAR;

/*              _
  ___ _ __   __| |
 / _ \ `_ \ / _` |
|  __/ | | | (_| |
 \___|_| |_|\__,_|

*/
