# R Script for Bayesian Optimization
This is a collaborative work with the Johnson group in Iowa State University on optimizing laser-induced graphene with mlr3mbo.

This script performs Bayesian Optimization using the `mlr3mbo` and `bbotk` libraries in R. It proposes new candidates for evaluation, updates the model with new data, and proposes the next set of candidates.

## Prerequisites

You need to have R installed on your machine. You also need the following R packages:

- mlr3mbo (version 0.2.2)
- bbotk
- data.table

You can install these packages in R with the commands:
```r
install.packages(c("remotes", "bbotk", "data.table"))
# Install version 0.2.2 of mlr3mbo
remotes::install_version("mlr3mbo", version = "0.2.2")
```

## How to Run

1. Open the `main.R` script in your R environment.
2. Set the working directory to the location of the `main.R` script.
3. Run the `main.R` script.

## Output

The script will output several files:

- `1_output.csv` and `2_output.csv`: These files contain the data without predictions.
- `1_output_with_preds.csv` and `2_output_with_preds.csv`: These files contain the data with predictions.
- `1_archive.rds`, `2_archive.rds`, `1_acq_function.rds`, `2_acq_function.rds`, `1_acq_optimizer.rds`, and `2_acq_optimizer.rds`: These are RDS files that store the state of the archive, acquisition function, and acquisition optimizer.

All output files are saved in the `output` directory.

## Note

The script currently expects an updated file named `1_updated.csv` in the root directory for the second batch of proposals. You can change the file name and path accordingly in the script.