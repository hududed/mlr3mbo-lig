# %%
# FIRST BATCH OF PROPOSALS
library(mlr3mbo)
library(bbotk)
library(data.table)

round_to_nearest <- function(x, value) {
  to_nearest = value
  if (is.data.table(x) || is.data.frame(x)) {
    x = lapply(x, function(col) {
      if (is.numeric(col)) {
        return(round(col / to_nearest) * to_nearest)
      } else {
        return(col)
      }
    })
    x = setDT(x) # Convert the list to a data.table
  } else if (is.numeric(x)) {
    x = round(x / to_nearest) * to_nearest
  }
  return(x)
}
# Load data
file <- "LIG_data.csv"
data <- as.data.table(read.csv(file))

# Set up the search space
domain = ps(Power = p_int(lower = 10, upper = 70),
            Speed = p_int(lower = 10, upper = 60),
            DPI = p_int(lower = 1, upper = 7),
            N2gas = p_int(lower = 0, upper = 1),
            Defocus = p_dbl(lower = -0.3, upper = 0.3))
codomain = ps(Resistance = p_dbl(tags = "minimize"))

# Set up the archive, surrogate, acquisition function, and acquisition optimizer
archive = Archive$new(search_space = domain, codomain = codomain)
archive$add_evals(xdt = data[, c("Power", "Speed", "DPI", "N2gas", "Defocus")],
                  ydt = data[, c("Resistance")])


surrogate <- srlrn(default_rf(), archive = archive)
acq_function = acqf("ei", surrogate = surrogate)
acq_optimizer = acqo(
  opt("random_search", batch_size = 1000),
  terminator = trm("evals", n_evals = 1000),
  acq_function = acq_function)
set.seed(42)

q = 2  # we want 2 proposals
lie = data.table()  # needed for constant liear
liar = mean  # liar function, e.g., constant mean

# Obtain the first candidate
acq_function$surrogate$update()
acq_function$update()
candidate = acq_optimizer$optimize()
candidate <- round_to_nearest(candidate, 0.01)

# prepare lie objects
tmp_archive = archive$clone(deep = TRUE)
acq_function$surrogate$archive = tmp_archive
lie[, archive$cols_y := liar(archive$data[[archive$cols_y]])]
candidate_new = candidate

# obtain the other q-1 candidates using fake archive
for (i in seq_len(q)) {
  prediction = acq_function$surrogate$predict(candidate_new)
  col_names <- c(paste0(archive$cols_y[1], "_mean"), paste0(archive$cols_y[1], "_se"))
  # Add new columns to candidate
  for (col_name in col_names) {
    if (!col_name %in% names(candidate)) {
      candidate[, (col_name) := NA]
    }
  }
  # Get the predicted mean and standard error
  prediction <- acq_function$surrogate$predict(candidate_new)
  candidate_new[, (col_names) := .(prediction$mean[1], prediction$se[1])]

  if (i > 1) {
    candidate <- rbind(candidate, candidate_new, fill = TRUE)
  }
  if (i <= q) {
    tmp_archive$add_evals(xdt = candidate_new,
                          xss_trafoed = transform_xdt_to_xss(candidate_new, tmp_archive$search_space),
                          ydt = lie)
  # update all objects with lie and obtain new candidate
    acq_function$surrogate$update()
    acq_function$update()
    candidate_new = acq_optimizer$optimize()
    candidate_new <- round_to_nearest(candidate_new, 0.01)
  }
}

x2_dt <- as.data.table(candidate)
print(x2_dt)
# Prepare data_with_preds
candidate_with_preds <- candidate[, -c(".already_evaluated","x_domain"), with = FALSE]
tmp_data_with_preds <- rbindlist(list(data, candidate_with_preds), fill = TRUE)
# Select only the last rows of length x2_dt
x2_with_preds <- tmp_data_with_preds[(nrow(tmp_data_with_preds) -
                                      nrow(x2_dt)+1):nrow(tmp_data_with_preds), ]
data_with_preds <- rbindlist(list(data, x2_with_preds), fill = TRUE)

data_no_preds <- subset(data_with_preds,
                        select = -c(acq_ei, Resistance_mean, Resistance_se))
# %%
# %%
# Save the data tables as CSV files
dir_path <- "output"
if (!dir.exists(dir_path)) {
  dir.create(dir_path)
}
data.table::fwrite(data_no_preds, paste0(dir_path, "/1_output.csv"))
data.table::fwrite(data_with_preds, paste0(dir_path, "/1_output_with_preds.csv"))

# Save RDS files
saveRDS(archive,  paste0(dir_path,  "/1_archive.rds"))
saveRDS(acq_function, paste0(dir_path, "/1_acq_function.rds"))
saveRDS(acq_optimizer, paste0(dir_path, "/1_acq_optimizer.rds"))
# %%

# RUN THE EXPERIMENTS AND SAVE THE NEW UPDATES
# Currently tested with updated file named `1_updated.csv` saved in root 
# Feel free to change the file name and path accordingly

# %%
# UPDATED EXPERIMENT WITH NEW DATA
library(data.table)
library(mlr3mbo)
library(bbotk)
# Load data
set.seed(42)

round_to_nearest <- function(x, value) {
  to_nearest = value
  if (is.data.table(x) || is.data.frame(x)) {
    x = lapply(x, function(col) {
      if (is.numeric(col)) {
        return(round(col / to_nearest) * to_nearest)
      } else {
        return(col)
      }
    })
    x = setDT(x) # Convert the list to a data.table
  } else if (is.numeric(x)) {
    x = round(x / to_nearest) * to_nearest
  }
  return(x)
}

q <- 2
file <- "1_updated.csv" # currently in root folder, update path and name accordingly
data_updated <- as.data.table(read.csv(file))

file <- "output/1_output_with_preds.csv"
data_with_preds <- as.data.table(read.csv(file))

# Load the RDS files
archive <- readRDS("output/1_archive.rds")
acq_function <- readRDS("output/1_acq_function.rds")
acq_optimizer <- readRDS("output/1_acq_optimizer.rds")
# reset the working archive to the actual one and not the temporary lie archive
acq_function$surrogate$archive = archive
proposed_rows <- tail(data_updated, n = q)
archive$add_evals(xdt = proposed_rows[, c("Power", "Speed", "DPI", "N2gas", "Defocus"), with = FALSE],
                  ydt = proposed_rows[, c("Resistance"), with = FALSE])

lie = data.table()  # needed for constant liar
liar = mean
# Obtain the first candidate
acq_function$surrogate$update()
acq_function$update()
candidate = acq_optimizer$optimize()
candidate <- round_to_nearest(candidate, 0.01)


# prepare lie objects
tmp_archive = archive$clone(deep = TRUE)
acq_function$surrogate$archive = tmp_archive
lie[, archive$cols_y := liar(archive$data[[archive$cols_y]])]
candidate_new = candidate

# obtain the other q-1 candidates using fake archive
for (i in seq_len(q)) {
  prediction = acq_function$surrogate$predict(candidate_new)
  col_names <- c(paste0(archive$cols_y[1], "_mean"), paste0(archive$cols_y[1], "_se"))
  # Add new columns to candidate
  for (col_name in col_names) {
    if (!col_name %in% names(candidate)) {
      candidate[, (col_name) := NA]
    }
  }
  # Get the predicted mean and standard error
  prediction <- acq_function$surrogate$predict(candidate_new)
  candidate_new[, (col_names) := .(prediction$mean[1], prediction$se[1])]

  if (i > 1) {
    candidate <- rbind(candidate, candidate_new, fill = TRUE)
  }
  if (i <= q) {
    tmp_archive$add_evals(xdt = candidate_new,
                          xss_trafoed = transform_xdt_to_xss(candidate_new, tmp_archive$search_space),
                          ydt = lie)
  # update all objects with lie and obtain new candidate
    acq_function$surrogate$update()
    acq_function$update()
    candidate_new = acq_optimizer$optimize()
    candidate_new <- round_to_nearest(candidate_new, 0.01)
  }
}
x2_dt <- as.data.table(candidate)

# Prepare data_no_preds
candidate_no_preds <- candidate[, c("Power","Speed","DPI","N2gas","Defocus"), with = FALSE]
data_no_preds <- rbindlist(list(data_updated, candidate_no_preds), fill = TRUE)

# Prepare data_with_preds
candidate_with_preds <- candidate[, -c(".already_evaluated","x_domain"), with = FALSE]
data_with_preds <- rbindlist(list(data_with_preds, candidate_with_preds), fill = TRUE)
# %%
# %%
# Save the data tables as CSV files
dir_path <- "output"
data.table::fwrite(data_no_preds, paste0(dir_path, "/2_output.csv"))
data.table::fwrite(data_with_preds, paste0(dir_path, "/2_output_with_preds.csv"))

# Save RDS files
saveRDS(archive,  paste0(dir_path,  "/2_archive.rds"))
saveRDS(acq_function, paste0(dir_path, "/2_acq_function.rds"))
saveRDS(acq_optimizer, paste0(dir_path, "/2_acq_optimizer.rds"))
# %%