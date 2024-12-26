# install.packages("exifr")
# install.packages("tidyverse")
library(exifr) #contains read_exif(), for image metadata
library(tidyverse) #contains stringr & lubridate

########################################################################################################################
# Luke Hafermann, 31 Aug 2024
# This script (`manifest_script.r`) is called by `main_script.r`, and generates a file manifest
# from the `photos` folder within the following directory structure:

# leafout_senescence/
# └── <year>/
#     └── <season>/
#         ├── photos/
#         ├── processed_data/
#         ├── main_script.r
#         └── manifest_script.r

# `manifest_script.r` requires a variable in the Rstudio environment called `season_path`,
# which is created by `main_script.r`. `manifest_script.r` outputs a variable to the Rstudio environment
# called `manifest`, another called `SOI`, and a function for creating/writing gifs.
########################################################################################################################
# PART 1: extract metadata; create manifest columns (to facilitate sorting/filtering).

# set the working directory from which to read photos
setwd(str_c(SEASON_PATH, "/photos"))
# Right now this folder just contains one camera's worth of photos, to speed runtime.

# get all the image metadata (actually, we just need DateTimeOriginal, which we specify to speed runtime)
# metadata thus has two columns: DateTimeOriginal, and SourceFile (which is created by read_exif() I think)
metadata <- read_exif(list.files(recursive = TRUE, full.names = TRUE), tags = "DateTimeOriginal")

# Here's a function which extends lubridate::parse_date_time()'s ability to parse date-time metadata into POSIXct
# format. My extension simply throws a more-helpful error in case of unrecognized date formats.
as_POSIXct <- function(date_strings) {
  parsed_dates <- lapply(date_strings, function(date_string) { # handle one date_string at a time from the input vector
    tryCatch({ # tries this code
      parsed_date <- withCallingHandlers( # returns custom error in case of warning
        lubridate::parse_date_time(date_string, orders = "Y:m:d H:M:S"), 
        # Add any additional date formats here, eg via`orders = c("Y:m:d H:M:S", "Y-m-d H:M:S")`
        
        # parse_date_time() returns dates as class POSIXct, which uses format "2023-05-17 12:10:00 UTC".
        # (UTC timezone is incorrect, but unproblematic. It gets tacked on whenever the timezone isn't specified.)
        warning = function(w) {
          stop(paste("Go modify manifest_script.r to accept this date format from cameras:", date_string))
        }
      )
      return(parsed_date)

    }, error = function(e) { # returns custom error in case of (generic) error in tried code
      stop(paste("Go modify manifest_script.r to accept this date format from cameras:", date_string))
    })
  })
  # lapply() returns `parsed_dates`, a list where each element is either a POSIXct object or NA. This list CAN be used
  # to generate a column in the metadata DF, but ends up causing problems bc usually DF functions expect columns to be
  # vectors, not lists. So: concatenate the list's elements into a vector (which retains class POSIXct, since all 
  # elements have that class (or NA))
 parsed_dates <- do.call(c, parsed_dates)
 return(parsed_dates)
}

# Create `manifest` with the following columns (for sorting/filtering/etc):
# POSIX_date, time_of_day, day_of_year; year, season, tree, cam_number
manifest <- metadata %>%
  # Add column `POSIX_date` for chronological sorting; POSIXct is a date-time format which plays nicely with arrange()
  mutate(POSIX_date = as_POSIXct(DateTimeOriginal)) %>%
  # For filtering/grouping, add columns “time_of_day” and “day_of_year”
  mutate(time_of_day = format(POSIX_date, "%H:%M"),
         day_of_year = as.Date(POSIX_date)) %>%
  # Add columns corresponding to each folder level (year, season, tree, camNumber) to enable additional grouping/sorting
  # Get the folder names from the pathname, which is stored in a column called `SourceFile` (format: ./tree/cam_number/)
    # First copy `SourceFile` to a `temporary` column; get rid of `SourceFile`'s leading `./` (important for pathnames)
  mutate(temporary = gsub("^\\./", "", SourceFile)) %>%
  # Separate the 'SourceFile' column into two new columns; remove `temporary`; drop any unused parts of the pathname
  separate(temporary, into = c("tree", "cam_number"), sep = "/", remove = TRUE, extra = "drop")

# Add columns `year` and `season` (which are the same for all photos); extract these from `season_path`.
# `season_path` returns `...leafout_senescence/2023/spring`. 
  # Split the `season_path` into a vector of its constituent folder names:
folders <- strsplit(SEASON_PATH, "/")[[1]]
  # Add columns for `year` and `season`:
manifest <- manifest %>%
  mutate(year = folders[length(folders) -1],# penultimate folder
         season = folders[length(folders)]) # ultimate folder

########################################################################################################################
# PART 2: write the gif_maker function, used by Part 3

# install.packages("magick")
library(magick)

gif_maker <- function(SOI, tree, cam_number, filters_version) {

  # make sure the working directory is correct
  setwd(str_c(SEASON_PATH, "/photos"))

  # Create an `images` list, by reading the pathnames column in `cam` (for the appropriate `tree`, `cam_number`, & 
  # `filters_version`)
  images <- lapply(
    SOI[[tree]][[cam_number]][[filters_version]]$photos_table$SourceFile, 
    # The [[]] are required when subsetting with variables. Pathnames are of form "./SM/1/.../<photo_name>.JPG", hence
    # setting wd to `/photos` (directly above `<tree_folder>/`).
    magick::image_read)
  # FUTURE TASK: COMPRESS THE HELL OUT OF EACH PHOTO, BEFORE TURNING THEM INTO A GIF; THIS GIF IS JUST FOR VISUAL 
  # INSPECTION. (I'm pretty sure that compression would help the photos get read more quickly, and that it would 
  # help the gif get written more quickly.)


  # Combine images into an animated GIF
  animation <- magick::image_join(images) %>% magick::image_animate(fps = 4)

  # Save the animation as a GIF
  image_write(animation, 
              # to the following `output_path`:
              str_c(
                SOI[[tree]][[cam_number]][[filters_version]]$output_path,
                "/leaves.gif"))
}

########################################################################################################################
# PART 3: Create `SOI`, a named-list structure to contain all the data we're about to generate (for all cameras) across
# this "Season Of Interest". Access the data (corresponding to a particular camera) by specifying a particular
# `SOI$tree$cam_number$filters_version`. This subsets you down to a particular camera's data structure,
# which is a list containing 
#   - `$photos_table` (this includes the pathname & greenness-index for each photo, alongside all of `manifest`'s handy
#     filtering columns.
#   - `$mask_cimg`, which holds the mask (aka cookie cutter, or Region of Interest) drawn by the user for each camera.
#   - `$output_path` is the directory where each particular `<tree>/<cam_number>`'s output gets written, stored under 
#     `/processed_data`. In that sense `/processed_data` is isomorphic with `/photos`, in that each contains the
#     subdirectories `/tree/cam_number`, where `/photos` is for inputs and `/processed_data` is for outputs.
#   - Here's what gets written to each `processed_data/<tree>/<cam_number>/default_filters/`:
#       - `$leaves.gif`: open this (in web browser) to check that the selected subset of photos are all keepers. (No
#         accidentally included selfies, photos of the ground, etc.)
#       - `mask_cimg.RData`: automatically saves the mask (the one that you drew) as an .RData file (that persists
#         outside the RStudio environment), since you can't be expected to 100% accurately recreate it by hand (if that
#         becomes necessary for some reason).
#       - `$roi_photo.jpg`: a quick visual reference for the above, more easily opened & examined.
#       - Possible extension (which I haven't finished): another list-element, called `photos_table.RData`, for
#         persisting the `photos_table` object (for instance, if calculating each gindex takes an egregious amount of
#         time, and you're in a situation where you'd like to save your computer from having to recalculate that).

# When subsetting SOI, remember to use `` to escape the numeric $cam_number; eg SOI$SM$`1`$default_filters.
# By default, the 4th tier $<filters_version> is redundant, with only one entry, but it does provide the user a place to
# store additional entries generated by custom filters in the main script (for instance, entries like `$only_9am`
# or `$no_raindrops`).


# Declare a named list with 4 tiers, for each level in `SOI$tree$cam_number$filters_version`:
SOI <- list() # (declares the first tier)

# Loop over each unique tree. (required knowledge for declaring the second tier)
for (tree in unique(manifest$tree)) {
  
  # Filter the manifest for the current tree and extract unique cam_numbers (which are required for declaring 3rd tier)
  unique_cams <- unique(manifest$cam_number[manifest$tree == tree])
  
  # Declare the 2nd tier: a cam$tree for each unique tree-code
  SOI[[tree]] <- list()
  
  # Loop over each unique cam_number for the current tree-code
  for (cam_number in unique_cams) {
    
    # Declare the 3rd tier: a cam$tree$cam_number (for each unique cam_number within this tree-code)
    SOI[[tree]][[cam_number]] <- list(
      
      # Declare the 4th tier; by default it's redundant, with only one entry, but it does provide the user a place to
      # store additional entries generated by custom filters in the main script (for instance, entries like `$only_9am`
      # or `$no_raindrops`).
      default_filters = list(
        # Finally, here's the lower-level structure (with data of actual interest):
        
        photos_table = 
          # `$photos_table` will eventually contain the pathname & greenness-index for each photo, alongside all of
          # `manifest`'s handy filtering columns. Once complete, it will be saved under
          # `processed_data/<tree>/<cam_number>/default_filters/` as `photos_table.RData`.
          
          # gindex has not yet been calculated, but we can still initialize `photos_table` with the pathname & filtering
          # columns from manifest:
          manifest %>%
          filter(tree == !!tree & cam_number == !!cam_number) %>% # filter for current tree & cam_number
          filter(time_of_day == "12:00") %>% # Here's the "default filter" (just an arbitrary way to improve runtime)
          arrange(POSIX_date)
        ,
        
        mask_cimg = NULL,  # Placeholder for `$mask_cimg`. Will also be written next to `photos_table.RData`.
        
        output_path =
          # `$output_path` stores the location (`processed_data/<tree>/<cam_number>/default_filters/`) where the above 2
          # objects will be written, alongside 2 additional objects: `leafout.gif` and `roi_photo.jpg`.
        
          # Go ahead and initialize `output_path`, since it requires the current `$tree` & $`cam_number` from
          # `manifest`. To assemble this, begin with `season_path` (which returns `.../leafout_senescence/2024/spring`,
          # whose `<year>` and `<season>` are true for all our photos). To `season_path`, we need to append something of
          # the form `processed_data/<tree>/<cam_number>/default_filters`, using `$tree` & `$cam_number` from the 
          # current loop-cycle.
          str_c(SEASON_PATH, "/processed_data/", tree, "/", cam_number, "/default_filters")
      )
    )
    # Check if the output directory exists, and create it if it doesn't
    output_dir <- SOI[[tree]][[cam_number]]$default_filters$output_path
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)  # `recursive = TRUE` ensures parent directories are created
    }
    
    # Call the gif_maker function; write a gif to the `output_path` for this `$tree` & $`cam_number`.
    gif_maker(SOI, tree, cam_number, filters_version = "default_filters")
    # Currently takes 3 minutes to make a gif from 45 photos (original resolution, uncompressed)
  }
}