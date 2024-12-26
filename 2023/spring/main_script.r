rm(list = ls())  # clear environment
# install.packages("tidyverse")
library(tidyverse)
########################################################################################################################
# RGB analysis, to ID when leaf-out & senescence occur.
# Written by Luke Hafermann for Matt Ayre's lab, 6/22/2024.
# Use this script once per season, each time the cameras are taken down.
# It has some interactive steps, so run the script line-by-line to keep the relevant comments/instructions visible.
# It enlists helper scripts, which abstract away technical details useful for revising or extending this project.
########################################################################################################################
# VERSION INFORMATION
# (author name)
# (date of this more-recent version)
# (please summarize your revisions, and any new purposes for this script compared to the original. Actually, Github's
#   much better for version-control & describing updates, especially w multiple contributors. Please use that instead!
########################################################################################################################
# PART 1: setup folders, copy sd cards, create file manifest.

# 1) Set up the following directory structure, with sufficient `.year/season/` subfolders to cover the photos you'd like
# to process. Feel free to locate `leafout_senescence/` on an external hard drive if storage space is a concern.

# leafout_senescence/
# └── 2023/
#     └── spring/                    # Be consistent when naming the season-folder; either type `spring` or `fall`.
#         ├── photos/
#         ├── processed_data/
#         ├── main_script.r
#         └── manifest_script.r

# It's important that each `.year/season/` subfolder contains a complete workflow: `processed_data/`, the scripts which
# generated it, and the raw `photos/` input used. This prevents confusion about which scripts-version was responsible
# for the `processed_data/` of a given `.year/season/`; it also enables use of multiple external hard drives, with each
# `.year/season/` subfolder remaining self-sufficient.


# 2) At the end of each season when the sd cards are collected, copy each camera's sd card into its own sub-folder
# within `photos` (where `photos` is nested under the appropriate year and season). Within `photos`, create the
# following directory structure. The folder-names are important, since `manifest_script.r` will turn them into column
# names useful for sorting and filtering; the folder-names need to be consistent across instances of `photos`.

## photos/
## ├── YB/      - Tree codes: `YB` for Yellow Birch, `SM` for Sugar Maple, `AB` for American Beech.
## |   ├── 1/       (feel free to create new tree-codes for additional tree-types; just add the new codes to
## |   ├── 2/       the above list, for future users to reference. Use however many letters you like. Be
## |   └── 3/       consistent with using ONE code per tree type. If you need to change an established tree
## |                code, note that all `processed_data/`s from prior `.year/season/`s will still have the old tree
## |                code as a column name, which would need to be renamed if you plan to mix old and new
## |                `processed_data/`s (for instance, in the same plot). The above also applies for renaming a season.
## |
## ├── SM/      -`1`, `2`, `3`, etc...: For if you set up multiple cameras per tree type, that season. Even if
## |   └── 1/       you just set up 1 camera per tree type, make sure to still use a (redundant) folder called
## |                `1`, so that my parsing does not get messed up.
## |
## └── AB/      - Within the camera-number folder, copy in all the photos from that camera's SD card (from this
##     ├── 1/       season). MAKE SURE THE SD CARD IS CLEAN, and that you are not copying any photos from a
##     └── 2/       prior season (or test photos with a different camera angle, etc.). It doesn't matter if the
##                  camera's sd card has subfolders (for instance by date, etc.). You can copy those in too.


# 3) Create a path variable for the `.year/season/` subfolder you're currently working within. UPPERCASE indicates it's
# a constant, intended to maintain one value set by the user (as opposed to a variable which the program may change).
SEASON_PATH <- "/Users/lukehafermann/CodeProjects/EcologyProjects/leafout_senescence/2023/spring"
# (this is its pathname on my computer; replace with the pathname from yours). 

# 4) This line runs the helper script `manifest_script.r`, which sorts all the photos you just uploaded into this 
# `/year/season` folder). `manifest_script.r` outputs 2 objects to the Rstudio environment: `manifest` and `SOI`, and
# it also writes a gif (one for each camera) to the `/processed_data` directory, for visual inspection in part 2.
source(str_c(SEASON_PATH, "/manifest_script.r"))
# `manifest` will help us filter and sort the input photos, and `SOI` is a named-list structure that can contain all the
# data we're about to generate (for all cameras) across this "Season Of Interest"; `SOI` also facilitates writing (some
# of) this data to the proper output directory.

########################################################################################################################

# PART 2: visually inspect the gif from each camera this season. If you need to clean a gif (remove photos), part 2 also
# lets you specify “custom_filters” for that gif's camera, which modifies both SOI and the “/processed_data” output
# directory for that camera. Finally, reinspect the new gif, and proceed to PART 3 if satisfied. (Potentially make a
# checklist for all the gifs, to make sure you inspect each one.)

# 1) Here's how you visually inspect each gif: first, navigate to the each gif using Finder (or Windows Explorer). All
# the gifs are accessible within the folder called `/processed_data`. They are organized into subdirectories by tree
# code & camera number. Right-click on a gif and choose "open with browser" in order to visually inspect it.

# Here's what to watch for while flipping through a gif: 
#   - Note down the date/time of any photos you'd like to remove. Each frame of the gif includes a timestamp, thanks
#     to the camera Matt's using. (If the gif's too fast, you can "pause" it by screenshotting. Look up your computer's
#     keyboard shortcut for that if need be.) In particular, note down for removal any...
#   - Test photos, such as ground photos/selfies/non-target tree canopies.
#   - Make sure the camera frame stays steady (i.e. make sure that it doesn't change mid-season, due to wind or
#     something else bumping the camera. (Hopefully these particular trees can sit still for long enough to have their
#     picture taken.)) Impact on ROI: if the frame shifts midseason, the ROI "cookie cutter" will suddenly be aimed at
#     part of the frame with a different RGB signature.

# 2) Specify "custom_filters", if it turns out there are photos you'd like to remove from a gif. The following section
# of code is template code, which you'll have to modify a little bit before executing. The modifications are minimal
# (for instance, specifying a photo's date & time). The template lets you work on one gif at a time; if there are
# multiple gifs for which you need to specify "custom_filters", copy/paste extra instances of the template-code below
# this first one, so you can retain a record of EACH gif's modifications. Call these extra sections 2a), 2b), 2c), etc.

# TEMPLATE CODE:
# Which gif would you like to modify? (Check the gif's storage path to find this info, if need be)
tree <- "SM"
cam_number <- 1

# Now we'll modify SOI to include a "custom_filters" list next to the "default_filters" list.
SOI[[tree]][[cam_number]]$custom_filters = list(
  # Copy in the 3 elements of "default_filters", one at a time, with modifications to each element as follows:
  photos_table = 
    manifest %>%
    filter(tree == !!tree & cam_number == !!cam_number) %>% # filter for current tree & cam_number
    filter(time_of_day == "12:00") %>% # Here's the "default filter" (just an arbitrary way to improve runtime)
    ## FEEL FREE TO INCLUDE ADDITIONAL FILTERS HERE, SUCH AS:
    filter(day_of_year != "2023-05-07") %>%
    arrange(POSIX_date),
  # The 2nd element's easier to just rewrite it from scratch; here you go. (No modifications needed from user.)
  output_path = str_c(SEASON_PATH, "/processed_data/", tree, "/", cam_number, "/custom_filters/"),
  mask_cimg = NULL)  # We haven't created any masks yet, so no modifications needed.



# This bit creates the new output directory "custom_filters", which lives next to this camera's "default_filters" folder
output_dir <- SOI[[tree]][[cam_number]]$custom_filters$output_path
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  # `recursive = TRUE` ensures parent directories are created
}

# Call the gif_maker function; write a gif to the `output_path` for this `$tree` & $`cam_number`.
gif_maker(SOI, tree, cam_number, filters_version = "custom_filters")
# Currently takes 3 minutes to make a gif from 45 photos (original resolution, uncompressed)

# Finally, reexamine the gif; repeat this process (specify filters, rewrite gif, reexamine gif) until satisfied. Then
# move on to the next gif (if there's another one that needs custom_filters). Then move on to Part 3.
# END OF TEMPLATE CODE


########################################################################################################################
# PART 3: Specify the Region Of Interest (ROI) for each camera, by drawing a shape on a reference photo from that
# camera. For leafout, the reference photo is the last photo of the season, which should be nicely representative of the
# canopy shape (with all the leaves fully out). The ROI is the "cookie cutter shape" from which we'll extract the RGB
# values. The ROI is superimposed over all photos taken by that camera, allowing us to analyze RGB within that tree
# canopy over the course of the season.

# Note: this assumes that the tree doesn't move within the camera frame, i.e. that the camera is not bumped or 
# readjusted, wind isn't displacing the canopy egregiously from within the ROI, and that this particular tree can sit 
# still for long enough to have its picture taken. Data from a bumped camera COULD be salvaged by drawing two ROIs...

# install.packages("phenopix")
library(phenopix)

# AGAIN, THE FOLLOWING SECTION IS TEMPLATE CODE. If you have multiple cameras, you will need to run this section once
# for each camera, changing the name-variables each time (as instructed). Actually, Parts 3, 4, 5, and 6 are all like
# this; each camera's data is handled separately from start to finish (where the "finish" is a line plot of gindex over
# the season for that camera). So, for cam1, run parts 3-6 before doing the same for cam2. 
#   Future modification: if one of the steps is very time intensive for the computer, it would be nice to have all the
#   cameras' data reach that step back to back, so the user can leave it running for one extended period of time, rather
#   than multiple shorter intervals. Some sort of loop & intermediate "holder variables" might be required.

# For which camera would you like to draw an ROI?
tree <- "SM"
cam_number <- 1
# Which filters_version would you like to use? (If you specified custom_filters for this camera's gif, I imagine you
# also want the custom_filters applied to all later steps, including ROI & gindex.)
filters_version <- "default_filters"
# filters_version <- "custom_filters"

# Get pathname for last photo of season; we'll draw the ROI on that one, since leaves & canopy spread are visible.
last_photo <- tail(SOI[[tree]][[cam_number]][[filters_version]]$photos_table$SourceFile, 1)

# This gets the camera's output directory, previously created & stored by SOI. 2 things are about to get written to this
# output directory: a `mask_cimg.RData`, and `roi_photo.jpg` (which is a more easily-opened visual reference of the
# first).
ROI_cold_storage_path <- SOI[[tree]][[cam_number]][[filters_version]]$output_path
roi.names <- "roi_photo"

# Display the image and draw an ROI. 
# While drawing the ROI, keep in mind how the canopy moved while watching the gif, due to wind, and due to leaf-weight
# making the branches droop, etc. Also make sure to avoid selecting overlapping tree canopies, which would confuse their
# two different leafout times.
#   (A possible extension to this project would be rewatching the gif after drawing the ROI, so you can see the gif
#   overlaid with the red outline, and see if the tree's wind-related movement in/out of the ROI explains any gindex
#   variation from that season.)
roi <- phenopix::DrawMULTIROI(last_photo, ROI_cold_storage_path, nroi = 1, roi.names, file.type='.JPG')
# Procedure:
# 1) Let the reference image appear in RStudio's plots window.
# 2) Define a polygon blob around the target tree canopy, by clicking wherever you'd 
#    like there to be a polygon vertex. Click frequently to create many vertices, 
#    so the blob is smoother & more accurate. Move the mouse in a path as if you were 
#    tracing around the tree canopy. Unfortunately your traced-out polygon doesn't appear 
#    onscreen until the next step. Watch the mouse crosshairs, not the blue map pin which appears.
# 3) When done, hit the escape key (on mac.) (On windows, please find out for me.) The traced polygon 
#    should appear now, for you to inspect it. Try to include as little sky (beyond the branch tips) as possible.
# 4) RStudio's Console should display "are you done with your roi? type y or n." Place the cursor in
#    the console, type y, and hit enter. (Alternatively, typing n lets you specify another polygon as
#    part of the ROI, for instance if the canopy has two parts. Probably unnecessary for our purposes, but it works.)
#    Ignore the warnings about [rast] unknown extent; I think these are fine.
# Output:
# 1) Your RStudio environment should now contain an object called roi, which is a list containing "mask" & "polygons".
#    The "mask" is a binary raster object, saying whether each pixel of the image is part of the ROI or not.
#    The "polygons" is an object of class SpatialPolygons (from the sp package), and is used for
#    visually drawing a shape onto an image, for instance in the plotting window, or on the photo which gets
#    saved in ROI_cold_storage_path.
# 2) A photo, with the ROI drawn on it, should now exist in ROI_cold_storage_path. This will not be used in
#    the program anywhere, but could be useful for papers or future reference.
########################################################################################################################
# Part 4: Access the mask & convert it to cimg format using the imager package.

# install.packages("imager") # If you're on a mac, you'll have to install quartz first (from https://www.xquartz.org/).
library(imager)

# Access the mask. (Type str(roi) to learn more about its structure.)
mask <- roi$roi_photo$mask
# The mask is a binary raster object, specifying "true" or "false" about whether each pixel 
# is in the region of interest. It is a RasterLayer object from the raster package. It's an S4 object and includes some
# usage of "slots" organization. (Navigate these using the @, as opposed to the $ for dataframes and lists).
#   Future task: phenopix assigns the same name to the roi$`__` and the output photo. I specified the name, above, by
#   typing `roi.names <- "roi_photo"`. It would be more descriptive if they had different names.

# The 2D raster data is stored in a 1D vector called values, in row-major order. This means that the values of
# the first row come first, followed by all the values of the second row, and so on. 
# Unfortunately, our target cimg format requires column-major order, so we'll have to reshape, transpose, and 
# flatten the data before as.cimg() can accept it.

# Put the mask in cimg format:
mask_cimg <- mask@data@values %>%
  # Reshape, transpose, and flatten:
  matrix(nrow = mask@extent@ymax, byrow = TRUE) %>%
  t() %>%
  as.vector() %>%
  # cimg format requires a 4d array. Pipe `array()` the flattened values and specify 
  # they came from a 2d image with width xmax and height ymax. The two additional dimensions
  # are for cimg with multiple frames or color channels. Those do not apply here and are set to 1.
  array(dim = c(mask@extent@xmax, mask@extent@ymax, 1, 1)) %>%
  imager::as.cimg()



# store it in SOI, in the pre-created spot for this particular camera.
SOI[[tree]][[cam_number]][[filters_version]]$mask_cimg <- mask_cimg

# write it as .RData, for "cold storage", in the appropriate output directory:
mask_cimg_output_path <- str_c(SOI[[tree]][[cam_number]][[filters_version]]$output_path, "/mask_cimg.RData")
save(mask_cimg, file = mask_cimg_output_path)

########################################################################################################################
# # THIS PART IS UNFINISHED: I tried to draw the ROI on the gif, so we could watch the gif flip through underneath the
# # ROI "cookie cutter". This probably won't be very useful, however, so I moved on. DrawMULTIROI() already 
# # outputs a .jpg photo with the ROI drawn, if a visual reference is later needed. Here was my initial work, though:
# 
# # Access the xy coordinates (of each ROI vertex), from within the "polygons" part of the roi object:
# # (`may7` was the name I provided to roi.names when making this particular roi)
# coords <- roi$may7$polygons@polygons[[1]]@Polygons[[1]]@coords
# # Separate x and y coordinates
# x_coords <- coords[, 1]
# y_coords <- coords[, 2]
# 
# # Problem/stopping point: resolution matching/scaling. The (x_coords, y_coords) have to match the resolution 
# # of the gif, before you can do the next step (drawing on the gif). Some scaling/troubleshooting would be required.
# 
# # Load the existing GIF (I accidentally deleted it from environment)
# animation <- image_read("/Volumes/CrucialX9/leafout_senescence/photos/may7.gif")
# 
# # Draw on all frames at once
# image_draw(animation)
# polygon(x_coords, y_coords, border = "red", lwd = 30) # Draw polygon
# dev.off()  # Complete the drawing
# 
# # Save the modified GIF
# image_write(animation, "may7_roi.gif")

########################################################################################################################
# # FOR TESTING PURPOSES ONLY. Check that the mask selects the desired ROI.
# 
# # Load an image (in the required cimg format). Here I'm using the same photo on which we drew the ROI.
# img <- imager::load.image(last_photo)
# 
# # Initialize a new image with the same dimensions as 'img' but with zeros (black)
# new_img <- as.cimg(array(0, dim(img)))
# 
# # Use the mask to retain original RGB values within the ROI. It's pretty elegant 
# # that mask_cimg can subset img, even though img has 3x the color channels.
# new_img[mask_cimg > 0] <- img[mask_cimg > 0]
# # Display the original image, mask, and the new image
# par(mfrow = c(1, 3))
# plot(img, main = "Original Image")
# plot(mask_cimg, main = "Mask")
# plot(new_img, main = "New Image with ROI")
#
# # CHECK THAT THE DIMENSIONS MATCH, FOR THE MASK AND IMAGE IT SELECTS UPON:
# dim(img)         # for instance, 6080 3420    1    3
# dim(mask_cimg)   # for instance, 6080 3420    1    1
#                         # ^^ Make sure the first numbers match, the second numbers match,
#                         # the third numbers are both 1, and (for the 4th number) img has 3 color channels
#                         # and mask_cimg has 1 color channel. The above is valid output.
# length(img)      # img should be 3x as long as mask_cimg
# length(mask_cimg)
#
# # NOW WE KNOW THAT MASKS ARE GENERATED CORRECTLY. PROCEED:
########################################################################################################################
# Part 5: Get the pixels, analyze RGB. (currently still set up for one camera's data at a time.)

# (Phenopix and magick also have functions for this, but I decided to write out a lightweight custom version not swamped
# in extra functionality. This one should be easier to understand, modify, & troubleshoot.)

# OK, CURRENTLY:
# - I have the mask_cimg, stored in SOI
# - I have the pathnames to the images, stored in SOI under photos_table
# - I want to give that table a new column, of the gindex for each image.
mask_cimg <- SOI[[tree]][[cam_number]][[filters_version]]$mask_cimg
pathnames <- SOI[[tree]][[cam_number]][[filters_version]]$photos_table$SourceFile
# create a vector of gindex values, with same length as "pathnames":

# in R, the preferred approach is to define a function, and apply that function to a vector (rather than looping).
calculate_gindex <- function(pathnames, mask_cimg) {
  # sapply loops over each pathname, and passes it as "file" to the anonymous function (defined within curly brackets)
  sapply(pathnames, function(file) {
    # Load an image, in the required cimg format, to extract its RGB.
    img <- imager::load.image(file)
    # Apply mask (using "logical indexing", which plays to the mask's strengths as a 2-d grid of booleans)
    r_values <- img[,,,1][mask_cimg]
    g_values <- img[,,,2][mask_cimg]
    b_values <- img[,,,3][mask_cimg]
    
    # Compute the means of the masked RGB values
    mean_r <- mean(r_values, na.rm = TRUE)
    mean_g <- mean(g_values, na.rm = TRUE)
    mean_b <- mean(b_values, na.rm = TRUE)
    
    # Compute and returns one gindex for that photo
    gindex <- mean_g / (mean_r + mean_g + mean_b)
    return(gindex)
    
    # # here's how I verified that green is the second channel:
    # no_green <- img
    # no_green[,,,2] <- 0
    # plot(no_green)
  })
}


# calculate the gindex for each photo:
gindex <- calculate_gindex(pathnames, mask_cimg)
# ^ begun at 1:58 pm. Very quick for 45 photos! 2pm finished.

# store the gindex in SOI:
SOI[[tree]][[cam_number]][[filters_version]]$photos_table$gindex <- gindex


########################################################################################################################
# Part 6: Plot the gindex over the season!

library(ggplot2)

# new table for brevity
photos_table <- SOI[[tree]][[cam_number]][[filters_version]]$photos_table

# Plot gindex over time
ggplot(photos_table, aes(x = POSIX_date, y = gindex)) +
  geom_line(color = "blue", size = 1) +  # Line graph
  geom_point(color = "red", size = 2) +  # Add points for each observation
  labs(
    title = "GIndex Over the Season",
    x = "Date",
    y = "GIndex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Would be neat to also plot gindex with a regression line, and confidence interval shading to show the 
# heteroskedasticity (the variance increases later in the season)





