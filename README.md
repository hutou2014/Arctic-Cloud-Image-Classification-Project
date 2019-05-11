# stat154_proj2

team member: H. Ham Huang, Xuanfu Lu

How to reproduce:
1. download the entire github repository, including the images
2. run all codes in smooth_image.R, this file is used to generate the smoothed images, but this step is extremely slow, so we suggest not running again, the resulted images are already inside the image_data file
3. run PI-DAN_proj2.Rmd with Rstudio, using R version 3.5.2, remember to set working directory to source file location. Rmd file will be sourcing the CV_generic.R and auxiliary_func.R; please make sure they are in the same file.
4. knit the PI-DAN_proj2.Rmd using Rstudio.
5. the output image should be inside the report_images file created by Rstudio
6. open Stat154proj2writeup.tex in the report_proj2 folder and compile, if you want to replace images, simply replace them using new images, but please remember to change the file name accordingly. Images used in this file are a subset of total images.
7. Last but not the least, do not change the structure of the repository, otherwise you will get errors.
