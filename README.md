# LDP_PreReg
 Group assignment for Living Data Productivity in Ecology and Evolution course

## Building the .pdf

When knitting the .pdf from the `prereg_ms.Rmd` file you might run into a
LaTeX compiling error which has to do with pandoc update. There is an
unreleased patch for `{prereg}` that fixes the issue and can be downloaded
if you call:

`remotes::install_github("crsh/prereg@issue-16")`