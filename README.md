# Reproducible Research (group assignment) — OPTION 1 pre-registration
 
Group assignment for Living Data Productivity in Ecology and Evolution
course. Members are: Sophia Fan, Keerthi Krutha, Javad Meghraz, Tanya
Strydom, and Hannah Wilson

This work is released by its authors under a CC-BY 4.0 license

## Repo structure

The pre-registration is written in RMarkdown and can be found in `prereg_ms.Rmd`. 
Dummy (simulated/toy) data can be found in the data/ folder. This script simulates
some mock data, runs a t-test and creates the figure in the pre-registration 
document.

## Building the .pdf

The .pdf has only been knitted on MacOS and preformance on other operating
systems is not a given but should not be a problem

When knitting the .pdf from the `prereg_ms.Rmd` file you might run into a
LaTeX compiling error which has to do with pandoc update. There is an
unreleased patch for `{prereg}` that fixes the issue and can be downloaded
if you call:

`remotes::install_github("crsh/prereg@issue-16")`