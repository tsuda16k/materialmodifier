## Test environments
- Local: x86_64-apple-darwin17.0 (64-bit), R 4.0.3
- R-hub fedora-clang-devel (r-devel)
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)

## R CMD check results
0 errors | 0 warnings | 1 notes  
1 note should be the first time submission note.

Also, r-hub may report a note, saying "Possibly mis-spelled words in DESCRIPTION: Tsuda (9:34)."  
But the spelling is OK.

When I ran rhub::check_for_cran(), there was an error, saying "there is no package called 'tiff'".  
But when I ran rhub::check_for_cran(env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")), no error was reported.

## Downstream dependencies
There are currently no downstream dependencies for this package.
