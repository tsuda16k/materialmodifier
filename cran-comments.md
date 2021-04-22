## Resubmission
This is a re-submission.

CRAN maintainers suggestion:

- Please omit examples for un-exported functions,
- dontrun should only be used if the example really cannot be executed, and
- in examples, it is not allowed for a function to write in the user's home filespace including getwd.

Therefore, I

- omitted examples for un-exported functions.
- either unwrapped examples, or replaced dontrun with donttest where execution time is more than 5 seconds.
- removed getwd function from examples.

## Test environments
- Local: x86_64-apple-darwin17.0 (64-bit), R 4.0.3
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
0 errors | 0 warnings | 1 notes  
1 note should be the first time submission note.

Also, r-hub may report a note, saying "Possibly mis-spelled words in DESCRIPTION: Boyadzhiev et al."  
But the spelling is OK.

## Downstream dependencies
There are currently no downstream dependencies for this package.
