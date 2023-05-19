## Package version update
This is an update of the package with some bug fixes and updates.

## Test environments
- Local: x86_64-apple-darwin17.0 (64-bit), R 4.0.3
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
There were no ERRORs or WARNINGs. 

There are 3 NOTEs:

On windows-x86_64-devel, 2 NOTEs were:

```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory
Found the following files/directories:
  'lastMiKTeXException'
```

The second one could be due to a bug/crash in MiKTeX and can likely be ignored.

On ubuntu-gcc-release and fedora-clang-devel, 1 NOTE was:

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

Although the note occurs on these environments, the HTML is able to be validated on windows and locally (mac) and does not seem critical.

Also, r-hub may report a note, saying "Possibly mis-spelled words in DESCRIPTION: Boyadzhiev et al."  
But the spelling is OK.

## Downstream dependencies
There are currently no downstream dependencies for this package.
