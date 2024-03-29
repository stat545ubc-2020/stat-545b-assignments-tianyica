## STAT 545B Assignment 4B

This is the repository for Assignment-4b in [__STAT 545B__](https://stat545.stat.ubc.ca/) @ UBC.


+ Description of what's in this folder:

This repository contains the assignment instruction(assignment-4B.Rmd, assignment-4B.html) and the make pipeline(make-activity folder) for practice on automation.
The make pipeline is based on a histogram analysis for the length of words from Webster's Dictionary, and is modified by **adding a new plot histogram_line.png to the original report**. The rational is that although the original discrete(dot) histogram is easier to lookup for datapoints, the continuous(line) version displays the general trend clear. 

   To engage with this repository, users can try make activities, such as make all, make clean.

+ Code to run an .R file in the background:

As explained in more detail on the [__STAT 545__](https://stat545.stat.ubc.ca/notes/notes-b05/) website, there is an option to run an .R file in the background, which allows for more flexibility in case the script would execute for a long time. To run `script.R` and record a log as same as in the interactive environment to a file called `log.out`, run this code:

```
nohup Rscript script.R > log.out 2>&1 &
```

[This R-Bloggers post](https://www.r-bloggers.com/2012/01/long-running-r-commands-unix-screen-nohup-and-r/) has provided more detail, where:

- `nohup` sends the code execution to the background.
- `> log.out 2>&1` directs output and messages to `log.out`.
- The final `&` frees up your terminal 


Graph for makefile:
![makefile](https://github.com/stat545ubc-2020/stat-545b-assignments-tianyica/blob/master/assignment-4b/make-activity/makefile.PNG)


Attribution: The original Make activity was put together by Jenny Bryan's STAT 545 team before 2017, with the associated content available in [stat545.com Chapter 36: Automating Data Analysis Pipelines](https://stat545.com/automating-pipeline.html).
