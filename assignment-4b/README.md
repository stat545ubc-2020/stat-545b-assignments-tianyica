## STAT 545B Assignment 4B!

This is the repository for Assignment-4b in [__STAT 545B__](https://stat545.stat.ubc.ca/) @ UBC.


+ Description of what's in this folder:

+ Code to run an .R file in the background:

As explained in more detail on the [__STAT 545__](https://stat545.stat.ubc.ca/notes/notes-b05/) website, there is an option to run an .R file in the background, which allows for more flexibility in case the script would execute for a long time. To run `script.R` and record a log as same as in the interactive environment in a file called `log.out`, run this code:

```
nohup Rscript script.R > log.out 2>&1 &
```

[This R-Bloggers post](https://www.r-bloggers.com/2012/01/long-running-r-commands-unix-screen-nohup-and-r/) has provided more detail, where:

- `nohup` sends the code execution to the background.
- `> log.out 2>&1` directs output and messages to `log.out`.
- The final `&` frees up your terminal 


The original Make activity was put together by Jenny Bryan's STAT 545 team before 2017, with the associated content available in [stat545.com Chapter 36: Automating Data Analysis Pipelines](https://stat545.com/automating-pipeline.html).
