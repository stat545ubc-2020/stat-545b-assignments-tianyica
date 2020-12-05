all: assignment1 assignment3 assignment4

clean:
	rm -f assignment-1b/assignment-1B.md assignment-1b/Rplots.pdf 
	rm -rf assignment-1b/assignment-1B_files
	cd assignment-4b/make-activity && make clean

assignment1: assignment-1b/assignment-1B.Rmd
	Rscript -e 'rmarkdown::render("$<")'
	rm -f assignment-1b/assignment-1B.html
	
assignment3: assignment-3b/BCL-Shiny/app.R assignment-3b/BCL-Shiny/data/bcl-data.csv
	nohup Rscript -e "shiny::runApp('$<',launch.browser= TRUE)" > log.out 2>&1 &

assignment4: assignment-4b/make-activity/Makefile assignment-4b/make-activity/histogram.r assignment-4b/make-activity/report.rmd 
	cd assignment-4b/make-activity && make all
