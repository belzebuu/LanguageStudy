
cleanall:
	rm -rf auto script-knitr.tex *.log *.dvi *.ps *.pdf figs/* *~ */*~


R_PROG = R
R_HOME = `${R_PROG} RHOME`



%.docx: %.Rmd
	echo "rmarkdown::render('$<')" | ${R_PROG} --no-save --no-restore --no-site-file --no-init-file --silent
	mv %.docx ../doc

%.pdf: %.Rmd
	echo "rmarkdown::render('$<')" | ${R_PROG} --no-save --no-restore --no-site-file --no-init-file --silent
	mv %.pdf ../pdf


%.html: %.Rmd
	echo "rmarkdown::render('$<')" | ${R_PROG} --no-save --no-restore --no-site-file --no-init-file --silent
	mv $(basename $<).html ../html
#	make script-TSP.tex

%.tex: %.Rnw
	echo "library(knitr); knit('$<')" | ${R_PROG} --no-save --no-restore --no-site-file --no-init-file --silent

%.R: %.Rnw
	echo "library(knitr); knit('$<',tangle=TRUE)" | ${R_PROG} --no-save --no-restore --no-site-file --no-init-file --silent



