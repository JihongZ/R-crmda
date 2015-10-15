Paul E. Johnson <pauljohn at ku.edu>
Center for Research Methods and Data Analysis, University of Kansas
2015-10-15

Github readme for top-level repository.

I had a repo in my personal GitHub account, then we forked it
into a group account named R-crmda. Around the time we created this
(2012), there was a student group at KU called KUseR. It may be
we meld that with this.

People who want to edit and push changes back to the repo should
create github accounts for themselves and we will add them as group
members.  

Web view (in a browser)

https://github.com/R-crmda/R-crmda

Clone a copy! Please create a main directory where all of
your GIT repo downloads can stay, so they are easy to find. I call
mine GIT.

$ mkdir GIT
$ cd GIT
$ git clone https://github.com/R-crmda/R-crmda.git
Cloning into 'R-crmda'...
remote: Counting objects: 1679, done.
remote: Total 1679 (delta 0), reused 0 (delta 0), pack-reused 1679
Receiving objects: 100% (1679/1679), 21.15 MiB | 13.54 MiB/s, done.
Resolving deltas: 100% (889/889), done.
Checking connectivity... done.


If you want to push changes back, you'll have to do the GitHub
security key setup, then use the ssh protocol to clone

git clone git@github.com:R-crmda/R-crmda.git


The Golden Rules

1. Write readable code following established guidelines (below) for
style and presentation.
2. Keep good Changelogs (in Emacs, M-x add-changelog-entry)
3. Respect the work of others by not damaging it. The git system
allows us to "roll back" mistakes, but we'd rather not bother.



The folders are defined as follows

1. documentation


2. packages: R packages.  Make nested folders like so:  

I suggest this structure for directories

packages
     my_project_name
	     miscellaneous_files
		 my_project_name
		     R
			 man

The sub-sub-sub folder "my_project_name" is the one where the
code for the package actually lives.  It has to be nested
that way because the project may need miscellaneous_files
that we don't redistribute with the package.

If a package becomes big, it is probably smarter to move it
into a repository of its own.  That happened to rockchalk
pretty quickly after it got started here.


projects: Example start-to-finish R code and accompanying
    material. Most of these are accumulated from advising on graduate
    projects or developing example code that might later go into packages.

WorkingExamples: single file example R programs that demonstrate
particular functions or packages.

The style for WorkingExamples is based on the R DESCRIPTION file
    format and other standards we follow. The style is explained 
    in the file WorkingExamples: r_style_guide.R

Here is the gist of it.

1. At the top, have the DESCRIPTION information.
2. In the code, follow these elementary bits
   A. indent 4 spaces (not a tab) for each level of scope.
   B. spaces are mandatory around assignments and mathematical
   operators, such as "<-", "*", "+" and so forth.

