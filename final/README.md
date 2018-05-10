# ARE 212 Problem Set #5

#### MARCH 2018

## What is it
The FIFTH (& final!!!) problem set for ARE 212.

## How do I use it
Clone the repo from github.com and open in R-studio. This can be done via the command line or directly in R-studio with the url from github.com (big green button above). 

## Basic workflow for version control

In your local directory, **check for updates** in remote repository using:

	git fetch

If changes have been commited by others, then your local repo will be behind. 

To **update your local master** use:

	git pull

before beginning to work on the code. 

When you've finished working for the day and want to commit your changes use:

	git status

to see which files have been changed. Note: you can also use `git diff` to see exactly what has changed in those files.

Then **add** all those files: 

	git add .

or if you just want to add a specific file to be committed use

	git add <filename>

Then **commit** with a message explaining progress

	git commit -m "your message here"

Finally, **push your local commits** to the remote repository on github

	git push origin master

Here, *origin* is the default nickname for the remote repo on github.com, and *master* is the name of the main branch we're working on. More on branches below!

**Note:** *RStudio integrates beautifully with git, so pretty much all of the above can also be done in the RStudio GUI. Whenever you're working with a local directory that contains a .git file (meaning the directory has been initialized as a git repository), you will see a "Git" window in the "Environment/History" pane. You can do adding, commiting, pushing and pulling all from here. I tend to use the command line, which is why I focus on that workflow- I also think you get a better sense of what git is actually doing and how it interacts with github.com that way.*

### Branch stuff

Mostly we'll be working on the main branch, called 'master'. Sometimes though, we may want to experiment with some code and save that to a separate branch before commiting and merging that with the main 'master' code. 

Branches are used to develop features isolated from each other. The master branch is the "default" branch when you create a repository. Use other branches for development and merge them back to the master branch upon completion.

To check which branch you're on use:

	git branch

To switch to a new branch use:

	git checkout -b newbranch

where 'newbranch' is the name of the new branch. 

**Note:** `git checkout -b` will create a new branch with the supplied name if none exists yet. You can also use `git checkout` to switch to an existing branch, like master. 

create a new branch named "feature_x" and switch to it using
	
	git checkout -b feature_x

switch back to master
	
	git checkout master

to delete the branch again

	git branch -d feature_x

a branch is not available to others unless you push the branch to your remote repository

	git push origin <branch>

to merge another branch into your active branch (e.g. master), use

	git merge <branch>

in both cases git tries to auto-merge changes. Unfortunately, this is not always possible and results in conflicts. You are responsible to merge those conflicts manually by editing the files shown by git. After changing, you need to mark them as merged with

	git add <filename>

before merging changes, you can also preview them by using

	git diff <source_branch> <target_branch>


### Replace local changes

In case you did something wrong, which for sure never happens ;), you can replace local changes using the command

git checkout -- <filename>

this replaces the changes in your working tree with the last content in HEAD. Changes already added to the index, as well as new files, will be kept.

If you instead want to drop all your local changes and commits, fetch the latest history from the server and point your local master branch at it like this

	git fetch origin
	git reset --hard origin/master
