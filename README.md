# Animated map of the spread of Covid-19
Joe Mitchell-Nelson and Joanne Sherrod

### Using this repo

You'll need R and preferrably Rstudio installed to use this repo. R is free and open-source. [You can find help install R and Rstudio here](https://courses.edx.org/courses/UTAustinX/UT.7.01x/3T2014/56c5437b88fa43cf828bff5371c6a924/).

This repo contains R code to create an animated map of the spread of Covid-19, using data pulled from the [New York Times' public github repository](https://github.com/nytimes/covid-19-data).

All of the R code is in the file main.R. One note is that **you'll need an API key from the US Census Bureau**. The Census isn't picky about who gets a key, but it may take a few hours for them to approve your request. [Request an API key here](http://api.census.gov/data/key_signup.html).

The individual frames generated by the code are located in the `frames` folder. The current iteration of the map is saved as THE_MAP.gif. We plan to update this gif weekly (on Thursdays).

To change, add, or remove map captions, edit the file `titles.txt`.

### Why the Trump quotes?

To us, January and February's developments feel like they happened 12 years ago. Since then, the president's response to the Covid-19 epidemic has taken a number of turns, and it's easy to lose track.  We hope this short gif provides a brief history of Trump's public stances on Covid-19 in the context of the spread of the virus in the US.

Rather than provide a cumulative measure of cases and deaths, we display the numbers of cases and deaths in the past two weeks. Cumulative measures can only increase or remain flat. As time passes, we hope that future updates show the virus receding.

### Sources

Demographic data come from the US Census Bureau. Data on confirmed Covid-19 cases and deaths come from the [New York Times](https://github.com/nytimes/covid-19-data). We gathered data on Trump's golf schedule from [Trumpgolfcount.com](https://trumpgolfcount.com/displayoutings). A list of Trump's campaign rallies can be found on [Wikipedia](https://en.wikipedia.org/wiki/List_of_post-election_Donald_Trump_rallies). Sources for the other captions are listed in the table below.

| Date | Caption | Link  |
|:-----:|:-----:|:-----|
|Jan 22nd | We have it totally under control. It’s one person coming in from China. It’s going to be just fine.| [Link](https://www.cnbc.com/2020/01/22/trump-on-coronavirus-from-china-we-have-it-totally-under-control.html)|
|Jan 28th | Trump holds campaign rally|
|Jan 29th | Peter Navarro circulates a memo warning that half a million US citizens could die from Covid 19| [Link](https://www.foxnews.com/politics/white-house-memos-warned-in-run-up-to-pandemic-of-up-to-2m-deaths-economic-devastation)|
|Jan 30th | Trump holds campaign rally|
|Feb 1st | Trump golfs|
|Feb 2nd | We pretty much shut it down coming in from China.|[Link](https://www.youtube.com/watch?v=yjrqd7xt2Xk)|
|Feb 10th | Trump holds campaign rally|
|Feb 15th | Trump golfs|
|Feb 19th | Trump holds campaign rally|
|Feb 20th | Trump holds campaign rally (again)|
|Feb 21st | Trump holds campaign rally (three days running)|
|Feb 24th | The Coronavirus is very much under control in the USA... Stock Market starting to look very good to me!| [Link](https://twitter.com/realdonaldtrump/status/1232058127740174339?lang=en)|
|Feb 26th | It’s a little like the regular flu that we have flu shots for.| [Link](https://www.whitehouse.gov/briefings-statements/remarks-president-trump-vice-president-pence-members-coronavirus-task-force-press-conference/)|
|Feb 27th | One day it’s like a miracle, it will disappear.|[Link](https://www.whitehouse.gov/briefings-statements/remarks-president-trump-meeting-african-american-leaders/)|
|Feb 28th | This is their new hoax.|[Link](https://thehill.com/homenews/campaign/485245-trump-hits-democrats-over-coronavirus-criticism-this-is-their-new-hoax)|
|March 2nd | You take a solid flu vaccine, you don’t think that could have an impact, or much of an impact, on corona?| [Link](https://www.c-span.org/video/?470902-1/president-coronavirus-test-negative-defense-production-act-ventilators)|
|March 4th | If we have...people that get better just by, you know, sitting around and even going to work, some of them go to work, but they get better.| [Link](https://www.youtube.com/watch?v=IlZPtrhzY5U)|
|March 5th | I NEVER said people that are feeling sick should go to work.| [Link](https://twitter.com/realDonaldTrump/status/1235573492004904961)|
|March 7th | Trump golfs|
|March 8th | Trump golfs (again)|
|March 9th | This blindsided the world.| [Link](https://www.c-span.org/video/?470172-1/president-trump-coronavirus-task-force-briefing)|
|March 13th | State of emergency declared|
|March 17th | This is a pandemic. I felt it was a pandemic long before it was called a pandemic.| [Link](https://thehill.com/homenews/administration/488031-trump-says-he-knew-coronavirus-was-a-pandemic-long-before-it-was)|
|March 18th | It’s not racist at all. No. Not at all. It comes from China. That’s why. It comes from China. I want to be accurate.| [Link](https://www.whitehouse.gov/briefings-statements/remarks-president-trump-vice-president-pence-members-coronavirus-task-force-press-briefing-6/)|
|March 25th | 3.3 million Americans file for unemployment|
|March 26th | I don’t believe you need 40,000 or 30,000 ventilators.| [Link](https://www.youtube.com/watch?v=NbnEx5j1Y9w)|
|April 4th | 6.6 million more Americans file for unemployment|
|April 6th | [The HHS report warning of hospital shortages] is just wrong...It still could be her opinion.| [Link](https://www.c-span.org/video/?470990-1/president-trump-coronavirus-task-force-briefing)|
|April 11th | It’s going to be based on a lot of facts and a lot of instinct.| [Link](https://thehill.com/homenews/administration/492391-trump-says-hell-use-fact-and-instincts-when-deciding-to-push-for-us)|
|April 13th | When somebody is the president of the United States, the authority is total.| [Link](https://www.c-span.org/video/?471160-1/president-trump-coronavirus-task-force-briefing)|
|April 15th | We have the right to do whatever we want, but we wouldn’t do that.| [Link](https://www.whitehouse.gov/briefings-statements/remarks-president-trump-vice-president-pence-members-coronavirus-task-force-press-briefing-26/)|
|April 17th | Liberate Minnesota, Liberate Michigan, Liberate Virginia, and save your great 2nd Amendment. It is under siege!| [Link]()|
|April 20th | Nobody that needed a ventilator in this country didn’t get one. | [Link](https://www.whitehouse.gov/briefings-statements/remarks-president-trump-vice-president-pence-members-coronavirus-task-force-press-briefing-29/)

### Other timelines of Trump's (and others') public statements

[TheBulwark.com](https://thebulwark.com/a-timeline-of-trumps-press-briefing-lies/)

[Factcheck.org](https://www.factcheck.org/2020/03/trumps-statements-about-the-coronavirus/)

[Foxnews.com](https://www.foxnews.com/politics/from-new-york-to-canada-to-the-white-house-initial-coronavirus-responses-havent-aged-well)

[NPR.org](https://www.npr.org/2020/04/21/837348551/timeline-what-trump-has-said-and-done-about-the-coronavirus)

[NYtimes.com](https://www.nytimes.com/2020/03/15/opinion/trump-coronavirus.html)


