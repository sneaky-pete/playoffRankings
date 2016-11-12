__author__ = 'P_Hlawitschka'

import os
import time
import timeit
import requests
import csv
from bs4 import BeautifulSoup

# Where the code file is saved
os.chdir('H:\git\playoffRankings')

######################################################
### Scrape pro-football-reference.com for NFL playoff result data
######################################################

# Where the data is saved
os.chdir('H:\playoffRankingData')

rows = []
for year in xrange(1980,2016):

	# Grab the HTML
	url = 'http://www.pro-football-reference.com/years/' + str(year)

	# Scrape the HTML at the url
	r = requests.get(url)

	# The site comments out the tables when it first loads for some reason
	# Fix that
	text = r.text.replace('<!--', '').replace('-->', '')

	# Turn the HTML into a Beautiful Soup object
	soup = BeautifulSoup(text, 'lxml')

	# Grab the table
	results = soup.find('table', {'id': 'playoff_results'})	

	for i in xrange(1,len(results.find_all('tr'))):
		row = results.find_all('tr')[i]
		cells = [val.text.encode('utf8') for val in row.find_all(['td','th'])]
		cells.append(year)
		rows.append(cells)

# Write to a CSV file
with open("nflPlayoffResults.csv", "wb") as f:
    writer = csv.writer(f)
    writer.writerows(rows)


######################################################
### Scrape Wikipedia for NFL playoff seeding data  ###
######################################################

rows = []
for year in xrange(1980,2016):


	url = 'https://en.wikipedia.org/wiki/' + str(year) + '-' + str(year+1)[2:] + '_NFL_playoffs'
	# Scrape the HTML at the url
	r = requests.get(url)
	text = r.text
	# Turn the HTML into a Beautiful Soup object
	soup = BeautifulSoup(text, 'lxml')


	# Find the table with seed data
	try:
		seedTable = soup.find('td', text='Playoff seeds').find_parent('table')
		for i in xrange(2,len(seedTable.find_all('tr'))):
			row = seedTable.find_all('tr')[i]
			cells = [val.text.encode('utf8') for val in row.find_all(['td','th'])]
			cells.append(year)
			rows.append(cells)
	except:
		print year
		continue

# Manually input 1985 season
cells = ['1', 'Denver Broncos', 'San Francisco 49ers', 1989]
rows.append(cells)
cells = ['2', 'Cleveland Browns', 'New York Giants', 1989]
rows.append(cells)
cells = ['3', 'Buffalo Bills', 'Minnesota Vikings', 1989]
rows.append(cells)
cells = ['4', 'Houston Oilers', 'Philadelphia Eagles', 1989]
rows.append(cells)
cells = ['5', 'Pittsburgh Steelers', 'Los Angeles Rams', 1989]
rows.append(cells)

# Manually input 1989 season
cells = ['1', 'Los Angeles Raiders', 'Chicago Bears', 1985]
rows.append(cells)
cells = ['2', 'Miami Dolphins', 'Los Angeles Rams', 1985]
rows.append(cells)
cells = ['3', 'Cleveland Browns', 'Dallas Cowboys', 1985]
rows.append(cells)
cells = ['4', 'New York Jets', 'New York Giants', 1985]
rows.append(cells)
cells = ['5', 'New England Patriots', 'San Francisco 49ers', 1985]
rows.append(cells)

# Write to a CSV file
with open("nflPlayoffSeeds.csv", "wb") as f:
	writer = csv.writer(f)
	writer.writerows(rows)



######################################################
### Scrape basketball-reference.com for NBA playoff result data
######################################################


# Where the data is saved
os.chdir('H:\playoffRankingData')

# Grab the HTML
url = 'http://www.basketball-reference.com/playoffs/series.html'

# Scrape the HTML at the url
r = requests.get(url).text

# Turn the HTML into a Beautiful Soup object
soup = BeautifulSoup(r, 'lxml')

# Grab the table
results = soup.find('table', {'id': 'playoffs_series'})	

rows = []
for i in xrange(1,len(results.find_all('tr'))):
	row = results.find_all('tr')[i]
	cells = [val.text.encode('utf8') for val in row.find_all(['td','th'])]
	rows.append(cells)

# Write to a CSV file
with open("test.csv", "wb") as f:
    writer = csv.writer(f)
    writer.writerows(rows)




######################################################
### Scrape baseball-reference.com for MLB playoff result data
######################################################

# Where the data is saved
os.chdir('H:\playoffRankingData')

rows = []

# Grab the HTML
url = 'http://www.baseball-reference.com/postseason/'

# Scrape the HTML at the url
r = requests.get(url)
text = r.text

# Turn the HTML into a Beautiful Soup object
soup = BeautifulSoup(text, 'lxml')

# Grab the table
results = soup.find('table', {'class': 'stats_table'})	

# Grab all of the table cells
for i in xrange(0,len(results.find_all('tr'))):
	row = results.find_all('tr')[i]
	if str(row.find('td', {'class': 'bold_text'})) != 'None':
		year = row.find('td', {'class': 'bold_text'}).text.encode('utf8').strip()
	for i in xrange(0, len(row.find_all('td'))):
		cell = row.find_all('td')[i]
		value = cell.text.encode('utf8').strip()
		rows.append(year + ' ' + value)	
	
with open('mlbPlayoffResults.csv', 'wb') as f:
    writer = csv.writer(f)
    for val in rows:
        writer.writerow([val])    


######################################################
### Scrape Wikipedia for MLB playoff seeding data  ###
######################################################

rows = []
for year in xrange(1980,2017):

	url = 'https://en.wikipedia.org/wiki/' + str(year) +'_Major_League_Baseball_season'
	# Scrape the HTML at the url
	r = requests.get(url)
	text = r.text
	# Turn the HTML into a Beautiful Soup object
	soup = BeautifulSoup(text, 'lxml')

	# Find all rows with a yellow background, which are the playoff teams on the Wikipedia page
	playoffTeams = soup.findAll('tr', style = "background:#CCFFCC")
	# Sometimes the yellow-background teams are coded using 'bgcolor' instead of 'background'
	playoffTeams.extend(soup.findAll('tr', bgcolor = "#CCFFCC"))
	try:	
		for i in xrange(0,len(playoffTeams)):
			cells = [val.text.encode('utf8') for val in playoffTeams[i].find_all(['td'])]
			cells.append(year)
			rows.append(cells)
	except:
		print year
		continue    


# Write to a CSV file
with open("mlbPlayoffSeeds.csv", "wb") as f:
	writer = csv.writer(f)
	writer.writerows(rows)
















#########################################################
### Scrape hockey-reference for NHL playoff outcomes  ###
#########################################################


# Where the data is saved
os.chdir('H:\playoffRankingData')

rows = []


# Grab the HTML
for year in xrange(1994, 2014):
	# Strike in '05
	if year == 2005:
		continue

	url = 'http://www.hockey-reference.com/playoffs/NHL_' + str(year) +'.html'
	r = requests.get(url)
	text = r.text

	# Turn the HTML into a Beautiful Soup object
	soup = BeautifulSoup(text, 'lxml')

	# Grab the table
	# Set recursive = F in find_all, because there are some rows you can un-collapse for more details and we DON'T want those
	results = soup.find('table', {'id': 'all_playoffs'}).find('tbody').find_all('tr', class_=lambda x: x != 'toggleable', recursive=False)

	# Grab all of the table cells
	for i in xrange(0,len(results)):
		# Find all of the td cells in that row
		row = results[i].find_all('td')
		# And throw them all into a list, so td can be put in its own column
		data = []
		for j in xrange(0, len(row)):
			cell = row[j]
			value = cell.text.encode('utf8').strip()
			data.append(value)
		data.append(year)
		rows.append(data)

with open('nhlPlayoffResults.csv', 'wb') as f:
	writer = csv.writer(f)
	writer.writerows(rows) 

######################################################
### Scrape Wikipedia for NHL playoff seeding data  ###
######################################################

rows = []
for year in xrange(1994, 2014):
	if year == 2005:
		continue
	url = 'https://en.wikipedia.org/wiki/'+ str(year) + '_Stanley_Cup_playoffs'
	# Scrape the HTML at the url
	r = requests.get(url)
	text = r.text
	# Turn the HTML into a Beautiful Soup object
	soup = BeautifulSoup(text, 'lxml')

	# Wikipedia lists the playoff seeding in ordered-list divs
	playoffTeams = soup.findAll('ol')
	try:	
		# Eastern conf
		cells = [val.text.encode('utf8') for val in playoffTeams[0].find_all(['li'])]
		for j in xrange(0, len(cells)):
			team = [cells[j]]
			team.append(year)
			team.append(j+1)
			team.append('Eastern')
			rows.append(team)
		# Western conf
		cells = [val.text.encode('utf8') for val in playoffTeams[1].find_all(['li'])]
		for j in xrange(0, len(cells)):
			team = [cells[j]]
			team.append(year)
			team.append(j+1)
			team.append('Western')
			rows.append(team)
	except:
		print year
	continue    


# Write to a CSV file
with open("nhlPlayoffSeeds.csv", "wb") as f:
	writer = csv.writer(f)
	writer.writerows(rows)
	




















for i in xrange(0,len(results)):
	# Find all of the td cells in that row
	row = results[i].find_all('td')
	# And throw them all into a list, so td can be put in its own column
	data = []
	for j in xrange(0, len(row)):
		cell = row[j]
		value = cell.text.encode('utf8').strip()
		data.append(value)
	data.append(year)
	rows.append(data)