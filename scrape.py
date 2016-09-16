__author__ = 'P_Hlawitschka'

# Where the code file is saved
os.chdir('H:\git\playoffRankings')

import os
import time
import timeit
import requests
import csv
from bs4 import BeautifulSoup


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
