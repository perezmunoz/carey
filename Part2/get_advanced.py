# Provide the .csv files of player's basic statistics and salaries

from bs4 import BeautifulSoup
import urllib, string, time, csv, os, random, re

# Scrap player's basic statistics
# Return player's Totals, Per Game, Salaries and Contract tables as .csv files
# Reminder: player's links are like /players/letter/name.html
def get_statistics_player(link_player):
    urlHandle = urllib.urlopen('http://www.basketball-reference.com'+link_player)
    html = urlHandle.read()
    soup = BeautifulSoup(html)
    # Construct the id of the player: letter+name
    # For /players/a/abdulal01.html, the player_id is aabdulal01, e.g letter+name
    player_id = (link_player.split('/')[2]+link_player.split('/')[3].split('.')[0]).encode('utf-8')
    # Getting relevant tables into the dictionary dict
    adv = soup.find(id='advanced')
    # Table adv is not empty and has a tbody 
    if adv is not None and adv.tbody is not None:
        write_table(adv,player_id)
    else:
        print 'Table advanced is missing for player '+player_id

# Writes the data contained in Totals or Per Game tables into totals.csv and per_game.csv files
def write_table(table,player_id):
    # Table is definitly not None here
    with open('advanced.csv', 'a') as output:
        # Get the body containing the data and store all tables's lines in a list
        body = table.tbody.find_all('tr')
        for row in body:
            # Get each value of the line and store it in a list
            data = row.find_all('td')
            # Writes the player_id at the beginning of the line
            output.write(player_id+',')
            # Range builts vector from 0: [0,1,..,29] for 30 elements
            for i in range(len(data)-1):
                # Special case #1 (unicode character such as a star). Usually in Season column
                if data[i].span is not None:
                    # Removes the span tag
                    data[i].span.extract()
                    output.write(data[i].get_text().encode('utf-8')+',')
                # Normal case
                else:
                    output.write(data[i].get_text().encode('utf-8')+',')
            # Writing the last element. Avoid having one more comma 
            output.write(write_last_element_of_data(data[-1]))

# Writes the last element of each line
# Avoid having an extra comma at the end of the line and consider the case didn't play during that Season
def write_last_element_of_data(e):
    # Special case #2 (Did not play this season)
    if e.get('colspan') is not None:
        return (','*(int(str(e.get('colspan')))-1)).encode('utf-8')+'\n'
    # Avoid unicode characters causing troubles
    elif e.span is not None:
        e.span.extract()
        return e.get_text().encode('utf-8')+'\n'
    # Normal case
    else:
        return e.get_text().encode('utf-8')+'\n'

from decimal import *
# Numbers displayed in progress computing
getcontext().prec = 3

# Main function computing all player's statistics and salaries
def all_statistics_players():
    # Total of attributes: 28
    advanced_header = 'PlayerID,Season,Age,Tm,Lg,Pos,G,MP,PER,TS%,3PAr,FTr, ORB%,DRB%,TRB%,AST%,STL%,BLK%,TOV%,USG%,Xx,OWS,DWS,WS,WS/48,Yy,OBPM,DBPM,BPM,VORP'
    # Write header to totals.csv, per_game.csv and salaries.csv
    write_header('advanced.csv',advanced_header)
    # Get all player's links
    res = open('active_players_2011.csv').readlines()
    for player in res:
        time.sleep(2)
        letter = player[0]
        name = player[1:].replace('\n','')
        get_statistics_player('/players/'+letter+'/'+name+'.html')
        print player+' done'
        print str(Decimal(res.index(player))/Decimal((len(res)-1))*Decimal(100))+' % computed\n'
    # Cleaning phase: resolution of bugs #4 and #5
    print 'Cleaning process starts'
    cleaning_csv('advanced')
    print 'Cleaning process ends'
    # Removing temporary files
    os.remove('advanced.csv')

# Writes header to each .csv file
def write_header(file_name,header):
    with open(file_name, 'w') as output:
        output.write(header.encode('utf-8')+'\n')

# Cleans the Season'format. Ex: for 2013-14, we keep 2013
# Returns a new .csv file: file_name_final.csv
def cleaning_csv(file_name):
    with open(file_name+'.csv', 'rb') as file_input:
        with open(file_name+'_final.csv', 'wb') as file_output:
            reader_data = csv.reader(file_input, delimiter = ',')
            writer_data = csv.writer(file_output)
            # Write the header to file_output by taking the first line in file_input
            writer_data.writerow(reader_data.next())
            # e is a row of the csv file in input and represented in a list format
            for row in reader_data:
                # Getting year season's start year
                row[1] = row[1][:4]
                # Writing the corrected line
                writer_data.writerow(row)


all_statistics_players()