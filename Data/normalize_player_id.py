import csv

player_id = []

with open("player_profile.csv","rU",) as player_file:
	r_player = csv.reader(player_file,delimiter = ',', quotechar = '|')
	for player in r_player:
		player_id.append(player[0])
	player_id = list(set(player_id))
player_file.close()

player_wrong_id = []



for ID in player_id:
	player_wrong_id.append(ID[1:len(ID)])


def find_good_ID(false_ID):
	for i in range(1,len(player_wrong_id)):
		if false_ID == player_wrong_id[i]:
			return player_id[i]


with open("playoff_statistics.csv","rU") as playoff_file:
	r_playoff = csv.reader(playoff_file,delimiter = ',', quotechar = '|')
	header = next(r_playoff)
	with open("playoff_statistics_temp.csv","wb") as playoff_temp:
		w_temp = csv.writer(playoff_temp,delimiter = ',', quotechar = '|')
		w_temp.writerow(header)
		for row in r_playoff:
			for j in range(4,9):
				row[j] = find_good_ID(row[j])
			w_temp.writerow(row)			
	playoff_temp.close()
playoff_file.close()

