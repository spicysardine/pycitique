#coding:utf-8

### Pycitique.py #################################################
#
#
#Author: Hilami Khaldoune
#Organism: CNRS
#Year: 2020
#License: This script is propriety of CNRS
#Descriptions: * This module takes a folder in the system and
#looks for csv files. If it finds none, it exits. If it finds any
#it imports them to the specified schema into the database
#
##################################################################

### library IMPORTS csv inserter ###
from psycopg2			import connect, Error
from psycopg2.extras 	import DictCursor
from psycopg2			import errors as PgError

###library IMPORTS pyreverser ###
from geocoder		import osm

### library IMPORTS darkskyextractorCSV ###
from darksky		import forecast # This package is the official darkskylib library installed with pip, it is not available in apt repos.
from datetime		import datetime as dt

### Parallel computing libraries
from threading import Thread
from multiprocessing import Process

### Time functions
from time import perf_counter, sleep

### other library IMPORTS ###
from re			import search
from csv			import reader, writer, QUOTE_ALL, DictReader, DictWriter
from itertools		import islice
from os			import chdir, getcwd, listdir
from collections	import OrderedDict




class Pycitique:




	def __init__(self):

		self.conn = connect(host='localhost', database='localbase10', user='beetroot', password='root', port=5432)
		self.server = 'http://localhost/nominatim/'
		self.count = 0
		self.APIkey= '865f840cdde4ab359bce9a5adee70f84'
		self.rglist = ['village','ville','chef_lieu', 'departement', 'departement_code','region','code_postal','pays','code_pays','adresse_complette']
		self.glist = ['lat', 'lon', 'quartier', 'ville', 'departement', 'departement_code', 'region', 'code_postal', 'pays', 'code_pays', 'adresse_complette']

		self.wobjlist = ['timezone','time','summary','icon','sunriseTime','sunsetTime','moonPhase',
					'precipIntensity','precipIntensityMax','precipIntensityMaxTime','precipAccumulation',
						'precipProbability','temperatureHigh','temperatureHighTime','temperatureLow','temperatureLowTime',
							'dewPoint','humidity','pressure','visibility','windGust','windGustTime','windSpeed','windBearing',
								'cloudCover','cloudCoverError','uvIndex','uvIndexTime','temperatureMin','temperatureMinTime','temperatureMax',
									'temperatureMaxTime','apparentTemperatureMin','apparentTemperatureMinTime','apparentTemperatureMax','apparentTemperatureMaxTime',
										'apparentTemperatureHigh','apparentTemperatureHighTime','apparentTemperatureLow','apparentTemperatureLowTime','DataSource']



	def insertcsvtable(self, inputdir, inputfile, outputschema, delimiter, quotechar):

		try:

			curs = self.conn.cursor()

			with open(inputdir+inputfile+'.csv', 'r') as DataInputFile:
				filestream = reader(DataInputFile, delimiter=delimiter , quotechar=quotechar)
				createschemasql = "CREATE SCHEMA IF NOT EXISTS {}".format(outputschema)
				curs.execute(createschemasql)
				droptablesql = "DROP TABLE IF EXISTS {}.{} ;".format(outputschema, inputfile)
				curs.execute(droptablesql)
				for line in islice(filestream,0,1):
					headerlist=line
				sqlformat = ''
				for col in headerlist:
					sqlformat += col+' varchar,'
				sqlformat = sqlformat[:-1]
				createtablesql = "CREATE TABLE {}.{} ({});".format( outputschema, inputfile, sqlformat )
				curs.execute(createtablesql)
				self.conn.commit()
				loop = 0
				for row in filestream:
					values = ''
					for field in row:
						values += "'"+field.replace("'", "’")+"',"
					values = values[:-1]
					insertintotablesql = 'INSERT INTO {}.{} VALUES ({}) ;'.format(outputschema, inputfile, values)
					loop +=1
					curs.execute( insertintotablesql )
					self.conn.commit()
					del values
				print("The Table "+inputfile+" is written to the database with ",loop," rows")
				self.count += loop

			curs.close()

		except (Exception, Error) as error:
			print ("Error while connecting to PostgreSQL", error)



	def batchinsertcsvtable(self, inputdir, outputschema, delimiter, quotechar):

		inputfilelist = listdir(inputdir)

		for inputfile in inputfilelist:
			if search(r'\.[cC][sS][vV]$', inputfile) is not None :
				inputfile = inputfile[0:-4]
				print("\nThe table ",inputfile," is being written to the specified schema of the database.")
				self.insertcsvtable(inputdir, inputfile, outputschema, delimiter, quotechar)
				print("\nThe table was inserted successfully to database. Operation completed.")
#			else:
#			     print("Wrong file type or path.")

		print("\nAll tables written successfully to database. Operation completed.")
		print("\nA total of ", self.count," rows is inserted to database.")
		self.count = 0
		self.conn.close()
		print("\nPostgreSQL connection is closed.\n")



	def rgeocoderCSV(self, inputdir, inputfile, outputdir, delimiter, quotechar):

		with open(inputdir+inputfile+'.csv', 'r') as DataInputFile:

			filestream = reader(DataInputFile, delimiter=delimiter , quotechar=quotechar )
			# Selecting only the first line of the file, usualy containing the deader
			# Converting the first line to a python list, in preparation of the iteration
			for line in islice(filestream,0,1):
				headerlist=line
			# Appending reverse geocoding fields to header list
			headerlist+=self.rglist
			with open(outputdir+inputfile+'_revgeocoded.csv', 'a') as DataOutputFile:
				streamwriter = writer( DataOutputFile, delimiter = delimiter, quotechar = "'", quoting=QUOTE_ALL   )
				# Header writing to the file
				streamwriter.writerow(headerlist)
				print("File headers writing is complete. Beginning  data extraction precess...\n")
				print("...\n")
				missingObjectCount=0
				loop=0
				row=''
				for field in filestream:
					loop+=1
					lat = float(field[1])
					lng = float(field[2])
					location = osm( [ lat , lng ], method='reverse', url=self.server)
					if location.address is not None: location.address=location.address.replace( ',' ,';')
					if location.address is not None: location.address=location.address.replace( "'" ,'’')
					if location.state   is not None: location.state = location.state.replace( "'" ,'’')
					if location.postal  is not None  and len(location.postal) < 5: location.postal = '0'+location.postal
					if location.address is not None and location.postal is not None:
						if   search(r';\sLyon;', location.address)    is not None: department_code = '69M'
						elif search('Corse-du-Sud', location.address) is not None: department_code = '2A'
						elif search('Haute-Corse' , location.address) is not None: department_code = '2B'
						elif search('Villefranche-sur-Saône', location.address) is not None: department_code = '69D'
						else: department_code = location.postal[0:-3]
					else: department_code = ''
					department_geocoded = osm( department_code , url=self.server )
					department = department_geocoded.county
					if department_code  == '14': department = 'Calvados'
					if department_code  == '27': department = 'Eure'
					if department is not None: department = department.replace( "'" ,'’')

					field+=[location.village, location.town, location.county, department, department_code, location.state,
													location.postal, location.country, location.country_code, location.address]
					field = [str(i or '') for i in field]
					try:
						streamwriter.writerow(field)
						print('Object '+str(loop)+' Written\n')
					except ValueError:
						print('Could not convert string to float!')
						missingObjectCount+=1
						print('The program will continue...')
			self.count += loop

		if (missingObjectCount>0):
			print('There was', missingObjectCount, 'missing dataPoint objects during the extraction process.\n')
		else:
			print('Congratulations! Extraction finished without missing dataPoint objects! You may check your file.\n')
			print(str(loop)+' Objects Written to the last file\n')


#	@static
	def batchrgeocoderCSV(self, inputdir, outputdir, delimiter, quotechar):

		inputfilelist=listdir(inputdir)

		for inputfile in inputfilelist:
			if search(r'\.[cC][sS][vV]$', inputfile) is not None :
				inputfile=inputfile[0:-4]
				self.rgeocoderCSV( inputdir, inputfile, outputdir, delimiter, quotechar )

		print("\nA total of ", self.count," rows is written\n.")
		self.count = 0



	def darkskyextractorCSV(self, inputdir, inputfile, outputdir, delimiter, quotechar):

		missingObjectCount=0
		loop=0
		headerlist = []
		valdict = {}

		with open(inputdir+inputfile+'.csv', 'r') as weatherDataInputFile:

			filestream = DictReader(weatherDataInputFile, delimiter=delimiter, quotechar = quotechar)
			# Header Construction
			for header in islice(filestream,0,1):
				for fieldname in header.keys():
					headerlist.append(fieldname)

			headerlist += self.wobjlist

			print("File headers construction is complete...\n")
			print("...\n")

			with open(self.outputdir+inputfile+'_weather.csv', 'a') as weatherDataOutputFile:

				streamwriter = DictWriter( weatherDataOutputFile, fieldnames = headerlist, delimiter = delimiter, quotechar = "'", quoting=QUOTE_ALL)
				# Header writing to the file
				streamwriter.writeheader()
				print("File headers writing is complete. Beginning  data extraction precess...\n")
				print("...\n")
				# Main extraction loop
				for row in filestream:
					try:
						date_piqure = row['date_piqure_saisie']

						if search(r'^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*/\s*(0?[1-9]|1[0-2])?\s*/\s*(19|20)[0-9][0-9]\s*$', date_piqure) :
							date = date_piqure
							datelist = date.split('/')
							timep = dt(int(datelist[2]), int(datelist[1]), int(datelist[0]) ).isoformat()
							lat = float(row['lat'])
							lon = float(row['lon'])

							location = forecast(self.APIkey, lat, lon, lang='en', units='si', time=timep, exclude='currently,minutely,hourly,alerts' )

							data = location['daily']
							datalist = data.get('data', None)
							weatherdict = datalist[0]
							for key, value in weatherdict.items():
								if search(r'[tT]ime$' , key ) is not None:
									weatherdict[key] = dt.utcfromtimestamp(value).strftime('%d-%m-%y %H:%M:%S')
								else: weatherdict[key] = value
							for wobj in self.wobjlist:
								valdict[wobj] = weatherdict.get(wobj, '')
							valdict['timezone']   = location.timezone
							valdict['DataSource'] = location.flags.sources
							for key, value in valdict.items():
								row[key] = value
							streamwriter.writerow(row)
							loop += 1
							print('Object ', loop,' Written\n')
							del date
							del date_piqure
						else:
							print('Date format is illegal...row will be skipped')
							missingObjectCount+=1
							pass
					except (Exception, AttributeError, ValueError, KeyError, TypeError, IndexError ) as error:
						missingObjectCount+=1
						print('The object is missing!', error)
						print('Weather Object is missing...row will be skipped')
						print('Extraction will continue...')

		# Input file is closed at this point

		if (missingObjectCount>0):
			print("There was", missingObjectCount, "missing dataPoint objects during the extraction process.")
		else:
			print("Congratulations! Extraction finished without missing dataPoint objects! You may check your file.")



	def batchdarkskyextractorCSV(self, inputdir, inputfile, outputdir, delimiter, quotechar):

		inputfilelist=listdir(inputdir)

		for inputfile in inputfilelist:
			if search(r'\.[cC][sS][vV]$', inputfile) is not None :
				inputfile=inputfile[0:-4]
				self.darkskyextractorCSV( inputdir, inputfile, outputdir, delimiter, quotechar )

		print("\nA total of ", self.count," rows is written\n.")
		self.count = 0



	def rgeocoderDB(self, inputschema, inputable, outputschema, outputable):

		missingObjectCount = 0
		loop = 0

		print('Connection to database')
		curs = self.conn.cursor(cursor_factory = DictCursor)
		print('Creating DictCursor')
		inputsql = "select * from {}.{};" .format( inputschema, inputable)
		curs.execute(inputsql)
		print('Executing query...Data is being fetched...')
		revdata = curs.fetchall()
		print('Data dictionary is fetched.')
		print('File creation is beginning.')

		createschemasql = "CREATE SCHEMA IF NOT EXISTS {}".format(outputschema)
		curs.execute(createschemasql)

		droptablesql = "DROP TABLE IF EXISTS {}.{} ;".format(outputschema, outputable)
		curs.execute(droptablesql)
		typetablesql = "select column_name, data_type from information_schema.columns where table_name = '{}';".format(inputable)
		curs.execute(typetablesql)
		typetable = curs.fetchall()
		fieldnames = ''
		for row in typetable:
			if row['data_type'] == 'USER-DEFINED' : row['data_type'] = 'geometry'
			fieldnames += "{} {},".format(row['column_name'], row['data_type'])

		for field in self.rglist:
			fieldnames += field+' varchar,'
		fieldnames = fieldnames[:-1]

		createtablesql = "CREATE TABLE {}.{} ({});".format( outputschema, outputable, fieldnames )
		curs.execute(createtablesql)

		self.conn.commit()
		print("Table creation is complete. Beginning  data extraction precess...\n")
		print("...\n")

		for row in revdata:

			try:

				loop+=1
				valdict = OrderedDict()
				lat = row['lat']
				lon = row['lon']

				location = osm( [ lat , lon ], method='reverse', url=self.server)

				if location.address is not None and location.postal is not None:
					if   search(r',\sLyon,', location.address)    is not None: department_code = '69M'
					elif search('Corse-du-Sud', location.address) is not None: department_code = '2A'
					elif search('Haute-Corse' , location.address) is not None: department_code = '2B'
					elif search('Villefranche-sur-Saône', location.address) is not None: department_code = '69D'
					elif location.county is None: location.county = ''
					else: department_code = location.postal[0:-3]
				else: department_code = ''

				department_geocoded = osm( department_code , url=self.server )
				department = department_geocoded.county

				rgdict = OrderedDict()

				rgdict['village'] = location.village
				rgdict['ville'] = location.town
				rgdict['chef_lieu'] = location.county
				rgdict['departement'] = department
				rgdict['departement_code'] = department_code
				rgdict['region'] = location.state
				rgdict['code_postal'] = location.postal
				rgdict['pays'] = location.country
				rgdict['code_pays'] = location.country_code
				rgdict['adresse_complette'] =  location.address

				for robj in self.rglist:
					valdict[robj] = rgdict.get(robj, '')
					if valdict[robj] is None: valdict[robj] = ''

				row = OrderedDict(row)
				row.update(valdict)
				values= ''
				for key, value in row.items() :
					value = str(value)
					values += "'"+value.replace("'", "’")+"',"
				values = values[:-1]
				insertintotablesql = 'INSERT INTO {}.{} VALUES ({}) ;'.format(outputschema, outputable, values)
				curs.execute( insertintotablesql )
				self.conn.commit()
				del values
				del valdict

				print('Row '+str(loop)+' Written\n')

			except ValueError:
				print('Could not convert string to float!')
				missingObjectCount+=1
				print('The program will continue...')

		if (missingObjectCount>0):
			print('There was', missingObjectCount, 'missing dataPoint objects during the extraction process.\n')
		else:
			print('Congratulations! Reverse-Geocoding finished without missing any dataPoint objects! You may check your table.\n')
			print(str(loop)+' Objects Written to the table\n')
		curs.close()
		self.conn.close()



	def geocoderDB(self, inputschema, inputable, outputschema, outputable):

		missingObjectCount = 0
		loop = 0

		print('Connection to database')
		curs = self.conn.cursor(cursor_factory = DictCursor)
		print('Creating DictCursor')
		inputsql = "select * from {}.{};" .format( inputschema, inputable)
		curs.execute(inputsql)
		print('Executing query...Data is being fetched...')
		gdata = curs.fetchall()
		print('Data dictionary is fetched.')
		print('File creation is beginning.')

		createschemasql = "CREATE SCHEMA IF NOT EXISTS {}".format(outputschema)
		curs.execute(createschemasql)

		droptablesql = "DROP TABLE IF EXISTS {}.{} ;".format(outputschema, outputable)
		curs.execute(droptablesql)
		typetablesql = "select column_name, data_type from information_schema.columns where table_name = '{}';".format(inputable)
		curs.execute(typetablesql)
		typetable = curs.fetchall()
		fieldnames = ''
		for row in typetable:
			if row['data_type'] == 'USER-DEFINED' : row['data_type'] = 'geometry'
			fieldnames += "{} {},".format(row['column_name'], row['data_type'])

		for field in self.glist:
			fieldnames += field+' varchar,'
		fieldnames = fieldnames[:-1]

		createtablesql = "CREATE TABLE {}.{} ({});".format( outputschema, outputable, fieldnames )
		curs.execute(createtablesql)

		self.conn.commit()
		print("Table creation is complete. Beginning  data extraction precess...\n")
		print("...\n")

		for row in gdata:

			try:

				loop+=1
				valdict = OrderedDict()
				address = row['adresse']
				commune = row['commune']
				query_string = address+", "+commune

				location = osm( query_string, url=self.server)

				if location.address is not None and location.postal is not None:
					if   search(r',\sLyon,', location.address)    is not None: department_code = '69M'
					elif search('Corse-du-Sud', location.address) is not None: department_code = '2A'
					elif search('Haute-Corse' , location.address) is not None: department_code = '2B'
					elif search('Villefranche-sur-Saône', location.address) is not None: department_code = '69D'
					elif location.county is None: location.county = ''
					else: department_code = location.postal[0:-3]
				else: department_code = ''

				gdict = OrderedDict()

				gdict['lat'] = location.lat
				gdict['lon'] = location.lng
				gdict['quartier'] = location.suburb
				gdict['ville'] = location.city
				gdict['departement'] = location.county
				gdict['departement_code'] = department_code
				gdict['region'] = location.state
				gdict['code_postal'] = location.postal
				gdict['pays'] = location.country
				gdict['code_pays'] = location.country_code
				gdict['adresse_complette'] =  location.address

				# Here we put the geocoded values into the empty ordered dictionary valdict
				for gobj in self.glist:
					valdict[gobj] = gdict.get(gobj, '')
					if valdict[gobj] is None: valdict[gobj] = ''

				# a recast of the current row dictionary so the update is possible with geocoded values
				row = OrderedDict(row)
				row.update(valdict)
				values= ''
				# correction of weired characters
				for key, value in row.items() :
					value = str(value)
					values += "'"+value.replace("'", "’")+"',"
				# removal of the trailing comma
				values = values[:-1]
				# insertion of the table in DB
				insertintotablesql = 'INSERT INTO {}.{} VALUES ({}) ;'.format(outputschema, outputable, values)
				curs.execute( insertintotablesql )
				self.conn.commit()
				# a little cleanup at the loop end to avoid confusion and memory saturation for large datasets
				del values
				del valdict

				print('Row '+str(loop)+' Written\n')

			except ValueError:
				print('Could not convert string to float!')
				missingObjectCount+=1
				print('The program will continue...')

		if (missingObjectCount>0):
			print('There was', missingObjectCount, 'missing dataPoint objects during the extraction process.\n')
		else:
			print('Congratulations! Reverse-Geocoding finished without missing any dataPoint objects! You may check your table.\n')
			print(str(loop)+' Objects Written to the table\n')
		curs.close()
		self.conn.close()



	def dskextractorDB(self, inputschema, inputable, outputschema, outputable, dateOffset):

		missingObjectCount=0
		loop=0

		print('Connection to database')
		curs = self.conn.cursor(cursor_factory = DictCursor)
		print('Creating DictCursor')

		inputsql = '''
		SELECT
			id,
			lat,
			lon,
			date_piqure_saisie - {} as date_piqure_saisie,
			annee_extract
		From {}.{}
		where date_piqure_saisie between '01/01/2017' and '05/04/2020'
		order by date_piqure_saisie
		; ''' .format( dateOffset, inputschema, inputable)

		curs.execute(inputsql)
		print('Executing query...Data is being fetched...')
		wdata = curs.fetchall()
		print('Data dictionary is fetched.')
		print('File creation is beginning.')

		createschemasql = "CREATE SCHEMA IF NOT EXISTS {}".format(outputschema)
		curs.execute(createschemasql)

		droptablesql = "DROP TABLE IF EXISTS {}.{} ;".format(outputschema, outputable)
		curs.execute(droptablesql)
		typetablesql = "select column_name, data_type from information_schema.columns where table_name = '{}';".format(inputable)
		curs.execute(typetablesql)
		typetable = curs.fetchall()
		fieldnames = 'id varchar(6), lat float, lon float, date_piqure_saisie_jm{} date, annee_extract text, '.format( dateOffset )
#		fieldnames = ''
#		for row in typetable:
#			if row['data_type'] == 'USER-DEFINED' : row['data_type'] = 'geometry'
#			fieldnames += "{} {},".format( row['column_name'], row['data_type'] )

		for field in self.wobjlist:
			fieldnames += field+' varchar,'
		fieldnames = fieldnames[:-1]

		createtablesql = "CREATE TABLE {}.{} ({});".format( outputschema, outputable, fieldnames )
		curs.execute(createtablesql)

		self.conn.commit()
		print("Table creation is complete. Beginning  data extraction precess...\n")
		print("...\n")

		for row in wdata:

			try:

				date_piqure = str(row['date_piqure_saisie'])
				#DMY iso format
#				if search(r'^\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*-\s*(0?[1-9]|1[0-2])?\s*-\s*(19|20)[0-9][0-9]\s*$', date_piqure) :
				#YMD iso format
				if search(r'^\s*(19|20)[0-9][0-9]\s*-\s*(0?[1-9]|1[0-2])?\s*-\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*$', date_piqure) :

					date = date_piqure
					datelist = date.split('-')
					year = datelist[0]; month = datelist[1] ; day = datelist[2]
					timep = dt(int(year), int(month), int(day) ).isoformat()
					lat = row['lat']
					lon = row['lon']

					location = forecast(self.APIkey, lat, lon, lang='en', units='si', time=timep, exclude='currently,minutely,hourly,alerts' )

					data = location['daily']
					datalist = data.get('data', None)
					weatherdict = datalist[0]

					for key, value in weatherdict.items():
						if search(r'[tT]ime$' , key ) is not None:
							weatherdict[key] = dt.utcfromtimestamp(value).strftime('%d-%m-%y %H:%M:%S')
						else: weatherdict[key] = value

					valdict = OrderedDict()

					for wobj in self.wobjlist:
						valdict[wobj] = weatherdict.get(wobj, '')
					valdict['timezone']   = location.timezone
					valdict['DataSource'] = location.flags.sources
					row = OrderedDict(row)
					row.update(valdict)
					values= ''

					for key, value in row.items() :
						value = str(value)
						values += "'"+value.replace("'", "’")+"',"

					values = values[:-1]
					insertintotablesql = 'INSERT INTO {}.{} VALUES ({}) ;'.format(outputschema, outputable, values)
					curs.execute( insertintotablesql )
					self.conn.commit()
					loop += 1
					print('Object ', loop,' Written\n')
					del date
					del date_piqure
					del values
					del valdict

				else:
					print('Date format is illegal...row will be skipped')
					missingObjectCount+=1
					pass

			except (Exception, AttributeError, ValueError, KeyError, TypeError, IndexError, PgError.DatetimeFieldOverflow) as error:
				missingObjectCount+=1
				print('\nThere was an error!', error)
				print('Weather Object is missing... the row will be skipped but extraction will continue...\n')
				pass

		if (missingObjectCount>0):
			print("There was", missingObjectCount, "missing dataPoint objects during the extraction process.")
			print(str(loop)+' Objects Written to the table', inputable,'\n')
		else:
			print("Congratulations! Extraction finished without missing dataPoint objects! You may check your table.")
			print(str(loop)+' Objects Written to the table', inputable,'\n')
		curs.close()
		self.conn.close()



	def dskstationextractorDB(self, inputschema, inputable, outputschema, outputable):

		curs = self.conn.cursor(cursor_factory = DictCursor)

		datesql = "SELECT cast(date as text) as date_releve FROM  meteo.synop_dates_dsk;"
		curs.execute(datesql)
		ddata = curs.fetchall()

		stationsql = "SELECT id as id_station, lat , lon FROM {}.{} order by id::int;".format(inputschema, inputable)
		curs.execute(stationsql)
		sdata = curs.fetchall()

		createschemasql = "CREATE SCHEMA IF NOT EXISTS {}".format(outputschema)
		curs.execute(createschemasql)

		droptablesql = "DROP TABLE IF EXISTS {}.{} ;".format(outputschema, outputable)
		curs.execute(droptablesql)

		fieldnames = "id_station varchar(5), date_releve date,  lat float, lon float, "

		for field in self.wobjlist:
			fieldnames += field+' varchar,'
		fieldnames = fieldnames[:-1]

		createtablesql = "CREATE TABLE IF NOT EXISTS {}.{} ({});".format( outputschema, outputable, fieldnames )
		curs.execute(createtablesql)

		self.conn.commit()
		print("Table creation is complete. Beginning  data extraction precess...\n")
		print("...\n")

		missingObjectCount=0
		loop=0


		for date in ddata:
			for station in sdata:

				try:
					#YMD iso format
					wdate = date['date_releve']
					if search(r'^\s*(19|20)[0-9][0-9]\s*-\s*(0?[1-9]|1[0-2])?\s*-\s*(0?[1-9]|1[0-9]|2[0-9]|3[0-1])?\s*$', wdate ) :

						datelist = wdate.split('-')
						year = datelist[0]; month = datelist[1] ; day = datelist[2]
						timep = dt(int(year), int(month), int(day) ).isoformat()
						lat = station['lat']
						lon = station['lon']

						location = forecast(self.APIkey, lat, lon, lang='en', units='si', time=timep, exclude='currently,minutely,hourly,alerts' )

						data = location['daily']
						datalist = data.get('data', None)
						weatherdict = datalist[0]

						for key, value in weatherdict.items():
							if search(r'[tT]ime$' , key ) is not None:
								weatherdict[key] = dt.utcfromtimestamp(value).strftime('%d-%m-%y %H:%M:%S')
							else: weatherdict[key] = value

						valdict = OrderedDict()
						valdict['id_station']  = station['id_station']
						valdict['date_releve'] = date['date_releve']
						valdict['latitude']    = station['lat']
						valdict['longitude']   = station['lon']

						for wobj in self.wobjlist:
							valdict[wobj] = weatherdict.get(wobj, '')

						valdict['timezone']   = location.timezone
						valdict['DataSource'] = location.flags.sources

						values= ''

						for key, value in valdict.items() :
							value = str(value)
							values += "'"+value.replace("'", "’")+"',"

						values = values[:-1]
						insertintotablesql = 'INSERT INTO {}.{} VALUES ({}) ;'.format(outputschema, outputable, values)
						curs.execute( insertintotablesql )
						self.conn.commit()
						loop += 1
						print('Object ', loop,' Written\n')
					else:
						print('Date format is illegal...row will be skipped')
						missingObjectCount+=1
						pass

				except (Exception, AttributeError, ValueError, KeyError, TypeError, IndexError, NameError, PgError.DatetimeFieldOverflow) as error:
					missingObjectCount+=1
					print('\nThere was an error!', error)
					print('Weather Object is missing... the row will be skipped but extraction will continue...\n')

		if (missingObjectCount>0):
			print("There was", missingObjectCount, "missing dataPoint objects during the extraction process.")
			print(str(loop)+' Objects Written to the table', outputable,'\n')
		else:
			print("Congratulations! Extraction finished without missing dataPoint objects! You may check your table.")
			print(str(loop)+' Objects Written to the table', outputable,'\n')
		curs.close()
		self.conn.close()

###################################################################   EOS   ################################################################


############################################## DB darkskyextraction  for synop stations ####################################################
#inputschema ='meteo'
#inputable = 'liste_stations_700'
#outputschema = 'meteo'
#outputable = 'darksky_maille_700_extraction'
#reverser = Pycitique()
#reverser.dskstationextractorDB(inputschema, inputable, outputschema, outputable)

############################################## DB darkskyextraction  ####################################################
#inputschema = 'citik'
#inputable = 'citik_animaux_clean'
#outputschema = 'meteo'
#outputable = 'testwdsk'
#reverser=Pycitique()
#reverser.dskextractorDB(inputschema, inputable, outputschema, outputable)

############################################## DB reversegeocoding  ####################################################
#inputschema = 'gis'
#inputable = 'france_maille_700'
#outputschema = 'meteo'
#outputable = 'liste_stations_700_duplicate_youcef'
#reverser=Pycitique()
#reverser.rgeocoderDB(inputschema, inputable, outputschema, outputable)

############################################## csv reversegeocoding  ####################################################
#inputdir = '/home/beetroot/Developer/python/CNRS/projetCitique/projetNominatim/inputest/'
#inputfile = 'citik_animaux_2018'
#outputdir = '/home/beetroot/Developer/python/CNRS/projetCitique/projetNominatim/inputest/'
#delimiter = ','
#quotechar = "'"
#reverser = Pycitique()
##reverser.rgeocoderCSV(inputdir, inputfile, outputdir, delimiter, quotechar)
#reverser.batchrgeocoderCSV(inputdir, outputdir, delimiter, quotechar)

########################################### DB insertion ######################
#inserter = Pycitique()
#inputdir = '/home/beetroot/Developer/GIS/geoCSV/projetHoussam'
#inputfile = 'parc_immo_houssam'
#outputschema = 'gis'
#delimiter = ';'
#quotechar = "'"
#inserter.insertcsvtable(inputdir, inputfile, outputschema, delimiter, quotechar)

########################################### Batch DB insertion ######################
inserter = Pycitique()
inputdir = '/home/beetroot/Developer/GIS/geoCSV/france_resencement_historique_1986-2017/france_metro_base_communes_serie_historique_1986_2017/'
outputschema = 'demography'
delimiter = ';'
quotechar = "'"
inserter.batchinsertcsvtable(inputdir, outputschema, delimiter, quotechar)

########################################### DB geocodage ######################
#inserter = Pycitique()
#inputschema = 'gis'
#inputable = 'parc_immo_houssam'
#outputschema = 'gis'
#outputable = 'parc_immo_houssam_geocoded'
#inserter.geocoderDB(inputschema, inputable, outputschema, outputable)
