# coding: utf-8

from pycitique import *

## Traitement parallélisé de l’extraction météorologique pour les animaux:

start  = perf_counter()

'------------------------------------------------------'

threadlist = []

for n in range(1,16):
	try:
		inputschema  = 'citik'
		inputable    = 'citik_animaux_clean'
		outputschema = 'meteo'
		outputable   = 'citik_animaux_clean_weather_strict_raw_jm{}'.format(n)
		dateOffset   = n
		reverser = Pycitique()
		t = Thread( target = reverser.dskextractorDB , args=[inputschema, inputable, outputschema, outputable, dateOffset] )
		t.start()
		threadlist.append(t)
	except RuntimeError as error:
		pass

for thread in threadlist:
	thread.join()

finish = perf_counter()

print('------------------------------------------------------')

duration = round(finish - start, 2)
print(f'it took {duration} seconds to finish')
