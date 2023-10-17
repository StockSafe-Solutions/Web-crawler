import csv
import requests
import zipfile

CSV_URL = "https://archive.ics.uci.edu/static/public/396/sales+transactions+dataset+weekly.zip"
with requests.Session() as s:
    download = s.get(CSV_URL)
    with open('sales+transactions+dataset+weekly.zip', 'wb') as f:
        f.write(download.content)

f = gzip.open('sales+transactions+dataset+weekly.zip', 'rt')
file_content=f.read()

cr = csv.reader(file_content.splitlines(), delimiter=',')
my_list = list(cr)
for row in my_list:
    print(row)