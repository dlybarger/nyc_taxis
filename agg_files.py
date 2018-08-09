import os,sys
import datetime as dt

filename=sys.argv[1]
outfilename=sys.argv[2]

data={}


infile=open(filename)
readLN=infile.readline()

for readLN in infile:
    readLN=readLN[:-1]
    temp=readLN.split(",")
    try:
        temp[0]
    except:
        continue
    else:
        if len(temp)>1:
            temp[0]=str(temp[0])            
            temp2=temp[1].split(' ')
            temp[1]=temp2[0]
            try:
                data[(temp[0],temp[1])]
            except:
                data[(temp[0],temp[1])]=1
            else:
                data[(temp[0],temp[1])]+=1
        
infile.close()

outfile=open(outfilename, "w")
kys=data.keys()
outfile.write("vendor,date,rides\n")
for k  in kys:
    outfile.write(str(k[0])+","+str(k[1])+","+str(data[(k[0],k[1])])+"\n")

outfile.close()
    
    