import os,sys
import datetime as dt

filename=sys.argv[1]
outfilename=filename+"_formatted.csv"
print (outfilename)
columns_needed=int(sys.argv[2])

yellow_columns=[0,1,3,4,7,16]
green_columns=[0,1,5,7,8,16]
fhv_columns=[0,1]

if(columns_needed==0):
    columns_get=yellow_columns
elif(columns_needed==1):
    columns_get=green_columns
elif(columns_needed==2):
    columns_get=fhv_columns
print (columns_get)
linecount=0
flag_open=0
outline=""
infile=open(filename)
for readLN in infile:
    readLN=readLN[:-1]
    temp=readLN.split(",")
    if len(temp)>1:        
        linecount+=1
        temp2=temp[1].split(" ")
        temp[1]=temp2[0]
        outline+=temp[columns_get[0]]
        for i in range(1,len(columns_get)):
            outline+=","+temp[columns_get[i]]
        outline+="\n"
        if (linecount % 50000)==0:
            if flag_open==0:
                outfile=open(outfilename,"w")
                outfile.write(outline)
                outfile.close()
                outline=""
                flag_open+=1
            else:
                outfile=open(outfilename,"a")
                outfile.write(outline)
                outfile.close()
                outline=""

outfile=open(outfilename,"a")
outfile.write(outline)
outfile.close()
outline=""
print(linecount)
            