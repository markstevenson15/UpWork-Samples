
import sys

import pandas as pd

import numpy as np

class GridLocality(object):

    def __init__(self,n,sdout):

        global q

        self.n = n

        self.sdout = sdout

        q=n

    def grid(self):

        if self.sdout == 1:

            z=np.empty([self.n**2,3+self.n])

            rangearray=list(range(1,self.n+1))*self.n

            z[0:self.n**2,1]=np.repeat(list(range(1,self.n+1)),self.n)

            z[0:self.n**2,2]=rangearray

            for i in range(0,self.n**2):

                z[i,0]=i+1

                z[i:self.n**2,3:self.n+3]=list(np.random.uniform(-1,1,self.n))

            z=pd.DataFrame(z)

            z=z.rename(columns={ 0 : 'Order',1:'X-Coord',2:'Y-Coord'})  

            global z

            return(z.iloc[:,0:3])

        else:

            return(z)

    def quickref(idxnum):

        return z[idxnum-1:idxnum]

    def neighbors(self,x,y,m,typ):

        if m > q:

            return(pd.DataFrame(columns=list(z.columns.values)))

        if typ == "square":

            return((z.loc[(z['X-Coord']<=x+m) & (z['X-Coord']>=x-m) & (z['Y-Coord']<=y+m) & (z['Y-Coord']>=y-m)]).iloc[:,0:3])

        elif typ == "cross":

            return((z.loc[((z['X-Coord']==x) | (z['Y-Coord']==y)) & ((z['X-Coord'] >= x-m) & (z['X-Coord'] <= x+m)& (z['Y-Coord'] >= x-m) & (z['Y-Coord'] <= x+m))]).iloc[:,0:3])

        elif typ == "diamond":

            diam=[]

            for i in range(-10,10):

                k=((z.loc[((z['X-Coord']==x+i) | (z['Y-Coord']==y)) & ((z['X-Coord'] >= (x-abs(i))) & (z['X-Coord'] <= (x+m))& (z['Y-Coord'] >= (y-(m-abs(i)))) & (z['Y-Coord'] <= (y+(m-abs(i)))))]))

                diam.append(k)

            diam = pd.concat(diam)

            return(diam.iloc[:,0:3])

    def allocation(k,g,strategy):

        smaldf=g.iloc[:,3:len(g.columns)]

        g['Magnitude']=smaldf.apply (lambda row: np.linalg.norm(row),axis=1)

        if strategy == 1:

            g['Amount Allocated'] = k*g['Magnitude']/sum(g['Magnitude'])

        else:

            g['Amount Allocated'] = k*g['Y-Coord']/sum(g['Y-Coord'])

        return(g)
