#This code will be for enumerating the dualists of any flat, simply-connected poly(n)-ringed molecule. 
#The code will work by taking the input as individual ring sizes and amounts of each (matrix) and the output will 
#be all unique, simply-connected molecules from this.
#Since each individual, unconnected ring shape has a unique angular component, the angles between nodes in the 
#dualist will be unique. Therefore, the algorithm enumerates the combinations of dualist angles given the input
#and returns dualists. The poly(n) molecule will be built on top of the resulting dualist.
