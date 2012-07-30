## STEP 1: Create a container (file on disk)
obj = hdf5Container('mytest.h5')

## STEP 2: Add an array to the container
## hdf5AddArray(container, name, dim, storage.mode)
hdf5AddArray(obj, 'A', dim=c(100, 5), 'double')

## STEP 3: Operate the array using $ as a regular array
for (i in 1:5) obj$A[,i] = rnorm(100)
obj$A[1:10,]

## Cleaning up
file.remove('mytest.h5')
