      program project
	implicit none
      integer :: n
	integer :: i,j,f
	integer year, mon, day, doy, time, pts
	real dist,sza, ptoz
	character(len=200)dir1,dir2,dir3,dir4,dir5,dir6,dir7,dir8,dir9
	character(len=200) dir10
	character(len = 200) c,d,e,filename,r0,r1,r2,r3
	character(len = 800) rall
	real L(21)
	real P(21)
	real, dimension(21) :: average = 0
	real, dimension(21) :: std = 0
	real, dimension (:,:), allocatable :: darray, darray2, darray3,x,y
	integer months(3000)
	integer years(3000) 
	character(len=100), dimension(:), allocatable :: filenames
	real count
	integer nRows, nFiles
	integer io
	real latitude
	real mac1(12) ! Mean Average Cycle
	real mac2(12) 
	real mac3(12)
	real athroisma1, athroisma2
	integer nPositions
	logical :: exist
	character(len=200) homeDirectory
	real trend
	real xav,yav
	integer nMonths
	nPositions = 0

c	 Defining home directory, change it to run the script
	homeDirectory = 'C:/Users/theoh/SUOMI-NPP-OMPS'

	inquire(file=homeDirectory, exist=exist)
	if (exist) then
	  write(*,*)'Sanity Check 1: OK'
	  write(*,*)'Home directory exists'
	else
	  write(*,*) 'Sanity Check 1: Failed'
	  write(*,*) 'Please define a valid home directory'
        stop
	endif

c     Reading filenames from data directory into a .txt file
	call system('dir/b C:\OMPS>C:\Users\theoh\inputfiles.txt')

c     Counting and printing number of files in inputfiles.txt
	nFiles=0
	open(1, file='C:\Users\theoh\inputfiles.txt')
	do
        read(1,*,iostat=io)
        if (io/=0) exit
        nFiles = nFiles + 1
      enddo
	write(*,*) "Number of available files:", nFIles
	close(1)

	allocate (filenames(nFiles))

	! Iterating over files
	open(1, file='C:\Users\theoh\inputfiles.txt')
	do i=1,nFiles
	read(1,*) filenames(i)
	enddo
	close(1)
	write(*,*) 'Shape',shape(filenames)

	do f=1,nFiles

c	  Data input
	  filename = 'C:\OMPS\'//filenames(f)

c	  latitude, pressure, meanlayerozone, standard deviation
	  dir1 = 'C:\Users\theoh\Results\1\'//filenames(f)

c 	  File for Surfer Data Grid Plot
        dir2 = 'C:\Users\theoh\Results\2\'//filenames(f)

c 	  Tropospheric Monthly means
        dir3 = 'C:\Users\theoh\Results\3\'//filenames(f)

c 	  Tropospheric mean yearly cycle
        dir4 = 'C:\Users\theoh\Results\4\'//filenames(f)

c 	  Tropospheric Monthly means minus the mean yearly cycle
        dir5 = 'C:\Users\theoh\Results\5\'//filenames(f)


c 	  Stratospheric Monthly means
        dir6 = 'C:\Users\theoh\Results\6\'//filenames(f)

c 	  Stratospheric mean yearly cycle
        dir7 = 'C:\Users\theoh\Results\7\'//filenames(f)

c 	  Stratospheric Monthly means minus the mean yearly cycle
        dir8 = 'C:\Users\theoh\Results\8\'//filenames(f)

c 	  Trend
        dir9 = 'C:\Users\theoh\Results\9\all.txt'

c 	  Trend
        dir10 = 'C:\Users\theoh\Results\10\all.txt'

c        Measuring rows of file
	  nRows = 0
        open (1, file = filename)
          do
            read(1,*,iostat=io)
            if (io/=0) exit
            nRows = nRows + 1
          enddo
        close (1)
        write(*,*) "Number of Rows:", nRows
	   
c        Allocating memory for array
	   allocate (darray(nRows,21))
	   allocate (darray2(nRows,21))
	   allocate (darray3(nRows,3))

c        Preparing format specifiers for
	   r0 = '(i4,i4,i4,i4,i7,f8.1,i5,f6.1,f8.1,'
	   r1 =  'f7.2,f7.2,f7.2,f7.2,f7.2,f7.2,f7.2'
	   r2 =  ',f7.2,f7.2,f7.2,f7.2,f7.2,f7.2,f8.3'
	   r3 =  ',f8.3,f8.3,f8.3,f8.3,f8.3,f8.3,f8.3)'
	   rall = r0//r1//r2//r3


c    	   Preparing format specifiers for
	   d = '(28x,f7.5,f6.6,f6.6,f6.6,f6.6,f6.6,f6.3,f4.4,f5.5,f5.4,'
	   e = 'f5.4,f5.4,f5.4,f5.4,f5.4,f5.4,f6.6,f6.6,f6.6,f6.6,f6.6)'

c        Opening 1 file
	   open(1,file=filename)	  
         do n=0,14
	     if (n .EQ. 0) then
	       read(1,1000) latitude
	     else if (n .EQ. 12) then
	       read(1,d//e) P
	     else
	       read(1,*)
	     end if
	   enddo
	   do n=0,nRows-15
           read(1,rall,end=20)year,mon,day,doy,time,dist,pts,sza,ptoz,L  
	     count = count + 1
	     darray(n,:) =  L
		 months(n) = mon
		 years(n) = year        
         enddo
        close(1)
  20	  continue


c       Calculating standard deviation
	  average = (SUM (darray, DIM=1))/(nRows-15)
	  	
        i=0
	  j=0
	  do i=1,nRows-15
	    do j=1,21
	      darray2(i,j) = sqrt ((darray(i,j)-average(j))**2)
	    enddo
	  enddo


c       Calculating Standard Deviation
	  std = (SUM (darray2, DIM=1))/(nRows-15)	  	
	  	  
        open(1,file=dir1, status = 'new')  
          do i=1,21  
            write(1,*) P(i), latitude, average(i), std(i) ! Question 1  
          end do  
        close(1)

	  inquire(file='C:\Users\theoh\Results\2\all.txt', exist=exist)
        if (exist) then
          open(2, file='C:\Users\theoh\Results\2\all.txt', 
     *		status="old", position="append", action="write")
        else
          open(2, file='C:\Users\theoh\Results\2\all.txt', 
     *		status="new", action="write")
        end if
	  do i=1,21  
	    write(2,1003) latitude, P(i), average(i)
	  enddo
	  close(2)
	  
c	  Tropospheric Monthly means
	  open(3,file=dir3, status = 'new')
	  athroisma1 = 0
	  count = 0
	  do i=2,nRows-15
	    if (months(i) .ne. months(i-1)) then
	      if (count .ne. 0) then
	        write(3,*)months(i-1),years(i-1),athroisma1/count
      	  endif
		  athroisma1 = 0
		  count = 0
		endif 
	    athroisma1 = athroisma1 + sum(darray(i,1:3)) 
	    count = count+1
	  enddo
	  close(3)

c	  Stratospheric	Monthly means
	  open(6,file=dir6, status = 'new')
	  athroisma2 = 0
	  count = 0
	  do i=2,nRows-15
	    if (months(i) .ne. months(i-1)) then
	      if (count .ne. 0) then
	        write(6,*)months(i-1),years(i-1),athroisma2/count
      	  endif
	      athroisma2 = 0
		  count = 0
		endif 
	    athroisma2 = athroisma2 + sum(darray(i,4:21)) 
	    count = count+1
	  enddo
	  close(6)

c	  Calculating and storing Mean Yearly Cycle

	  c = '(f10.5,f10.5,f10.5)'
	
	  do i=1,nRows-15
	 	mac1(int(months(i)))=mac1(int(months(i)))+sum(darray(i,1:3))
	    mac2(int(months(i)))=mac2(int(months(i)))+sum(darray(i,4:21))
	    mac3(int(months(i)))=mac3(int(months(i)))+1
	  enddo  

c	  Tropospheric mean yearly cycle
	  open(4, file=dir4)
	    do i=1,12
	      write(4,c) mac1(i)/mac3(i)
	    enddo
	  close(4)

c	  Stratospheric mean yearly cycle
	  open(7, file=dir7)
	     do i=1,12
	      write(7,c) mac2(i)/mac3(i)
	    enddo
	  close(7)

	  nMonths=0

c	  Tropospheric Monthly means - mean yearly Cycle
	  open(5,file=dir5, status = 'new')
	  athroisma1 = 0
	  count = 0
	  do i=2,nRows-15
	    if (months(i) .ne. months(i-1)) then
	      if (count .ne. 0) then
	        nMonths= nMonths+1
	        write(5,*) (float(months(i-1))/12)+float(years(i-1)),
     *	athroisma1/count-mac1(int(months(i)))/mac3(int(months(i)))
      	  endif
		  athroisma1 = 0
		  count = 0
		endif 
	    athroisma1 = athroisma1 + sum(darray(i,1:3)) 
	    count = count+1
	  enddo
	  close(5)

c	  Stratospheric	Monthly means  - mean yearly Cycle
	  open(8,file=dir8, status = 'new')
	  athroisma2 = 0
	  count = 0
	  do i=2,nRows-15
	    if (months(i) .ne. months(i-1)) then
	      if (count .ne. 0) then
	        write(8,*)(float(months(i-1))/12)+float(years(i-1)),
     *	athroisma2/count-mac2(int(months(i)))/mac3(int(months(i)))
      	  endif
	      athroisma2 = 0
		  count = 0
		endif 
	    athroisma2 = athroisma2 + sum(darray(i,4:21)) 
	    count = count+1
	  enddo
	  close(8)

c	  Tropospheric Latitude vs trend

c       Measuring rows of file
	  nRows = 0
        open (5, file = dir5)
          do
            read(5,*,iostat=io)
            if (io/=0) exit
            nRows = nRows + 1
          enddo
        close (5)
	  allocate (x(nRows,2))
c	  Reading File
	  open(5,file=dir5, status = 'old')
	    do i=1,nRows-1
	      read(5,*) x(i,1), x(i,2)
	    enddo
	  close(5)

	  xav = sum(x(:,1))/nMonths
	  yav = sum(x(:,2))/nMonths
	  trend = 0
	  do i=1,nRows-1
	    trend = trend + ((x(i,1)-xav)*(x(i,2)-yav))/((x(i,1)-xav)**2)
	  enddo

	  inquire(file='C:\Users\theoh\Results\9\all.txt', exist=exist)
        if (exist) then
          open(9, file='C:\Users\theoh\Results\9\all.txt', 
     *		status="old", position="append", action="write")
        else
          open(9, file='C:\Users\theoh\Results\9\all.txt', 
     *		status="new", action="write")
        end if  
	  write(9,*) latitude, trend
	  close(9)


c	  Stratospheric Latitude vs trend

c       Measuring rows of file
	  nRows = 0
        open (8, file = dir8)
          do
            read(8,*,iostat=io)
            if (io/=0) exit
            nRows = nRows + 1
          enddo
        close (8)
	  allocate (y(nRows,2))

	  open(8,file=dir8, status = 'old')
	    do i=0,nRows-1
	      read(8,*) y(i,1), y(i,2)
	    enddo
	  close(8)

	  xav = sum(y(:,1))/nMonths
	  yav = sum(y(:,2))/nMonths
	  trend = 0
	  do i=1,nRows-1
	    trend = trend + ((y(i,1)-xav)*(y(i,2)-yav))/((y(i,1)-xav)**2)
	  enddo

	  inquire(file='C:\Users\theoh\Results\10\all.txt', exist=exist)
        if (exist) then
          open(10, file='C:\Users\theoh\Results\10\all.txt', 
     *		status="old", position="append", action="write")
        else
          open(10, file='C:\Users\theoh\Results\10\all.txt', 
     *		status="new", action="write")
        end if  
	  write(10,*) latitude, trend
	  close(10)

	  deallocate(darray)
	  deallocate(darray2)
	  deallocate(darray3)
	  deallocate(x)
	  deallocate(y)

	enddo

1000	format(73x,f5.5)
1003	format(f16.6,f16.6,f16.6)
1004	format(i12,f16.6) 
      end


