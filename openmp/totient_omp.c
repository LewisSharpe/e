// compile: gcc -Wall -O -o totient_omp totient_omp.c
// run:     ./totient_omp lower_num uppper_num

// Greg Michaelson 14/10/2003
// Patrick Maier   29/01/2010 [enforced ANSI C compliance]

// This program calculates the sum of the totients between a lower and an 
// upper limit using C longs. It is based on earlier work by:
// Phil Trinder, Nathan Charles, Hans-Wolfgang Loidl and Colin Runciman

// OpenMP implementation by Lewis Sharpe.

#include <stdio.h>
#include <time.h>
#include <omp.h>

long hcf(long x, long y)
{
  long t;
 
while (y != 0) {
		t = x % y;
		x = y;
		y = t;
 }
 
  return x;
}

int relprime(long x, long y)
{
  return hcf(x, y) == 1;
}

long euler(long n)
{
  long length, i;

  length = 0;
  for (i = 1; i < n; i++)
    if (relprime(n, i))
      length++;
  return length;
}

long sumTotient(long lower, long upper)
{
  
  long sum[1];
  sum[0] = 0;
   // Do this part in parallel 
   
   omp_set_num_threads(8);
   omp_set_schedule(omp_sched_dynamic, 1);
   
 #pragma omp parallel
 {
	long i;
	#pragma omp for 
	for (i = lower; i <= upper; i++)
	{
		long e = euler(i);
		
		#pragma	omp	critical
		sum[0] += e;
				    
	}
 }   
  return sum[0];
}

int main(int argc, char ** argv)
{
  long lower, upper;

  if (argc != 3) {
    printf("not 2 arguments\n");
    return 1;
  }
  
  
  sscanf(argv[1], "%ld", &lower);
  sscanf(argv[2], "%ld", &upper);
  
  
  
  printf("C: Sum of Totients  between [%ld..%ld] is %ld\n",
         lower, upper, sumTotient(lower, upper));
         
         
  return 0;
}
