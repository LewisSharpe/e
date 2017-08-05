// TotientRange.c - PThreads Parallel Euler Totient Function (C Version)
// compile: gcc -o totient_pt totient_pt.c -lpthread
// run: ./totient_pt low high
// Lewis Sharpe

// This modified sequential program calculates the sum of the totients between a lower and an 
// upper limit using C longs. It is based on earlier work by:
// Greg Michaelson, Phil Trinder, Nathan Charles, Hans-Wolfgang Loidl and Colin Runciman

#include <stdio.h>
#include <pthread.h>

#define NUM_THREADS     8

/* create thread argument struct for thr_func() */
typedef struct _thread_data_t {
  int tid;
  double stuff;
} thread_data_t;

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


// relprime x y = hcf x y == 1

int relprime(long x, long y)
{
  return hcf(x, y) == 1;
}


// euler n = length (filter (relprime n) [1 .. n-1])

long euler(long n)
{
  long length, i;

  length = 0;
  for (i = 1; i < n; i++)
    if (relprime(n, i))
      length++;
  return length;
}

// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

long sumTotient(long lower, long upper)
{
  long sum, i;
/* pthreads defintions */
pthread_t thr[NUM_THREADS];
  int in, rc;
  // create a thread_data_t argument array
  thread_data_t thr_data[NUM_THREADS];

  sum = 0;
 for (i = 0; i < NUM_THREADS; ++i) {
  for (in = lower; i <= upper; i++)
    sum = sum + euler(i);
  return sum;
}
/* pthread block: block until all threads complete */
  for (i = 0; i < NUM_THREADS; ++i) {
    pthread_join(thr[i], NULL);
  }

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
