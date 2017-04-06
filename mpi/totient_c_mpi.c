// Environment:	lamboot, to start mpi runtime environment.
// Compile:		mpicc -Wall -O -o totient_c_mpi totient_c_mpi.c
// Run:			mpirun -np N totient_c_mpi

#include "totient_c_mpi.h"
#include <stdio.h>
#include <time.h>
#include <mpi.h>

clock_t stop, start;

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
// CHANGE IT TO GETTOTIENT TO RUN IT
long getPhilsTotient(long n)
{
  long length, i;

  length = 0;
  for (i = 1; i < n; i++)
    if (relprime(n, i))
      length++;
  return length;
}

// This is a clever version of totient calculating function, it makes use of totient's properties:
// totient(prime_number) = prime_number - 1
// totient(number) is even for numbers > 2 
// totient is multiplicative for relatively prime numbers so: totient(MN) = totient(M) * totinet(N),
// where greatest_common_denominator(M, N) = 1
long getTotient (long number)
{
    long result;
    long j;

    if(number == 1)
        return 1;

    // IN ERLANG THIS IS: FIRST/1

    result = number;

    // Check for divisibility by every prime number below the square root. 
    // Start with 2. 
    if(number % 2 == 0){
        result -= result / 2;
        do 
            number /= 2;
        while(number %2 == 0);
    }

    // IN ERLANG THIS IS SECOND/2

    // Since this doesn't use a list of primes, check every odd number. Ideally, skip past composite numbers.
    for(j = 3; j * j <= number; j += 2){
        if(number %j == 0){
            result -= result / j;
            do 
                number /= j;
            while(number % j == 0);
        }
    }

    // IN ERLANG THIS IS THIRD/2

    // If i > 1, then it's the last factor at this point. 
    if(number > 1) 
        result -= result / number;

    // Return the result. 
    return result; 
}

// Use mpi to spread workload across processes.
long getTotientRangeMPI(long arg_1, long arg_2, int ierr)
{
	MPI_Status status;
	long sum, partial_result;
	int my_id, root_process_id, i, num_rows, num_procs, helper,
		begin_at, finish_at, proc_id, avg_rows_per_process/*, sender */;

		sum = 0;

		root_process_id = 0;

		// Get process id and number of all processes.
		ierr = MPI_Comm_rank(MPI_COMM_WORLD, &my_id);
		ierr = MPI_Comm_size(MPI_COMM_WORLD, &num_procs);

		// I am the root.
		if(my_id == root_process_id){
			// Start the clock.
			// START;

			num_rows = (arg_2 - arg_1) + 1;

			avg_rows_per_process = num_rows / num_procs;

			// Distribute the rest of the numbers to other processes.
			for (proc_id = 1; proc_id < num_procs; ++proc_id)
			{
				// The last bit of the input array, might not be of avg_rows_per_process length.
				if(proc_id == (num_procs - 1)){
					begin_at = proc_id * avg_rows_per_process + arg_1;
					finish_at = arg_2;
					
					// Send start and end index of the last bit to the last process.
					ierr = MPI_Send(&begin_at, 1, MPI_INT, proc_id, SENDING_TAG, MPI_COMM_WORLD);
					ierr = MPI_Send(&finish_at, 1, MPI_INT, proc_id, SENDING_TAG, MPI_COMM_WORLD);
				}
				// Equal parts of avg_rows_per_process.
				else{
					begin_at = arg_1 + proc_id * avg_rows_per_process;
					finish_at = arg_1 + (proc_id + 1) * avg_rows_per_process - 1; 
					
					// Send them start and end index of their part of the computation.
					ierr = MPI_Send(&begin_at, 1, MPI_INT, proc_id, SENDING_TAG, MPI_COMM_WORLD);
					ierr = MPI_Send(&finish_at, 1, MPI_INT, proc_id, SENDING_TAG, MPI_COMM_WORLD);
				}
			}

			// Calculate the partial result (root).
			sum = 0;
			helper = arg_1;
			
			for(i = 0; i < avg_rows_per_process; i++)
				sum += getTotient(helper++);
				
			// Now ask all the slaves to come back with their partial sums.
			for (proc_id = 1; proc_id < num_procs; ++proc_id){
				ierr = MPI_Recv(&partial_result, 1, MPI_LONG, MPI_ANY_SOURCE, RETURNING_TAG, MPI_COMM_WORLD, &status);
				// sender = status.MPI_SOURCE;
				sum += partial_result;
				// printf("Partial sum is: %ld, returned from process: %i.\n", partial_result, sender);
			}
			
			// We have all of the partial results added up, stop the clock and print the result.
			// printf("\n<-------------------------------------------------------->\n");
			// printf("GLOBAL SUM OF EULER TOTIENT NUMBERS FROM: %ld TO: %ld\nCALCULATED USING: %d PROCESSES IS: %ld.\n", arg_1, arg_2, num_procs, sum);
			// STOP;
			// PRINT_TIME;
			// printf("<-------------------------------------------------------->\n\n");
		}
		// If a process falls in here it is a slave.
		else{
			// Get the boundries.
			ierr = MPI_Recv(&begin_at, 1, MPI_INT, root_process_id, SENDING_TAG, MPI_COMM_WORLD, &status);
			ierr = MPI_Recv(&finish_at, 1, MPI_INT, root_process_id, SENDING_TAG, MPI_COMM_WORLD, &status);

			partial_result = 0;

			// Calculate partial sum.
			for (i = begin_at; i <= finish_at; ++i)
				partial_result += getTotient(i);
			
			// printf("partial_result is:    %ld\n", partial_result);
			
			// Return the result to the root process.
			ierr = MPI_Send(&partial_result, 1, MPI_LONG, root_process_id, RETURNING_TAG, MPI_COMM_WORLD);
		}

		ierr = MPI_Finalize();

		return sum;
}

int initialize()
{
	return MPI_Init(NULL, NULL);
}

// Main method.
int main(int argc, char ** argv)
{
	// Initialize MPI.
	int ierr = initialize();
	
	// Get the arguments.
	long arg_1, arg_2, result;
	sscanf(argv[1], "%ld", &arg_1);
	sscanf(argv[2], "%ld", &arg_2);
	
	// Run the method.
	result = getTotientRangeMPI(arg_1, arg_2, ierr);
	printf("result is: %ld\n", result);
			
	return 0;
}
