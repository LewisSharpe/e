#ifndef SYMBOL
#define START if ((stop = clock()) == -1) {printf("Error while calling clock");};
#endif

#ifndef SYMBOL
#define STOP if ((stop = clock()) == -1) {printf("Error while calling clock");};
#endif

#ifndef SYMBOL
#define PRINT_TIME printf("TIME ELAPSED:%6.3f seconds (used by all the processes).\n", ((double)stop-start)/CLOCKS_PER_SEC);
#endif

#ifndef SYMBOL
#define SENDING_TAG 9000
#endif

#ifndef SYMBOL
#define RETURNING_TAG 9001
#endif

long getTotient (long number);

long getTotientRangeMPI(long arg_1, long arg_2, int ierr);

int initialize();