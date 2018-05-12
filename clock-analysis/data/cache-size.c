#include <pthread.h>
#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>

#define N_BYTES (1<<26)
#define AVG_ITS (100)

struct timespec get_time() {
    struct timespec ret;
    clock_gettime(CLOCK_MONOTONIC,&ret);
    return ret;
}

int main(void) {
    uint8_t* data = malloc(N_BYTES);
    if(!data) {
        printf("Couldn't malloc data array\n");
        return -1;
    }
    size_t n_bytes_used;
    size_t i,j;

    double time_taken = 0;
    double prev_miss_rate = 0;

    for(n_bytes_used = 1; n_bytes_used < N_BYTES; n_bytes_used *= 2) {

        size_t misses = 0;
        double total_avg_time = 0;

        for(j=0; j<AVG_ITS; ++j) { 

            for(i=0; i<n_bytes_used; ++i) {
                ++data[i];
            }

            {
                struct timespec t1,t2;

                t1 = get_time();

                {
                    volatile uint8_t x = 1;
                    for(i=0; i < n_bytes_used; ++i) {
                        x *= data[i];
                    }
                }

                t2 = get_time();

                time_taken = 1000.0*t2.tv_sec + 1e-6*t2.tv_nsec
                            - (1000.0*t1.tv_sec + 1e-6*t1.tv_nsec);
            }

            double avg_time;
            avg_time = time_taken/n_bytes_used;
            total_avg_time += avg_time;
            for(i=0; i<n_bytes_used; ++i) {
                ++data[i];
            }

            {

#define CHUNK_SIZE 128

                volatile uint8_t x = 1;
                for(i=0; i < n_bytes_used; i += CHUNK_SIZE) {
                    struct timespec t3,t4;
                    size_t nxt = i+CHUNK_SIZE;
                    t3 = get_time();

                    for(; i < n_bytes_used && i < nxt; ++i) {
                        x *= data[i];
                    }
                    t4 = get_time();

                    int missed;
                    missed = (1000.0*t4.tv_sec + 1e-6*t4.tv_nsec
                       - (1000.0*t3.tv_sec + 1e-6*t3.tv_nsec) > 1.1*CHUNK_SIZE*avg_time);
                    misses += missed;
                }
            }

        }

        double miss_rate;
        miss_rate = (misses/(double)AVG_ITS)
                           /(n_bytes_used/(double)CHUNK_SIZE);

        printf("  %010lu bytes: %lf ms/byte, ~%lf miss rate (%lu misses)\n", n_bytes_used,
                total_avg_time/AVG_ITS, miss_rate, misses);

        if(misses > 3 && miss_rate > 1.3*prev_miss_rate) {
            printf("Probable cache size: %lu\n", n_bytes_used/2);
        }

        prev_miss_rate = miss_rate;
    }

    printf("Done!\n");
    return 0;
}

