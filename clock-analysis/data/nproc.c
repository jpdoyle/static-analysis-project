#include <pthread.h>
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

struct timespec get_time() {
    struct timespec ret;
    clock_gettime(CLOCK_MONOTONIC,&ret);
    return ret;
}

void work() {
    volatile double d = 0;
    for (int n=0; n<10000; ++n) {
        for (int m=0; m<10000; ++m) {
            d += d*n*m;
        }
    }
}

typedef struct {
    pthread_mutex_t lock;
    double work_time;
    double other_time;
    size_t num_samples;
    int keep_running;
} timing_data;

void* timing_thread(void* thd_data) {
    timing_data* data = thd_data;
    struct timespec t1,t2;
    t1 = get_time();
    t2 = get_time();

    double outer_time = 0, inner_time = 0;

    while(data->keep_running) {
        t1 = get_time();

        outer_time =    1000.0*t1.tv_sec + 1e-6*t1.tv_nsec
                     - (1000.0*t2.tv_sec + 1e-6*t2.tv_nsec);

        work();

        t2 = get_time();
        pthread_mutex_lock(&data->lock);
        inner_time = 1000.0*t2.tv_sec + 1e-6*t2.tv_nsec
                     - (1000.0*t1.tv_sec + 1e-6*t1.tv_nsec);
        data->work_time += inner_time;
        data->other_time += outer_time;
        ++data->num_samples;
        pthread_mutex_unlock(&data->lock);
    }
    return NULL;
}

#define MAX_N_THREADS 100

int main(void) {
    size_t i = 0;
    timing_data timings[MAX_N_THREADS];
    memset(timings,0,sizeof(timings));
    pthread_t threads[MAX_N_THREADS];
    memset(threads,0,sizeof(threads));
    size_t n_threads;

    for(i = 0; i < MAX_N_THREADS; ++i) {
        pthread_mutex_init(&timings[i].lock,NULL);
    }

    int nproc_found = 0;
    double prev_max_time = 0;
    double min_time = 100, max_time = 0;

    while(!nproc_found) {
        if(n_threads >= MAX_N_THREADS) {
            return -1;
        }

        size_t ix = n_threads++;
        timings[ix].keep_running = 1;
        pthread_create(&threads[ix],NULL,&timing_thread,&timings[ix]);

        usleep(50000);

        min_time = 100000;
        max_time = 0;
        for(i=0; i < n_threads; ++i) {
            pthread_mutex_lock(&timings[i].lock);
            while(!timings[i].num_samples) {
                pthread_mutex_unlock(&timings[i].lock);
                usleep(50000);
                pthread_mutex_lock(&timings[i].lock);
            }

            double on_time = timings[i].work_time/timings[i].num_samples;
            timings[i].work_time = timings[i].other_time = 0;

            timings[i].num_samples = 0;

            pthread_mutex_unlock(&timings[i].lock);


            if(on_time > max_time) {
                max_time = on_time;
            }
            if(on_time < min_time) {
                min_time = on_time;
            }
        }

        printf("%lu threads, Min time: %lf, Max time: %lf\n",
                n_threads, min_time, max_time);
        if(max_time > 1.2*min_time) {
            nproc_found = 1;
        }
    }

    printf("Found! nproc = %lu\n",n_threads-1);
    printf("Joining timing threads...\n");

    for(i=0; i < n_threads; ++i) {
        timings[i].keep_running = 0;
    }

    for(i=0; i < n_threads; ++i) {
        pthread_join(threads[i],NULL);
    }

    printf("Done!\n");
    return 0;
}

