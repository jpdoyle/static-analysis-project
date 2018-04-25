#include <stdio.h>
#include <limits.h>

int main(void) {
    size_t non_recip = 0, neg_sq = 0, total = 0;
    int i = -1;
    do {
        if(i != 0 && i/i != 1) {
            ++non_recip;
        }
        if(i*i < 0) {
            ++neg_sq;
            //printf("%d**2 = %d\n",i,i*i);
        }
        ++total;
    } while(--i != -1);
    printf("%lu/%lu (%f) ints are non-reciprocal\n",non_recip,total,
            non_recip/(float)total);
    printf("%lu/%lu (%f) squares were negative\n",neg_sq,total,
            neg_sq/(float)total);
    return 0;
}
