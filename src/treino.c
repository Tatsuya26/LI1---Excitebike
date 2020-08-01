#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Ficha 5

void insere (int v[], int N, int x) {
    for (int i = 0; i <= N ; i++) {
        if(v[i] > x) {
            for(int j = N; j > i ; j--) v[j] = v[j-1];
            v[i] = x;
            break; 
        }
    }
    if (v[N-1] < x) v[N] = x;
}

int main() {
  int arr[5] = {1,2,3,5,7};
  insere(arr,5,4);
  return 0;
}