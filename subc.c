#include <math.h>
#include <stdio.h>

//void sub_c_(double* f_3, double* f_2, double* f_1, double* f0, double* f1, double* f2, double* f3, double* fhp);

void sub_c_(double* f_3, double* f_2, double* f_1, double* f0, double* f1, double* f2, double* f3, double* fhp){ //positively biased
	int i;
	float w[4], beta[4], alfa[4], sumalfa;
	double fh[4];
	double d[4];
	double TV[4], TVR, TV_MAX, TV_MIN;
	double epsilon = 1e-6; 

	//optimal weight
	d[0] = 1.0 / 35.0;//fortran: d(0)=1.0d0/35.0d0  精度问题？？
	d[1] = 12.0 / 35.0;
	d[2] = 18.0 / 35.0;
	d[3] = 4.0 / 35.0;

	fh[0] = -1.0 / 4.0 * *f_3 + 13.0 / 12.0 * *f_2 - 23.0 / 12.0 * *f_1 + 25.0 / 12.0 * *f0;
	fh[1] = 1.0 / 12.0 * *f_2 - 5.0 / 12.0 * *f_1 + 13.0 / 12.0 * *f0 + 1.0 / 4.0 * *f1;
	fh[2] = -1.0 / 12.0 * *f_1 + 7.0 / 12.0 * *f0 + 7.0 / 12.0 * *f1 - 1.0 / 12.0 * *f2;
	fh[3] = 1.0 / 4.0 * *f0 + 13.0 / 12.0 * *f1 - 5.0 / 12.0 * *f2 + 1.0 / 12.0 * *f3;

	TV[0] = fabs(*f_3 - *f_2) + fabs(*f_2 - *f_1) + fabs(*f_1 - *f0); // TV(0)=abs(f_3-f_2)+abs(f_2-f_1)+abs(f_1-f0)
	TV[1] = fabs(*f_2 - *f_1) + fabs(*f_1 - *f0) + fabs(*f0 - *f1);
	TV[2] = fabs(*f_1 - *f0) + fabs(*f0 - *f1) + fabs(*f1 - *f2);
	TV[3] = fabs(*f0 - *f1) + fabs(*f1 - *f2) + fabs(*f2 - *f3);

	//TV_MAX = MaxVal(TV)
	//TV_MIN = MinVal(TV)
	TV_MAX = TV[0];
	TV_MIN = TV[0];
	for (int i = 1; i < 4; i++) {    //4 is the size of TV
		TV_MAX = TV_MAX >= TV[i] ? TV_MAX : TV[i];
		TV_MIN = TV_MIN <= TV[i] ? TV_MIN : TV[i];
	}
	
	
	TVR = TV_MAX / (TV_MIN + epsilon); 
	//for test
	//printf("%lf,%lf,%lf,%lf,", TV[0], TV[1], TV[2], TV[3]);
	//printf("c TV_MAX=%lf,TV_MIN=%lf,TVR=%lf", TV_MAX, TV_MIN, TVR);

	if (TV_MAX < 0.2 && TVR < 5.0) {
		for (int i = 0; i <= 3; i++) {
			w[i] = d[i];
		}
	}

	else {
		beta[0] = *f_3 * (547.0 * *f_3 - 3882.0 * *f_2 + 4642.0 * *f_1 - 1854.0 * *f0) + *f_2 * (7043.0 * *f_2 - 17246.0 * *f_1 + 7042.0 * *f0)\
			+ *f_1 * (11003.0 * *f_1 - 9402.0 * *f0) + 2107.0 * (*f0**f0);
		beta[1] = *f_2 * (267.0 * *f_2 - 1642.0 * *f_1 + 1602.0 * *f0 - 494.0 * *f1) + *f_1 * (2843.0 * *f_1 - 5966.0 * *f0 + 1922.0 * *f1)\
			+ *f0 * (3443.0 * *f0 - 2522.0 * *f1) + 547.0 * (*f1**f1);
		beta[2] = *f_1 * (547.0 * *f_1 - 2522.0 * *f0 + 1922.0 * *f1 - 494.0 * *f2) + *f0 * (3443.0 * *f0 - 5966.0 * *f1 + 1602.0 * *f2) \
			+ *f1 * (2843.0 * *f1 - 1642.0 * *f2) + 267.0 * (*f2**f2) ;
		beta[3] = *f0 * (2107.0 * *f0 - 9402.0 * *f1 + 7042.0 * *f2 - 1854.0 * *f3) + *f1 * (11003.0 * *f1 - 17246.0 * *f2 + 4642.0 * *f3) \
			+ *f2 * (7043.0 * *f2 - 3882.0 * *f3) + 547.0 * (*f3**f3) ;

		for (int i = 0; i <= 3; i++) {
			alfa[i] = d[i] / ((epsilon + beta[i]) * (epsilon + beta[i])); //alfa(i)=d(i)/(epsilon+beta(i))**2
		}

		sumalfa = alfa[0] + alfa[1] + alfa[2] + alfa[3]; 

		//for test
		//printf("sumalfa=%.16f,alfa[0]=%.16f", sumalfa, alfa[0]);

		for (int i = 0; i <= 3; i++) {
			w[i] = alfa[i] / sumalfa;
		}
	}
	*fhp = 0.0;
	for (int i = 0; i <= 3; i++) {
		*fhp = *fhp + w[i] * fh[i];
	}
	
}



