/* Examples for testing */

x/;  /* SLASH means binding the variable to the global context */
x;
x/;
y/;
y x;
z/;
(lambda x. x) (lambda y. y x z);
lambda x. x (lambda y. y x z);
lambda x. x;
lambda w. y (w (lambda x. x));