
%-----------------------------------------------------------------------------------------
%-- VASILEIOS KAPSALIS AM:4080
 
%-- ASKHSH 1

thesis(A,B,C,0) :- A<0.
thesis(A,B,C,0) :- A>100.
thesis(A,B,C,0) :- B<0.
thesis(A,B,C,0) :- B>100.
thesis(A,B,C,0) :- C<0.
thesis(A,B,C,0) :- C>100.
thesis(A,B,C,49) :- A<50,B<50,mo(A,B,C,MO),MO>=50.
thesis(A,B,C,49) :- A<50,C<50,mo(A,B,C,MO),MO>=50.
thesis(A,B,C,49) :- C<50,B<50,mo(A,B,C,MO),MO>=50.
thesis(A,B,C,50) :- A>=50,B>=50,mo(A,B,C,MO),MO<50.
thesis(A,B,C,50) :- C>=50,B>=50,mo(A,B,C,MO),MO<50.
thesis(A,B,C,50) :- A>=50,C>=50,mo(A,B,C,MO),MO<50.
thesis(A,B,C,S) :- mo(A,B,C,MO),S is MO.

mo(A,B,C,MO):-A=<B,B=<C, MO is ((C*50)+(A*35)+(B*40))//125.
mo(A,B,C,MO):-A=<C,C=<B, MO is ((B*50)+(A*35)+(C*40))//125.
mo(A,B,C,MO):-B=<A,A=<C, MO is ((C*50)+(B*35)+(A*40))//125.
mo(A,B,C,MO):-B=<C,C=<A, MO is ((A*50)+(B*35)+(C*40))//125.
mo(A,B,C,MO):-C=<A,A=<B, MO is ((B*50)+(C*35)+(A*40))//125.
mo(A,B,C,MO):-C=<B,B=<A, MO is ((A*50)+(C*35)+(B*40))//125.








%-----------------------------------------------------------------------------------------

%-- ASKHSH 2

lcd(M,N,D) :- help(M,N,2,D).

help(M,N,K,D) :- K>M,D is 0.
help(M,N,K,D) :- K>N,D is 0.
help(M,N,K,D) :- M mod K =:=0, N mod K =:=0, D is K.
help(M,N,K,D) :- K2 is K+1, help(M,N,K2,D2), D is D2.




%-----------------------------------------------------------------------------------------