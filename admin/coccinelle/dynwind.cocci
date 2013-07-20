@normal_return@
expression E1;
@@
- RETURN_UNGCPRO (E1);
+ return E1;

@unbind_to_expr@
expression E;
position p;
@@
unbind_to(...)@E@p

@rule@
identifier X1, X2, fld;
expression E1, E2;
fresh identifier tem = "tem";
position p != unbind_to_expr.p;
@@
(
- ptrdiff_t X1 = SPECPDL_INDEX ();
+ dynwind_begin ();
|
- X1 = SPECPDL_INDEX ();
+ dynwind_begin ();
)
... when strict
    when != X1
(
+ dynwind_end ();
  return;
|
+ dynwind_end ();
  return E1@p;
|
- return unbind_to (X1, X2);
+ dynwind_end();
+ return X2;
|
- return unbind_to (X1, X2->fld);
+ dynwind_end();
+ return X2;
|
- return unbind_to (X1, E2);
+ Lisp_Object tem = E2;
+ dynwind_end ();
+ return tem;
|
- unbind_to (X1, X2);
+ dynwind_end ();
|
- unbind_to (X1, X2->fld);
+ dynwind_end ();
|
- unbind_to (X1, E2);
+ E2;
+ dynwind_end ();
)
