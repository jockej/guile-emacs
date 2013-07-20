@double_unbind@
identifier X1;
position p;
@@
unbind_to (X1, ...)
... when != X1 = SPECPDL_INDEX ()
unbind_to@p (X1, ...)

@script:python@
p << double_unbind.p;
@@
coccilib.report.print_report (p[0], "double unbind")

@count_condition@
identifier X1;
position p;
@@
X1 = SPECPDL_INDEX ()
...
if (<+... X1@p ...+>) { ... }

@script:python@
p << count_condition.p;
@@
coccilib.report.print_report (p[0], "specpdl count in condition")
