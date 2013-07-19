/* Guile utilities.

Copyright (C) 2013 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include "lisp.h"

scm_t_bits c_closure_tag;

typedef SCM (*c_closure_0_t) (void *);
typedef SCM (*c_closure_1_t) (void *, SCM);
typedef SCM (*c_closure_2_t) (void *, SCM, SCM);
typedef SCM (*c_closure_3_t) (void *, SCM, SCM, SCM);
typedef SCM (*c_closure_4_t) (void *, SCM, SCM, SCM, SCM);
typedef SCM (*c_closure_5_t) (void *, SCM, SCM, SCM, SCM, SCM);
typedef SCM (*c_closure_6_t) (void *, SCM, SCM, SCM, SCM, SCM, SCM);
typedef SCM (*c_closure_7_t) (void *, SCM, SCM, SCM, SCM, SCM, SCM, SCM);

SCM
make_c_closure (SCM (*func) (), void *data, int req, int opt)
{
  SCM smob;

  if (req > 3 || opt > 1)
    emacs_abort ();

  SCM_NEWSMOB2 (smob, c_closure_tag, func, data);
  SCM_SET_SMOB_FLAGS (smob, req | (opt << 2));
  return smob;
}

static SCM
apply_c_closure (SCM c_closure, SCM args)
{
  int req, opt;
  SCM cargs[7];
  long nargs = scm_to_long (scm_length (args));
  scm_t_bits flags = SCM_SMOB_FLAGS (c_closure);
  scm_t_bits func = SCM_SMOB_DATA (c_closure);
  void *data = (void *) SCM_SMOB_DATA_2 (c_closure);

  req = flags & 3;
  opt = (flags >> 2) & 1;

  for (int i = 0; i < req + opt; i++)
    {
      if (scm_is_pair (args))
        {
          cargs[i] = scm_car (args);
          args = scm_cdr (args);
        }
      else if (opt)
        {
          cargs[i] = SCM_UNDEFINED;
        }
      else
        scm_wrong_num_args (c_closure);
    }

  switch (req + opt)
    {
    case 0: return ((c_closure_0_t) func) (data);
    case 1: return ((c_closure_1_t) func) (data, cargs[0]);
    case 2: return ((c_closure_2_t) func) (data, cargs[0], cargs[1]);
    case 3: return ((c_closure_3_t) func) (data, cargs[0], cargs[1], cargs[2]);
    case 4: return ((c_closure_4_t) func) (data, cargs[0], cargs[1], cargs[2], cargs[3]);
    default:
      emacs_abort ();
    }
}

void
init_guile (void)
{
  c_closure_tag = scm_make_smob_type ("c-closure", 0);
  scm_set_smob_apply (c_closure_tag, apply_c_closure, 0, 0, 1);
}
