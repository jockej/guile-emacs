/* Storage allocation and gc for GNU Emacs Lisp interpreter.

Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2014 Free Software
Foundation, Inc.

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

#include <stdio.h>

#ifdef ENABLE_CHECKING
#include <signal.h>		/* For SIGABRT.  */
#endif

#ifdef HAVE_PTHREAD
#include <pthread.h>
#endif

#include <gc.h>

#include "lisp.h"
#include "process.h"
#include "intervals.h"
#include "character.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "frame.h"
#include "termhooks.h"		/* For struct terminal.  */
#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

#include <verify.h>
#include <execinfo.h>           /* For backtrace.  */

#if (defined ENABLE_CHECKING			\
     && defined HAVE_VALGRIND_VALGRIND_H	\
     && !defined USE_VALGRIND)
# define USE_VALGRIND 1
#endif

#if USE_VALGRIND
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>
static bool valgrind_p;
#endif

#include <unistd.h>
#include <fcntl.h>

#ifdef USE_GTK
# include "gtkutil.h"
#endif
#ifdef WINDOWSNT
#include "w32.h"
#include "w32heap.h"	/* for sbrk */
#endif

/* Default value of gc_cons_threshold (see below).  */

#define GC_DEFAULT_THRESHOLD (100000 * word_size)

/* Global variables.  */
struct emacs_globals globals;

/* Number of bytes of consing done since the last gc.  */

EMACS_INT consing_since_gc;

/* Similar minimum, computed from Vgc_cons_percentage.  */

EMACS_INT gc_relative_threshold;

/* Minimum number of bytes of consing since GC before next GC,
   when memory is full.  */

EMACS_INT memory_full_cons_threshold = 1 << 10;

/* True during GC.  */

bool gc_in_progress;

/* True means abort if try to GC.
   This is for code which is written on the assumption that
   no GC will happen, so as to verify that assumption.  */

bool abort_on_gc;

/* Number of live and free conses etc.  */

static EMACS_INT total_conses, total_markers, total_symbols, total_buffers;
static EMACS_INT total_free_conses, total_free_markers, total_free_symbols;
static EMACS_INT total_free_floats, total_floats;

/* Points to memory space allocated as "spare", to be freed if we run
   out of memory. */

static void *spare_memory;

/* Amount of spare memory to keep in large reserve block, or to see
   whether this much is available when malloc fails on a larger request.  */

#define SPARE_MEMORY (1 << 15)

/* If nonzero, this is a warning delivered by malloc and not yet
   displayed.  */

const char *pending_malloc_warning;

static Lisp_Object Qgc_cons_threshold;
Lisp_Object Qchar_table_extra_slots;

/* Hook run after GC has finished.  */

static Lisp_Object Qpost_gc_hook;

#if !defined REL_ALLOC || defined SYSTEM_MALLOC
static void refill_memory_reserve (void);
#endif
static Lisp_Object make_empty_string (int);
extern Lisp_Object which_symbols (Lisp_Object, EMACS_INT) EXTERNALLY_VISIBLE;

#ifndef DEADP
# define DEADP(x) 0
#endif

/* Recording what needs to be marked for gc.  */

struct gcpro *gcprolist;

static void
XFLOAT_INIT (Lisp_Object f, double n)
{
  XFLOAT (f)->data = n;
}


/************************************************************************
				Malloc
 ************************************************************************/

/* Function malloc calls this if it finds we are near exhausting storage.  */

void
malloc_warning (const char *str)
{
  pending_malloc_warning = str;
}


/* Display an already-pending malloc warning.  */

void
display_malloc_warning (void)
{
  call3 (intern ("display-warning"),
	 intern ("alloc"),
	 build_string (pending_malloc_warning),
	 intern ("emergency"));
  pending_malloc_warning = 0;
}

/* Called if we can't allocate relocatable space for a buffer.  */

void
buffer_memory_full (ptrdiff_t nbytes)
{
  /* If buffers use the relocating allocator, no need to free
     spare_memory, because we may have plenty of malloc space left
     that we could get, and if we don't, the malloc that fails will
     itself cause spare_memory to be freed.  If buffers don't use the
     relocating allocator, treat this like any other failing
     malloc.  */

#ifndef REL_ALLOC
  memory_full (nbytes);
#else
  /* This used to call error, but if we've run out of memory, we could
     get infinite recursion trying to build the string.  */
  xsignal (Qnil, Vmemory_signal_data);
#endif
}

/* Like GC_MALLOC but check for no memory.  */

void *
xmalloc (size_t size)
{
  void *val = GC_MALLOC (size);
  if (!val && size)
    memory_full (size);
  return val;
}

/* Like the above, but zeroes out the memory just allocated.  */

void *
xzalloc (size_t size)
{
  return xmalloc (size);
}

/* Like GC_REALLOC but check for no memory.  */

void *
xrealloc (void *block, size_t size)
{
  void *val = GC_REALLOC (block, size);
  if (!val && size)
    memory_full (size);
  return val;
}

void
xfree (void *block)
{
  return;
}

/* Allocate pointerless memory.  */

void *
xmalloc_atomic (size_t size)
{
  void *val = GC_MALLOC_ATOMIC (size);
  if (! val && size)
    memory_full (size);
  return val;
}

void *
xzalloc_atomic (size_t size)
{
  return xmalloc_atomic (size);
}

/* Allocate uncollectable memory.  */

void *
xmalloc_uncollectable (size_t size)
{
  void *val = GC_MALLOC_UNCOLLECTABLE (size);
  if (! val && size)
    memory_full (size);
  return val;
}

/* Allocate memory, but if memory is exhausted, return NULL instead of
   signalling an error.  */

void *
xmalloc_unsafe (size_t size)
{
  return GC_MALLOC (size);
}

/* Allocate pointerless memory, but if memory is exhausted, return
   NULL instead of signalling an error.  */

void *
xmalloc_atomic_unsafe (size_t size)
{
  return GC_MALLOC_ATOMIC (size);
}

/* Other parts of Emacs pass large int values to allocator functions
   expecting ptrdiff_t.  This is portable in practice, but check it to
   be safe.  */
verify (INT_MAX <= PTRDIFF_MAX);


/* Allocate an array of NITEMS items, each of size ITEM_SIZE.
   Signal an error on memory exhaustion.  */

void *
xnmalloc (ptrdiff_t nitems, ptrdiff_t item_size)
{
  eassert (0 <= nitems && 0 < item_size);
  if (min (PTRDIFF_MAX, SIZE_MAX) / item_size < nitems)
    memory_full (SIZE_MAX);
  return xmalloc (nitems * item_size);
}

/* Like xnmalloc for pointerless objects.  */

void *
xnmalloc_atomic (ptrdiff_t nitems, ptrdiff_t item_size)
{
  eassert (0 <= nitems && 0 < item_size);
  if (min (PTRDIFF_MAX, SIZE_MAX) / item_size < nitems)
    memory_full (SIZE_MAX);
  return xmalloc_atomic (nitems * item_size);
}

/* Reallocate an array PA to make it of NITEMS items, each of size ITEM_SIZE.
   Signal an error on memory exhaustion.  */

void *
xnrealloc (void *pa, ptrdiff_t nitems, ptrdiff_t item_size)
{
  eassert (0 <= nitems && 0 < item_size);
  if (min (PTRDIFF_MAX, SIZE_MAX) / item_size < nitems)
    memory_full (SIZE_MAX);
  return xrealloc (pa, nitems * item_size);
}


/* Grow PA, which points to an array of *NITEMS items, and return the
   location of the reallocated array, updating *NITEMS to reflect its
   new size.  The new array will contain at least NITEMS_INCR_MIN more
   items, but will not contain more than NITEMS_MAX items total.
   ITEM_SIZE is the size of each item, in bytes.

   ITEM_SIZE and NITEMS_INCR_MIN must be positive.  *NITEMS must be
   nonnegative.  If NITEMS_MAX is -1, it is treated as if it were
   infinity.

   If PA is null, then allocate a new array instead of reallocating
   the old one.

   If memory exhaustion occurs, set *NITEMS to zero if PA is null, and
   signal an error (i.e., do not return).

   Thus, to grow an array A without saving its old contents, do
   { xfree (A); A = NULL; A = xpalloc (NULL, &AITEMS, ...); }.
   The A = NULL avoids a dangling pointer if xpalloc exhausts memory
   and signals an error, and later this code is reexecuted and
   attempts to free A.  */

void *
xpalloc (void *pa, ptrdiff_t *nitems, ptrdiff_t nitems_incr_min,
	 ptrdiff_t nitems_max, ptrdiff_t item_size)
{
  /* The approximate size to use for initial small allocation
     requests.  This is the largest "small" request for the GNU C
     library malloc.  */
  enum { DEFAULT_MXFAST = 64 * sizeof (size_t) / 4 };

  /* If the array is tiny, grow it to about (but no greater than)
     DEFAULT_MXFAST bytes.  Otherwise, grow it by about 50%.  */
  ptrdiff_t n = *nitems;
  ptrdiff_t tiny_max = DEFAULT_MXFAST / item_size - n;
  ptrdiff_t half_again = n >> 1;
  ptrdiff_t incr_estimate = max (tiny_max, half_again);

  /* Adjust the increment according to three constraints: NITEMS_INCR_MIN,
     NITEMS_MAX, and what the C language can represent safely.  */
  ptrdiff_t C_language_max = min (PTRDIFF_MAX, SIZE_MAX) / item_size;
  ptrdiff_t n_max = (0 <= nitems_max && nitems_max < C_language_max
		     ? nitems_max : C_language_max);
  ptrdiff_t nitems_incr_max = n_max - n;
  ptrdiff_t incr = max (nitems_incr_min, min (incr_estimate, nitems_incr_max));

  eassert (0 < item_size && 0 < nitems_incr_min && 0 <= n && -1 <= nitems_max);
  if (! pa)
    *nitems = 0;
  if (nitems_incr_max < incr)
    memory_full (SIZE_MAX);
  n += incr;
  pa = xrealloc (pa, n * item_size);
  *nitems = n;
  return pa;
}


/* Like strdup, but uses xmalloc.  */

char *
xstrdup (const char *s)
{
  ptrdiff_t size;
  eassert (s);
  size = strlen (s) + 1;
  return memcpy (xmalloc_atomic (size), s, size);
}

/* Like above, but duplicates Lisp string to C string.  */

char *
xlispstrdup (Lisp_Object string)
{
  ptrdiff_t size = SBYTES (string) + 1;
  return memcpy (xmalloc_atomic (size), SSDATA (string), size);
}

/* Assign to *PTR a copy of STRING, freeing any storage *PTR formerly
   pointed to.  If STRING is null, assign it without copying anything.
   Allocate before freeing, to avoid a dangling pointer if allocation
   fails.  */

void
dupstring (char **ptr, char const *string)
{
  char *old = *ptr;
  *ptr = string ? xstrdup (string) : 0;
  xfree (old);
}


/* Like putenv, but (1) use the equivalent of xmalloc and (2) the
   argument is a const pointer.  */

void
xputenv (char const *string)
{
  if (putenv ((char *) string) != 0)
    memory_full (0);
}

/* Return a newly allocated memory block of SIZE bytes, remembering
   to free it when unwinding.  */
void *
record_xmalloc (size_t size)
{
  void *p = xmalloc (size);
  record_unwind_protect_ptr (xfree, p);
  return p;
}

/***********************************************************************
			 Interval Allocation
 ***********************************************************************/

/* Return a new interval.  */

INTERVAL
make_interval (void)
{
  INTERVAL val = xmalloc (sizeof (struct interval));
  RESET_INTERVAL (val);
  return val;
}

/***********************************************************************
			  String Allocation
 ***********************************************************************/

/* Initialize string allocation.  Called from init_alloc_once.  */

static void
init_strings (void)
{
  empty_unibyte_string = make_empty_string (0);
  empty_multibyte_string = make_empty_string (1);
}

/* Return a new Lisp_String.  */

static struct Lisp_String *
allocate_string (void)
{
  return xmalloc (sizeof (struct Lisp_String));
}


/* Set up Lisp_String S for holding NCHARS characters, NBYTES bytes,
   plus a NUL byte at the end.  Allocate an sdata structure for S, and
   set S->data to its `u.data' member.  Store a NUL byte at the end of
   S->data.  Set S->size to NCHARS and S->size_byte to NBYTES.  Free
   S->data if it was initially non-null.  */

void
allocate_string_data (struct Lisp_String *s,
		      EMACS_INT nchars, EMACS_INT nbytes)
{
  unsigned char *data;

  if (STRING_BYTES_BOUND < nbytes)
    string_overflow ();

  data = GC_MALLOC_ATOMIC (nbytes + 1);
  s->data = data;
  s->size = nchars;
  s->size_byte = nbytes;
  s->data[nbytes] = '\0';
}

void
string_overflow (void)
{
  error ("Maximum string size exceeded");
}

static Lisp_Object
make_empty_string (int multibyte)
{
  Lisp_Object string;
  struct Lisp_String *s;

  s = allocate_string ();
  allocate_string_data (s, 0, 0);
  XSETSTRING (string, s);
  if (! multibyte)
    STRING_SET_UNIBYTE (string);

  return string;
}

DEFUN ("make-string", Fmake_string, Smake_string, 2, 2, 0,
       doc: /* Return a newly created string of length LENGTH, with INIT in each element.
LENGTH must be an integer.
INIT must be an integer that represents a character.  */)
  (Lisp_Object length, Lisp_Object init)
{
  register Lisp_Object val;
  int c;
  EMACS_INT nbytes;

  CHECK_NATNUM (length);
  CHECK_CHARACTER (init);

  c = XFASTINT (init);
  if (ASCII_CHAR_P (c))
    {
      nbytes = XINT (length);
      val = make_uninit_string (nbytes);
      memset (SDATA (val), c, nbytes);
      SDATA (val)[nbytes] = 0;
    }
  else
    {
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      ptrdiff_t len = CHAR_STRING (c, str);
      EMACS_INT string_len = XINT (length);
      unsigned char *p, *beg, *end;

      if (string_len > STRING_BYTES_BOUND / len)
	string_overflow ();
      nbytes = len * string_len;
      val = make_uninit_multibyte_string (string_len, nbytes);
      for (beg = SDATA (val), p = beg, end = beg + nbytes; p < end; p += len)
	{
	  /* First time we just copy `str' to the data of `val'.  */
	  if (p == beg)
	    memcpy (p, str, len);
	  else
	    {
	      /* Next time we copy largest possible chunk from
		 initialized to uninitialized part of `val'.  */
	      len = min (p - beg, end - p);
	      memcpy (p, beg, len);
	    }
	}
      *p = 0;
    }

  return val;
}

/* Fill A with 1 bits if INIT is non-nil, and with 0 bits otherwise.
   Return A.  */

Lisp_Object
bool_vector_fill (Lisp_Object a, Lisp_Object init)
{
  EMACS_INT nbits = bool_vector_size (a);
  if (0 < nbits)
    {
      unsigned char *data = bool_vector_uchar_data (a);
      int pattern = NILP (init) ? 0 : (1 << BOOL_VECTOR_BITS_PER_CHAR) - 1;
      ptrdiff_t nbytes = bool_vector_bytes (nbits);
      int last_mask = ~ (~0u << ((nbits - 1) % BOOL_VECTOR_BITS_PER_CHAR + 1));
      memset (data, pattern, nbytes - 1);
      data[nbytes - 1] = pattern & last_mask;
    }
  return a;
}

/* Return a newly allocated, uninitialized bool vector of size NBITS.  */

Lisp_Object
make_uninit_bool_vector (EMACS_INT nbits)
{
  Lisp_Object val;
  EMACS_INT words = bool_vector_words (nbits);
  EMACS_INT word_bytes = words * sizeof (bits_word);
  EMACS_INT needed_elements = ((bool_header_size - header_size + word_bytes
				+ word_size - 1)
			       / word_size);
  struct Lisp_Bool_Vector *p
    = (struct Lisp_Bool_Vector *) allocate_vector (needed_elements);
  XSETVECTOR (val, p);
  XSETPVECTYPESIZE (XVECTOR (val), PVEC_BOOL_VECTOR, 0, 0);
  p->size = nbits;

  /* Clear padding at the end.  */
  if (words)
    p->data[words - 1] = 0;

  return val;
}

DEFUN ("make-bool-vector", Fmake_bool_vector, Smake_bool_vector, 2, 2, 0,
       doc: /* Return a new bool-vector of length LENGTH, using INIT for each element.
LENGTH must be a number.  INIT matters only in whether it is t or nil.  */)
  (Lisp_Object length, Lisp_Object init)
{
  Lisp_Object val;

  CHECK_NATNUM (length);
  val = make_uninit_bool_vector (XFASTINT (length));
  return bool_vector_fill (val, init);
}

DEFUN ("bool-vector", Fbool_vector, Sbool_vector, 0, MANY, 0,
       doc: /* Return a new bool-vector with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
usage: (bool-vector &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;
  Lisp_Object vector;

  vector = make_uninit_bool_vector (nargs);
  for (i = 0; i < nargs; i++)
    bool_vector_set (vector, i, !NILP (args[i]));

  return vector;
}

/* Make a string from NBYTES bytes at CONTENTS, and compute the number
   of characters from the contents.  This string may be unibyte or
   multibyte, depending on the contents.  */

Lisp_Object
make_string (const char *contents, ptrdiff_t nbytes)
{
  register Lisp_Object val;
  ptrdiff_t nchars, multibyte_nbytes;

  parse_str_as_multibyte ((const unsigned char *) contents, nbytes,
			  &nchars, &multibyte_nbytes);
  if (nbytes == nchars || nbytes != multibyte_nbytes)
    /* CONTENTS contains no multibyte sequences or contains an invalid
       multibyte sequence.  We must make unibyte string.  */
    val = make_unibyte_string (contents, nbytes);
  else
    val = make_multibyte_string (contents, nchars, nbytes);
  return val;
}


/* Make an unibyte string from LENGTH bytes at CONTENTS.  */

Lisp_Object
make_unibyte_string (const char *contents, ptrdiff_t length)
{
  register Lisp_Object val;
  val = make_uninit_string (length);
  memcpy (SDATA (val), contents, length);
  return val;
}


/* Make a multibyte string from NCHARS characters occupying NBYTES
   bytes at CONTENTS.  */

Lisp_Object
make_multibyte_string (const char *contents,
		       ptrdiff_t nchars, ptrdiff_t nbytes)
{
  register Lisp_Object val;
  val = make_uninit_multibyte_string (nchars, nbytes);
  memcpy (SDATA (val), contents, nbytes);
  return val;
}


/* Make a string from NCHARS characters occupying NBYTES bytes at
   CONTENTS.  It is a multibyte string if NBYTES != NCHARS.  */

Lisp_Object
make_string_from_bytes (const char *contents,
			ptrdiff_t nchars, ptrdiff_t nbytes)
{
  register Lisp_Object val;
  val = make_uninit_multibyte_string (nchars, nbytes);
  memcpy (SDATA (val), contents, nbytes);
  if (SBYTES (val) == SCHARS (val))
    STRING_SET_UNIBYTE (val);
  return val;
}


/* Make a string from NCHARS characters occupying NBYTES bytes at
   CONTENTS.  The argument MULTIBYTE controls whether to label the
   string as multibyte.  If NCHARS is negative, it counts the number of
   characters by itself.  */

Lisp_Object
make_specified_string (const char *contents,
		       ptrdiff_t nchars, ptrdiff_t nbytes, bool multibyte)
{
  Lisp_Object val;

  if (nchars < 0)
    {
      if (multibyte)
	nchars = multibyte_chars_in_text ((const unsigned char *) contents,
					  nbytes);
      else
	nchars = nbytes;
    }
  val = make_uninit_multibyte_string (nchars, nbytes);
  memcpy (SDATA (val), contents, nbytes);
  if (!multibyte)
    STRING_SET_UNIBYTE (val);
  return val;
}


/* Return an unibyte Lisp_String set up to hold LENGTH characters
   occupying LENGTH bytes.  */

Lisp_Object
make_uninit_string (EMACS_INT length)
{
  Lisp_Object val;

  if (!length)
    return empty_unibyte_string;
  val = make_uninit_multibyte_string (length, length);
  STRING_SET_UNIBYTE (val);
  return val;
}


/* Return a multibyte Lisp_String set up to hold NCHARS characters
   which occupy NBYTES bytes.  */

Lisp_Object
make_uninit_multibyte_string (EMACS_INT nchars, EMACS_INT nbytes)
{
  Lisp_Object string;
  struct Lisp_String *s;

  if (nchars < 0)
    emacs_abort ();
  if (!nbytes)
    return empty_multibyte_string;

  s = allocate_string ();
  s->intervals = NULL;
  allocate_string_data (s, nchars, nbytes);
  XSETSTRING (string, s);
  return string;
}

/* Print arguments to BUF according to a FORMAT, then return
   a Lisp_String initialized with the data from BUF.  */

Lisp_Object
make_formatted_string (char *buf, const char *format, ...)
{
  va_list ap;
  int length;

  va_start (ap, format);
  length = vsprintf (buf, format, ap);
  va_end (ap);
  return make_string (buf, length);
}


/***********************************************************************
			   Float Allocation
 ***********************************************************************/

/* Return a new float object with value FLOAT_VALUE.  */

Lisp_Object
make_float (double float_value)
{
  register Lisp_Object val;
  XSETFLOAT (val, xmalloc_atomic (sizeof (struct Lisp_Float)));
  XFLOAT_INIT (val, float_value);
  return val;
}



/***********************************************************************
			   Cons Allocation
 ***********************************************************************/

DEFUN ("cons", Fcons, Scons, 2, 2, 0,
       doc: /* Create a new cons, give it CAR and CDR as components, and return it.  */)
  (Lisp_Object car, Lisp_Object cdr)
{
  register Lisp_Object val;

  XSETCONS (val, xmalloc (sizeof (struct Lisp_Cons)));
  XSETCAR (val, car);
  XSETCDR (val, cdr);
  return val;
}

/* Make a list of 1, 2, 3, 4 or 5 specified objects.  */

Lisp_Object
list1 (Lisp_Object arg1)
{
  return Fcons (arg1, Qnil);
}

Lisp_Object
list2 (Lisp_Object arg1, Lisp_Object arg2)
{
  return Fcons (arg1, Fcons (arg2, Qnil));
}


Lisp_Object
list3 (Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Qnil)));
}


Lisp_Object
list4 (Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3, Lisp_Object arg4)
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Fcons (arg4, Qnil))));
}


Lisp_Object
list5 (Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3, Lisp_Object arg4, Lisp_Object arg5)
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Fcons (arg4,
						       Fcons (arg5, Qnil)))));
}

/* Make a list of COUNT Lisp_Objects, where ARG is the
   first one.  Allocate conses from pure space if TYPE
   is CONSTYPE_PURE, or allocate as usual if type is CONSTYPE_HEAP.  */

Lisp_Object
listn (enum constype type, ptrdiff_t count, Lisp_Object arg, ...)
{
  va_list ap;
  ptrdiff_t i;
  Lisp_Object val, *objp;

  /* Change to SAFE_ALLOCA if you hit this eassert.  */
  eassert (count <= MAX_ALLOCA / word_size);

  objp = alloca (count * word_size);
  objp[0] = arg;
  va_start (ap, arg);
  for (i = 1; i < count; i++)
    objp[i] = va_arg (ap, Lisp_Object);
  va_end (ap);

  for (val = Qnil, i = count - 1; i >= 0; i--)
    {
      if (type == CONSTYPE_PURE)
	val = pure_cons (objp[i], val);
      else if (type == CONSTYPE_HEAP)
	val = Fcons (objp[i], val);
      else
	emacs_abort ();
    }
  return val;
}

DEFUN ("list", Flist, Slist, 0, MANY, 0,
       doc: /* Return a newly created list with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
usage: (list &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  register Lisp_Object val;
  val = Qnil;

  while (nargs > 0)
    {
      nargs--;
      val = Fcons (args[nargs], val);
    }
  return val;
}


DEFUN ("make-list", Fmake_list, Smake_list, 2, 2, 0,
       doc: /* Return a newly created list of length LENGTH, with each element being INIT.  */)
  (register Lisp_Object length, Lisp_Object init)
{
  register Lisp_Object val;
  register EMACS_INT size;

  CHECK_NATNUM (length);
  size = XFASTINT (length);

  val = Qnil;
  while (size > 0)
    {
      val = Fcons (init, val);
      --size;

      if (size > 0)
	{
	  val = Fcons (init, val);
	  --size;

	  if (size > 0)
	    {
	      val = Fcons (init, val);
	      --size;

	      if (size > 0)
		{
		  val = Fcons (init, val);
		  --size;

		  if (size > 0)
		    {
		      val = Fcons (init, val);
		      --size;
		    }
		}
	    }
	}

      QUIT;
    }

  return val;
}



/***********************************************************************
			   Vector Allocation
 ***********************************************************************/

/* The only vector with 0 slots, allocated from pure space.  */

Lisp_Object zero_vector;

/* Called once to initialize vector allocation.  */

static void
init_vectors (void)
{
  XSETVECTOR (zero_vector, xmalloc (header_size));
  XVECTOR (zero_vector)->header.size = 0;
}

/* Value is a pointer to a newly allocated Lisp_Vector structure
   with room for LEN Lisp_Objects.  */

static struct Lisp_Vector *
allocate_vectorlike (ptrdiff_t len)
{
  if (len == 0)
    return XVECTOR (zero_vector);
  else
    return xmalloc (header_size + len * word_size);
}


/* Allocate a vector with LEN slots.  */

struct Lisp_Vector *
allocate_vector (EMACS_INT len)
{
  struct Lisp_Vector *v;
  ptrdiff_t nbytes_max = min (PTRDIFF_MAX, SIZE_MAX);

  if (min ((nbytes_max - header_size) / word_size, MOST_POSITIVE_FIXNUM) < len)
    memory_full (SIZE_MAX);
  v = allocate_vectorlike (len);
  v->header.size = len;
  return v;
}


/* Allocate other vector-like structures.  */

struct Lisp_Vector *
allocate_pseudovector (int memlen, int lisplen, enum pvec_type tag)
{
  struct Lisp_Vector *v = allocate_vectorlike (memlen);
  int i;

  /* Catch bogus values.  */
  eassert (tag <= PVEC_FONT);
  eassert (memlen - lisplen <= (1 << PSEUDOVECTOR_REST_BITS) - 1);
  eassert (lisplen <= (1 << PSEUDOVECTOR_SIZE_BITS) - 1);

  /* Only the first lisplen slots will be traced normally by the GC.  */
  for (i = 0; i < lisplen; ++i)
    v->contents[i] = Qnil;

  XSETPVECTYPESIZE (v, tag, lisplen, memlen - lisplen);
  return v;
}

struct buffer *
allocate_buffer (void)
{
  struct buffer *b = xmalloc (sizeof *b);

  BUFFER_PVEC_INIT (b);
  /* Put B on the chain of all buffers including killed ones.  */
  b->next = all_buffers;
  all_buffers = b;
  /* Note that the rest fields of B are not initialized.  */
  return b;
}

struct Lisp_Hash_Table *
allocate_hash_table (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct Lisp_Hash_Table, count, PVEC_HASH_TABLE);
}

struct window *
allocate_window (void)
{
  struct window *w;

  w = ALLOCATE_PSEUDOVECTOR (struct window, current_matrix, PVEC_WINDOW);
  /* Users assumes that non-Lisp data is zeroed.  */
  memset (&w->current_matrix, 0,
	  sizeof (*w) - offsetof (struct window, current_matrix));
  return w;
}

struct terminal *
allocate_terminal (void)
{
  struct terminal *t;

  t = ALLOCATE_PSEUDOVECTOR (struct terminal, next_terminal, PVEC_TERMINAL);
  /* Users assumes that non-Lisp data is zeroed.  */
  memset (&t->next_terminal, 0,
	  sizeof (*t) - offsetof (struct terminal, next_terminal));
  return t;
}

struct frame *
allocate_frame (void)
{
  struct frame *f;

  f = ALLOCATE_PSEUDOVECTOR (struct frame, face_cache, PVEC_FRAME);
  /* Users assumes that non-Lisp data is zeroed.  */
  memset (&f->face_cache, 0,
	  sizeof (*f) - offsetof (struct frame, face_cache));
  return f;
}

struct Lisp_Process *
allocate_process (void)
{
  struct Lisp_Process *p;

  p = ALLOCATE_PSEUDOVECTOR (struct Lisp_Process, pid, PVEC_PROCESS);
  /* Users assumes that non-Lisp data is zeroed.  */
  memset (&p->pid, 0,
	  sizeof (*p) - offsetof (struct Lisp_Process, pid));
  return p;
}

DEFUN ("make-vector", Fmake_vector, Smake_vector, 2, 2, 0,
       doc: /* Return a newly created vector of length LENGTH, with each element being INIT.
See also the function `vector'.  */)
  (register Lisp_Object length, Lisp_Object init)
{
  Lisp_Object vector;
  register ptrdiff_t sizei;
  register ptrdiff_t i;
  register struct Lisp_Vector *p;

  CHECK_NATNUM (length);

  p = allocate_vector (XFASTINT (length));
  sizei = XFASTINT (length);
  for (i = 0; i < sizei; i++)
    p->contents[i] = init;

  XSETVECTOR (vector, p);
  return vector;
}


DEFUN ("vector", Fvector, Svector, 0, MANY, 0,
       doc: /* Return a newly created vector with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
usage: (vector &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;
  register Lisp_Object val = make_uninit_vector (nargs);
  register struct Lisp_Vector *p = XVECTOR (val);

  for (i = 0; i < nargs; i++)
    p->contents[i] = args[i];
  return val;
}

void
make_byte_code (struct Lisp_Vector *v)
{
  /* Don't allow the global zero_vector to become a byte code object.  */
  eassert (0 < v->header.size);

  if (v->header.size > 1 && STRINGP (v->contents[1])
      && STRING_MULTIBYTE (v->contents[1]))
    /* BYTECODE-STRING must have been produced by Emacs 20.2 or the
       earlier because they produced a raw 8-bit string for byte-code
       and now such a byte-code string is loaded as multibyte while
       raw 8-bit characters converted to multibyte form.  Thus, now we
       must convert them back to the original unibyte form.  */
    v->contents[1] = Fstring_as_unibyte (v->contents[1]);
  XSETPVECTYPE (v, PVEC_COMPILED);
}

DEFUN ("make-byte-code", Fmake_byte_code, Smake_byte_code, 4, MANY, 0,
       doc: /* Create a byte-code object with specified arguments as elements.
The arguments should be the ARGLIST, bytecode-string BYTE-CODE, constant
vector CONSTANTS, maximum stack size DEPTH, (optional) DOCSTRING,
and (optional) INTERACTIVE-SPEC.
The first four arguments are required; at most six have any
significance.
The ARGLIST can be either like the one of `lambda', in which case the arguments
will be dynamically bound before executing the byte code, or it can be an
integer of the form NNNNNNNRMMMMMMM where the 7bit MMMMMMM specifies the
minimum number of arguments, the 7-bit NNNNNNN specifies the maximum number
of arguments (ignoring &rest) and the R bit specifies whether there is a &rest
argument to catch the left-over arguments.  If such an integer is used, the
arguments will not be dynamically bound but will be instead pushed on the
stack before executing the byte-code.
usage: (make-byte-code ARGLIST BYTE-CODE CONSTANTS DEPTH &optional DOCSTRING INTERACTIVE-SPEC &rest ELEMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;
  register Lisp_Object val = make_uninit_vector (nargs);
  register struct Lisp_Vector *p = XVECTOR (val);

  /* We used to purecopy everything here, if purify-flag was set.  This worked
     OK for Emacs-23, but with Emacs-24's lexical binding code, it can be
     dangerous, since make-byte-code is used during execution to build
     closures, so any closure built during the preload phase would end up
     copied into pure space, including its free variables, which is sometimes
     just wasteful and other times plainly wrong (e.g. those free vars may want
     to be setcar'd).  */

  for (i = 0; i < nargs; i++)
    p->contents[i] = args[i];
  make_byte_code (p);
  XSETCOMPILED (val, p);
  return val;
}



/***********************************************************************
			   Symbol Allocation
 ***********************************************************************/

static void
set_symbol_name (Lisp_Object sym, Lisp_Object name)
{
  XSYMBOL (sym)->name = name;
}

DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
       doc: /* Return a newly allocated uninterned symbol whose name is NAME.
Its value is void, and its function definition and property list are nil.  */)
  (Lisp_Object name)
{
  register Lisp_Object val;
  register struct Lisp_Symbol *p;

  CHECK_STRING (name);

  XSETSYMBOL (val, xmalloc (sizeof (struct Lisp_Symbol)));
  p = XSYMBOL (val);
  set_symbol_name (val, name);
  set_symbol_plist (val, Qnil);
  p->redirect = SYMBOL_PLAINVAL;
  SET_SYMBOL_VAL (p, Qunbound);
  set_symbol_function (val, Qnil);
  set_symbol_next (val, NULL);
  p->interned = SYMBOL_UNINTERNED;
  p->constant = 0;
  p->declared_special = false;
  p->pinned = false;
  return val;
}



/***********************************************************************
		       Marker (Misc) Allocation
 ***********************************************************************/

/* Return a newly allocated Lisp_Misc object of specified TYPE.  */

static Lisp_Object
allocate_misc (enum Lisp_Misc_Type type)
{
  Lisp_Object val;

  XSETMISC (val, xmalloc (sizeof (union Lisp_Misc)));
  XMISCANY (val)->type = type;
  return val;
}

/* Free a Lisp_Misc object.  */

void
free_misc (Lisp_Object misc)
{
  return;
}

/* Verify properties of Lisp_Save_Value's representation
   that are assumed here and elsewhere.  */

verify (SAVE_UNUSED == 0);
verify (((SAVE_INTEGER | SAVE_POINTER | SAVE_FUNCPOINTER | SAVE_OBJECT)
	 >> SAVE_SLOT_BITS)
	== 0);

/* Return Lisp_Save_Value objects for the various combinations
   that callers need.  */

Lisp_Object
make_save_int_int_int (ptrdiff_t a, ptrdiff_t b, ptrdiff_t c)
{
  Lisp_Object val = allocate_misc (Lisp_Misc_Save_Value);
  struct Lisp_Save_Value *p = XSAVE_VALUE (val);
  p->save_type = SAVE_TYPE_INT_INT_INT;
  p->data[0].integer = a;
  p->data[1].integer = b;
  p->data[2].integer = c;
  return val;
}

Lisp_Object
make_save_obj_obj_obj_obj (Lisp_Object a, Lisp_Object b, Lisp_Object c,
			   Lisp_Object d)
{
  Lisp_Object val = allocate_misc (Lisp_Misc_Save_Value);
  struct Lisp_Save_Value *p = XSAVE_VALUE (val);
  p->save_type = SAVE_TYPE_OBJ_OBJ_OBJ_OBJ;
  p->data[0].object = a;
  p->data[1].object = b;
  p->data[2].object = c;
  p->data[3].object = d;
  return val;
}

Lisp_Object
make_save_ptr (void *a)
{
  Lisp_Object val = allocate_misc (Lisp_Misc_Save_Value);
  struct Lisp_Save_Value *p = XSAVE_VALUE (val);
  p->save_type = SAVE_POINTER;
  p->data[0].pointer = a;
  return val;
}

Lisp_Object
make_save_ptr_int (void *a, ptrdiff_t b)
{
  Lisp_Object val = allocate_misc (Lisp_Misc_Save_Value);
  struct Lisp_Save_Value *p = XSAVE_VALUE (val);
  p->save_type = SAVE_TYPE_PTR_INT;
  p->data[0].pointer = a;
  p->data[1].integer = b;
  return val;
}

#if ! (defined USE_X_TOOLKIT || defined USE_GTK)
Lisp_Object
make_save_ptr_ptr (void *a, void *b)
{
  Lisp_Object val = allocate_misc (Lisp_Misc_Save_Value);
  struct Lisp_Save_Value *p = XSAVE_VALUE (val);
  p->save_type = SAVE_TYPE_PTR_PTR;
  p->data[0].pointer = a;
  p->data[1].pointer = b;
  return val;
}
#endif

Lisp_Object
make_save_funcptr_ptr_obj (void (*a) (void), void *b, Lisp_Object c)
{
  Lisp_Object val = allocate_misc (Lisp_Misc_Save_Value);
  struct Lisp_Save_Value *p = XSAVE_VALUE (val);
  p->save_type = SAVE_TYPE_FUNCPTR_PTR_OBJ;
  p->data[0].funcpointer = a;
  p->data[1].pointer = b;
  p->data[2].object = c;
  return val;
}

/* Return a Lisp_Save_Value object that represents an array A
   of N Lisp objects.  */

Lisp_Object
make_save_memory (Lisp_Object *a, ptrdiff_t n)
{
  Lisp_Object val = allocate_misc (Lisp_Misc_Save_Value);
  struct Lisp_Save_Value *p = XSAVE_VALUE (val);
  p->save_type = SAVE_TYPE_MEMORY;
  p->data[0].pointer = a;
  p->data[1].integer = n;
  return val;
}

/* Free a Lisp_Save_Value object.  Do not use this function
   if SAVE contains pointer other than returned by xmalloc.  */

void
free_save_value (Lisp_Object save)
{
  xfree (XSAVE_POINTER (save, 0));
  free_misc (save);
}

/* Return a Lisp_Misc_Overlay object with specified START, END and PLIST.  */

Lisp_Object
build_overlay (Lisp_Object start, Lisp_Object end, Lisp_Object plist)
{
  register Lisp_Object overlay;

  overlay = allocate_misc (Lisp_Misc_Overlay);
  OVERLAY_START (overlay) = start;
  OVERLAY_END (overlay) = end;
  set_overlay_plist (overlay, plist);
  XOVERLAY (overlay)->next = NULL;
  return overlay;
}

DEFUN ("make-marker", Fmake_marker, Smake_marker, 0, 0, 0,
       doc: /* Return a newly allocated marker which does not point at any place.  */)
  (void)
{
  register Lisp_Object val;
  register struct Lisp_Marker *p;

  val = allocate_misc (Lisp_Misc_Marker);
  p = XMARKER (val);
  p->buffer = 0;
  p->bytepos = 0;
  p->charpos = 0;
  p->next = NULL;
  p->insertion_type = 0;
  p->need_adjustment = 0;
  return val;
}

/* Return a newly allocated marker which points into BUF
   at character position CHARPOS and byte position BYTEPOS.  */

Lisp_Object
build_marker (struct buffer *buf, ptrdiff_t charpos, ptrdiff_t bytepos)
{
  Lisp_Object obj;
  struct Lisp_Marker *m;

  /* No dead buffers here.  */
  eassert (BUFFER_LIVE_P (buf));

  /* Every character is at least one byte.  */
  eassert (charpos <= bytepos);

  obj = allocate_misc (Lisp_Misc_Marker);
  m = XMARKER (obj);
  m->buffer = buf;
  m->charpos = charpos;
  m->bytepos = bytepos;
  m->insertion_type = 0;
  m->need_adjustment = 0;
  m->next = BUF_MARKERS (buf);
  BUF_MARKERS (buf) = m;
  return obj;
}

/* Return a newly created vector or string with specified arguments as
   elements.  If all the arguments are characters that can fit
   in a string of events, make a string; otherwise, make a vector.

   Any number of arguments, even zero arguments, are allowed.  */

Lisp_Object
make_event_array (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;

  for (i = 0; i < nargs; i++)
    /* The things that fit in a string
       are characters that are in 0...127,
       after discarding the meta bit and all the bits above it.  */
    if (!INTEGERP (args[i])
	|| (XINT (args[i]) & ~(-CHAR_META)) >= 0200)
      return Fvector (nargs, args);

  /* Since the loop exited, we know that all the things in it are
     characters, so we can make a string.  */
  {
    Lisp_Object result;

    result = Fmake_string (make_number (nargs), make_number (0));
    for (i = 0; i < nargs; i++)
      {
	SSET (result, i, XINT (args[i]));
	/* Move the meta bit to the right place for a string char.  */
	if (XINT (args[i]) & CHAR_META)
	  SSET (result, i, SREF (result, i) | 0x80);
      }

    return result;
  }
}



/************************************************************************
			   Memory Full Handling
 ************************************************************************/


/* Called if xmalloc (NBYTES) returns zero.  If NBYTES == SIZE_MAX,
   there may have been size_t overflow so that xmalloc was never
   called, or perhaps xmalloc was invoked successfully but the
   resulting pointer had problems fitting into a tagged EMACS_INT.  In
   either case this counts as memory being full even though xmalloc
   did not fail.  */

void
memory_full (size_t nbytes)
{
  /* Do not go into hysterics merely because a large request failed.  */
  bool enough_free_memory = 0;
  if (SPARE_MEMORY < nbytes)
    {
      void *p = xmalloc_atomic_unsafe (SPARE_MEMORY);
      if (p)
	{
	  xfree (p);
	  enough_free_memory = 1;
	}
    }

  if (! enough_free_memory)
    {
      Vmemory_full = Qt;

      /* The first time we get here, free the spare memory.  */
      if (spare_memory)
        {
          xfree (spare_memory);
          spare_memory = NULL;
        }
    }

  /* This used to call error, but if we've run out of memory, we could
     get infinite recursion trying to build the string.  */
  xsignal (Qnil, Vmemory_signal_data);
}

/* If we released our reserve (due to running out of memory),
   and we have a fair amount free once again,
   try to set aside another reserve in case we run out once more.

   This is called when a relocatable block is freed in ralloc.c,
   and also directly from this file, in case we're not using ralloc.c.  */

void
refill_memory_reserve (void)
{
  if (spare_memory == NULL)
    spare_memory = xmalloc_atomic_unsafe (SPARE_MEMORY);

  if (spare_memory)
    Vmemory_full = Qnil;
}

/* Determine whether it is safe to access memory at address P.  */
static int
valid_pointer_p (void *p)
{
#ifdef WINDOWSNT
  return w32_valid_pointer_p (p, 16);
#else
  int fd[2];

  /* Obviously, we cannot just access it (we would SEGV trying), so we
     trick the o/s to tell us whether p is a valid pointer.
     Unfortunately, we cannot use NULL_DEVICE here, as emacs_write may
     not validate p in that case.  */

  if (emacs_pipe (fd) == 0)
    {
      bool valid = emacs_write (fd[1], p, 16) == 16;
      emacs_close (fd[1]);
      emacs_close (fd[0]);
      return valid;
    }

    return -1;
#endif
}

/* Return 2 if OBJ is a killed or special buffer object, 1 if OBJ is a
   valid lisp object, 0 if OBJ is NOT a valid lisp object, or -1 if we
   cannot validate OBJ.  This function can be quite slow, so its primary
   use is the manual debugging.  The only exception is print_object, where
   we use it to check whether the memory referenced by the pointer of
   Lisp_Save_Value object contains valid objects.  */

int
valid_lisp_object_p (Lisp_Object obj)
{
  void *p;

  if (INTEGERP (obj))
    return 1;

  p = (void *) XPNTR (obj);

  if (p == &buffer_defaults || p == &buffer_local_symbols)
    return 2;

  return valid_pointer_p (p);
}

/* If GC_MARK_STACK, return 1 if STR is a relocatable data of Lisp_String
   (i.e. there is a non-pure Lisp_Object X so that SDATA (X) == STR) and 0
   if not.  Otherwise we can't rely on valid_lisp_object_p and return -1.
   This function is slow and should be used for debugging purposes.  */

int
relocatable_string_data_p (const char *str)
{
  return -1;
}

/***********************************************************************
                 Pure Storage Compatibility Functions
 ***********************************************************************/

void
check_pure_size (void)
{
  return;
}

Lisp_Object
make_pure_string (const char *data,
		  ptrdiff_t nchars, ptrdiff_t nbytes, bool multibyte)
{
  return make_specified_string (data, nchars, nbytes, multibyte);
}

Lisp_Object
make_pure_c_string (const char *data, ptrdiff_t nchars)
{
  return build_string (data);
}

Lisp_Object
pure_cons (Lisp_Object car, Lisp_Object cdr)
{
  return Fcons (car, cdr);
}

DEFUN ("purecopy", Fpurecopy, Spurecopy, 1, 1, 0,
       doc: /* Return OBJ.  */)
  (register Lisp_Object obj)
{
  return obj;
}

/***********************************************************************
			  Protection from GC
 ***********************************************************************/

void
staticpro (Lisp_Object *varaddress)
{
  return;
}

DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 0, "",
       doc: /* Reclaim storage for Lisp objects no longer needed.
Garbage collection happens automatically if you cons more than
`gc-cons-threshold' bytes of Lisp data since previous garbage collection.
`garbage-collect' normally returns a list with info on amount of space in use,
where each entry has the form (NAME SIZE USED FREE), where:
- NAME is a symbol describing the kind of objects this entry represents,
- SIZE is the number of bytes used by each one,
- USED is the number of those objects that were found live in the heap,
- FREE is the number of those objects that are not live but that Emacs
  keeps around for future allocations (maybe because it does not know how
  to return them to the OS).
However, if there was overflow in pure space, `garbage-collect'
returns nil, because real GC can't be done.
See Info node `(elisp)Garbage Collection'.  */)
  (void)
{
  GC_gcollect ();
  return Qt;
}

#ifdef ENABLE_CHECKING

bool suppress_checking;

void
die (const char *msg, const char *file, int line)
{
  fprintf (stderr, "\r\n%s:%d: Emacs fatal error: assertion failed: %s\r\n",
	   file, line, msg);
  terminate_due_to_signal (SIGABRT, INT_MAX);
}
#endif

/* Initialization.  */

void
init_alloc_once (void)
{
  /* Used to do Vpurify_flag = Qt here, but Qt isn't set up yet!  */

  init_strings ();
  init_vectors ();

  refill_memory_reserve ();
  gc_cons_threshold = GC_DEFAULT_THRESHOLD;
}

void
init_alloc (void)
{
  gcprolist = 0;
  Vgc_elapsed = make_float (0.0);
  gcs_done = 0;

#if USE_VALGRIND
  valgrind_p = RUNNING_ON_VALGRIND != 0;
#endif
}

void
syms_of_alloc (void)
{
  DEFVAR_INT ("gc-cons-threshold", gc_cons_threshold,
	      doc: /* Number of bytes of consing between garbage collections.
Garbage collection can happen automatically once this many bytes have been
allocated since the last garbage collection.  All data types count.

Garbage collection happens automatically only when `eval' is called.

By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.
See also `gc-cons-percentage'.  */);

  DEFVAR_LISP ("gc-cons-percentage", Vgc_cons_percentage,
	       doc: /* Portion of the heap used for allocation.
Garbage collection can happen automatically once this portion of the heap
has been allocated since the last garbage collection.
If this portion is smaller than `gc-cons-threshold', this is ignored.  */);
  Vgc_cons_percentage = make_float (0.1);

  DEFVAR_INT ("pure-bytes-used", pure_bytes_used,
	      doc: /* Number of bytes of shareable Lisp data allocated so far.  */);

  DEFVAR_LISP ("purify-flag", Vpurify_flag,
	       doc: /* Non-nil means loading Lisp code in order to dump an executable.
This means that certain objects should be allocated in shared (pure) space.
It can also be set to a hash-table, in which case this table is used to
do hash-consing of the objects allocated to pure space.  */);

  DEFVAR_BOOL ("garbage-collection-messages", garbage_collection_messages,
	       doc: /* Non-nil means display messages at start and end of garbage collection.  */);
  garbage_collection_messages = 0;

  DEFVAR_LISP ("post-gc-hook", Vpost_gc_hook,
	       doc: /* Hook run after garbage collection has finished.  */);
  Vpost_gc_hook = Qnil;
  DEFSYM (Qpost_gc_hook, "post-gc-hook");

  DEFVAR_LISP ("memory-signal-data", Vmemory_signal_data,
	       doc: /* Precomputed `signal' argument for memory-full error.  */);
  /* We build this in advance because if we wait until we need it, we might
     not be able to allocate the memory to hold it.  */
  Vmemory_signal_data
    = listn (CONSTYPE_PURE, 2, Qerror,
	     build_pure_c_string ("Memory exhausted--use M-x save-some-buffers then exit and restart Emacs"));

  DEFVAR_LISP ("memory-full", Vmemory_full,
	       doc: /* Non-nil means Emacs cannot get much more Lisp memory.  */);
  Vmemory_full = Qnil;

  DEFSYM (Qgc_cons_threshold, "gc-cons-threshold");
  DEFSYM (Qchar_table_extra_slots, "char-table-extra-slots");

  DEFVAR_LISP ("gc-elapsed", Vgc_elapsed,
	       doc: /* Accumulated time elapsed in garbage collections.
The time is in seconds as a floating point value.  */);
  DEFVAR_INT ("gcs-done", gcs_done,
	      doc: /* Accumulated number of garbage collections done.  */);

  defsubr (&Scons);
  defsubr (&Slist);
  defsubr (&Svector);
  defsubr (&Sbool_vector);
  defsubr (&Smake_byte_code);
  defsubr (&Smake_list);
  defsubr (&Smake_vector);
  defsubr (&Smake_string);
  defsubr (&Smake_bool_vector);
  defsubr (&Smake_symbol);
  defsubr (&Smake_marker);
  defsubr (&Spurecopy);
  defsubr (&Sgarbage_collect);
}

/* When compiled with GCC, GDB might say "No enum type named
   pvec_type" if we don't have at least one symbol with that type, and
   then xbacktrace could fail.  Similarly for the other enums and
   their values.  Some non-GCC compilers don't like these constructs.  */
#ifdef __GNUC__
union
{
  enum CHARTAB_SIZE_BITS CHARTAB_SIZE_BITS;
  enum CHAR_TABLE_STANDARD_SLOTS CHAR_TABLE_STANDARD_SLOTS;
  enum char_bits char_bits;
  enum CHECK_LISP_OBJECT_TYPE CHECK_LISP_OBJECT_TYPE;
  enum DEFAULT_HASH_SIZE DEFAULT_HASH_SIZE;
  enum Lisp_Bits Lisp_Bits;
  enum Lisp_Compiled Lisp_Compiled;
  enum maxargs maxargs;
  enum MAX_ALLOCA MAX_ALLOCA;
  enum More_Lisp_Bits More_Lisp_Bits;
  enum pvec_type pvec_type;
} const EXTERNALLY_VISIBLE gdb_make_enums_visible = {0};
#endif	/* __GNUC__ */
