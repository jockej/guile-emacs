### lisp.mk --- src/Makefile fragment for GNU Emacs

## Copyright (C) 1985, 1987-1988, 1993-1995, 1999-2014 Free Software
## Foundation, Inc.

## This file is part of GNU Emacs.

## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

### Commentary:

## This is the list of all Lisp files that might be loaded into the
## dumped Emacs.  Some of them are not loaded on all platforms, but
## the DOC file on every platform uses them (because the DOC file is
## supposed to be platform-independent).
## It is arranged like this because it is easier to generate it
## semi-mechanically from loadup.el this way.
## Eg something like:
##   sed -e 's/"[ )].*//' -n -e '/(load "/ s/.*load "//p' loadup.el | \
##   grep -vE 'site-|ldefs-boot'
## minus any duplicates.
## Note that you can generally just add a ".elc" extension to every file
## that does not have an explicit .el extension, but beware of any
## no-byte-compile ones.

## Confusingly, international/cp51932 and international/eucjp-ms are
## unconditionally loaded from language/japanese, instead of being
## loaded directly from loadup.el; FIXME.

## Note that this list should not include lisp files which might not
## be present, like site-load.el and site-init.el; this makefile
## expects them all to be either present or buildable.

## Place loaddefs.el first, so it gets generated first, since it is on
## the critical path (relevant in parallel compilations).

### Code:

## NB: This list is parsed by sed in the main src/Makefile.
## Do not change the formatting.
lisp = \
	$(lispsource)/loaddefs.el \
	$(lispsource)/loadup.el \
	$(lispsource)/emacs-lisp/byte-run.el \
	$(lispsource)/emacs-lisp/backquote.el \
	$(lispsource)/subr.el \
	$(lispsource)/version.el \
	$(lispsource)/widget.el \
	$(lispsource)/custom.el \
	$(lispsource)/emacs-lisp/map-ynp.el \
	$(lispsource)/cus-start.el \
	$(lispsource)/international/mule.el \
	$(lispsource)/international/mule-conf.el \
	$(lispsource)/env.el \
	$(lispsource)/format.el \
	$(lispsource)/bindings.el \
	$(lispsource)/files.el \
	$(lispsource)/emacs-lisp/macroexp.el \
	$(lispsource)/cus-face.el \
	$(lispsource)/faces.el \
	$(lispsource)/button.el \
	$(lispsource)/startup.el \
	$(lispsource)/emacs-lisp/nadvice.el \
	$(lispsource)/minibuffer.el \
	$(lispsource)/abbrev.el \
	$(lispsource)/simple.el \
	$(lispsource)/help.el \
	$(lispsource)/jka-cmpr-hook.el \
	$(lispsource)/epa-hook.el \
	$(lispsource)/international/mule-cmds.el \
	$(lispsource)/case-table.el \
	$(lispsource)/international/characters.el \
	$(lispsource)/composite.el \
	$(lispsource)/international/charprop.el \
	$(lispsource)/language/chinese.el \
	$(lispsource)/language/cyrillic.el \
	$(lispsource)/language/indian.el \
	$(lispsource)/language/sinhala.el \
	$(lispsource)/language/english.el \
	$(lispsource)/language/ethiopic.el \
	$(lispsource)/language/european.el \
	$(lispsource)/language/czech.el \
	$(lispsource)/language/slovak.el \
	$(lispsource)/language/romanian.el \
	$(lispsource)/language/greek.el \
	$(lispsource)/language/hebrew.el \
	$(lispsource)/language/japanese.el \
	$(lispsource)/international/cp51932.el \
	$(lispsource)/international/eucjp-ms.el \
	$(lispsource)/language/korean.el \
	$(lispsource)/language/lao.el \
	$(lispsource)/language/tai-viet.el \
	$(lispsource)/language/thai.el \
	$(lispsource)/language/tibetan.el \
	$(lispsource)/language/vietnamese.el \
	$(lispsource)/language/misc-lang.el \
	$(lispsource)/language/utf-8-lang.el \
	$(lispsource)/language/georgian.el \
	$(lispsource)/language/khmer.el \
	$(lispsource)/language/burmese.el \
	$(lispsource)/language/cham.el \
	$(lispsource)/indent.el \
	$(lispsource)/window.el \
	$(lispsource)/frame.el \
	$(lispsource)/term/tty-colors.el \
	$(lispsource)/font-core.el \
	$(lispsource)/facemenu.el \
	$(lispsource)/emacs-lisp/syntax.el \
	$(lispsource)/font-lock.el \
	$(lispsource)/jit-lock.el \
	$(lispsource)/mouse.el \
	$(lispsource)/scroll-bar.el \
	$(lispsource)/select.el \
	$(lispsource)/emacs-lisp/timer.el \
	$(lispsource)/isearch.el \
	$(lispsource)/rfn-eshadow.el \
	$(lispsource)/menu-bar.el \
	$(lispsource)/emacs-lisp/lisp.el \
	$(lispsource)/textmodes/page.el \
	$(lispsource)/register.el \
	$(lispsource)/textmodes/paragraphs.el \
	$(lispsource)/progmodes/prog-mode.el \
	$(lispsource)/emacs-lisp/lisp-mode.el \
	$(lispsource)/textmodes/text-mode.el \
	$(lispsource)/textmodes/fill.el \
	$(lispsource)/newcomment.el \
	$(lispsource)/replace.el \
	$(lispsource)/emacs-lisp/tabulated-list.el \
	$(lispsource)/buff-menu.el \
	$(lispsource)/fringe.el \
	$(lispsource)/emacs-lisp/regexp-opt.el \
	$(lispsource)/image.el \
	$(lispsource)/international/fontset.el \
	$(lispsource)/dnd.el \
	$(lispsource)/tool-bar.el \
	$(lispsource)/dynamic-setting.el \
	$(lispsource)/x-dnd.el \
	$(lispsource)/term/common-win.el \
	$(lispsource)/term/x-win.el \
	$(lispsource)/w32-vars.el \
	$(lispsource)/term/w32-win.el \
	$(lispsource)/ls-lisp.el \
	$(lispsource)/disp-table.el \
	$(lispsource)/w32-common-fns.el \
	$(lispsource)/dos-w32.el \
	$(lispsource)/w32-fns.el \
	$(lispsource)/dos-fns.el \
	$(lispsource)/dos-vars.el \
	$(lispsource)/term/pc-win.el \
	$(lispsource)/term/internal.el \
	$(lispsource)/term/ns-win.el \
	$(lispsource)/mwheel.el \
	$(lispsource)/emacs-lisp/float-sup.el \
	$(lispsource)/vc/vc-hooks.el \
	$(lispsource)/vc/ediff-hook.el \
	$(lispsource)/electric.el \
	$(lispsource)/uniquify.el \
	$(lispsource)/tooltip.el


### lisp.mk ends here
