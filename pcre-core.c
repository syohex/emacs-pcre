/*
  Copyright (C) 2016 by Syohei YOSHIDA

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <emacs-module.h>

#include <pcre.h>

#define MAX_MATCH 999

int plugin_is_GPL_compatible;

static char*
retrieve_string(emacs_env *env, emacs_value str, ptrdiff_t *size)
{
	*size = 0;

	env->copy_string_contents(env, str, NULL, size);
	char *p = malloc(*size);
	if (p == NULL) {
		*size = 0;
		return NULL;
	}
	env->copy_string_contents(env, str, p, size);

	return p;
}

static int
point(emacs_env *env)
{
	emacs_value Qpoint = env->intern(env, "point");
	emacs_value p = env->funcall(env, Qpoint, 0, NULL);
	return (int)env->extract_integer(env, p);
}

static emacs_value
pcre_string_match(emacs_env *env, ptrdiff_t nargs, emacs_value args[], bool savedata)
{
	ptrdiff_t reg_size;
	char *regexp = retrieve_string(env, args[0], &reg_size);
	bool buffer;
	int max_match = MAX_MATCH;

	int flags = env->extract_integer(env, args[2]);
	int offset;

	if (nargs >= 4) {
		intmax_t v = env->extract_integer(env, args[3]);

		// 0: search string, not 0: search buffer
		// >0: forward search <0: backward search
		buffer = (v != 0);
		if (v > 0) {
			offset = point(env) - 1;
		} else if (v < 0) {
			offset = 0;
		}
	} else {
		buffer = false;
		offset = 0;
	}

	if (nargs >= 5) {
		intmax_t n = env->extract_integer(env, args[4]);
		if (n == -1) {
			max_match = MAX_MATCH;
		} else {
			max_match = (int)n;
		}
	}

	const char *errstr = NULL;
	int erroff = 0;
	pcre *re = pcre_compile(regexp, flags, &errstr, &erroff, NULL);
	free(regexp);
	if (re == NULL) {
		emacs_value errmsg = env->make_string(env, errstr, strlen(errstr));
		env->non_local_exit_signal(env, env->intern(env, "error"), errmsg);
		return env->intern(env, "nil");
	}

	ptrdiff_t str_size;
	char *str = retrieve_string(env, args[1], &str_size);
	int match[MAX_MATCH];
	int regno = pcre_exec(re, NULL, str, (size_t)str_size - 1, offset, 0, match, max_match);
	free(str);

	pcre_free(re);

	if (regno < 0)
		return env->intern(env, "nil");

	if (!savedata)
		return env->intern(env, "t");

	size_t len = regno * 2;
	emacs_value *match_args = malloc(len * sizeof(emacs_value));
	if (match_args == NULL)
		return env->intern(env, "nil");

	// buffer position starts from 1, while string position starts from 0
	int pos = buffer ? 1 : 0;
	for (int i = 0; i < regno; ++i) {
		size_t idx = i * 2;
		match_args[idx] = env->make_integer(env, match[idx]+pos);
		match_args[idx+1] = env->make_integer(env, match[idx+1]+pos);
	}

	emacs_value v = env->funcall(env, env->intern(env, "list"), len, match_args);
	emacs_value Qset_match_data = env->intern(env, "set-match-data");
	emacs_value set_match_data_args[] = {v};
	env->funcall(env, Qset_match_data, 1, set_match_data_args);

	return env->intern(env, "t");
}

static emacs_value
Fpcre_string_match(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	return pcre_string_match(env, nargs, args, true);
}


static emacs_value
Fpcre_string_match_p(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	return pcre_string_match(env, nargs, args, false);
}

static bool
case_fold_search(emacs_env *env)
{
	emacs_value args[] = {env->intern(env, "case-fold-search")};
	emacs_value ret = env->funcall(env, env->intern(env, "symbol-value"), 1, args);

	return env->is_not_nil(env, ret);
}

static emacs_value
Fpcre_flags(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	emacs_value syms = args[0];
	unsigned long flags;

	flags = case_fold_search(env) ? PCRE_CASELESS : 0;

	size_t len = (size_t)env->vec_size(env, syms);
	for (size_t i = 0; i < len; ++i) {
		emacs_value sym = env->vec_get(env, syms, i);

		if (env->eq(env, sym, env->intern(env, "ignorecase"))) {
			flags |= PCRE_CASELESS;
		} else if (env->eq(env, sym, env->intern(env, "multiline"))) {
			flags |= PCRE_MULTILINE;
		} else if (env->eq(env, sym, env->intern(env, "dotall"))) {
			flags |= PCRE_DOTALL;
		} else if (env->eq(env, sym, env->intern(env, "extended"))) {
			flags |= PCRE_EXTENDED;
		} else {
			// unsupported flags
		}
	}

	return env->make_integer(env, (intmax_t)flags);
}

static void
bind_function(emacs_env *env, const char *name, emacs_value Sfun)
{
	emacs_value Qfset = env->intern(env, "fset");
	emacs_value Qsym = env->intern(env, name);
	emacs_value args[] = { Qsym, Sfun };

	env->funcall(env, Qfset, 2, args);
}

static void
provide(emacs_env *env, const char *feature)
{
	emacs_value Qfeat = env->intern(env, feature);
	emacs_value Qprovide = env->intern (env, "provide");
	emacs_value args[] = { Qfeat };

	env->funcall(env, Qprovide, 1, args);
}

int
emacs_module_init(struct emacs_runtime *ert)
{
	emacs_env *env = ert->get_environment(ert);

#define DEFUN(lsym, csym, amin, amax, doc, data) \
	bind_function (env, lsym, env->make_function(env, amin, amax, csym, doc, data))

	DEFUN("pcre--core-string-match", Fpcre_string_match, 3, 5, NULL, NULL);
	DEFUN("pcre--core-string-match-p", Fpcre_string_match_p, 3, 5, NULL, NULL);
	DEFUN("pcre--core-flags", Fpcre_flags, 1, 1, NULL, NULL);
#undef DEFUN

	provide(env, "pcre-core");
	return 0;
}

/*
  Local Variables:
  c-basic-offset: 8
  indent-tabs-mode: t
  End:
*/
