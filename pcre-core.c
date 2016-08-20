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

static emacs_value
pcre_match_string(emacs_env *env, ptrdiff_t nargs, emacs_value args[], bool savedata)
{
	ptrdiff_t reg_size;
	char *regexp = retrieve_string(env, args[0], &reg_size);

	int flags = 0;
	int erroff = 0;
	const char *errstr = NULL;
	bool buffer;
	int max_match = MAX_MATCH;

	if (nargs >= 3) {
		buffer = env->is_not_nil(env, args[2]);
	} else {
		buffer = false;
	}

	if (nargs >= 4) {
		intmax_t n = env->extract_integer(env, args[3]);
		if (n == -1) {
			max_match = MAX_MATCH;
		} else {
			max_match = (int)n;
		}
	}

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
	int regno = pcre_exec(re, NULL, str, (size_t)str_size - 1, 0, 0, match, max_match);
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
Fpcre_match_string(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	return pcre_match_string(env, nargs, args, true);
}


static emacs_value
Fpcre_match_string_p(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	return pcre_match_string(env, nargs, args, false);
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

	DEFUN("pcre-match-string", Fpcre_match_string, 2, 4, NULL, NULL);
	DEFUN("pcre-match-string-p", Fpcre_match_string_p, 2, 4, NULL, NULL);
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
