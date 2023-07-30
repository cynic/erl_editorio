#include <erl_nif.h>
#include <ncurses.h>
#include <string.h>

/* key_atom, parts of mapping, and key_to_elixir are from ex_ncurses by Jim Freeze.
License for these is as follows.

Copyright (c) 2015-2018 Jim Freeze

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

struct key_atom {
    int code;
    const char *atom;
};

static struct key_atom mapping[] = {
    { KEY_DOWN, "down" },
    { KEY_UP, "up" },
    { KEY_LEFT, "left" },
    { KEY_RIGHT, "right" },
    { KEY_HOME, "home" },
    { KEY_BACKSPACE, "backspace" },
    { KEY_F0, "f0" },
    { KEY_F(1), "f1" },
    { KEY_F(2), "f2" },
    { KEY_F(3), "f3" },
    { KEY_F(4), "f4" },
    { KEY_F(5), "f5" },
    { KEY_F(6), "f6" },
    { KEY_F(7), "f7" },
    { KEY_F(8), "f8" },
    { KEY_F(9), "f9" },
    { KEY_F(10), "f10" },
    { KEY_F(11), "f11" },
    { KEY_F(12), "f12" },
    { KEY_F(13), "f13" },
    { KEY_F(14), "f14" },
    { KEY_ENTER, "enter" },
    { KEY_COPY, "copy" },
    { KEY_END, "end" },
};

ERL_NIF_TERM key_to_elixir(ErlNifEnv *env, int code)
{
    // Handle normal characters
    if (code < 256)
        return enif_make_int(env, code);

    // Handle special characters
    size_t i;
    for (i = 0; i < sizeof(mapping) / sizeof(mapping[0]); i++) {
        if (code == mapping[i].code)
            return enif_make_atom(env, mapping[i].atom);
    }

    // If no other option.
    return enif_make_int(env, code);
}

static ERL_NIF_TERM getch_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cbreak();
    noecho();
    int ch = getch();
    nocbreak();
    echo();
    return key_to_elixir(env, ch);
}

static ERL_NIF_TERM clear_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    clear();
    move(0, 0);
    refresh();
    return enif_make_int(env, "ok");
}

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
{
    WINDOW *win = initscr();
    *priv = (void *) win;
    return 0;
}

static void unload(ErlNifEnv *env, void *priv)
{
    endwin();
}

static ErlNifFunc nif_funcs[] = {
    {"getch", 0, getch_nif},
    {"clear", 0, clear_nif}
};

ERL_NIF_INIT(ncurses_nif, nif_funcs, load, NULL, NULL, unload)