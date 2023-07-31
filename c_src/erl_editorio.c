#include <erl_nif.h>
// #include <ncurses.h>
#include <string.h>
#include <sys/epoll.h>
#include <unistd.h>

static ERL_NIF_TERM getch_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // set up epoll to read a character at a time from stdin
    int epoll_fd = epoll_create1(STDIN_FILENO);
    struct epoll_event event;
    event.events = EPOLLIN;
    event.data.fd = STDIN_FILENO;
    epoll_ctl(epoll_fd, EPOLL_CTL_ADD, 0, &event);
    // now read from stdin.  If it's an escape-key sequence, read the rest of the sequence
    // and return the appropriate atom.  Otherwise, return the character.
    // Use read or fread instead of ncurses getch() because getch() doesn't work with epoll.
    // We actually want to read these keys in, in addition to the "ordinary" characters:
    // delete, backspace, up, down, left, right, home, end, enter.
    // We want to return the same atoms as for the corresponding keys.
    if (epoll_wait(epoll_fd, &event, 1, 10) > 0) {
        char c;
        read(0, &c, 1);
        if (c == 27) { // escape key
            if (epoll_wait(epoll_fd, &event, 1, 10) > 0) {
                read(STDIN_FILENO, &c, 1);
                if (c == 91) { // '['
                    if (epoll_wait(epoll_fd, &event, 1, 10) > 0) {
                        read(STDIN_FILENO, &c, 1);
                        switch (c) {
                            case 'A': // up
                                return enif_make_atom(env, "up");
                            case 'B': // down
                                return enif_make_atom(env, "down");
                            case 'C': // right
                                return enif_make_atom(env, "right");
                            case 'D': // left
                                return enif_make_atom(env, "left");
                            case 'H': // home
                                return enif_make_atom(env, "home");
                            case 'F': // end
                                return enif_make_atom(env, "end");
                            default:
                                if (epoll_wait(epoll_fd, &event, 1, 10) > 0) {
                                    read(STDIN_FILENO, &c, 1);
                                    if (c == '~') {
                                        switch (c) {
                                            case '1': // home
                                            case '7': // home
                                                return enif_make_atom(env, "home");
                                            case '2': // insert
                                                return enif_make_atom(env, "insert");
                                            case '3': // delete
                                                return enif_make_atom(env, "delete");
                                            case '4': // end
                                            case '8': // end
                                                return enif_make_atom(env, "end");
                                            default:
                                                return enif_make_atom(env, "unknown");
                                        }
                                    } else {
                                        return enif_make_atom(env, "unknown");
                                    }
                                } else {
                                    return enif_make_atom(env, "unknown");
                                }
                        }
                    } else {
                        return enif_make_atom(env, "unknown");
                    }
                } else {
                    return enif_make_atom(env, "unknown");
                }
            } else {
                ungetc(c, stdin);
                return enif_make_atom(env, "escape");
            }
        } else {
            return enif_make_int(env, c);
        }
    } else {
        return enif_make_atom(env, "timeout");
    }
}

static ERL_NIF_TERM clear_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // clear();
    // move(0, 0);
    // refresh();
    return enif_make_int(env, "ok");
}

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
{
    // WINDOW *win = initscr();
    // cbreak();
    // noecho();
    // *priv = (void *) win;
    return 0;
}

static void unload(ErlNifEnv *env, void *priv)
{
    // nocbreak();
    // echo();
    // endwin();
}

static ErlNifFunc nif_funcs[] = {
    {"getch_internal", 0, getch_nif},
    // {"clear_internal", 0, clear_nif}
};

ERL_NIF_INIT(erl_editorio, nif_funcs, load, NULL, NULL, unload)