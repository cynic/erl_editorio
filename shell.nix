{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs.buildPackages; [
      erlang rebar3 lfe ncurses elixir gcc
    ];
}
