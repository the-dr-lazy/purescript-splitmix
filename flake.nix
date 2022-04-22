{
  description = "Fast Splittable PRNG";

  inputs = {
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    easy-purescript-src = {
      url = "github:justinwoo/easy-purescript-nix/5dca2f0f3b9ec0bceabb23fa1fd2b5f8ec30fa53";
      flake = false;
    };
  };

  outputs = { nixpkgs-stable, nixpkgs-unstable, easy-purescript-src, utils, ... }:
    let name = "purescript-splitmix";
    in
    utils.lib.eachDefaultSystem (system:
      let
        easy-purescript = import easy-purescript-src { pkgs = pkgs.stable; };
        pkgs = {
          stable = import nixpkgs-stable {
            inherit system;
            overlays = [ (_: _: { inherit (easy-purescript) purs-tidy pulp spago; purescript = easy-purescript.purs-0_15_2; }) ];
          };
          unstable = import nixpkgs-unstable { inherit system; };
        };
      in
      {
        devShell = import ./shell.nix {
          inherit name pkgs;
        };
      });
}
