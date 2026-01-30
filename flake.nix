{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    oxcaml.url = "github:oxcaml/opam-repository";
  };
  outputs =
    {
      self,
      flake-utils,
      opam-nix,
      nixpkgs,
      oxcaml,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        scope = on.buildOpamProject { } package ./. { ocaml-base-compiler = "*"; };
        overlay = final: prev: {
          # Your overrides go here
        };
      in
      {
        legacyPackages = scope.overrideScope overlay;

        packages.default = self.legacyPackages.${system}.${package};
      }
    );
}
