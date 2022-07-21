{
  description = "Haskell bindings to libnfc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {};
      in {
        packages = rec {
          nfc = pkgs.haskellPackages.callPackage ./nix/nfc.nix {};
          defaultPackage = nfc;
        };
      }
    );
}
