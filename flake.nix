{
  description = "Haskell bindings to libnfc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "haskell-nfc";
        exampleName = "print-mifare-uid-forever";

        overlay = self: super:
          {
            haskellPackages = super.haskellPackages.override ({
              overrides = hsSelf: hsSuper: {
                nfc = super.libnfc;
                haskell-nfc = hsSelf.callCabal2nix packageName ./. {};
                print-mifare-uid-forever = hsSelf.callCabal2nix exampleName ./examples {};
              };
            });
          };

        pkgs = nixpkgs.legacyPackages.${system}.appendOverlays([ overlay ]);

        haskellPackages = pkgs.haskellPackages;

      in {
        overlays.default = overlay;

        packages.${packageName} = haskellPackages.${packageName};
        packages.${exampleName} = haskellPackages.${exampleName};

        defaultPackage = self.packages.${system}.${exampleName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghcid
            cabal-install
            haskellPackages.${packageName}
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
