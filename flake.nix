{
  inputs.nixpkgs.url = "nixpkgs";
  description = "A flake for Data.Choice";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        choice = final.haskellPackages.callCabal2nix "choice" ./. { };
      });

      packages = forAllSystems (system: {
        choice = nixpkgsFor.${system}.choice;
      });

      defaultPackage = forAllSystems (system: self.packages.${system}.choice);

      checks = self.packages;

      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in
        haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.choice ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
        });
    };
}
