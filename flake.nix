{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs =
    { flake-parts, nixpkgs, ... }@inputs:
    let
      hs-project =
        {
          pkgs,
          isShell ? false,
        }:
        pkgs.haskellPackages.developPackage {
          root = ./.;
          returnShellEnv = isShell;
          modifier =
            drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs; [
              mkdocs haskell-language-server cabal-install
            ]);
        };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.platforms.unix;
      perSystem =
        { pkgs, ... }:
        {
          packages.default = hs-project { inherit pkgs; };
          devShells.default = hs-project {
            inherit pkgs;
            isShell = true;
          };
        };
    };
}
