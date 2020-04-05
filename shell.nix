with import <nixpkgs> {};

let
  haskellDeps = ps: with ps; [
    base
    bytestring
    containers
    directory
    filepath
    hinotify
    lens
    stm
    stylish-haskell
  ];
  ghc = haskellPackages.ghcWithPackages haskellDeps;
  nixPackages = [
    ghc
    pkgs.gdb
    haskellPackages.cabal-install
  ];
in
stdenv.mkDerivation {
  name = "entities-hs";
  buildInputs = nixPackages;
}
