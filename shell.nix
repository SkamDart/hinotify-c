with import <nixpkgs> {};

let
  ahpkgs = anduril.haskell.lts1316;
  haskellDeps = ps: with ps; [
    base
    bytestring
    containers
    directory
    filepath
    hinotify
    stm
    stylish-haskell
  ];
  ghc = ahpkgs.ghcWithPackages haskellDeps;
  nixPackages = [
    ghc
    pkgs.gdb
    ahpkgs.cabal-install
  ];
in
stdenv.mkDerivation {
  name = "entities-hs";
  buildInputs = nixPackages;
}
