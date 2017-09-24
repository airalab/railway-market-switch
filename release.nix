let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages;

  jobs = rec {

    tarball =
      pkgs.releaseTools.sourceTarball {
        name = "railway-market-switch";
        src = <railway-market-switch>;
        buildInputs = (with pkgs; with haskellPackages;
        [ base web3 z21 miku warp text queue logging transformers ]);
      };

    build =
      { system ? builtins.currentSystem }:

      let pkgs = import <nixpkgs> { inherit system; }; in
      pkgs.releaseTools.nixBuild {
        name = "railway-market-switch";
        src = jobs.tarball;
      };
  };
in
  jobs
