{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
     system = "x86_64-linux";
     pkgs = nixpkgs.legacyPackages."${system}";
  in
  {
    devShells.${system}.default = pkgs.stdenv.mkDerivation {
      name = "fs-devshell";
      buildInputs = [ pkgs.tesseract5 ];
      nativeBuildInputs = [ pkgs.fsharp pkgs.dotnet-sdk pkgs.fsautocomplete pkgs.nuget ];
    };
  };
}
