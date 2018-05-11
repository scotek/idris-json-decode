{ pkgs ? import /home/ssdd/dev/nixpkgs {}, ...} :
let
  idrisWithPackages = pkgs.idrisPackages.with-packages;
  idris = idrisWithPackages (with pkgs.idrisPackages; [ pkgs.idrisPackages.builtins contrib derive ]);
in pkgs.stdenv.mkDerivation rec {
  name = "json-decode";
  buildInputs = [ idris ];
  buildPhase = ''
    idris --build ${name}.ipkg
  '';
  installPhase = ''
    mkdir -p $out
    cp ${name} $out/bin/${name}
  '';
}
