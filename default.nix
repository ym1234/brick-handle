{ mkDerivation, base, brick, stdenv, vty }:
mkDerivation {
  pname = "brick-handle";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ base brick vty ];
  executableHaskellDepends = [ base brick vty ];
  license = stdenv.lib.licenses.mit;
}
